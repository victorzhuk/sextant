(in-package :sextant)

;;; ============================================================
;;; SBCL Debugger Integration
;;; Hooks into SBCL's condition system and debug internals
;;; ============================================================

(defvar *dap-debugger-active* nil
  "Whether the DAP debugger is currently active.")

(defvar *dap-stopped-callback* nil
  "Function called when a condition is caught. Takes (condition thread).")

(defvar *dap-breakpoints* (make-hash-table :test 'equal)
  "Hash of file-path -> list of line numbers for breakpoints.")

(defvar *dap-function-breakpoints* (make-hash-table :test 'equal)
  "Hash of function-name -> t for function breakpoints.")

(defvar *dap-break-on-exceptions* nil
  "Whether to break on all conditions.")

(defvar *dap-current-condition* nil
  "The condition currently being debugged.")

(defvar *dap-current-frames* nil
  "Cached stack frames for the current stopped state.")

(defvar *dap-stepping-mode* nil
  "Current stepping mode: nil, :into, :over, :out.")

(defvar *dap-frame-vars-cache* (make-hash-table)
  "Cache of frame-id -> variable list.")

(defvar *dap-next-var-ref* 1
  "Next variable reference ID for DAP.")

(defvar *dap-var-ref-map* (make-hash-table)
  "Map of variable reference ID -> (frame-index scope-type).")

(defvar *dap-debuggee-threads* (make-hash-table :test 'eq)
  "Threads currently running debuggee code. The debugger hook blocks only
these threads on continue: without this scoping, an error in any other thread
of the image (the LSP main loop, the diagnostics worker) would stall the whole
server until a DAP client sends a continue.")

(defun debuggee-thread-p (thread)
  (bt:with-lock-held (*dap-continue-lock*)
    (gethash thread *dap-debuggee-threads*)))

(defun register-debuggee-thread (&optional (thread (bt:current-thread)))
  (bt:with-lock-held (*dap-continue-lock*)
    (setf (gethash thread *dap-debuggee-threads*) t)))

(defun unregister-debuggee-thread (&optional (thread (bt:current-thread)))
  (bt:with-lock-held (*dap-continue-lock*)
    (remhash thread *dap-debuggee-threads*)))

(defun clear-debuggee-threads ()
  (bt:with-lock-held (*dap-continue-lock*)
    (clrhash *dap-debuggee-threads*)))

(defvar *dap-previous-debugger-hook* nil
  "The *invoke-debugger-hook* value present before ours was installed, so it
can be restored on disconnect instead of being clobbered.")

(defun install-dap-debugger-hook ()
  "Install our custom debugger hook to intercept conditions in debuggee threads."
  (unless *dap-previous-debugger-hook*
    (setf *dap-previous-debugger-hook* sb-ext:*invoke-debugger-hook*))
  (setf sb-ext:*invoke-debugger-hook*
        (lambda (condition hook)
          (if (and *dap-debugger-active*
                   *dap-stopped-callback*
                   (debuggee-thread-p (bt:current-thread))
                   (or *dap-break-on-exceptions*
                       (typep condition 'error)))
              (progn
                ;; Reset before stopping (under the lock the waiter uses) so a
                ;; continue racing with the stop cannot be lost
                (bt:with-lock-held (*dap-continue-lock*)
                  (setf *dap-should-continue* nil))
                (setf *dap-current-condition* condition)
                (setf *dap-current-frames* (capture-stack-frames))
                (clrhash *dap-frame-vars-cache*)
                (setf *dap-next-var-ref* 1)
                (clrhash *dap-var-ref-map*)
                (funcall *dap-stopped-callback* condition
                         sb-thread:*current-thread*)
                ;; Block this thread until the debugger tells us to continue
                (dap-wait-for-continue))
              ;; Not a debuggee stop: defer to whatever hook was installed
              ;; before ours (or the default debugger)
              (when *dap-previous-debugger-hook*
                (funcall *dap-previous-debugger-hook* condition hook)))))
  (lsp-log "DAP debugger hook installed"))

(defun uninstall-dap-debugger-hook ()
  "Remove our custom debugger hook, restoring the previous one."
  (when *dap-previous-debugger-hook*
    (setf sb-ext:*invoke-debugger-hook* *dap-previous-debugger-hook*)
    (setf *dap-previous-debugger-hook* nil))
  (setf *dap-debugger-active* nil)
  (clear-debuggee-threads)
  (lsp-log "DAP debugger hook removed"))

(defvar *dap-continue-lock* (bt:make-lock "dap-continue-lock"))
(defvar *dap-continue-cv* (bt:make-condition-variable :name "dap-continue-cv"))
(defvar *dap-should-continue* nil)

(defun dap-wait-for-continue ()
  "Block the current thread until the DAP client sends a continue command.
*dap-should-continue* is reset by the debugger hook when the stop happens,
not here, so a continue arriving between the stop and the wait is not lost."
  (bt:with-lock-held (*dap-continue-lock*)
    (loop until *dap-should-continue*
          do (bt:condition-wait *dap-continue-cv* *dap-continue-lock*))))

(defun dap-signal-continue ()
  "Signal the debugged thread to continue execution."
  (bt:with-lock-held (*dap-continue-lock*)
    (setf *dap-should-continue* t)
    (bt:condition-notify *dap-continue-cv*)))

;;; --- Stack Frame Capture ---

(defun capture-stack-frames ()
  "Capture the current backtrace as a list of frame info alists.
Each entry: (:name :file :line :column :frame-index)"
  (let ((frames nil)
        (index 0))
    (handler-case
        (sb-debug:map-backtrace
         (lambda (frame)
           (let* ((debug-fun (handler-case
                                 (sb-di:frame-debug-fun frame)
                               (error () nil)))
                  (name (if debug-fun
                            (handler-case
                                (sb-di:debug-fun-name debug-fun)
                              (error () "unknown"))
                            "unknown"))
                  (location (handler-case
                                (sb-di:frame-code-location frame)
                              (error () nil)))
                  (source (when location
                            (handler-case
                                (sb-di:code-location-debug-source location)
                              (error () nil))))
                  (file (when source
                          (handler-case
                              (sb-di:debug-source-namestring source)
                            (error () nil))))
                  (line 0))
             (push (list :name (format nil "~a" name)
                         :file file
                         :line line
                         :column 0
                         :frame-index index)
                   frames)
             (incf index)))
         :from :current-frame)
      (error (e)
        (lsp-log "Error capturing frames: ~a" e)))
    (nreverse frames)))

(defun get-frame-locals (frame-index)
  "Get local variables for a stack frame by index.
Returns list of (:name :value :type)."
  (or (gethash frame-index *dap-frame-vars-cache*)
      (let ((vars nil))
        (handler-case
            (sb-debug:map-backtrace
             (lambda (frame)
               (when (= (sb-di:frame-number frame) frame-index)
                 (handler-case
                     (let ((debug-fun (sb-di:frame-debug-fun frame)))
                       (handler-case
                           (sb-di:do-debug-fun-vars (var debug-fun)
                             (handler-case
                                 (let* ((loc (sb-di:frame-code-location frame))
                                        (valid (sb-di:debug-var-validity var loc)))
                                   (when (eq valid :valid)
                                     (let ((name (sb-di:debug-var-symbol var))
                                           (value (sb-di:debug-var-value var frame)))
                                       (push (list :name (format nil "~a" name)
                                                   :value (format nil "~s" value)
                                                   :type (format nil "~a" (type-of value)))
                                             vars))))
                               (error () nil)))
                         (error () nil)))
                   (error () nil))
                 (return-from get-frame-locals
                   (setf (gethash frame-index *dap-frame-vars-cache*)
                         (nreverse vars)))))
             :from :current-frame)
          (error (e)
            (lsp-log "Error getting frame locals: ~a" e)))
        (setf (gethash frame-index *dap-frame-vars-cache*)
              (nreverse vars)))))

(defun allocate-var-ref (frame-index scope-type)
  "Allocate a variable reference ID for a frame/scope pair."
  (let ((ref *dap-next-var-ref*))
    (incf *dap-next-var-ref*)
    (setf (gethash ref *dap-var-ref-map*)
          (list frame-index scope-type))
    ref))

(defun get-condition-restarts ()
  "Get available restarts for the current condition.
Returns list of (:name :description :index)."
  (when *dap-current-condition*
    (let ((restarts (compute-restarts *dap-current-condition*))
          (result nil)
          (index 0))
      (dolist (restart restarts)
        (push (list :name (format nil "~a" (restart-name restart))
                    :description (format nil "~a" restart)
                    :index index)
              result)
        (incf index))
      (nreverse result))))

(defun invoke-restart-by-index (index)
  "Invoke a restart by its index in the current restart list."
  (when *dap-current-condition*
    (let ((restarts (compute-restarts *dap-current-condition*)))
      (when (< index (length restarts))
        (invoke-restart (nth index restarts))))))

(defun eval-in-context (expression &key (allow-debugger nil))
  "Evaluate an expression string in the current debug context.
When ALLOW-DEBUGGER is true, let errors propagate to the debugger hook."
  (if allow-debugger
      ;; Let errors trigger the debugger hook (for REPL eval)
      (let ((form (read-from-string expression)))
        (format nil "~s" (eval form)))
      ;; Catch errors and return as text (for hover eval etc.)
      (handler-case
          (let ((form (read-from-string expression)))
            (format nil "~s" (eval form)))
        (error (e)
          (format nil "Error: ~a" e)))))

(defun list-all-threads ()
  "List all SBCL threads. Returns list of (:id :name)."
  (let ((threads (sb-thread:list-all-threads))
        (result nil)
        (id 1))
    (dolist (thread threads)
      (push (list :id id
                  :name (sb-thread:thread-name thread))
            result)
      (incf id))
    (nreverse result)))

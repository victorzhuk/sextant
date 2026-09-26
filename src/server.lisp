(in-package :sextant)

;;; ============================================================
;;; LSP Server Main Loop
;;; Reads messages from stdin, dispatches, writes responses
;;; ============================================================

(defun open-log-file (log-file)
  "Open LOG-FILE for appending. Returns NIL (and logs to stderr) on failure
rather than killing the server."
  (handler-case
      (open log-file :direction :output
                     :if-exists :append
                     :if-does-not-exist :create)
    (error (e)
      (format *error-output* "sextant: cannot open log file ~a: ~a~%" log-file e)
      nil)))

(defun start-server (&key (log-file nil) (dap-port 6009))
  "Start the Sextant LSP server on stdio with DAP server on TCP."
  (when log-file
    (setf *lsp-log* (open-log-file log-file)))
  ;; Rebind stdio to explicit UTF-8 streams so message framing (which counts
  ;; UTF-8 bytes) stays correct regardless of the locale's default format.
  (let ((in (make-stdio-stream 0 :input))
        (out (make-stdio-stream 1 :output)))
    (when in (setf *lsp-input* in))
    (when out (setf *lsp-output* out)))
  (lsp-log "Sextant LSP server starting...")
  ;; Start DAP server on TCP in background
  (start-dap-server :port dap-port)
  (unwind-protect
       (loop
         (let ((msg (handler-case (read-lsp-message *lsp-input*)
                      (error (e)
                        (lsp-log "Input read error: ~a" e)
                        nil))))
           (cond
             ;; Parse errors are skipped, not fatal: a malformed message must
             ;; not take down the whole server.
             ((eq msg :parse-error))
             ((null msg)
              (lsp-log "EOF on input, exiting")
              (return))
             (t
              (handler-case (dispatch-message msg)
                (error (e)
                  (lsp-log "Dispatch error: ~a" e)))))))
    (stop-dap-server)
    (when *lsp-log*
      (lsp-log "Sextant LSP server shutting down")
      (close *lsp-log*)
      (setf *lsp-log* nil))))

(defun dispatch-message (msg)
  "Dispatch one parsed JSON-RPC message."
  (let ((method (json-get msg "method"))
        (id (json-get msg "id"))
        (params (json-get msg "params")))
    (cond
      ;; Request (has id and method)
      ((and id method)
       (let ((response (handle-request method id params)))
         (write-lsp-message response *lsp-output*)))
      ;; Response to our request (has id, no method) - ignore for now
      ((and id (not method))
       (lsp-log "Got response for id ~a" id))
      ;; Notification (has method, no id)
      (method
       (handler-case (handle-notification method params)
         (error (e)
           (lsp-log "Notification handler error for ~a: ~a" method e))))
      (t
       (lsp-log "Unknown message format")))))

(in-package :sextant)

;;; ============================================================
;;; Source File Indexer
;;; Provides ANSI-portable definitions and references by reading
;;; source files with CL:READ — no sb-introspect needed.
;;; ============================================================

;;; --- Index data structures ---

(defstruct index-entry
  "A definition found by the source indexer."
  (name     "" :type string)    ; Uppercased symbol name
  (package  "" :type string)    ; Package name, or "" if unknown
  (kind     :function)          ; :function :macro :generic :method :variable
                                ; :parameter :constant :class :struct :condition
  (file     "" :type string)    ; Absolute filesystem path
  (line     0  :type fixnum)    ; 0-based line
  (col      0  :type fixnum)    ; 0-based column
  (arglist  nil)                ; Lambda list (for functions/macros), or NIL
  (uri      nil))               ; file:// URI (cached)

(defstruct ref-entry
  "A symbol reference found by the source indexer."
  (name "" :type string)
  (file "" :type string)
  (line 0  :type fixnum)
  (col  0  :type fixnum)
  (uri  nil))

;;; --- Global index tables ---

(defvar *definition-index* (make-hash-table :test 'equal)
  "Map from uppercased symbol name to list of index-entry structs.")

(defvar *reference-index* (make-hash-table :test 'equal)
  "Map from uppercased symbol name to list of ref-entry structs.")

(defvar *indexed-files* (make-hash-table :test 'equal)
  "Set of files that have been indexed (path -> timestamp).")

(defvar *index-lock* (bt:make-lock "source-index-lock")
  "Lock protecting all index tables.")

;;; --- Definition form recognition ---
;;; *definition-operators* is defined in document.lisp (loaded earlier) and
;;; shared with the document-level definition searchers.

(defun definition-form-p (form)
  "If FORM is a definition form, return (kind name arglist) or NIL.
NAME is returned as an uppercase string."
  (when (and (consp form) (symbolp (car form)))
    (let* ((op-name (symbol-name (car form)))
           (entry (assoc op-name *definition-operators* :test #'string-equal)))
      (when (and entry (cdr form))
        (let ((kind (cdr entry))
              (name-form (second form)))
          ;; Extract the symbol name
          (let ((name (cond
                        ;; (defmethod foo :around ((x bar) y) ...)
                        ;; or (defmethod foo ((x bar) y) ...)
                        ((and (eq kind :method)
                              (symbolp name-form))
                         (symbol-name name-form))
                        ;; (defstruct (foo (:constructor ...)) slot ...)
                        ;; or (defstruct foo slot ...)
                        ((and (eq kind :struct)
                              (consp name-form))
                         (let ((sname (car name-form)))
                           (when (symbolp sname)
                             (symbol-name sname))))
                        ((symbolp name-form)
                         (symbol-name name-form))
                        ;; (defun (setf foo) ...)
                        ((and (consp name-form)
                              (eq (car name-form) 'setf)
                              (symbolp (second name-form)))
                         (format nil "(SETF ~a)" (symbol-name (second name-form))))
                        (t nil))))
            (when name
              (let ((arglist (extract-arglist kind form)))
                (list kind (string-upcase name) arglist)))))))))

(defun extract-arglist (kind form)
  "Extract the lambda list from a definition FORM, if applicable."
  (case kind
    ((:function :macro :generic)
     (when (>= (length form) 3)
       (third form)))
    (:method
     ;; Skip qualifiers to find the lambda list
     (let ((rest (cddr form)))
       (loop for item in rest
             when (listp item) return item)))
    (t nil)))

;;; --- Read-based file indexing ---

(defun index-file (path)
  "Index a single source file at PATH.
Reads all top-level forms with CL:READ and records definitions.
Returns the number of definitions found."
  (let ((definitions nil)
        (file-path nil))
    (handler-case
        (with-open-file (stream path :direction :input
                                     :if-does-not-exist nil)
          (when stream
            (setf file-path (namestring (truename path)))
            (let ((*package* (or (find-package "COMMON-LISP-USER")
                                 *package*))
                  (*read-eval* nil)
                  (text (alexandria:read-file-into-string path)))
              ;; Read all top-level forms, tracking file position
              (let ((form-positions nil))
                ;; First pass: collect file positions before each form
                (file-position stream 0)
                (loop
                  (let ((pos (file-position stream)))
                    (handler-case
                        (let ((form (read stream nil *eof-form*)))
                          (when (eq form *eof-form*) (return))
                          (push (cons pos form) form-positions))
                      (error () (return)))))
                ;; Process collected forms
                (dolist (pos-form (nreverse form-positions))
                  (let* ((pos (car pos-form))
                         (form (cdr pos-form))
                         (line-col (offset-to-line-col text
                                     (min pos (length text))))
                         (def-info (definition-form-p form)))
                    (when def-info
                      (destructuring-bind (kind name arglist) def-info
                        (push (make-index-entry
                               :name name
                               :package (package-name *package*)
                               :kind kind
                               :file file-path
                               :line (car line-col)
                               :col (cdr line-col)
                               :arglist arglist
                               :uri (path-to-uri file-path))
                              definitions)))))
                ;; Store definitions first so references to this file's own
                ;; definitions are indexed in the same pass
                (bt:with-lock-held (*index-lock*)
                  (remove-file-from-index file-path)
                  (dolist (def definitions)
                    (push def (gethash (index-entry-name def) *definition-index*)))
                  (setf (gethash file-path *indexed-files*) (get-universal-time)))
                ;; Then collect references from this file
                (index-references-in-text text file-path)))))
      (error (e)
        (lsp-log "Index error for ~a: ~a" path e)))
    (length definitions)))

(defun index-references-in-text (text file-path)
  "Scan TEXT for symbol references and add them to *reference-index*.
Finds occurrences of known definition names in non-comment, non-string positions."
  (let ((refs nil)
        ;; Snapshot the set of known definition names once, outside the hot scan
        (known (bt:with-lock-held (*index-lock*)
                 (let ((names (make-hash-table :test 'equal)))
                   (maphash (lambda (name entries)
                              (declare (ignore entries))
                              (setf (gethash name names) t))
                            *definition-index*)
                   names))))
    (dolist (occ (scan-symbol-occurrences text))
      (let ((name (string-upcase (subseq text (car occ) (cdr occ)))))
        (when (gethash name known)
          (let ((lc (offset-to-line-col text (car occ))))
            (push (make-ref-entry
                   :name name
                   :file file-path
                   :line (car lc)
                   :col (cdr lc)
                   :uri (path-to-uri file-path))
                  refs)))))
    ;; Store references
    (bt:with-lock-held (*index-lock*)
      ;; Remove old refs for this file
      (maphash (lambda (name entries)
                 (setf (gethash name *reference-index*)
                       (remove file-path entries
                               :key #'ref-entry-file :test #'string=)))
               *reference-index*)
      ;; Add new
      (dolist (ref refs)
        (push ref (gethash (ref-entry-name ref) *reference-index*))))))

(defun scan-symbol-occurrences (text)
  "Single pass over TEXT collecting (start . end) offset pairs for every
symbol-name occurrence in code (outside strings, comments, character
literals and pipe-escaped symbols). Package qualifiers are skipped: for
\"pkg:sym\" and \"#:sym\" the recorded occurrence points at just \"sym\".
Runs in O(length of text), independent of how many known names exist."
  (let ((len (length text))
        (occurrences nil)
        (i 0)
        (in-string nil)
        (escape nil))
    (labels ((record-token (start end)
               ;; Trim package prefixes off the token, keeping the offsets
               ;; pointed at the bare symbol name
               (let ((j start))
                 (loop while (and (< j end) (char= (char text j) #\#))
                       do (incf j))
                 (let ((colon (position #\: text :start j :end end :from-end t)))
                   (when colon (setf j (1+ colon))))
                 (when (< j end)
                   (push (cons j end) occurrences))))
             (skip-block-comment ()
               ;; i is just past the opening #|
               (let ((depth 1))
                 (loop while (and (< i len) (> depth 0))
                       do (cond
                            ((and (< (1+ i) len)
                                  (char= (char text i) #\|)
                                  (char= (char text (1+ i)) #\#))
                             (decf depth) (incf i 2))
                            ((and (< (1+ i) len)
                                  (char= (char text i) #\#)
                                  (char= (char text (1+ i)) #\|))
                             (incf depth) (incf i 2))
                            (t (incf i))))))
             (skip-quoted-symbol ()
               ;; i is at the opening | of a |...| symbol
               (incf i)
               (loop while (and (< i len) (not (char= (char text i) #\|)))
                     do (if (and (char= (char text i) #\\) (< (1+ i) len))
                            (incf i 2)
                            (incf i)))
               (incf i)))
      (loop while (< i len)
            do (let ((c (char text i)))
                 (cond
                   ;; Inside a string, only the closing quote matters
                   (in-string
                    (cond
                      (escape (setf escape nil) (incf i))
                      ((char= c #\\) (setf escape t) (incf i))
                      ((char= c #\") (setf in-string nil) (incf i))
                      (t (incf i))))
                   ((char= c #\")
                    (setf in-string t) (incf i))
                   ;; Line comment: skip to end of line
                   ((char= c #\;)
                    (setf i (or (position #\Newline text :start i) len)))
                   ;; #| ... |# block comment (nestable)
                   ((and (char= c #\#) (< (1+ i) len) (char= (char text (1+ i)) #\|))
                    (incf i 2)
                    (skip-block-comment))
                   ;; #\x character literal: skip #\ and the whole name, so
                   ;; e.g. #\Newline does not register a "Newline" reference
                   ((and (char= c #\#) (< (1+ i) len) (char= (char text (1+ i)) #\\))
                    (incf i 2)
                    (loop while (and (< i len) (symbol-char-p (char text i)))
                          do (incf i)))
                   ;; |...| multiple-escaped symbol
                   ((char= c #\|) (skip-quoted-symbol))
                   ;; Symbol token
                   ((symbol-char-p c)
                    (let ((start i))
                      (loop while (and (< i len) (symbol-char-p (char text i)))
                            do (incf i))
                      (record-token start i)))
                   (t (incf i))))))
    (nreverse occurrences)))

;;; --- Buffer indexing (for unsaved content) ---

(defun index-buffer (uri text)
  "Index the in-memory buffer TEXT for document URI.
Used for didOpen/didChange to keep the index current before saving."
  (let ((file-path (uri-to-path uri))
        (definitions nil))
    (handler-case
        (with-input-from-string (stream text)
          (let ((*package* (or (find-package "COMMON-LISP-USER")
                               *package*))
                (*read-eval* nil))
            ;; Read forms and track positions
            (loop
              (let ((pos (file-position stream)))
                (handler-case
                    (let ((form (read stream nil *eof-form*)))
                      (unless (eq form *eof-form*)
                        (let ((def-info (definition-form-p form)))
                          (when def-info
                            (destructuring-bind (kind name arglist) def-info
                              (let ((line-col (offset-to-line-col text
                                                (min pos (length text)))))
                                (push (make-index-entry
                                       :name name
                                       :package (package-name *package*)
                                       :kind kind
                                       :file file-path
                                       :line (car line-col)
                                       :col (cdr line-col)
                                       :arglist arglist
                                       :uri uri)
                                      definitions))))))
                      (when (eq form *eof-form*) (return)))
                  (error () (return)))))))
      (error (e)
        (lsp-log "Buffer index error for ~a: ~a" uri e)))
    ;; Update global index
    (bt:with-lock-held (*index-lock*)
      (remove-file-from-index file-path)
      (dolist (def definitions)
        (push def (gethash (index-entry-name def) *definition-index*))))
    ;; Index references too, so an open-but-unsaved buffer participates in
    ;; find-references just like a saved file
    (index-references-in-text text file-path)
    (length definitions)))

;;; --- ASDF system indexing ---

(defun system-source-files (system-name)
  "Get all source file paths for an ASDF system.
Returns a list of pathname strings."
  (handler-case
      (let ((system (asdf:find-system system-name nil)))
        (when system
          (let ((files nil))
            (labels ((collect-components (component)
                       (typecase component
                         (asdf:cl-source-file
                          (let ((path (asdf:component-pathname component)))
                            (when (and path (probe-file path))
                              (push (namestring (truename path)) files))))
                         (asdf:module
                          (dolist (child (asdf:component-children component))
                            (collect-components child))))))
              (collect-components system))
            (nreverse files))))
    (error (e)
      (lsp-log "Error finding system files for ~a: ~a" system-name e)
      nil)))

(defun index-system (system-name)
  "Index all source files of an ASDF system.
Returns the total number of definitions found."
  (let ((files (system-source-files system-name))
        (total 0))
    (lsp-log "Indexing system ~a (~d files)" system-name (length files))
    (dolist (file files)
      (let ((count (index-file file)))
        (incf total count)
        (lsp-log "  ~a: ~d definitions" (file-namestring file) count)))
    (lsp-log "Index complete: ~d definitions across ~d files" total (length files))
    total))

(defun index-directory (dir &optional (extensions '("lisp" "cl" "asd")))
  "Index all Lisp source files under directory DIR recursively."
  (let ((total 0))
    (dolist (extension extensions)
      (let ((pattern (make-pathname :directory (append (pathname-directory dir)
                                                       '(:wild-inferiors))
                                    :name :wild
                                    :type extension)))
        (dolist (file (directory pattern))
          (incf total (index-file file)))))
    total))

;;; --- Index queries ---

(defun index-lookup-definitions (name)
  "Look up all definitions of NAME in the index.
Returns a list of index-entry structs."
  (bt:with-lock-held (*index-lock*)
    (gethash (string-upcase name) *definition-index*)))

(defun index-lookup-references (name)
  "Look up all references to NAME in the index.
Returns a list of ref-entry structs."
  (bt:with-lock-held (*index-lock*)
    (gethash (string-upcase name) *reference-index*)))

(defun index-search-symbols (query &optional (limit 100))
  "Search the index for symbols matching QUERY (substring match).
Returns a list of index-entry structs."
  (let ((uquery (string-upcase query))
        (results nil)
        (count 0))
    (bt:with-lock-held (*index-lock*)
      (maphash (lambda (name entries)
                 (when (and (< count limit)
                            (search uquery name))
                   (dolist (entry entries)
                     (when (< count limit)
                       (push entry results)
                       (incf count)))))
               *definition-index*))
    (nreverse results)))

(defun index-completions (prefix &optional (limit 50))
  "Return completion candidates from the index matching PREFIX.
Each entry is (name kind package-name) matching symbol-completions format."
  (let ((uprefix (string-upcase prefix))
        (results nil)
        (count 0)
        (seen (make-hash-table :test 'equal)))
    (bt:with-lock-held (*index-lock*)
      (maphash (lambda (name entries)
                 (when (and (< count limit)
                            (>= (length name) (length uprefix))
                            (string= uprefix name :end2 (length uprefix))
                            (not (gethash name seen)))
                   (setf (gethash name seen) t)
                   (let ((entry (first entries)))
                     (push (list (string-downcase name)
                                 (index-kind-to-lsp-completion-kind
                                  (index-entry-kind entry))
                                 (index-entry-package entry))
                           results)
                     (incf count))))
               *definition-index*))
    (nreverse results)))

;;; --- Kind conversion ---

(defun index-kind-to-lsp-completion-kind (kind)
  "Convert an index-entry kind to LSP CompletionItemKind number."
  (case kind
    (:function   3)   ; Function
    (:macro      14)  ; Keyword (macro)
    (:generic    3)   ; Function
    (:method     3)   ; Function
    (:variable   6)   ; Variable
    (:parameter  6)   ; Variable
    (:constant   21)  ; Constant
    (:class      7)   ; Class
    (:struct     22)  ; Struct
    (:condition  7)   ; Class
    (:type       25)  ; TypeParameter
    (:package    9)   ; Module
    (t           6))) ; Variable

(defun index-kind-to-lsp-symbol-kind (kind)
  "Convert an index-entry kind to LSP SymbolKind number."
  (case kind
    (:function   12)  ; Function
    (:macro      14)  ; Constant (macro)
    (:generic    6)   ; Method
    (:method     6)   ; Method
    (:variable   13)  ; Variable
    (:parameter  13)  ; Variable
    (:constant   14)  ; Constant
    (:class      5)   ; Class
    (:struct     23)  ; Struct
    (:condition  5)   ; Class
    (:type       26)  ; TypeParameter
    (:package    4)   ; Package
    (t           13)))

;;; --- Index maintenance ---

(defun remove-file-from-index (file-path)
  "Remove all index entries for FILE-PATH. Caller must hold *index-lock*."
  (maphash (lambda (name entries)
             (let ((remaining (remove file-path entries
                                      :key #'index-entry-file :test #'string=)))
               (if remaining
                   (setf (gethash name *definition-index*) remaining)
                   (remhash name *definition-index*))))
           *definition-index*)
  ;; Also remove references
  (maphash (lambda (name entries)
             (let ((remaining (remove file-path entries
                                      :key #'ref-entry-file :test #'string=)))
               (if remaining
                   (setf (gethash name *reference-index*) remaining)
                   (remhash name *reference-index*))))
           *reference-index*))

(defun clear-index ()
  "Clear all index data."
  (bt:with-lock-held (*index-lock*)
    (clrhash *definition-index*)
    (clrhash *reference-index*)
    (clrhash *indexed-files*)))

(defun index-stats ()
  "Return a summary of the current index state as a string."
  (bt:with-lock-held (*index-lock*)
    (let ((def-count 0)
          (ref-count 0)
          (file-count (hash-table-count *indexed-files*)))
      (maphash (lambda (k v) (declare (ignore k)) (incf def-count (length v)))
               *definition-index*)
      (maphash (lambda (k v) (declare (ignore k)) (incf ref-count (length v)))
               *reference-index*)
      (format nil "~d definitions, ~d references across ~d files"
              def-count ref-count file-count))))

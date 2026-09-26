(in-package :sextant)

;;; ============================================================
;;; Document Management
;;; Tracks open files and provides symbol-at-position lookup
;;; ============================================================

(defvar *documents* (make-hash-table :test 'equal)
  "Map of URI -> document content (string).")

(defvar *eof-form* (cons :sextant-eof-form nil)
  "Unique sentinel for READ: using a keyword like :EOF would truncate
processing when the file itself contains a top-level occurrence of that
keyword, so a fresh cons is used instead.")

(defvar *position-encoding* :utf-16
  "Negotiated LSP position encoding. utf-16 is the protocol default; when the
client offers utf-32 we pick it because our offsets count codepoints, which
matches utf-32 code units exactly (utf-16 is handled by counting surrogate
pairs as two units; see char-position-units).")

(defun uri-percent-decode (string)
  "Decode %XX escapes in STRING as UTF-8 (invalid sequences are kept as-is)."
  (if (find #\% string)
      (let ((bytes (make-array 0 :element-type '(unsigned-byte 8)
                                 :fill-pointer 0 :adjustable t)))
        (loop with len = (length string)
              for i from 0 below len
              do (let ((c (char string i)))
                   (if (and (char= c #\%)
                            (< (+ i 2) len)
                            (digit-char-p (char string (1+ i)) 16)
                            (digit-char-p (char string (+ i 2)) 16))
                       (progn
                         (vector-push-extend
                          (parse-integer string :start (1+ i) :end (+ i 3) :radix 16)
                          bytes)
                         (incf i 2))
                       (vector-push-extend (char-code c) bytes))))
        (babel:octets-to-string bytes :encoding :utf-8))
      string))

(defun uri-percent-encode (string)
  "Percent-encode STRING as UTF-8 for use in a file URI path: every byte
outside the unreserved set (plus the path separator '/') becomes %XX."
  (with-output-to-string (out)
    (loop for byte across (babel:string-to-octets string :encoding :utf-8)
          do (if (or (and (>= byte #x30) (<= byte #x39))    ; 0-9
                     (and (>= byte #x41) (<= byte #x5A))    ; A-Z
                     (and (>= byte #x61) (<= byte #x7A))    ; a-z
                     (member byte '(#x2D #x5F #x2E #x7E #x2F))) ; - _ . ~ /
                 (write-char (code-char byte) out)
                 (format out "%~2,'0x" byte)))))

(defun uri-to-path (uri)
  "Convert a file:// URI to a filesystem path, decoding percent escapes."
  (let ((path (if (and (>= (length uri) 7)
                       (string= "file://" (subseq uri 0 7)))
                  (subseq uri 7)
                  uri)))
    (uri-percent-decode path)))

(defun path-to-uri (path)
  "Convert a filesystem path to a file:// URI, percent-encoding as needed."
  (if (and (>= (length path) 7)
           (string= "file://" (subseq path 0 7)))
      path
      (concatenate 'string "file://" (uri-percent-encode path))))

(defun document-open (uri text)
  "Register an opened document."
  (setf (gethash uri *documents*) text)
  (lsp-log "Document opened: ~a (~d chars)" uri (length text)))

(defun document-change (uri text)
  "Update document content (full sync)."
  (setf (gethash uri *documents*) text))

(defun apply-incremental-change (uri change)
  "Apply a single incremental CHANGE to the document at URI.
CHANGE is a JSON object with 'range' and 'text' keys.
Range has 'start' and 'end' positions, each with 'line' and 'character'."
  (let ((text (gethash uri *documents*))
        (range (json-get change "range"))
        (new-text (json-get change "text")))
    (when text
      (if range
          ;; Incremental: replace the range
          (let* ((start-pos (json-get range "start"))
                 (end-pos (json-get range "end"))
                 (start-offset (line-col-to-offset text
                                                    (json-get start-pos "line")
                                                    (json-get start-pos "character")))
                 (end-offset (line-col-to-offset text
                                                  (json-get end-pos "line")
                                                  (json-get end-pos "character"))))
            (setf (gethash uri *documents*)
                  (concatenate 'string
                               (subseq text 0 start-offset)
                               new-text
                               (subseq text end-offset))))
          ;; No range means full replacement
          (setf (gethash uri *documents*) new-text)))))

(defun document-close (uri)
  "Remove a closed document."
  (remhash uri *documents*))

(defun document-text (uri)
  "Get the current text of a document."
  (gethash uri *documents*))

(defun char-position-units (c)
  "Width of character C in code units of the negotiated position encoding:
a codepoint outside the BMP is one utf-32 unit but two utf-16 units."
  (if (and (eq *position-encoding* :utf-16)
           (>= (char-code c) #x10000))
      2
      1))

(defun line-col-to-offset (text line col)
  "Convert 0-based LINE and COL (counted in position-encoding code units)
to a character offset in TEXT. The result is clamped to the end of LINE."
  (let ((pos 0)
        (current-line 0))
    (loop while (and (< pos (length text))
                     (< current-line line))
          do (when (char= (char text pos) #\Newline)
               (incf current-line))
             (incf pos))
    (let ((units 0))
      (loop while (and (< pos (length text))
                       (< units col)
                       (not (char= (char text pos) #\Newline)))
            do (incf units (char-position-units (char text pos)))
               (incf pos)))
    pos))

(defun offset-to-line-col (text offset)
  "Convert character OFFSET to (line . col), where COL is counted in
position-encoding code units."
  (let ((line 0)
        (col 0))
    (loop for i from 0 below (min offset (length text))
          do (let ((c (char text i)))
               (if (char= c #\Newline)
                   (progn (incf line) (setf col 0))
                   (incf col (char-position-units c)))))
    (cons line col)))

(defun symbol-at-position (text line col)
  "Extract the Lisp symbol at LINE, COL in TEXT.
Returns the symbol string or NIL."
  (let* ((offset (line-col-to-offset text line col))
         (len (length text)))
    (when (and (> len 0) (<= offset len))
      ;; Find start of symbol
      (let ((start offset)
            (end offset))
        ;; Scan backward
        (loop while (and (> start 0)
                         (symbol-char-p (char text (1- start))))
              do (decf start))
        ;; Scan forward
        (loop while (and (< end len)
                         (symbol-char-p (char text end)))
              do (incf end))
        (when (> end start)
          (subseq text start end))))))

(defun symbol-char-p (c)
  "Return T if C can be part of a Lisp symbol."
  (and (not (member c '(#\Space #\Tab #\Newline #\Return
                         #\( #\) #\' #\" #\` #\, #\;)))
       (graphic-char-p c)))

;;; ============================================================
;;; S-expression Range Utilities
;;; ============================================================

(defun find-all-enclosing-sexps (text offset)
  "Find all s-expressions enclosing OFFSET, innermost first.
Returns a list of (start . end) pairs."
  (let ((len (length text))
        (paren-pairs nil)
        (in-string nil)
        (escape nil)
        (paren-stack nil))
    ;; First pass: collect all matching paren pairs
    (loop for i from 0 below len
          for c = (char text i)
          do (cond
               (escape (setf escape nil))
               ((char= c #\\) (setf escape t))
               ((char= c #\")
                (if in-string
                    (setf in-string nil)
                    (setf in-string t)))
               (in-string nil)
               ((char= c #\;)
                (loop while (and (< i (1- len))
                                 (not (char= (char text (1+ i)) #\Newline)))
                      do (incf i)))
               ((char= c #\()
                (push i paren-stack))
               ((char= c #\))
                (when paren-stack
                  (let ((start (pop paren-stack)))
                    (push (cons start (1+ i)) paren-pairs))))))
    ;; Filter to those containing offset, sort innermost first
    (let ((enclosing (remove-if-not
                      (lambda (pair)
                        (and (<= (car pair) offset)
                             (<= offset (cdr pair))))
                      paren-pairs)))
      (sort enclosing #'> :key (lambda (p) (car p))))))

(defun find-all-symbol-occurrences (text symbol-name)
  "Find all occurrences of SYMBOL-NAME in TEXT.
Returns list of (start-offset . end-offset) pairs."
  (let ((results nil)
        (len (length text))
        (slen (length symbol-name))
        (uname (string-upcase symbol-name)))
    (loop for i from 0 below len
          do (when (and (<= (+ i slen) len)
                        (string-equal uname (subseq text i (+ i slen)))
                        ;; Check boundaries
                        (or (zerop i) (not (symbol-char-p (char text (1- i)))))
                        (or (= (+ i slen) len) (not (symbol-char-p (char text (+ i slen))))))
               (push (cons i (+ i slen)) results)))
    (nreverse results)))

(defun find-top-level-forms (text)
  "Find all top-level forms in TEXT.
Returns list of (start-offset end-offset start-line end-line) tuples."
  (let ((len (length text))
        (forms nil)
        (in-string nil)
        (escape nil)
        (depth 0)
        (form-start nil))
    (loop for i from 0 below len
          for c = (char text i)
          do (cond
               (escape (setf escape nil))
               ((char= c #\\) (setf escape t))
               ((char= c #\")
                (if in-string
                    (setf in-string nil)
                    (setf in-string t)))
               (in-string nil)
               ((char= c #\;)
                ;; Skip comment
                (loop while (and (< i (1- len))
                                 (not (char= (char text (1+ i)) #\Newline)))
                      do (incf i)))
               ((char= c #\()
                (when (zerop depth)
                  (setf form-start i))
                (incf depth))
               ((char= c #\))
                (decf depth)
                (when (and (zerop depth) form-start)
                  (let ((start-line (count #\Newline text :end form-start))
                        (end-line (count #\Newline text :end (1+ i))))
                    (push (list form-start (1+ i) start-line end-line) forms))
                  (setf form-start nil)))))
    (nreverse forms)))

(defun format-lisp-text (text)
  "Format Common Lisp TEXT with proper indentation.
Returns the formatted text string."
  (let ((lines (split-string-by-newline text))
        (result nil)
        (depth 0))
    (dolist (line lines)
      (let* ((trimmed (string-trim '(#\Space #\Tab) line))
             ;; Count how many net parens close at the start
             (leading-closes (count-leading-closes trimmed)))
        ;; Decrease depth for leading close parens
        (decf depth leading-closes)
        (when (< depth 0) (setf depth 0))
        ;; Indent
        (let ((indent (make-string (* 2 depth) :initial-element #\Space)))
          (push (if (zerop (length trimmed))
                    ""
                    (concatenate 'string indent trimmed))
                result))
        ;; Update depth based on all parens in this line
        (let ((net (count-net-parens trimmed)))
          (incf depth (+ net leading-closes))
          (when (< depth 0) (setf depth 0)))))
    (format nil "~{~a~^~%~}" (nreverse result))))

(defun split-string-by-newline (string)
  "Split STRING by newline characters."
  (let ((result nil)
        (start 0))
    (loop for i from 0 below (length string)
          do (when (char= (char string i) #\Newline)
               (push (subseq string start i) result)
               (setf start (1+ i))))
    (push (subseq string start) result)
    (nreverse result)))

(defun count-net-parens (line)
  "Count net open parens in LINE (open minus close), respecting strings and comments."
  (let ((net 0)
        (in-string nil)
        (escape nil))
    (loop for c across line
          do (cond
               (escape (setf escape nil))
               ((char= c #\\) (setf escape t))
               ((char= c #\")
                (if in-string
                    (setf in-string nil)
                    (setf in-string t)))
               (in-string nil)
               ((char= c #\;) (return))
               ((char= c #\() (incf net))
               ((char= c #\)) (decf net))))
    net))

(defun count-leading-closes (line)
  "Count close parens at the start of LINE (after whitespace)."
  (let ((count 0))
    (loop for c across line
          do (cond
               ((member c '(#\Space #\Tab)) nil)
               ((char= c #\)) (incf count))
               (t (return))))
    count))

(defun find-references-in-documents (sym-name)
  "Search all open documents for occurrences of SYM-NAME.
Returns a list of LSP Location objects."
  (let ((results nil))
    (maphash
     (lambda (doc-uri doc-text)
       (let ((occurrences (find-all-symbol-occurrences doc-text sym-name)))
         (dolist (occ occurrences)
           (let ((start-lc (offset-to-line-col doc-text (car occ)))
                 (end-lc (offset-to-line-col doc-text (cdr occ))))
             (push (make-json-object
                    "uri" doc-uri
                    "range" (make-json-object
                             "start" (make-json-object
                                      "line" (car start-lc)
                                      "character" (cdr start-lc))
                             "end" (make-json-object
                                    "line" (car end-lc)
                                    "character" (cdr end-lc))))
                   results)))))
     *documents*)
    (nreverse results)))

;;; ============================================================
;;; Definition form recognition
;;; Shared by the source indexer (source-index.lisp), the document
;;; definition/symbol searchers and the handlers.
;;; ============================================================

(defparameter *definition-operators*
  '(("DEFUN"             . :function)
    ("DEFMACRO"          . :macro)
    ("DEFGENERIC"        . :generic)
    ("DEFMETHOD"         . :method)
    ("DEFVAR"            . :variable)
    ("DEFPARAMETER"      . :parameter)
    ("DEFCONSTANT"       . :constant)
    ("DEFCLASS"          . :class)
    ("DEFSTRUCT"         . :struct)
    ("DEFINE-CONDITION"  . :condition)
    ("DEFTYPE"           . :type)
    ("DEFPACKAGE"        . :package))
  "Alist of (operator-name . kind) for recognized definition forms.")

(defun definition-operator-kind (operator)
  "If OPERATOR (a string) is a recognized definition operator, return its
kind keyword, else NIL. Matching any \"def*\" prefix is not enough: a call
like (default-foo bar) must not be mistaken for a definition of BAR."
  (cdr (assoc operator *definition-operators* :test #'string-equal)))

(defun operator-after-paren (text paren-pos)
  "Return the operator token following the '(' at PAREN-POS in TEXT, or NIL."
  (let ((len (length text))
        (start (1+ paren-pos)))
    (loop while (and (< start len)
                     (member (char text start)
                             '(#\Space #\Tab #\Newline #\Return)))
          do (incf start))
    (let ((end start))
      (loop while (and (< end len) (symbol-char-p (char text end)))
            do (incf end))
      (when (> end start)
        (subseq text start end)))))

(defun form-name-token (text start)
  "Read the name token of a definition form, where START is the offset just
after the operator. Handles plain symbol names and wrapped names such as
(setf foo) and (defstruct (foo ...)). Returns (values name start end), where
START/END delimit the name in TEXT, or NIL if there is no name."
  (let ((len (length text))
        (ws '(#\Space #\Tab #\Newline #\Return)))
    (labels ((skip-ws (i)
               (loop while (and (< i len) (member (char text i) ws))
                     do (incf i))
               i)
             (read-token (i)
               (let* ((s (skip-ws i))
                      (e s))
                 (loop while (and (< e len) (symbol-char-p (char text e)))
                       do (incf e))
                 (if (> e s) (values s e) (values nil nil)))))
      (setf start (skip-ws start))
      (when (< start len)
        (if (char= (char text start) #\()
            ;; Wrapped name: (foo ...) or (setf foo)
            (multiple-value-bind (first-start first-end) (read-token (1+ start))
              (when first-start
                (if (string-equal "setf" (subseq text first-start first-end))
                    ;; (setf foo): the name is the second token
                    (multiple-value-bind (s e) (read-token first-end)
                      (when s (values (subseq text s e) s e)))
                    (values (subseq text first-start first-end)
                            first-start first-end))))
            ;; Plain symbol name
            (multiple-value-bind (s e) (read-token start)
              (when s (values (subseq text s e) s e))))))))

(defun find-definition-in-documents (name)
  "Search all open documents for a definition form naming NAME.
Returns (uri line col) or NIL."
  (let ((uname (string-upcase name))
        (result nil))
    (maphash
     (lambda (uri text)
       (unless result
         (loop for i from 0 below (length text)
                 do (when (char= (char text i) #\()
                        (let ((op (operator-after-paren text i)))
                          (when (and op (definition-operator-kind op))
                            (multiple-value-bind (def-name name-start)
                                (form-name-token text (+ i 1 (length op)))
                              (when (and def-name (string-equal uname def-name))
                                (let* ((def-line (count #\Newline text :end name-start))
                                       (prev-nl (position #\Newline text :end name-start :from-end t))
                                       (line-start (if prev-nl (1+ prev-nl) 0))
                                       (def-col (- name-start line-start)))
                                  (setf result (list uri def-line def-col))
                                  (return-from find-definition-in-documents result))))))))))
     *documents*)
    result))

(defun find-symbol-range-at (text line col)
  "Find the range of the symbol at LINE, COL in TEXT.
Returns (start-line start-col end-line end-col) or NIL."
  (let* ((offset (line-col-to-offset text line col))
         (len (length text)))
    (when (and (> len 0) (<= offset len))
      (let ((start offset)
            (end offset))
        (loop while (and (> start 0)
                         (symbol-char-p (char text (1- start))))
              do (decf start))
        (loop while (and (< end len)
                         (symbol-char-p (char text end)))
              do (incf end))
        (when (> end start)
          (let ((start-lc (offset-to-line-col text start))
                (end-lc (offset-to-line-col text end)))
            (list (car start-lc) (cdr start-lc)
                  (car end-lc) (cdr end-lc))))))))

;;; ============================================================
;;; Form-position utilities
;;; Used both by the source indexer / introspection layer (locating
;;; the Nth top-level form of a file) and by diagnostics (navigating
;;; SBCL source paths).
;;; ============================================================

(defun skip-whitespace-and-comments (stream)
  "Advance STREAM past whitespace and line comments.
Returns the file-position of the first non-whitespace, non-comment character."
  (loop
    (let ((c (peek-char nil stream nil nil)))
      (cond
        ((null c) (return (file-position stream)))
        ((member c '(#\Space #\Tab #\Newline #\Return #\Page))
         (read-char stream))
        ((char= c #\;)
         ;; Skip to end of line
         (loop for ch = (read-char stream nil nil)
               while (and ch (not (char= ch #\Newline)))))
        ;; Skip #| ... |# block comments
        ((char= c #\#)
         (let ((next (progn (read-char stream)
                            (peek-char nil stream nil nil))))
           (if (and next (char= next #\|))
               (progn
                 (read-char stream) ; consume |
                 (let ((depth 1))
                   (loop while (> depth 0)
                         for ch = (read-char stream nil nil)
                         while ch
                         do (cond
                              ((and (char= ch #\#)
                                    (eql (peek-char nil stream nil nil) #\|))
                               (read-char stream)
                               (incf depth))
                              ((and (char= ch #\|)
                                    (eql (peek-char nil stream nil nil) #\#))
                               (read-char stream)
                               (decf depth))))))
               ;; Not a block comment - back up and return
               (progn
                 (file-position stream (1- (file-position stream)))
                 (return (file-position stream))))))
        (t (return (file-position stream)))))))

(defun find-nth-toplevel-form-position (text n)
  "Find the character position of the Nth top-level form (0-indexed) in TEXT.
Returns (line . col) or NIL."
  (handler-case
      (with-input-from-string (stream text)
        (let ((form-count 0))
          (loop
            ;; Skip whitespace and comments to find actual form start
            (let ((pos (skip-whitespace-and-comments stream)))
              ;; Read the next form
              (let ((form (read stream nil *eof-form*)))
                (when (eq form *eof-form*)
                  (return nil))
                (when (= form-count n)
                  ;; This is the form we want
                  (return (offset-to-line-col text (min pos (length text)))))
                (incf form-count))))))
    (error () nil)))

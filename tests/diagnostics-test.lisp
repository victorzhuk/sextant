(defpackage :sextant/tests
  (:use :cl :fiveam)
  (:import-from :sextant
                ;; diagnostics
                #:compile-buffer-for-diagnostics
                #:captured-condition-position
                #:captured-condition-source-form
                #:make-diagnostic-range
                ;; json
                #:json-parse
                #:json-to-string
                #:json-array
                #:json-empty-array
                #:+json-empty-array+
                #:make-json-object
                #:json-get
                ;; document
                #:*position-encoding*
                #:*documents*
                #:offset-to-line-col
                #:line-col-to-offset
                #:symbol-at-position
                #:find-all-enclosing-sexps
                #:find-top-level-forms
                #:format-lisp-text
                #:uri-to-path
                #:path-to-uri
                #:find-definition-in-documents
                ;; source-index
                #:scan-symbol-occurrences
                #:index-buffer
                #:index-lookup-definitions
                #:index-lookup-references
                #:clear-index
                #:ref-entry-line
                #:ref-entry-col
                ;; lisp-introspection
                #:find-symbol-in-packages
                #:form-number-to-position
                ;; handlers
                #:handle-rename
                #:defpackage-export-edit
                #:find-next-definition
                ;; debugger
                #:*dap-debugger-active*
                #:*dap-stopped-callback*
                #:install-function-breakpoint
                #:remove-function-breakpoint)
  (:export #:run-tests))

(in-package :sextant/tests)

(def-suite :sextant-tests)
(in-suite :sextant-tests)

(defparameter *type-mismatch-fixture*
  (format nil "(defun bad-add (x)~%  (+ x \"not-a-number\"))~%")
  "Line 1 (0-indexed) has a type mismatch: adding a string to a number.
Reproduces TODO.md bug #1 on unpatched master: position collapses to NIL,
make-diagnostic-range then defaults to line 0, col 0.")

(test diagnostics-position-mid-file
  (let* ((conditions (compile-buffer-for-diagnostics *type-mismatch-fixture* "file:///fixture.lisp"))
         (cc (first conditions))
         (pos (and cc (captured-condition-position cc))))
    (is (not (null cc)))
    (is (not (null pos)))
    (is (eql 1 (car pos)))
    (let ((range (make-diagnostic-range pos *type-mismatch-fixture*
                                         (captured-condition-source-form cc))))
      (is (eql 1 (json-get (json-get range "start") "line"))))))

(test rename-skips-comments-and-strings
  (let ((*documents* (make-hash-table :test 'equal))
        (uri "file:///fake-rename.lisp"))
    (setf (gethash uri *documents*)
          (format nil ";; helper comment~%(defun helper (x) x)~%(princ \"helper in string\")~%(helper 1)~%"))
    (let ((result (handle-rename
                   (make-json-object
                    "textDocument" (make-json-object "uri" uri)
                    "position" (make-json-object "line" 1 "character" 7)
                    "newName" "square"))))
      (let ((edits (cdr (assoc uri (json-get result "changes") :test #'string=))))
        ;; two code occurrences: the definition (line 1) and the call
        ;; (line 3); the comment and the string literal must not be edited
        (is (= 2 (length edits)))
        (let ((lines (mapcar (lambda (edit)
                               (json-get (json-get (json-get edit "range") "start") "line"))
                             edits)))
          (is (equal '(1 3) lines))
          (is (every (lambda (edit) (string= "square" (json-get edit "newText"))) edits)))))))

(test defpackage-export-edit-inserts-into-export-list
  (let ((text (format nil "(defpackage :foo~%  (:use :cl)~%  (:export #:bar))~%")))
    (multiple-value-bind (start end new-text)
        (defpackage-export-edit text "baz")
      (is (numberp start))
      (is (= start end))
      (is (string= "#:baz " new-text))
      ;; splicing the edit yields an export list containing baz
      (is (search ":export #:baz #:bar"
                  (concatenate 'string
                               (subseq text 0 start)
                               new-text
                               (subseq text end)))))))

(test defpackage-export-edit-creates-missing-clause
  (let ((text (format nil "(defpackage :foo~%  (:use :cl))~%")))
    (multiple-value-bind (start end new-text)
        (defpackage-export-edit text "baz")
      (is (numberp start))
      (is (= start end))
      (is (search "(:export #:baz)" new-text))
      (is (search ":export #:baz)"
                  (concatenate 'string
                               (subseq text 0 start)
                               new-text
                               (subseq text end)))))))

(test find-next-definition-ignores-def-prefixed-calls
  (let ((text (format nil "(default-thing foo)~%(defun foo (x) x)~%")))
    (multiple-value-bind (match-start match-end name kind line)
        (find-next-definition text 0)
      (declare (ignore match-end))
      ;; the first match is the real defun on line 1, not the call on line 0
      (is (numberp match-start))
      (is (string= "foo" name))
      (is (= 12 kind))                 ; Function
      (is (= 1 line)))))

(defun run-tests ()
  (run! :sextant-tests))

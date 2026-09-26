(in-package :sextant/tests)

(in-suite :sextant-tests)

;;; --- Single-pass symbol scanner ---

(test scan-symbol-occurrences-skips-non-code
  (let* ((text (format nil
                       ";; comment (foo here)~%\"string foo here\"~%(defun foo (x) (foo (foo x)))~%"))
         (foos (remove-if-not
                (lambda (occ)
                  (string-equal "FOO" (subseq text (car occ) (cdr occ))))
                (scan-symbol-occurrences text))))
    ;; The comment and the string must be skipped; the defun name and the
    ;; two call sites remain
    (is (= 3 (length foos)))))

(test scan-symbol-occurrences-trims-package-qualifiers
  (let* ((text "(pkg:foo #:bar |escape me| baz)")
         (occs (scan-symbol-occurrences text)))
    ;; pkg:foo records just "foo"; #:bar records just "bar"
    (is (some (lambda (occ) (string= "foo" (subseq text (car occ) (cdr occ)))) occs))
    (is (some (lambda (occ) (string= "bar" (subseq text (car occ) (cdr occ)))) occs))
    (is (some (lambda (occ) (string= "baz" (subseq text (car occ) (cdr occ)))) occs))
    ;; |escape me| is not tokenized as code
    (is (not (some (lambda (occ) (string= "escape" (subseq text (car occ) (cdr occ))))
                   occs)))))

;;; --- Indexing ---

(test index-buffer-registers-definitions-and-references
  (unwind-protect
       (progn
         (clear-index)
         (let ((text (format nil "(defun alpha-fn (x)~%  (beta-fn x))~%(defun beta-fn (y) y)~%")))
           (index-buffer "file:///fake-index.lisp" text)
           ;; definitions
           (is (= 1 (length (index-lookup-definitions "alpha-fn"))))
           ;; the call to beta-fn inside alpha-fn is found, with position
           ;; (the defun's own name on line 2 counts as a second occurrence)
           (let ((refs (index-lookup-references "beta-fn")))
             (is (= 2 (length refs)))
             (is (some (lambda (ref)
                         (and (= 1 (ref-entry-line ref))
                              (= 3 (ref-entry-col ref))))
                       refs)))))
    (clear-index)))

(test index-buffer-survives-eof-keyword-content
  ;; A top-level :EOF-like keyword literal must not truncate indexing
  (unwind-protect
       (progn
         (clear-index)
         (index-buffer "file:///fake-eof.lisp"
                       (format nil ":eof~%(defun after-keyword (x) x)~%"))
         (is (= 1 (length (index-lookup-definitions "after-keyword")))))
    (clear-index)))

;;; --- Form-number to position conversion ---

(test form-number-to-position-locates-nth-form
  (let ((path (merge-pathnames "sextant-test-form-positions.lisp"
                               (uiop:temporary-directory))))
    (unwind-protect
         (progn
           (with-open-file (f path :direction :output :if-exists :supersede)
             ;; three top-level forms; the third starts on line 4 (0-based)
             (format f ";; comment~%(defun one (x) x)~%~%(defun two (y)~%  y)~%(defun three (z)~%  z)~%"))
           (let ((pos0 (form-number-to-position (namestring path) 0))
                 (pos2 (form-number-to-position (namestring path) 2))
                 (cache (make-hash-table :test 'equal)))
             (is (equal '(1 . 0) pos0))
             ;; form 2 is on line 4: form numbers are ordinals, not lines
             (is (equal '(5 . 0) pos2))
             ;; cached lookups agree
             (is (equal pos2 (form-number-to-position (namestring path) 2 cache)))))
      (ignore-errors (delete-file path)))))

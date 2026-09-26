(in-package :sextant/tests)

(in-suite :sextant-tests)

;;; --- Position encoding ---

(test line-col-roundtrip-ascii
  (let ((text (format nil "foo bar~%(baz quux)")))
    ;; Default utf-16 encoding behaves like plain columns for ASCII
    (is (equal '(1 . 0) (offset-to-line-col text 8)))
    (is (equal '(1 . 1) (offset-to-line-col text 9)))
    (is (= 9 (line-col-to-offset text 1 1)))
    ;; col beyond end of line clamps to the line's newline
    (is (= 7 (line-col-to-offset text 0 100)))))

(test line-col-utf16-vs-utf32
  ;; An emoji outside the BMP counts as 2 utf-16 units, 1 utf-32 unit:
  ;; "x😀y" is 3 codepoints but 4 utf-16 units long
  (let ((text (format nil "x😀y")))
    (setf *position-encoding* :utf-16)
    (is (equal '(0 . 4) (offset-to-line-col text 4)))
    ;; col 3 in utf-16 lands on "y" (x=1 unit, emoji=2 units)
    (is (= 2 (line-col-to-offset text 0 3)))
    (setf *position-encoding* :utf-32)
    (is (equal '(0 . 3) (offset-to-line-col text 3)))
    (is (= 3 (line-col-to-offset text 0 3)))
    (setf *position-encoding* :utf-16)))

;;; --- URI conversion ---

(test uri-path-roundtrip
  (is (string= "/tmp/foo.lisp" (uri-to-path "file:///tmp/foo.lisp")))
  (is (string= "file:///tmp/foo.lisp" (path-to-uri "/tmp/foo.lisp")))
  ;; Spaces and non-ASCII characters are percent-encoded/decoded
  (is (string= "/tmp/my project.lisp"
               (uri-to-path "file:///tmp/my%20project.lisp")))
  (let ((uri (path-to-uri "/tmp/my project.lisp")))
    (is (not (find #\Space uri)))
    (is (string= "/tmp/my project.lisp" (uri-to-path uri))))
  (is (string= "/tmp/日本.lisp"
               (uri-to-path (path-to-uri "/tmp/日本.lisp")))))

;;; --- Symbol extraction ---

(test symbol-at-position-basic
  (let ((text "(format t \"hello\")"))
    (is (string= "format" (symbol-at-position text 0 3)))
    (is (string= "format" (symbol-at-position text 0 5)))
    (is (null (symbol-at-position text 0 10)))))

;;; --- Enclosing sexps / top-level forms ---

(test find-all-enclosing-sexps-ordering
  (let ((text "(defun f (x) (+ x 1))"))
    (let ((enclosing (find-all-enclosing-sexps text 17)))
      ;; innermost first, all contain the offset
      (is (> (length enclosing) 1))
      (loop for (a . b) in enclosing
            do (is (<= a 17 b))))))

(test find-top-level-forms-skips-strings-and-comments
  (let ((text ";; (not a form
\"a string with ) paren\"
(defun f ())
"))
    (is (= 1 (length (find-top-level-forms text))))))

;;; --- Definition search over documents ---

(test find-definition-in-documents-filters-def-prefixes
  (let ((*documents* (make-hash-table :test 'equal)))
    (setf (gethash "file:///fake.lisp" *documents*)
          (format nil "(defun foo (x) x)~%(default-foo foo)"))
    ;; matches the real definition...
    (is (equal '("file:///fake.lisp" 0 7)
               (find-definition-in-documents "foo")))
    ;; ...but not the (default-foo foo) call site, which is not a def form
    (is (null (find-definition-in-documents "default-foo")))))

(test find-definition-in-documents-wrapped-names
  (let ((*documents* (make-hash-table :test 'equal)))
    (setf (gethash "file:///fake.lisp" *documents*)
          (format nil "(defstruct (point (:constructor make-point)) x y)"))
    (is (equal '("file:///fake.lisp" 0 12)
               (find-definition-in-documents "point")))))

;;; --- Formatting keeps balanced text balanced ---

(test format-lisp-text-preserves-content
  (let ((text (format nil "(defun f (x)~%  (let ((y x))~%    (+ y 1)))")))
    ;; already-correct indentation survives unchanged
    (is (string= text (format-lisp-text text)))))

(in-package :sextant/tests)

(in-suite :sextant-tests)

(defun breakpoint-target (x)
  (* x 2))

(test function-breakpoint-calls-original-definition
  (is-true (install-function-breakpoint "sextant/tests::breakpoint-target"))
  (unwind-protect
       (let ((stops 0))
         (is (= 6 (breakpoint-target 3)))
         ;; An active session must not stop threads it does not debug
         (let ((*dap-debugger-active* t)
               (*dap-stopped-callback* (lambda (reason thread)
                                         (declare (ignore reason thread))
                                         (incf stops))))
           (is (= 8 (breakpoint-target 4))))
         (is (= 0 stops)))
    (remove-function-breakpoint "sextant/tests::breakpoint-target"))
  (is (= 10 (breakpoint-target 5))))

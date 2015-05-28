(defpackage :rjson-tests
  (:use :common-lisp :rjson)
  (:export #:run-all-tests)
  (:documentation "Testing suite for the rjson encoding library"))

(in-package :rjson-tests)

(stefil:defsuite rjson-tests)

(stefil:in-suite rjson-tests)

(defun run-all-tests ()
  (rjson-tests)
  (paren-test:run-paren-system :rjson-tests))

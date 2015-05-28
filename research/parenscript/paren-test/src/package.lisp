(defpackage :paren-test
  (:use :common-lisp :parenscript)
  (:export #:run-paren-system))
  
(in-package :paren-test)

(defvar *suite* nil)
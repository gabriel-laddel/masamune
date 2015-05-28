(in-package :cl-user)

(defpackage arith-system
  (:use :cl :asdf))

(in-package :arith-system)

(asdf:operate 'asdf:load-op :paren-files)

(defsystem :arith
  :name "arith"
  :author "Red Daly <reddaly@gmail.com.>"
  :version "0.0.1"
  :license "MIT"
  :description "Basic Parenscript functions for AJAX."
  :components ((:parenscript-file "arith"))

  :depends-on (:parenscript :paren-test))
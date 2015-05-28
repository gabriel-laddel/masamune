;;; -*- Lisp -*- mode
(defpackage org.iodb.rjson-system
  (:use :common-lisp :asdf))
(in-package :org.iodb.rjson-system)

(operate 'load-op :paren-files)

(defsystem rjson
  :description "Red's Javascript Object Notation encoder for common lisp.
Part of the Suave project."
  :version "0.2.1"
  :author "Red Daly <reddaly at gmail>"
  :license "GPL version 2: http://www.gnu.org/licenses/gpl.html"
  :components ((:module "src"
                        :components
			((:file "package")
			 (:file "rjson-encode" :depends-on ("package"))

			 (:module "paren"
				  :components ((:parenscript-file "rjson-decode"))))))
  :depends-on ("parenscript" "cl-json" "closer-mop" "paren-util"))

(defsystem rjson-tests
  :name "rjson-tests"
  :author "Red Daly <reddaly@gmail.com>"
  :version "0.0.1"
  :licence "MIT"
  :description "RJSON test suite."
  :components ((:static-file "rjson.asd")
               (:module "test"
                        :components
			((:file "package")
			 (:file "all-tests" :depends-on ("package"))
			 (:parenscript-file "rjson-test" :depends-on ("package")))))
  :depends-on ("stefil" "rjson" "paren-test" "paren-util"))

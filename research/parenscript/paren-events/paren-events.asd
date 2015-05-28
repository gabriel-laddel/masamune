;;; -*- Lisp -*- mode
(defpackage #:org.iodb.paren-events-system
  (:use #:cl #:asdf))

(in-package :org.iodb.paren-events-system)

(asdf:operate 'asdf:load-op :paren-files)

(defsystem :paren-events
  :description "ParenScript Object System - A CLOS-like object system for ParenScript."
  :version "0.2.1"
  :author "Red Daly <reddaly at gmail>"
  :license "GPL version 2: http://www.gnu.org/licenses/gpl.html"
  :components ((:module "src"
                        :components 
			((:file "packages")
			 
			 (:module "paren"
				  :components
				  ((:parenscript-file "package")
				   (:javascript-file "qevent")
				   (:parenscript-file "events" :depends-on ("package" "qevent")))))))

  :depends-on ("parenscript" "closer-mop" "paren-util" "paren-psos"))

(defsystem :paren-events-test
  :description "Lisp and Parenscript tests for the Parenscript Object System."
  :version "0.2.0"
  :author "Red Daly"
  :license "GPL version 2"
  :components ((:module "test"
			:components
			((:module "paren"
				  :components
				  ((:parenscript-file "events-test"))))))
  :depends-on ("parenscript" "paren-test" "paren-events"))

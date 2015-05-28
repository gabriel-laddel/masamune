(in-package :cl-user)

(defpackage paren-util-system
  (:use :cl :asdf))

(in-package :paren-util-system)

(asdf:operate 'asdf:load-op :paren-files)

(defsystem :paren-util
  :name "paren-util"
  :author "Red Daly <reddaly@gmail.com.>"
  :version "0.0.1"
  :license "MIT"
  :description "Basic Parenscript functions for AJAX."
  :components ((:static-file "paren-util.asd")
	       (:module "src"
			:components
			((:file "package")
			 (:file "util-macrology" :depends-on ("package"))
			 (:module "paren"
				  :components
				  ((:parenscript-file "util")
				   ;;(:parenscript-file "archetype")
				   )))))
;		 (:parenscript-file "core-ajax" :depends-on ("package")))
  :depends-on (:parenscript))

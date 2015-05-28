(in-package :cl-user)

(defpackage paren-files-system
  (:use :cl :asdf))

(in-package :paren-files-system)

(asdf:operate 'asdf:load-op :parenscript)

(defsystem :paren-files
  :name "paren-files"
  :author "Red Daly <reddaly@gmail.com.>"
  :version "0.0.1"
  :license "MIT"
  :description "Parenscript library for compiling files and ASDF systems."
  :components ((:module "src"
			:components
			((:file "package")
			 (:file "reader" :depends-on ("package"))
			 (:file "compile-file" :depends-on ("package" "reader"))
			 (:file "paren-asdf" :depends-on ("compile-file")))))
  :depends-on (:parenscript))
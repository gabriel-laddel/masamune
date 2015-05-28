;;; -*- Lisp -*- mode
(defpackage #:org.iodb.xml-mop-system
  (:use #:cl #:asdf))
(in-package :org.iodb.xml-mop-system)
 
(defsystem :xml-mop
  :description "xml-mop allows representing parts of XML documents as CLOS objects"
  :version "0.0.1"
  :author "Red Daly <reddaly at gmail>"
  :license "MIT License"
  :components ((:module	"src"
			:components ((:file "package")
				     (:file "xml-model" :depends-on ("package"))
				     (:file "parsing" :depends-on ("xml-model"))
				     )))
  :depends-on ("s-xml" "closer-mop"))

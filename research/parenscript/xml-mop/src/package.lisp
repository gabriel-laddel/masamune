(defpackage org.iodb.xml-mop
  (:nicknames xml-mop)
  (:documentation "xml-mop allows representing parts of XML documents as CLOS objects")
  (:use :cl :s-xml :closer-mop)
  (:export :child-element-value :element-class :element :element-value :element-text
	   :parse-xml-stream :on-loaded-from-xml :on-start-xml-load))
(in-package :xml-mop)



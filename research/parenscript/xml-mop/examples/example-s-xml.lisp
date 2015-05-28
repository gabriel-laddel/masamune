(in-package :xml-mop)

(with-open-file (stream "test/ant-build-file.xml")
  (s-xml:start-parse-xml
   stream
   (make-instance 's-xml:xml-parser-state
		  :new-element-hook
		  (lambda (name attribs seed)
		    (format t "Started ~A~%" name)
		    seed))))
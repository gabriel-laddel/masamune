(defpackage :org.iodb.rjson
  (:nicknames :rjson)
  (:use :common-lisp :closer-mop :parenscript)
  (:export #:encode-rjson
	   #:encode-rjson-to-string
	   #:represent
	   #:represent-rjson
	   #:represent-rjson-slot
	   #:rjson-type
	   #:create
	   
	   ;; parenscript side
	   #:decode-rjson
	   #:rjtype))

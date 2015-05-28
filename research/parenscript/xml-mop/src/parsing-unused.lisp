(in-package :org.iodb.xml-mop)

(defgeneric assign-subelement (parent-element subelement) )
(defmethod assign-subelement ((parent-element element) subelement)
  (let ((matching-slot-definition (find-slot-matching-subelement
				   parent-element subelement)))
    (if matching-slot-definition
	(setf (slot-value-using-class (class-of element)
				      element
				      matching-slot-definition)
	      value)
	(restart-case (error (make-condition 'encountered-unmatched-subelement))
	  (continue ()))))
;  (format t "Should be assigning a sub element right now...~%")
  )

(defmethod assign-subelement ((parent-element element-class) subelement)
  (format t "Nothing happens when attempting to assign subelements to an
element class.~%"))


(defun make-node-descriptor-from-def-form (def-form)
  "This function can be called with a little work from the standard macro idiom for
initializing node descriptors: ('NodeString' :case-sensitive blah :arg2 blah"
  (apply #'make-instance 'named-node-descriptor
	 :matcher (first def-form)
	 (rest def-form)))
(defclass document-class (standard-class)
  ((parser :initform (make-instance 'standard-parser) :reader document-parser))
  (:documentation "A document metaclass."))

(defgeneric add-node-description (parser node-descriptor target-class-or-lambda)
  (:documentation "Adds a handler to a parser.  This handler should be
either a function or an element class.  If it is a function, when called
with a tag string and attributes it should return a value."))

(defmethod add-node-description ((parser standard-parser) (descriptor named-node-descriptor) target-class-or-lambda)
  (if (stringp (descriptor-matcher descriptor))
      (setf (gethash (descriptor-matcher descriptor)
		     (if (descriptor-case-sensitive descriptor)
			 (parser-case-sensitive-table parser)
			 (parser-case-insensitive-table parser)))
	    target-class-or-lambda)
      (push (cons (descriptor-matcher descriptor)
		  target-class-or-lambda)
	    (slot-value parser 'lambda-table))))

(defgeneric find-element-handler (parser tag-name)
  (:documentation "Looks up an element in a parser."))

(defmethod find-element-handler ((parser standard-parser) tag-name)
  (let ((match
	 (or (gethash tag-name (parser-case-sensitive-table parser))
	     (gethash tag-name (parser-case-insensitive-table parser))
	     (find-if #'(lambda (entry)
			  (funcall (car entry) tag-name))
		      (parser-lambda-table parser)))))
    match))

;(defgeneric element-value (parser tag-name attributes)
;  (:documentation "Creates an element given an active parser, tag name and attributes."))

(defun resolve-s-element-value (parser tag-name attributes parent-element)
;(defmethod make-element-instance ((parser standard-parser) tag-name attributes)
  (let ((handler (find-element-handler parser tag-name)))
    (if (subtypep handler (find-class 'element))
	(make-instance handler)
	(funcall handler tag-name attributes parent-element))))


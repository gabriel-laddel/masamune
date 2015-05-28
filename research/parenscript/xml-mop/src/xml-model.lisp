(in-package :org.iodb.xml-mop)

(defclass standard-parser ()
  ((case-sensitive-table
    :documentation "For looking up member elements by tag name."
    :initform (make-hash-table :test #'equal)
    :initarg :case-sensitive-table
    :reader parser-case-sensitive-table)
   (case-insensitive-table
    :documentation "For looking up member elements by tag name."
    :initform (make-hash-table :test #'equalp)
    :initarg :cased-sensitive-table
    :reader parser-case-insensitive-table)
   (lambda-table 
    :documentation "For looking up elements by calling a test function."
    :initform () :initarg :lambda-table :reader parser-lambda-table))
  (:documentation "The parser class keeps track of element or attributes
an XML node can have.  Typically, each element class has a parser associated
with it that will attempt to look up a node.  If that fails, a parser is invoked
for the document."))

(defclass named-node-descriptor ()
  ((matcher :initform nil :initarg :string :initarg :matcher :reader descriptor-matcher)
   (case-sensitive :initform nil :initarg :case-sensitive :reader descriptor-case-sensitive)
   (element-type :initform nil :initarg :element-type :reader descriptor-element-type)
   (multiple :initform nil :initarg :multiple :reader descriptor-multiple)
   (primary-description :initform nil :initarg :primary :reader descriptor-primary-description
			:documentation "If this is true, then this is the description used to
                                        output an XML version of this descriptor."))
  (:documentation "This is the standard way to describe an XML node.  A node descriptor helps map
from a cased or uncased string to a handler class or function.  Several descriptors may map to a 
single element."))

;(defmethod shared-initialize :after ((descriptor named-node-descriptor) initargs &key &allow-other-keys)
;  (format t "********Initializing descriptor ~A~%" (descriptor-matcher descriptor)))

(defclass subelement-descriptor ()
  ((element-type :initform nil :initarg :element-type :reader descriptor-element-type)
   (multiple :initform nil :initarg :multiple :reader descriptor-multiple)
   (aliases :initform nil :initarg :aliases :reader descriptor-tag-aliases
	    :documentation "alternative node descriptors for this subelement."))
  (:documentation "Describes a subelement associated with a slot on an element class."))

(defclass xml-treenode-class (standard-class)
  ((allowed-elements :initarg :allowed-elements :initform nil :accessor node-class-allowed-elements)
   (parser :reader element-class-parser :initarg :parser :initform (make-instance 'standard-parser))))

(defmethod node-class-allowed-elements ((node-class xml-treenode-class))
  (mapcar #'find-class (slot-value node-class 'allowed-elements)))
;;(defmethod initialize-instance :around (&rest initargs &key allowed-elements

(defclass element-class (xml-treenode-class)
  ((tags :accessor element-class-tag-descriptors :initarg :tags :initform nil)
   (auto :accessor element-class-auto :initarg :auto :initform t)
   (reduces-to :accessor element-class-reduces-to :initarg :reduces-to :initform nil))
  (:documentation "A metaclass for XML elements.  An additional option that
can be passed to element classes is the :tags option, which accepts a sequence
of node-string identifiers."))
	 
(defgeneric element-slot-subelements (slot-definition))
(defgeneric element-slot-attributes (slot-definition))

(defclass element-direct-slot-definition (standard-direct-slot-definition)
  ((subelement :accessor element-slot-subelement :initarg :subelement :initform nil)
   (attribute :accessor element-slot-attribute :initarg :attribute :initform nil)))
;(defmethod element-slot-subelements ((slot-definition element-direct-slot-definition)))
;(defmethod element-slot-attributes ((slot-definition element-direct-slot-definition)))

(defclass element-effective-slot-definition (standard-effective-slot-definition)
  ((subelements :accessor element-slot-subelements :initarg :subelements :initform nil) ;:type named-node-descriptor)
   (attributes :accessor element-slot-attributes :initarg :attributes :initform nil))) ;:type named-node-descriptor)))

(defmethod direct-slot-definition-class ((class element-class) &key &allow-other-keys)
  (find-class 'element-direct-slot-definition))

(defmethod effective-slot-definition-class ((class element-class) &key &allow-other-keys)
  (find-class 'element-effective-slot-definition))

(defun resolve-subelement-descriptor-definition (descriptor-definition)
  "Does not allow multiple descriptor definitions as is the case for a
node descriptor."
;  (format t "***************DESCRIPTOR DEFINITION**************:~%~A~%" descriptor-definition)
  (when descriptor-definition
    (apply #'make-instance 'subelement-descriptor
	   (apply
	    #'parse-subelement-descriptor-aliases
	    (if (keywordp (first descriptor-definition))
		descriptor-definition
		(append (list :element-type)
			descriptor-definition))))))

(defun ensure-list (my-list)
  (if (listp my-list)
      my-list
      (list my-list)))

(defun parse-subelement-descriptor-aliases (&key element-type alias multiple)
  (let ((aliases (ensure-list
		  (resolve-node-descriptor-definition
		   (ensure-list alias)))))
    (list :element-type element-type :aliases aliases :multiple multiple)))

(defun resolve-node-descriptor-definition (descriptor-definition)
  "Takes either a list of descriptor-definitions or a single
descriptor definition."
;  (format t "Resolving descriptor definition ~A~%" descriptor-definition)
  ; determine if this is a single definition or many definitions
  ; this is a stupidly complicated logical statement, sorry
  (labels ((notempty-listp (list) (and (listp list) (not (null list)))))
    (let ((many (and (listp descriptor-definition)
		     (or (stringp (second descriptor-definition))
			 (notempty-listp (second descriptor-definition))))))

    (if many
	(mapcar #'resolve-node-descriptor-definition
		descriptor-definition)
	(when (not (null descriptor-definition))
	  (if (or (stringp descriptor-definition)
		   (keywordp descriptor-definition))
	      (progn
;		(format t "making effective descriptor definition from ~A~%" descriptor-definition)
		(make-instance 'named-node-descriptor
			       :matcher descriptor-definition))
	      (when (notempty-listp descriptor-definition)
;		(format t "making effective descriptor definition from ~A~%" descriptor-definition)
		(apply #'make-instance 'named-node-descriptor
		       :matcher (first descriptor-definition)
		       (rest descriptor-definition)))))))))
	
;; Initialize the effective slot.
(defmethod compute-effective-slot-definition ((class element-class) slot-name direct-slot-definitions)
  (declare (ignore slot-name))
  (labels ((ensure-list (possible-list)
	     (if (and (not (null possible-list))
		      (listp possible-list))
		 possible-list
		 (list possible-list))))
    (let ((effective-slotd (call-next-method)) ;let CLOS do the lifting
	  (resolved-attribute-definitions
	   (remove-if #'null
		      (ensure-list
		       (resolve-node-descriptor-definition
			(remove-if #'null
				   (ensure-list
				    (element-slot-attribute
				     (first direct-slot-definitions))))))))
	  (resolved-subelement-definitions
	   (remove-if #'null
		      (ensure-list
		       (resolve-subelement-descriptor-definition
			(element-slot-subelement (first direct-slot-definitions)))))))
;      (format t "Resolved effective slot: ~A~%" (element-slot-subelements effective-slotd))
;      (format t "Resolved attribute definitions for ~A slot: ~A~%"
;	      slot-name resolved-attribute-definitions)
      (setf (element-slot-attributes effective-slotd)
	    resolved-attribute-definitions)
      (setf (element-slot-subelements effective-slotd)
	    resolved-subelement-definitions)
;      (format t "Resolved effective slot: ~A~%" (element-slot-subelements effective-slotd))
      effective-slotd)))

;; Instances of MY-METACLASS can have superclasses which are instances
;; of any other metaclass.
(defmethod validate-superclass ((class element-class) super)
  t)

(defclass element ()
  ((text :initarg :text :initform nil :accessor element-text)
   (tag-string :initarg :tag :initform nil :accessor element-tag-string))
  (:metaclass element-class))

;; the following code gives objects with element-class as their metaclass
;; a default superclass of element
(defmethod initialize-instance :around
  ((class element-class) &rest initargs  &key direct-superclasses tags)
  (declare (dynamic-extent initargs))

  (if (loop for class in direct-superclasses
            thereis (subtypep class (find-class 'element)))

     ;; 'my-object is already one of the (indirect) superclasses
     (call-next-method)

     ;; 'my-object is not one of the superclasses, so we have to add it
     (apply #'call-next-method
            class
            :direct-superclasses (append direct-superclasses
					 (list (find-class 'element)))
            initargs))
  (assign-node-tag-descriptors class 
			       (parse-node-tag-descriptors tags))
  class)


(defmethod reinitialize-instance :around
  ((class element-class) &rest initargs
   &key (direct-superclasses '() direct-superclasses-p) tags)
  (declare (dynamic-extent initargs))

;  (format t "Reinitializing ~A~% w/direct superclasses? ~A: ~A ~%" (class-name class) direct-superclasses-p direct-superclasses)

  (let ((initargs (append (list :tags (parse-node-tag-descriptors tags))
			  initargs)))
    (if direct-superclasses-p
	;; if direct superclasses are explicitly passed
	;; this is exactly like above
	(if (or (loop for class in direct-superclasses
		      thereis (subtypep class (find-class 'element)))
		(eql 'element (class-name class)))

	    (progn
	      (apply #'call-next-method class 
		     :direct-superclasses direct-superclasses initargs))
	    
	    (apply #'call-next-method
              class
              :direct-superclasses
              (append direct-superclasses
                      (list (find-class 'element)))
              initargs))
	;; if direct superclasses are not explicitly passed
	;; we _must_ not change anything
	(apply #'call-next-method class :direct-superclasses direct-superclasses initargs))))
;    (assign-node-tag-descriptors class 
;				 (parse-node-tag-descriptors tags))
;    class)


(defun assign-node-tag-descriptors (node-class parsed-tag-descriptors)
;  (format t "Assigning ~A~% to ~A~%" parsed-tag-descriptors node-class)
  (setf (element-class-tag-descriptors node-class)
	parsed-tag-descriptors))

(defun parse-node-tag-descriptors (tag-descriptors)
  ;(setf (element-class-tag-descriptors node-class)
;  (format t "resolving tag descriptors ~A~%~A~%" tag-descriptors   (mapcar #'resolve-node-descriptor-definition
;  tag-descriptors))
  (mapcar #'resolve-node-descriptor-definition
	  tag-descriptors))

; class initialization for the element class should instatiate the tags
; in the initargs and index the attribute/subelement parser with relevant
; information passed in on the slots
(defmethod initialize-instance :after ((class element-class) &key)
  (finalize-inheritance class)
  nil)

(defmethod reinitialize-instance :after ((class element-class) &key)
  (finalize-inheritance class)
  nil)

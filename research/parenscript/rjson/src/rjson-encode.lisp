(in-package :org.iodb.rjson)

(defparameter *encoding-session* nil)

(defparameter *allocation-identifier* 'rjalloc
  "Javascript identifier to use as the function name for allocating an
object.

This decoder function has the following properties:
   It takes 2 arguments: the object index, and the object type
as given by RJSON-TYPE on the original object.
   It returns an allocated instance of an object of the given type.
   It side-effects into a lookup-table or something of the like so
that subsequent calls to *XREF-IDENTIFIER* return the allocated object.")

(defparameter *initialization-identifier* 'rjinit
  "Javascript identifier to use as the function name for initializing an
object once it has been allocated.

This decoder function has the following properties:
   It takes 2 arguments: the object index, and initialization data object.
   It returns the object referenced by the provided object index.
   It initializes that object with the provided initialization data object")

(defparameter *xref-identifier* 'rjref
  "Javascript identifier to use as the function name for referencing
a previously loaded object.  Takes one argument: the reference number.

The client function it describes has the following properties:
   It takes 1 argument: the object index of the object referenced.
   It should return the referenced object.")

(defparameter *construction-identifier* 'rjconstruct
  "Javascript identifier to use as the function name for constructing.

The client function it describes has the following properties:
   It takes 2 arguments: the object type to allocate and init, and
the initialization data object.
   It should return an allocated and initialized object.")

(defparameter *rjson-natural-types*
  '("json:object" "json:array" "json:string" "json:number"))

(defgeneric rjson-type (object)
  (:documentation "Returns a string RJSON type specifier as described in the spec.")
  (:method (object)              (declare (ignore object)) "json:object")
  (:method ((object string))     (declare (ignore object)) "json:string")
  (:method ((object number))     (declare (ignore object)) "json:number")
  (:method ((object hash-table)) (declare (ignore object)) "json:object")
  (:method ((object sequence))   (declare (ignore object)) "json:array")
  (:method ((object symbol))     (declare (ignore object)) "lisp:symbol")
  (:method ((object (eql t)))    (declare (ignore object)) "json:true"))

(defgeneric xref-p (object)
  (:documentation "True if the object can or should be cross-referenced while being serialized.")
  (:method (obj)          (declare (ignore obj)) t)
  (:method ((obj string))                        (< 50 (length obj)))
  (:method ((obj null))   (declare (ignore obj)) nil)
  (:method ((obj symbol)) (declare (ignore obj)) (error "Cannot encode symbol!."))
  (:method ((obj number)) (declare (ignore obj)) nil)
  (:method ((obj hash-table)) (declare (ignore obj)) t)
  (:method ((object (eql t))) (declare (ignore object)) nil)
  (:method ((object (eql 'ps:true))) (declare (ignore object)) nil)
  (:method ((object (eql 'ps:false))) (declare (ignore object)) nil))

(defgeneric represent-rjson (object)
  (:documentation
   "Given an object, returns an intermediary SEXP representation of that object.  The SEXP
form allows Javascript literals using the ARRAY and CREATE operators, lisp number objects,
and lisp string objects.  To represent an object referenced by the representation of this
object, call REPRESENT with the object as the argument."))


(defclass encoding-session ()
  ((history        :accessor session-history
                   :initform (make-hash-table)
		   :documentation "Maps objects to history entries.")
   (history-vector :accessor session-history-vector
                   :initform (make-array 10 :fill-pointer 0 :adjustable t)
		   :documentation "Array of history entries."))
  (:documentation "An environmental object for encoding a set of stuff to RJSON."))

(defclass history-entry ()
  ((object    :accessor entry-object :initform nil :initarg :object)
   (parenform :accessor entry-paren-form :initform nil :initarg :parenform)
   (refcount :accessor entry-refcount :initform 0 :initarg :refcount)
   (tag :accessor entry-reftag :initarg :tag :type number)))

(defun register-xref-object (session object &optional (refcount-increase 1))
  "Registers an xref"
  (declare (type (satisfies xref-p) object))
  (with-accessors ((history-vector session-history-vector))
    session
    (let ((entry
	   ;; find the object in the history vector or create a new entry
	   (or (find object history-vector :key #'entry-object)
	       (progn
		 ;; 1. create the entry.
		 (let ((entry
			(make-instance 'history-entry
				       :object object :tag (fill-pointer history-vector))))
		   ;; 2. push it onto the vector
		   (vector-push-extend entry history-vector)
		   ;; 3. set the intermediary represention of it
		   (setf (entry-paren-form entry) (represent-rjson object))
		   entry)))))
      (declare (type history-entry entry))
      ;; increase the refcount 
      (incf (entry-refcount entry) refcount-increase)
      (values
       (entry-reftag entry)
       (eql 1 (entry-refcount entry))))))



(defun represent (object)
  "This is the function users call to generate an appropriate parenscript form with embeddded
cross-references.  This should be called instead of represent-rjson on objects in custom
represent-rjson methods."
  (if (xref-p object)
      (multiple-value-bind (object-tag)
	  (register-xref-object *encoding-session* object)
	`(rjson-xref ,object-tag))
      (represent-rjson object)))

(defmethod represent-rjson ((string string))
  string)

(defmethod represent-rjson ((number number))
  number)

(defmethod represent-rjson ((nothing null)) 
  (declare (ignore nothing))
  nil)

(defmethod represent-rjson ((true (eql t)))
  (declare (ignore true))
  t)

(defmethod represent-rjson ((true (eql 'ps:true)))
  (declare (ignore true))
  t)

(defmethod represent-rjson ((false (eql 'ps:false)))
  (declare (ignore false))
  'ps:false)

(defmethod represent-rjson((seq sequence))
  `(array ,@(map 'list #'(lambda (obj) (represent obj)) seq)))

(defmethod represent-rjson ((object hash-table))
  (let ((create-args (list)))
    (maphash #'(lambda (key value)
		 (push key create-args)
		 (push (represent value) create-args))
	     object)
    (apply #'list 'create (nreverse create-args))))

;(defmethod rjson-type ((object string))
;  "json:string")

(defun encode-rjson (object stream &optional (session (make-instance 'encoding-session)))
  "Encodes an object in RJSON format to the given stream."
  (let* ((*encoding-session* session)
	 ;; parenscript form (intermediate representation) of the object
	 (ir-form (represent object)) 
	 ;; objects where refcount > 1
	 (multiref-entries
	  (remove nil (map 'list #'(lambda (entry)
				     (declare (type history-entry entry))
				     (when (< 1 (entry-refcount entry)) entry))
			   (session-history-vector session))))
	 ;; parenscript forms for allocating multiply-referenced objects 
	 (alloc-forms (mapcar #'(lambda (entry)
				  (declare (type history-entry entry))
				  `(,*allocation-identifier* ,(entry-reftag entry) ,(rjson-type (entry-object entry))))
			      multiref-entries))
	 ;; parenscript forms for initializing multiply-referenced objects
	 (init-forms (mapcar #'(lambda (entry)
				  `(,*initialization-identifier* ,(entry-reftag entry) ,(entry-paren-form entry)))
			     multiref-entries)))
    ;; serialize the parenscript forms to 
    (fast-encode `(create
		   :header (create
			    :allocs (array ,@alloc-forms)
			    :inits (array ,@init-forms))
		   :content ,ir-form)
		 stream)))

;;;; SEXP form serializer

;;; fast-encode-* functions take a stream as the first argument and then sexp forms
;;; as arguments.  The *rjson-forms* alist maps operators to fast-encode functions
(defparameter *rjson-forms*
  `((array . fast-encode-array)
    (create . fast-encode-object)
    (object . fast-encode-object)
    (rjson-xref . fast-encode-xref)))

(defun fast-encode (form stream)
  "Encodes a parenscript form into javascript-compatable notation.
This take place after the initial history bookeeping has been performed."
  (cond
    ((and (consp form) (not (null form)))
     (let ((encoder (cdr (assoc (car form) *rjson-forms*))))
	  (if encoder
	      (apply encoder stream (rest form))
	      (apply #'fast-encode-funcall stream form))))
    (t (json::encode-json form stream))))

(defun fast-encode-array (stream &rest forms)
  "Given a bunch of forms, encodes them as an array."
  (write-char #\[ stream)
  (fast-encode-comma-delimited stream forms)
  (write-char #\] stream)
  (values))

(defun fast-encode-object (stream &rest args)
  "Encodes a Javascript object.  Args is a list of keyword arguments (i.e. (:key1 val1
:key2 val2))."
  (write-char #\{ stream)
  (loop for (name value more?) on args by #'cddr
	do
	(fast-encode name stream)
	(write-char #\: stream)
	(fast-encode value stream)
	(when more? (write-char #\, stream)))
  (write-char #\} stream)
  (values))

(defun fast-encode-funcall (stream func-name &rest args)
  "Encodes a function call."
  (write-string (string-downcase (string func-name)) stream)
  (write-char #\( stream)
  (fast-encode-comma-delimited stream args)
  (write-char #\) stream)
  (values))

(defun fast-encode-comma-delimited (stream forms &optional (form-encoder #'fast-encode))
  "Encodes the list of forms, FORMS, into the output stream, STREAM, placing a comma
in between each.  The function, FORM-ENCODER, is called with 2 arguments: the form to encode
and the stream to write to."
  (loop for (value . more?) on forms
     do
       (funcall form-encoder value stream)
       (when more? (write-char #\, stream)))
  (values))

(defun fast-encode-xref (stream reftag)
  "Serializes an intermediary RJSON-XREF form into javascript."
  (let* ((entry (aref (session-history-vector *encoding-session*) reftag))
	 (type (rjson-type (entry-object entry))))
    (declare (type history-entry entry))
    (with-accessors ((refcount entry-refcount)
		     (reftag entry-reftag)
		     (object entry-object)
		     (paren-form entry-paren-form))
      entry
      (fast-encode
       (cond
	 ((and (= 1 refcount) type (not (= 5 (mismatch "json:" type))))
	  `(,*construction-identifier* ,type ,paren-form))
	 ((and (= 1 refcount))
	  paren-form)
	 (t
	  `(,*xref-identifier* ,reftag)))
       stream))))

(defun encode-rjson-to-string (object &optional (session (make-instance 'encoding-session)))
  "Encodes an object with ENCODE-RJSON and outputs it to a string."
  (with-output-to-string (stream)
    (encode-rjson object stream session)))
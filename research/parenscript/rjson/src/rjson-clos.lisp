(in-package :rjson)
;;; This File Includes Utilities for sending CLOS instances across the net as
(defmethod represent-rjson ((object structure-object))
  "Represents an object's slots and such in an RJSON-compatible way."
  (error "Cannot represent structures with RJSON."))

;;; RJSON
(defgeneric represent-rjson-slot (object slot)
  (:documentation "Should return two parenscript forms, one for
the key to the slot and the other for the value of the slot.
Returns nil if the slot should not be included."))

(defmethod represent-rjson-slot ((object standard-object) slot)
  (let ((value
	 (if (slot-boundp-using-class (class-of object) object slot)
	     (slot-value-using-class (class-of object) object slot)
	     'undefined)))
    (if (or (null value) (eql 'undefined value))
	nil
	(values (parenscript:symbol-to-js-string (slot-definition-name slot))
		(represent value)))))

(defmethod rjson-type ((object standard-object))
  (string-downcase (symbol-name (class-name (class-of object)))))

(defmethod represent-rjson ((object standard-object))
  "Represents an object's slots and such in an RJSON-compatible way."
  `(create ,@(mapcan #'(lambda (slot)
			 (multiple-value-bind (key value)
			     (represent-rjson-slot object slot)
			   (if (null key) nil (list key value))))
		     (class-slots (class-of object)))))
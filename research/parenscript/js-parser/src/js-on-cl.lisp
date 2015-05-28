(defpackage js-in-cl
  (:use)
  )

(defclass js-value ()
  ((value :accessor value :initform nil :initarg :value
	  :documentation "a JS value takes on one of a 6 language types:
undefined; null; boolean; string; number; object.")
   (type :accessor type :initform nil :initarg :type)))
	 
    
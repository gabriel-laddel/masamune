(in-package :xml-mop)
;;; We wish to let users create a :SPECIAL-OPTION slot option.
;;; The goal isn't to do anything interesting (currently), just show
;;; one of the basics in using the MOP.

;; We define the metaclass we want.
(defclass my-metaclass (standard-class) ())

;; Define the slots we want. The "direct slot definition" (DSD) is
;; pretty much the slot definitions we see in DEFCLASS.
;; The "effective slot definition" (ESD) is the object built
;; from the DSD.
(defclass my-direct-slot-definition (closer-mop:standard-direct-slot-definition)
  ((special-option :accessor special-option :initarg :special-option)))

(defclass my-effective-slot-definition (standard-effective-slot-definition)
  ((special-option :accessor special-option :initarg :special-option)))


;; Now the object system knows to use our new slot definitions when 
;; dealing with MY-METACLASS, rather than the default ones.
(defmethod direct-slot-definition-class ((class my-metaclass) &key &allow-other-keys)
  (find-class 'my-direct-slot-definition))

(defmethod effective-slot-definition-class ((class my-metaclass) &key &allow-other-keys)
  (find-class 'my-effective-slot-definition))


;; Initialize the effective slot.
(defmethod compute-effective-slot-definition ((class my-metaclass) slot-name direct-slot-definitions)
  (declare (ignore slot-name))
  (let ((effective-slotd (call-next-method))) ;let CLOS do the lifting
    (setf (special-option effective-slotd)
          (special-option (first direct-slot-definitions)))
    (print (special-option effective-slotd))
    effective-slotd))

;; Instances of MY-METACLASS can have superclasses which are instances
;; of any other metaclass.
(defmethod closer-mop:validate-superclass ((class my-metaclass) super)
  t)


;; So finally, we can use the :SPECIAL-OPTION slot option.

(defclass userclass-1 ()
  ((userslot1 :accessor userslot1 :initarg :userslot1 :special-option :bar))
  (:metaclass my-metaclass))

;(special-option (first (class-slots (find-class 'userclass-1))))
; => :bar

(defclass userclass-2 ()
  ((userslot1 :accessor userslot1 :initarg :userslot1
              :special-option :bar
              :special-option :quux))
  (:metaclass my-metaclass))

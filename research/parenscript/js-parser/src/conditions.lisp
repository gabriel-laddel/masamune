;;;; conditions.lisp
;;;
;;; Defines the hierarchy of custom conditions that are used by js-parser
;;;
;;; Copyright (c) 2006 by James Wright
;;;
(in-package :js-parser)

(define-condition positioned-condition (condition)
  ((filename :initarg :filename :initform nil :reader filename)
   (pos :initarg :pos :reader pos)
   (row :initarg :row :reader row)
   (column :initarg :column :reader column))
  (:documentation "Represents a condition that has a source position associated with it"))

(define-condition syntax-error (error positioned-condition)
  ((token :initarg :token :reader token)
   (expected-terminals :initarg :expected-terminals :reader expected-terminals))
  (:documentation "Indicates that an error occured during parsing"))

(defmethod print-object ((e syntax-error) s)
  (unless (and (slot-boundp e 'row)
               (slot-boundp e 'column)
               (slot-boundp e 'token))
    (return-from print-object (call-next-method)))

  (cond
    (*print-escape*
     (print-unreadable-object (e s :type t :identity nil)
       (format s "~A (~D,~D): Unexpected terminal ~S"
	       (or (filename e) "")
	       (row e) (column e)
	       (or (token-terminal (token e)) 'eof))))
    ((slot-boundp e 'expected-terminals)
     (format s "~A (~D,~D): Unexpected terminal ~S (value: ~S)~%~
                  Expected one of ~S"
	     (or (filename e) "")
	     (row e) (column e)
	     (or (token-terminal (token e)) 'eof) (token-value (token e))
	     (expected-terminals e)))
    (t
     (format s "~A (~D,~D): Unexpected terminal ~S (value: ~S)"
	     (or (filename e) "")
	     (row e) (column e)
	     (or (token-terminal (token e)) 'eof) (token-value (token e))))))

(define-condition missing-import (error positioned-condition)
  ((parent-uripath :initarg :parent-uripath :reader parent-uripath) ; Name of the module where the bad import occurs
   (import-uripath :initarg :import-uripath :reader import-uripath)) ; URI-path of the missing import
  (:documentation "Indicates that an import could not be located"))

(defmethod print-object ((e missing-import) s)
  (unless (and (slot-boundp e 'row)
               (slot-boundp e 'column)
               (slot-boundp e 'parent-uripath)
               (slot-boundp e 'import-uripath))
    (return-from print-object (call-next-method)))

  (if *print-escape*
    (print-unreadable-object (e s :type t :identity nil)
      (format s "~A:~D,~D: Missing import ~S" (parent-uripath e) (row e) (column e) (import-uripath e)))
    (format s "~A:~D,~D: Missing import ~S" (parent-uripath e) (row e) (column e) (import-uripath e))))

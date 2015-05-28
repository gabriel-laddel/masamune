;;;; ugly-print.lisp 
;;; 
;;; Provides functions for printing ASTs in a pareseable, but hopefully un-human readable form
;;; The key functionality provided is a source transformation to ensure ALL identifiers are made 
;;; unique
;;;
;;; Note that pretty-print was modified to output without formatting if *pretty-mode*
;;; and *opt-space* are correctly set.
;;; Essentially all we do in this is the uniquifying transformation.
;;;
;;; Unit tests in tests/ugly-print.lisp.
;;;
;;; Copyright (c) 2005 Greg Smolyn and James Wright
;;; See LICENSE for full licensing details.
;;;
(in-package :js-parser) 

; Our main entry point to the ugly printer
;   Please note that we're cheating somewhat-- the source
(defun ugly-print (elm stream)
  "Outputs the AST to a stream with variables and function names converted to
   unique identifiers (ie. JW0) and with all formatting removed."
  (let* ((*pretty-mode* nil)
         (*opt-space* "")
         (new-elm (uglify-vars elm)))
    (pretty-print new-elm stream)))

;;; ==================================================
;;;
;;; Simple environment ADT (ribcage)
;;;
;;; the environment is a list of association lists 
;;; alists contain pairs of (oldname . newname), in this case they will be strings
;;;
;;; Everytime we enter a new lexical environment, we add a new alist to the front.
;;; When looking for a binding, we start at the front (innermost) and look through until we get to the back (global)

(defparameter *environment* '()
  "list of assoc lists representing the current lexical environment")

(defmacro with-added-environment (&body body)
  "Executes BODY with a new environment added to the environment stack"
  `(let ((*environment* (cons '() *environment*)))
    ,@body))

(defun find-binding (var-name)
  "Looks through the set of environments and finds the most recently bound variable, returns its bound value"
  (labels ((f-b-h (environment)
             (if (null environment)
               nil
               (let ((var-pair (assoc var-name (car environment) :test #'equal)))
                 (if (null var-pair)
                   (f-b-h (cdr environment))
                   (cdr var-pair))))))
    (f-b-h *environment*)))

(defun add-binding (var-name var-newname)
  "Add a binding to the environment. In our case, name and new name"
  (push (cons var-name var-newname) (car *environment*)))

(defun ensure-unique-binding (var-name)
  "Adds a unique name for VAR-NAME to the environment, and returns the unique name.
   The unique name will be uglified or otherwise 'uniquified' for non-toplevel
   identifiers; toplevel identifiers will be left as-is."
  (cond
    (*in-local-scope*
     (let ((ugly-name (genvar var-name)))
       (add-binding var-name ugly-name)
       ugly-name))
    (t
     ;;TODO Warnings may have to be done differently once compiler errors are
     ;; being properly reported.
     (when (find-binding var-name)
       (warn "Duplicate top-level identifier '~A'" var-name))
     (add-binding var-name var-name)
     var-name)))

(defparameter *genvar-counter* 0)

(defun genvar (&optional orig-name)
  "Generates a unique string that will be our ugly name for variables.
   When *PRETTY-MODE* is non-NIL and ORIG-NAME is provided, the name will
   be unique but not all that ugly."
  (let ((old *genvar-counter*))
    (incf *genvar-counter*)
    (if (and *pretty-mode* orig-name)
      (format nil "~A$~D" orig-name old)
      (format nil "JW~D" old))))


;;; ==================================================
;;;
;;; uniquify source transformation
;;;
;;; Converts all variable declarations and function names into indiscernable unique names such as JW0.
;;; 
;;; Subsequently ensures that all identifiers are unique (all scoping will have been sorted out by this point)

(defun uglify-vars (program)
  "Entry point for our source transformation. Turn all var and function declarations and their related 
   references s into ugly ones"
  (with-added-environment
    (transform-in-scope program)))

(defun transform-in-scope (elm)
  "Transforms source elements for the current scope. Given an element, collects all
   var-decls and fun-decls and adds them to the current environment. THEN goes through
   and transforms identifiers + names in said environment. This calls into the main
   uniquify transform methods, and subsequently will recurse through the tree"
  (dolist (var-decl (collect-in-scope elm 'var-decl)) 
    (ensure-unique-binding (var-decl-name var-decl)))
  (dolist (fun-decl (collect-in-scope elm 'function-decl))
    (ensure-unique-binding (function-decl-name fun-decl)))
  (transform 'uniquify elm))

;;; Guarantee that we will always have an environment available
(defmethod transform :around ((xform (eql 'uniquify)) elm)
  (if (null *environment*)
    (with-added-environment
      (transform-in-scope elm))
    (call-next-method)))

(defmethod transform ((xform (eql 'uniquify)) (elm identifier))
  (let ((new-name (find-binding (identifier-name elm))))
    ;; If the identifier is defined in this script, use its unique name.
    ;; Otherwise, return it unmodified (to account for system globals like document or XmlHttpRequest)
    (if new-name
      (make-identifier :name new-name
                       :start (source-element-start elm)
                       :end (source-element-end elm))
      elm)))

(defmethod transform ((xform (eql 'uniquify)) (elm function-decl))
  (with-added-environment

    ;; Make sure that the function name has a binding in the current environment
    ;; (It will if this source-element was part of a list, but it won't if this
    ;; is a singleton source element)
    (unless (find-binding (function-decl-name elm))
      (ensure-unique-binding (function-decl-name elm)))

    (in-local-scope
      (let ((new-params (mapcar #'ensure-unique-binding (function-decl-parameters elm))))
        (make-function-decl :name (find-binding (function-decl-name elm))
                            :parameters new-params
                            :body (transform-in-scope (function-decl-body elm))
                            :start (source-element-start elm)
                            :end (source-element-end elm))))))
  
(defmethod transform ((xform (eql 'uniquify)) (elm function-expression))
  (with-added-environment
    (in-local-scope
      (let* ((new-name (ensure-unique-binding (function-expression-name elm)))
             (new-params (mapcar #'ensure-unique-binding (function-expression-parameters elm))))
        (make-function-expression :name new-name
                                  :parameters new-params
                                  :body (transform-in-scope (function-expression-body elm))
                                  :start (source-element-start elm)
                                  :end (source-element-end elm))))))
  
(defmethod transform ((xform (eql 'uniquify)) (elm var-decl))
  (unless (find-binding (var-decl-name elm))
    (ensure-unique-binding (var-decl-name elm)))
  (make-var-decl :name (find-binding (var-decl-name elm))
                 :initializer (transform xform (var-decl-initializer elm))
                 :start (source-element-start elm)
                 :end (source-element-end elm)))
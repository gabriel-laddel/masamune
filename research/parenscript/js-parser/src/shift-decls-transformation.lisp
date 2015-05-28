;;;; shift-function-decls-transformation.lisp
;;;
;;; Define the shift-function-decls transformation.
;;;
;;; Copyright (c) 2005 James Wright
;;; See LICENSE for full licensing details.
;;;
(in-package :js-parser)

;;;; ======= shift-decls transformation ============================================================
;;;
;;; This transformation moves function declarations to the beginning
;;; of each scope in the provided AST.  Function decls are never moved
;;; to a different scope, and they will always appear in the same order
;;; as originally, so this transfomation is semantically neutral.
;;; 
;;; This transformation also converts variable declarations at the toplevel scope
;;; into simple assignments, and adds empty variable declarations to the top of the
;;; scope.
;;;
;;; These shifts are to ensure that visibility is maintained even after cps conversion.
;;; We want to make sure that function declarations and global variable declarations
;;; never get absorbed into the declaration of a continuation function, since they will
;;; then be invisible to code that occurs outside the continuation function.
;;;
;;; NOTE: The Javascript definition of "scope" is slightly different
;;; than you might expect.  In particular, only function-decls and
;;; function-expressions create new scopes; statement blocks do not.
;;;
;;; Note also that function-expressions will /not/ be moved, since
;;; they are values (and also their identifiers do not affect their
;;; enclosing scope; see Pg 71 of ECMA-262)

(defun shift-function-decls (elm-list)
  "Moves all function-decls in elm-list to the front"
  (let ((fn-decls (collect-in-scope elm-list 'function-decl))
        (other-statements (remove-if #'function-decl-p elm-list)))
    (append fn-decls other-statements)))

(defun shift-var-decls (elm-list)
  "Adds empty variable declarations at the top of elm-list for variable declarations
   that have new-exprs or fn-calls as initializers.  The original variable decls are
   converted to simple assignments."
  (let ((stripped-var-decl-stmts (mapcar (lambda (decl)
                                           (make-var-init (var-decl-name decl) nil))
                                         (collect-in-scope elm-list 'var-decl)))
        (stripped-elm-list (transform 'strip-var-decls-in-scope elm-list)))
    (append stripped-var-decl-stmts
            stripped-elm-list)))

(defmethod transform ((xform (eql 'shift-decls)) (elm-list list))
  (let ((shifted-elms (if *in-local-scope*
                         (shift-function-decls elm-list)
                         (shift-var-decls (shift-function-decls elm-list)))))
    (mapcar (lambda (elm)
              (transform 'shift-decls elm))
            shifted-elms)))

(defmethod transform ((xform (eql 'shift-decls)) (elm source-element))
  (apply
   (get-constructor elm)
   (loop for slot in (structure-slots elm)
         unless (function-decl-p (slot-value elm slot)) collect (make-keyword slot)
         unless (function-decl-p (slot-value elm slot)) collect (transform xform (slot-value elm slot)))))


(defmethod transform ((xform (eql 'shift-decls)) (elm object-literal))
  (make-object-literal
   :properties
   (loop for (prop-name . prop-value) in (object-literal-properties elm)
         collect (cons
                  (transform 'shift-decls prop-name)
                  (transform 'shift-decls prop-value)))))

(defmethod transform ((xform (eql 'shift-decls)) (elm var-decl-statement))
  (make-var-decl-statement :var-decls (mapcar (lambda (decl)
                                                (transform 'shift-decls decl))
                                              (var-decl-statement-var-decls elm))
                           :start (source-element-start elm)
                           :end (source-element-end elm)))

(defmethod transform ((xform (eql 'shift-decls)) (elm function-decl))
  (in-local-scope
    (call-next-method)))

(defmethod transform ((xform (eql 'shift-decls)) (elm function-expression))
  (in-local-scope
    (call-next-method)))

;;;; ======= strip-var-decls-in-scope transformation ===============================================

;;; Shifting function decls is relatively easy, because they can't be nested inside of other
;;; statements.  We can get a list of var-decls to shift relatively easily using COLLECT-IN-SCOPE,
;;; but we need a tranformation to do the actual stripping.

;; Strip all the var decls that we encounter (ie, convert them into assignments)
(defmethod transform ((xform (eql 'strip-var-decls-in-scope)) (elm var-decl-statement))
  (loop for decl in (var-decl-statement-var-decls elm)
        for name = (var-decl-name decl)
        for initializer = (var-decl-initializer decl)
        unless (null initializer)
        collect (make-binary-operator :op-symbol :assign
                                      :left-arg (make-identifier :name name)
                                      :right-arg initializer
                                      :start (source-element-start decl)
                                      :end (source-element-end decl))))

;; Don't recurse into functions (that's the "in-scope" part)
(defmethod transform ((xform (eql 'strip-var-decls-in-scope)) (elm function-decl))
  elm)

(defmethod transform ((xform (eql 'strip-var-decls-in-scope)) (elm function-expression))
  elm)


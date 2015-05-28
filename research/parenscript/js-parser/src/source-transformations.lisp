;;;; source-transformations.lisp
;;;
;;; Define the base functionality for source transformations.
;;; The TRANSFORM generic function is defined, as well as some
;;; transformation-related utility functions.  The main source
;;; transformations will implement methods on TRANSFORM in their
;;; own source files.
;;;
;;; Copyright (c) 2005 James Wright
;;; See LICENSE for full licensing details.
;;;
(in-package :js-parser)

;;;; Utilities 
(defun structure-slots (object)
  "Returns a list of the slot-names of the provided structure object"
  #+openmcl
  (let* ((sd (gethash (class-name (class-of object)) ccl::%defstructs%))
	 (slots (if sd (ccl::sd-slots sd))))
    (mapcar #'car (if (symbolp (caar slots)) slots (cdr slots))))
  #+cmu
  (mapcar #'pcl:slot-definition-name (pcl:class-slots (class-of object)))
  #+lispworks
  (structure:structure-class-slot-names (class-of object))
  #+sbcl
  (mapcar #'sb-mop:slot-definition-name (sb-mop:class-slots (class-of object)))
  #+allegro
  (mapcar #'mop:slot-definition-name (mop:class-slots (class-of object))))

(defparameter *constructor-cache* (make-hash-table :test 'eq)
  "Map from structure-type symbol to copy-function symbol")

(defun get-constructor (struct-object)
  "Accept a structure object and return the (likely) name of its constructor.
   CAVEAT: Assumes that the structure was defined in the same package as its name."
  (let* ((name (type-of struct-object))
         (make-fn (gethash name *constructor-cache*)))
    (if (null make-fn)
      (setf (gethash name *constructor-cache*) (intern (format nil "MAKE-~A" name)
                                                       (symbol-package name)))
      make-fn)))

(defmacro forbid-transformation-elements (xform elm-type-list)
  "Generate DEFMETHOD forms that throw an error if the transformation
   specified in XFORM is applied to any of the element types in
   ELM-TYPE-LIST"
  `(progn
    ,@(loop for elm-type in elm-type-list
            collect `(defmethod transform ((xform (eql ',xform)) (elm ,elm-type))
                      (error "~A source-element encountered during ~A transformation!" ',elm-type ',xform)))))
                   

;;;; Scope tracking 

;;; Some transformations need to keep track of toplevel vs. local scopes.
;;; They should use this macro and variable.

(defparameter *in-local-scope* nil
  "T when the lexical scope is currently inside a function decl, NIL when the
   lexical scope is currently the global scope")

(defmacro in-local-scope (&body body)
  "Execute BODY with *IN-LOCAL-SCOPE* bound to T"
  `(let ((*in-local-scope* t))
    ,@body))

;;;; Collection within a single scope  
(defgeneric collect-in-scope (elm target-type)
  (:documentation
   "Finds and returns a list of all elements of TARGET-TYPE in the same scope as
    ELM.  Does not recurse into function-decl or function-expression elements.
    So for example, searching for function-decls in this code:

       var x = 10;
       function foo(x) { function bar(y) { return 10; } return bar(x); }

    FOO would be returned but BAR would not, since the decl of BAR is in
    an innermore scope (namely, FOO's body)."))

;;;; Rules about recursing into children 
(defmethod collect-in-scope (elm target-type)
  nil)

(defmethod collect-in-scope ((elm-list list) target-type)
  (loop for elm in elm-list
        append (collect-in-scope elm target-type)))

(defmethod collect-in-scope ((elm source-element) target-type)
  (loop for slot in (structure-slots elm)
        append (collect-in-scope (slot-value elm slot) target-type)))

;; Don't recurse, because the body is a new, innermore scope.
(defmethod collect-in-scope ((elm function-decl) target-type)
  nil)

;; Don't recurse, because the body is a new, innermore scope.
(defmethod collect-in-scope ((elm function-expression) target-type)
  nil)

;; Don't recurse, because the body is a new, innermore scope.
(defmethod collect-in-scope ((elm object-literal) target-type)
  nil)

;;;; Rule for returning matches. 
;; We don't recurse into matching elements
(defmethod collect-in-scope :around (elm target-type)
  (if (typep elm target-type)
    (list elm)
    (call-next-method)))

;;;; Default transformation behaviour 

;;; The top-level TRANSFORM methods provide the default code-walking behaviour,
;;; so that individual transformations can override just the important parts.
;;;
;;; The transformation of any "obligate statement" may return a list instead of
;;; a single source-element, and the default behaviour will handle it correctly.
;;; An obligate statement is a source element that must be a statement (ie, which
;;; can never be an expression).  For example, a function call is _not_ an
;;; obligate statement, since it can appear as a sub-expression, but a break
;;; statement is.

(defgeneric transform (xform elm)
  (:documentation
   "Accepts a transformation name (symbol) and a source element, and returns a new
    source element that has been transformed in some way.  Methods should /not/ perform
    destructive updates on the provided source-element."))

;; The default behaviour for any transformation is to do nothing
(defmethod transform (xform elm)
  elm)

(defun make-keyword (x)
  "Makes a keyword out of a symbol."
  (if (keywordp x) x (intern (symbol-name x) 'keyword)))

;; The default behaviour for any transformation on a source-element that has children
;; is to return a new source-element whose children have been transformed.
(defmethod transform (xform (elm source-element))
  (apply
   (get-constructor elm)
   (loop for slot in (structure-slots elm)
         collect (make-keyword slot)
         collect (transform xform (slot-value elm slot)))))

;; Sometimes we're dealing with lists of source elements, and sometimes (rarely) we're
;; dealing with some other sort of list.  We only flatten source element lists.
;; We decide if it is a source-element list based on the first element only.
(defmethod transform (xform (elm-list list))
  (if (source-element-p (car elm-list))
    (loop for elm in elm-list
          for tx-elm = (transform xform elm)
          if (listp tx-elm)
          append tx-elm
          else
          collect tx-elm)
    (loop for elm in elm-list
          for tx-elm = (transform xform elm)
          collect tx-elm)))

;; Override the default slot-traversing behaviour for elements that have single-statement
;; children, since we might need to single-statement them.
(defmethod transform (xform (elm if-statement))
  (with-slots (condition then-statement else-statement) elm
    (make-if-statement :condition (transform xform condition)
                       :then-statement (single-statement (transform xform then-statement))
                       :else-statement (single-statement (transform xform else-statement)))))

(defmethod transform (xform (elm while))
  (with-slots (label body condition) elm
    (make-while :label label
                :condition (transform xform condition)
                :body (single-statement (transform xform body)))))

(defmethod transform (xform (elm do-statement))
  (with-slots (label body condition) elm
    (make-do-statement :label label
                       :condition (transform xform condition)
                       :body (single-statement (transform xform body)))))
  
(defmethod transform (xform (elm for))
  (with-slots (label body initializer condition step) elm
    (make-for :label label
              :initializer (transform xform initializer)
              :condition (transform xform condition)
              :step (transform xform step)
              :body (single-statement (transform xform body)))))

(defmethod transform (xform (elm for-in))
  (with-slots (label body binding collection) elm
    (make-for-in :label label
                 :binding (transform xform binding)
                 :collection (transform xform collection)
                 :body (single-statement (transform xform body)))))

(defmethod transform (xform (elm with))
  (with-slots (label scope-object body) elm
    (make-with :label label
               :scope-object (transform xform scope-object)
               :body (single-statement (transform xform body)))))

(defmethod transform (xform (elm try))
  (with-slots (label body catch-clause finally-clause) elm
    (make-try :label label
              :body (transform xform body)
              :catch-clause (transform xform catch-clause)
              :finally-clause (transform xform finally-clause))))

;; Special case for object-literals to account for the fact that object-literal-properties
;; is an alist rather than a list of structures.
(defmethod transform (xform (elm object-literal))
  (make-object-literal
   :properties
   (loop for (prop-name . prop-value) in (object-literal-properties elm)
         collect (cons
                  (transform xform prop-name)
                  (transform xform prop-value)))
   :start (source-element-start elm)
   :end (source-element-end elm)))

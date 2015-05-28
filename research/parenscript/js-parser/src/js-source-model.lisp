;;;; js-source-model.lisp
;;;
;;; Defines the data structures that are used for the internal representation
;;; of parsed Javascript source files.
;;;
;;; Copyright (c) 2005-2006 James Wright
;;; See LICENSE for full licensing details.
;;;
(in-package :js-parser)

#-(or sbcl cmu)
(defmacro defelement (name &rest slots)
  `(defstruct ,name ,@slots))

#+(or sbcl cmu)
(defmacro defelement (name &rest slots)
  (let ((type (if (consp name) (first name) name)))
    `(progn
       (defstruct ,name ,@slots)
       (defmethod make-load-form ((self ,type) &optional environment)
         (make-load-form-saving-slots self :environment environment)))))

;;;; ======= Javascript source element structures ==================================================

(defelement source-element
  "A common base type for all source elements"
  (label nil :type (or string null))
  (start nil :type (or number null))
  (end nil :type (or number null)))

(defelement (expression (:include source-element))
  "A common base type for all source elements that can be expressions.
   (note that every expression can be a statement, but not all statements
    can be expressions)")

(defelement (special-value (:include expression))
  (symbol nil :type symbol))

(defelement (identifier (:include expression))
  (name nil :type string))

(defelement (numeric-literal (:include expression))
  (value nil :type number))

(defelement (string-literal (:include expression))
  (value nil :type string))

(defelement (array-literal (:include expression))
  (elements nil :type list))

(defelement (object-literal (:include expression))
  (properties nil :type list))  ; List of (PROPERTY-NAME . PROPERTY-VALUE)
                                ; PROPERTY-NAME is a STRING-LITERAL

(defelement (re-literal (:include expression))
  pattern
  options)

(defelement (new-expr (:include expression))
  (constructor nil :type expression)
  (args nil :type (or (cons source-element) null)))

(defelement (fn-call (:include expression))
  (fn nil :type expression)
  (args nil :type (or (cons source-element) null)))

(defelement (property-access (:include expression))
  (target nil :type source-element)
  (field nil :type source-element))

(defelement (unary-operator (:include expression))
  (op-symbol nil :type symbol)
  (arg nil :type source-element))

(defelement (binary-operator (:include expression))
  (op-symbol nil :type symbol)
  (left-arg nil :type source-element)
  (right-arg nil :type source-element))

(defelement (conditional (:include expression))
  (condition nil :type expression)
  (true-arg nil :type expression)
  (false-arg nil :type expression))

(defelement (comma-expr (:include expression))
  (exprs nil :type (or (cons expression) null)))

(defelement (var-decl-statement (:include source-element))
  (var-decls nil :type (cons var-decl)))

(defelement (var-decl (:include source-element))
  (name nil :type string)
  (initializer nil :type (or expression null)))

(defelement (statement-block (:include source-element))
  (statements nil :type (or (cons source-element) null)))

(defelement (if-statement (:include source-element))
  (condition nil :type expression)
  (then-statement nil :type (or source-element null))
  (else-statement nil :type (or source-element null)))

(defelement (do-statement (:include source-element))
  (condition nil :type expression)
  (body nil :type source-element))

(defelement (while (:include source-element))
  (condition nil :type expression)
  (body nil :type (or source-element null)))

(defelement (for (:include source-element))
  (initializer nil :type (or source-element null))
  (condition nil :type (or expression null))
  (step nil :type (or expression null))
  (body nil :type (or source-element null)))

(defelement (for-in (:include source-element))
  (binding nil :type source-element)
  (collection nil :type expression)
  (body nil :type (or source-element null)))

(defelement (continue-statement (:include source-element))
  (target-label nil :type (or string null)))

(defelement (break-statement (:include source-element))
  (target-label nil :type (or string null)))

(defelement (return-statement (:include source-element))
  (arg nil :type (or source-element null)))

(defelement (with (:include source-element))
  (scope-object nil :type expression)
  (body nil :type source-element))

(defelement (switch (:include source-element))
  (value nil :type expression)
  (clauses nil :type list))

(defelement (case-clause (:include source-element))
  (value nil :type expression)
  (body nil :type (or (cons source-element) null)))

(defelement (default-clause (:include source-element))
  (body nil :type (or (cons source-element) null)))

(defelement (throw-statement (:include source-element))
  (value nil :type (or expression null))
  (target nil :type (or expression null)))

(defelement (try (:include source-element))
  (body nil :type (or (cons source-element) null))
  (catch-clause nil :type (or catch-clause null))
  (finally-clause nil :type (or finally-clause null)))

(defelement (catch-clause (:include source-element))
  (binding nil :type string)
  (body nil :type (or (cons source-element) null)))

(defelement (finally-clause (:include source-element))
  (body nil :type (or (cons source-element) null)))

(defelement (function-decl (:include source-element))
  (name nil :type string)
  (parameters nil :type (or (cons string) null))
  (body nil :type (or (cons source-element) (cons null) null)))

(defelement (function-expression (:include expression))
  (name nil :type (or string null))
  (parameters nil :type (or (cons string) null))
  (body nil :type (or (cons source-element) null)))

;;;; ------- "Administrative" source elements ------------------------------------------------------
(defelement (continuation-function (:include function-expression))
  "A function expression that is used as a continuation")

(defelement (thunk-function (:include function-expression))
  "A function expression that is used as a thunk in a boxed trampoline result")

(defelement (continuation-call (:include fn-call))
  "A call to a continuation (as opposed to a call to any other sort of function)")

(defelement (add-handler (:include source-element))
  "Indicates that a new handler should be added to the handler stack at this point"
  (handler nil :type (or identifier function-expression))
  (thunk-body nil :type (or (cons source-element) null)))

(defelement (remove-handler (:include source-element))
  "Indicates that a handler should be removed from the top of the handler stack at this point"
  (handler nil :type (or identifier function-expression))
  (thunk-body nil :type (or (cons source-element) null)))

;;;; ------- js-on-cl extended syntax -----------------------------------------------------------------
(defelement (suspend-statement (:include source-element)))

(defelement (resume-statement (:include source-element))
  (target nil :type expression)
  (arg nil :type (or expression null)))

(defelement (import-decl (:include source-element))
  (type-symbol nil :type symbol)
  (uripath nil :type string))

;;;; ======= Static source element properties ======================================================

;;;; ------- Broader classes of source element -----------------------------------------------------
(defun call-expression-p (elm)
  "Return non-NIL if ELM is a CallExpression, or NIL otherwise."
  (or (fn-call-p elm)
      (and (property-access-p elm)
           (call-expression-p (property-access-target elm)))))

;;;; ------- Element precedence and associativity --------------------------------------------------
(defgeneric elm-precedence (elm)
  (:documentation
   "Returns an integer specifying the precedence of the source element
    ELM.  Smaller numbers represent higher precedence.  The precedence
    numbers have no significance except relative to each other."))

(defmethod elm-precedence ((elm source-element))
  (assert (member (type-of elm)
                  '(special-value
                    identifier
                    numeric-literal
                    string-literal
                    array-literal
                    object-literal
                    re-literal)))
  0)

(defmethod elm-precedence ((elm new-expr))
  1)

(defmethod elm-precedence ((elm fn-call))
  2)

(defmethod elm-precedence ((elm property-access))
  (if (call-expression-p (property-access-target elm))
    2
    1))

(defmethod elm-precedence ((elm function-expression))
  3)

(defmethod elm-precedence ((elm unary-operator))
  (ecase (unary-operator-op-symbol elm)
    ((:post-incr :post-decr)
     3)
    ((:delete :void :typeof :pre-incr :pre-decr :unary-plus :unary-minus :logical-not :bitwise-not)
     4)))

(defmethod elm-precedence ((elm binary-operator))
  (ecase (binary-operator-op-symbol elm)
    ((:multiply :divide :modulo)
     5)
    ((:add :subtract)
     6)
    ((:lshift :rshift :urshift)
     7)
    ((:less-than :greater-than :less-than-equals :greater-than-equals :instanceof)
     8)
    ((:equals :not-equals :strict-equals :strict-not-equals)
     9)
    (:bitwise-and
     10)
    (:bitwise-xor
     11)
    (:bitwise-or
     12)
    (:logical-and
     13)
    (:logical-or
     14)
    ((:assign :times-equals :divide-equals :mod-equals :plus-equals :minus-equals
      :lshift-equals :rshift-equals :urshift-equals :and-equals :xor-equals :or-equals)
     16)))

(defmethod elm-precedence ((elm conditional))
  15)

(defmethod elm-precedence ((elm comma-expr))
  17)

(defgeneric elm-associativity (operator-or-elm)
  (:documentation
   "Returns either :LEFT, :RIGHT, or NIL to indicate the associativity of the operator
    represented by OP-SYMBOL-OR-ELM (representing left-, right-, and no-associativity
    respectively."))

(defmethod elm-associativity (operator-or-elm)
  nil)

(defmethod elm-associativity ((operator-or-elm binary-operator))
  (case (binary-operator-op-symbol operator-or-elm)
    ((:multiply :divide :modulo :add :subtract :lshift :rshift :urshift
                :less-than :greater-than :less-than-equals :greater-than-equals :instanceof
                :equals :not-equals :strict-equals :strict-not-equals
                :bitwise-and :bitwise-or :bitwise-xor :logical-and :logical-or
                fn-call property-access)
     :left)
    ((:assign :times-equals :divide-equals :mod-equals :plus-equals :minus-equals
              :lshift-equals :rshift-equals :urshift-equals :and-equals :xor-equals :or-equals
              new-expr)
     :right)))

(defmethod elm-associativity ((operator-or-elm fn-call))
  :left)

(defmethod elm-associativity ((operator-or-elm property-access))
  :left)

(defmethod elm-associativity ((operator-or-elm new-expr))
  (if (zerop (length (new-expr-args operator-or-elm)))
    :right
    :left))

;;;; ------- Other static properties of source elements --------------------------------------------

;;TODO is this function in the right place?
;;TODO This function is not complete:
;; - binary operators are idempotent so long as they
;;   aren't assignment operators and so long as both args are idempotent.
;; - conditional operators are idempotent so long as all args are idempotent
;; - a small number of unary operators (logical-not, unary-negate, unary-plus, bitwise-not)
;;   are idempotent so long as their arg is idempotent
(defun idempotent-expression-p (elm)
  "Return true if ELM is an 'idempotent expression', ie one which it is
   safe to add repetitions of"
  (typecase elm
    ((or special-value identifier numeric-literal string-literal re-literal)
     t)
    (otherwise
     nil)))

;;;; ======= Convenience functions =================================================================
(defun make-var-init (var-name init-value)
  "Create a VAR-DECL-STATEMENT that initializes a variable named VAR-NAME to INIT-VALUE"
  (make-var-decl-statement :var-decls
                           (list (make-var-decl :name var-name :initializer init-value))))

(defun combine-statements (&rest elm-arguments)
  "Combine ELM-ARGUMENTS into a single list, stripping out statement-blocks
   if necessary"
  (labels ((combine (elm-arguments)
    (cond
      ((null elm-arguments)
       nil)
      ((listp (car elm-arguments))
       (append (car elm-arguments)
               (combine (cdr elm-arguments))))
      ((statement-block-p (car elm-arguments))
       (append (statement-block-statements (car elm-arguments))
               (combine (cdr elm-arguments))))
      (t
       (cons (car elm-arguments)
             (combine (cdr elm-arguments)))))))
    (combine elm-arguments)))

(defun single-statement (&rest elm-arguments)
  "Takes a list of source elements and distills them into a single statement.
   If there is only one statement once all the lists have been flattened and
   the statement-blocks pulled apart, then returns that single statement.
   Otherwise, wraps the entire flattened sequence in a statement-block."
  (let ((statement-list (apply #'combine-statements elm-arguments)))
    (if (> (length statement-list) 1)
      (make-statement-block :statements statement-list)
      (first statement-list))))


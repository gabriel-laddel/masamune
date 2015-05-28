;;;; package.lisp
;;;
;;; Define the packages used by the js-parser system.
;;;
;;; Copyright (c) 2005 James Wright
;;; See LICENSE for full licensing details.

;; Eventually this may want to be several sub-packages, but let's start simple for now
(defpackage :js-parser
  (:use :cl :cl-ppcre)
  (:nicknames)
  (:export
   ;; the main functions that we export
   #:parse
   #:process
   #:syntax-error

   ;; misc.
   #:*symbols-to-tokens*
   
   ;; source-model structure types
   #:source-element
   #:expression
   #:special-value
   #:identifier
   #:numeric-literal
   #:string-literal
   #:array-literal
   #:object-literal
   #:re-literal
   #:new-expr
   #:fn-call
   #:property-access
   #:unary-operator
   #:binary-operator
   #:conditional
   #:comma-expr
   #:var-decl-statement
   #:var-decl
   #:statement-block
   #:if-statement
   #:do-statement
   #:while
   #:for
   #:for-in
   #:continue-statement
   #:break-statement
   #:return-statement
   #:with
   #:switch
   #:case-clause
   #:default-clause
   #:throw-statement
   #:try
   #:catch-clause
   #:finally-clause
   #:function-decl
   #:function-expression

   ;; source model slots
   #:expression
   #:expression-end
   #:expression-label
   #:expression-p
   #:expression-start

   #:array-literal-elements
   #:array-literal-end
   #:array-literal-label
   #:array-literal-p
   #:array-literal-start

   #:binary-operator-end
   #:binary-operator-label
   #:binary-operator-left-arg
   #:binary-operator-op-symbol
   #:binary-operator-p
   #:binary-operator-right-arg
   #:binary-operator-start

   #:break-statement-end
   #:break-statement-label
   #:break-statement-p
   #:break-statement-start
   #:break-statement-target-label

   #:case-clause-body
   #:case-clause-end
   #:case-clause-label
   #:case-clause-p
   #:case-clause-start
   #:case-clause-value

   #:catch-clause-binding
   #:catch-clause-body
   #:catch-clause-end
   #:catch-clause-label
   #:catch-clause-p
   #:catch-clause-start

   #:comma-expr-end
   #:comma-expr-exprs
   #:comma-expr-label
   #:comma-expr-p
   #:comma-expr-start

   #:conditional-condition
   #:conditional-end
   #:conditional-expression
   #:conditional-expression-no-in
   #:conditional-expression-no-lbf
   #:conditional-false-arg
   #:conditional-label
   #:conditional-p
   #:conditional-start
   #:conditional-true-arg

   #:continue-statement-end
   #:continue-statement-label
   #:continue-statement-p
   #:continue-statement-start
   #:continue-statement-target-label

   #:default-clause-body
   #:default-clause-end
   #:default-clause-label
   #:default-clause-p
   #:default-clause-start

   #:do-statement-body
   #:do-statement-condition
   #:do-statement-end
   #:do-statement-label
   #:do-statement-p
   #:do-statement-start

   #:finally-clause-body
   #:finally-clause-end
   #:finally-clause-label
   #:finally-clause-p
   #:finally-clause-start

   #:fn-call-args
   #:fn-call-end
   #:fn-call-fn
   #:fn-call-label
   #:fn-call-p
   #:fn-call-start

   #:for-body
   #:for-condition
   #:for-end

   #:for-in
   #:for-in-binding
   #:for-in-body
   #:for-in-collection
   #:for-in-end
   #:for-in-label
   #:for-in-p
   #:for-in-start

   #:for-initializer
   #:for-label
   #:for-p
   #:for-start
   #:for-step

   #:function-decl-body
   #:function-decl-end
   #:function-decl-label
   #:function-decl-name
   #:function-decl-p
   #:function-decl-parameters
   #:function-decl-start

   #:function-expression-body
   #:function-expression-end
   #:function-expression-label
   #:function-expression-name
   #:function-expression-p
   #:function-expression-parameters
   #:function-expression-start

   #:identifier-end
   #:identifier-label
   #:identifier-name
   #:identifier-p
   #:identifier-start

   #:if-statement-condition
   #:if-statement-else-statement
   #:if-statement-end
   #:if-statement-label
   #:if-statement-no-if
   #:if-statement-p
   #:if-statement-start
   #:if-statement-then-statement

   #:new-expr-args
   #:new-expr-constructor
   #:new-expr-end
   #:new-expr-label

   #:new-expr-p
   #:new-expr-start
   #:new-expression
   #:new-expression-no-lbf

   #:numeric-literal-end
   #:numeric-literal-label
   #:numeric-literal-p
   #:numeric-literal-start
   #:numeric-literal-value

   #:object-literal-end
   #:object-literal-label
   #:object-literal-p
   #:object-literal-properties
   #:object-literal-start

   #:property-access-end
   #:property-access-field
   #:property-access-label
   #:property-access-p
   #:property-access-start
   #:property-access-target

   #:re-literal-end
   #:re-literal-label
   #:re-literal-options
   #:re-literal-p
   #:re-literal-pattern
   #:re-literal-start

   #:return-statement-arg
   #:return-statement-end
   #:return-statement-label
   #:return-statement-p
   #:return-statement-start

   #:source-element-end
   #:source-element-label
   #:source-element-p
   #:source-element-start

   #:special-value-end
   #:special-value-label
   #:special-value-p
   #:special-value-start
   #:special-value-symbol

   #:statement-block-end
   #:statement-block-label
   #:statement-block-p
   #:statement-block-start
   #:statement-block-statements

   #:string-literal-end
   #:string-literal-label
   #:string-literal-p
   #:string-literal-start
   #:string-literal-value

   #:switch-clauses
   #:switch-end
   #:switch-label
   #:switch-p
   #:switch-start
   #:switch-statement
   #:switch-value

   #:throw-statement-end
   #:throw-statement-label
   #:throw-statement-p
   #:throw-statement-start
   #:throw-statement-target
   #:throw-statement-value

   #:try-body
   #:try-catch-clause
   #:try-end
   #:try-finally-clause
   #:try-label
   #:try-p
   #:try-start
   #:try-statement

   #:unary-operator-arg
   #:unary-operator-end
   #:unary-operator-label
   #:unary-operator-op-symbol
   #:unary-operator-p
   #:unary-operator-start

   #:var-decl-end
   #:var-decl-initializer
   #:var-decl-label
   #:var-decl-name
   #:var-decl-p
   #:var-decl-start

   #:var-decl-statement
   #:var-decl-statement
   #:var-decl-statement-end
   #:var-decl-statement-label
   #:var-decl-statement-p
   #:var-decl-statement-start
   #:var-decl-statement-var-decls

   #:while-body
   #:while-condition
   #:while-end
   #:while-label
   #:while-p
   #:while-start

   #:with-body
   #:with-end
   #:with-p
   #:with-scope-object
   #:with-start

   ;; source element construction
   #:make-source-element
   #:make-special-value
   #:make-identifier
   #:make-numeric-literal
   #:make-string-literal
   #:make-array-literal
   #:make-object-literal
   #:make-re-literal
   #:make-new-expr
   #:make-fn-call
   #:make-property-access
   #:make-unary-operator
   #:make-binary-operator
   #:make-conditional
   #:make-comma-expr
   #:make-var-decl-statement
   #:make-var-decl
   #:make-statement-block
   #:make-if-statement
   #:make-do-statement
   #:make-while
   #:make-for
   #:make-for-in
   #:make-continue-statement
   #:make-break-statement
   #:make-return-statement
   #:make-with
   #:make-switch
   #:make-case-clause
   #:make-default-clause
   #:make-throw-statement
   #:make-try
   #:make-catch-clause
   #:make-finally-clause
   #:make-function-decl
   #:make-function-expression
   #:make-suspend-statement
   #:make-resume-statement
   #:make-import-decl
   #:make-add-handler
   #:make-remove-handler
   
   ;; frequently-used accessors
   #:source-element-start
   #:source-element-end
   
   ;; structure management
   #:get-constructor
   #:structure-slots
   #:make-keyword
   ))

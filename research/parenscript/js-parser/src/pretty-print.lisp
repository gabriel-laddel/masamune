;;;; pretty-print.lisp
;;;
;;; Provides generic functions for printing abstract source trees in a parseable (and
;;; human-readable) fashion.  The PRETTY-PRINT generic function is the main interface.
;;; Unit tests are in tests/test-pretty-print.lisp.
;;;
;;; Copyright (c) 2005 James Wright
;;; See LICENSE for full licensing details.
;;;
(in-package :js-parser)

;;;; Semicolon-termination rules
;;;
;;; All statements are responsible for printing their own semicolons, with two exceptions:
;;; Expression-statements and var-decl-statements should have their semicolons printed by
;;; the parent element.
;;;
;;; Expression statements and expressions are not distinguished in the source-model, so the
;;; only way to know that an expression is actually an expression-statement is from the context
;;; in which it appears.  And of course only the parent element actually knows the context, so
;;; the parent element is in charge of semicolons (because expression-statements get semicolons,
;;; but expressions do not).
;;;
;;; Var-decl-statements are an exception because they should be terminated when they appear in a
;;; statement context, but not when they appear in the initializer of a for-statement or
;;; for-in-statement.  Once again, the context is the way you decide whether to semicolon-terminate
;;; or not, so the parent element is in charge.
;;;
;;; In practice, the only two places where we need to make the special-case checks for
;;; expression-statements and var-decl-statements is when we're pretty-printing a list of elements
;;; or when we're pretty-printing a "subordinate" statement.  So, we do the checks in the LIST
;;; method of PRETTY-PRINT and in PRETTY-PRINT-SUBORDINATE and that seems to do the trick.
;;;
;;; As an additional special case, null subordinate-statements are semicolon-terminated by the
;;; parent element, whereas they are ignored by the LIST method of PRETTY-PRINT.

;;;; ======= Indentation helpers ===================================================================
(defparameter *indent-step* 2
  "Number of spaces per indentation step")

(defparameter *indent* 0
  "Current indentation level, in spaces (not steps)")

(defparameter *pretty-mode* t
  "When non-NIL, print nicely (ie, with indentation and newlines).
   When NIL, print compactly (ie, with no unnecessary whitespace).")

(defparameter *escape-script-end-tags* t
  "When non-NIL, escape script end tags (for supporting inline scripts")

(defparameter *opt-space* " ")

(defun fresh-line-indented (s)
  "Start a new, indented line."
  (when *pretty-mode* 
    (fresh-line s)
    (dotimes (n *indent*)
        (format s " "))))

(defmacro with-indent (&body body)
  "Execute the contained forms with *indent* set one step deeper."
  `(let ((*indent* (+ *indent* *indent-step*)))
    ,@body))

;; When we print subordinate statements (as for a while or for loop), we want to indent
;; single statements, but not blocks (because blocks will do the indentation for us).
;; In those situations, use pretty-print-subordinate instead of pretty-print; it will
;; indent correctly depending upon the type of source-element that it receives.
(defgeneric pretty-print-subordinate (elm stream)
  (:documentation
   "pretty-print source element ELM to stream STREAM as a 'subordinate statement'.
    This has differing indentation implications depending upon whether or not ELM is a BLOCK."))

(defmethod pretty-print-subordinate ((elm statement-block) s)
  (pretty-print elm s))

(defmethod pretty-print-subordinate (elm s)
  (with-indent
    (fresh-line-indented s)
    (pretty-print elm s)
    (when (or (expression-p elm)
              (var-decl-statement-p elm)
              (null elm))
      (format s ";"))))

;;;; ======= General helpers =======================================================================
(defun pretty-print-separated-list (elm-list s &optional (sep-string ",~a"))
  "Pretty print the elements of ELM-LIST to S separated by SEP-STRING."
  (loop
      for idx upfrom 0
      for elm in elm-list
      do
      (unless (zerop idx)
        (format s sep-string *opt-space*))
      (pretty-print elm s)))

(defun force-space (s)
  "Prints a space to S in ugly mode to ensure that two elements will be separated"
  (unless *pretty-mode*
    (format s " ")))

;;;; ======= The pretty-print generic function =====================================================
(defgeneric pretty-print (elm stream)
  (:documentation
   "Print source element ELM to stream STREAM as parseable and human-readable text."))

;;;; ------- Standard Javascript -------------------------------------------------------------------

(defmethod pretty-print ((elm special-value) s)
  (cond
    ((find (special-value-symbol elm) *keyword-symbols*)
     (format s "~A" (string-downcase (special-value-symbol elm))))
    ((eq :arguments (special-value-symbol elm))
     (format s "arguments"))
    (t
     (error "Unknown special value symbol ~S" (special-value-symbol elm)))))
   
(defmethod pretty-print ((elm identifier) s)
  (format s "~A" (identifier-name elm)))

(defmethod pretty-print ((elm numeric-literal) s)
  (format s "~D" (numeric-literal-value elm)))

(defmethod pretty-print ((elm string-literal) s)
  (with-slots (value) elm
    (let ((script-idx (and *escape-script-end-tags*
                           (search "</script>" (string-literal-value elm) :test 'string-equal))))
      (if script-idx
        (format s "(\"~A\"+\"~A\")" (subseq value 0 (1+ script-idx))
                                    (subseq value (1+ script-idx)))
        
        (format s "\"~A\"" value)))))

(defmethod pretty-print ((elm array-literal) s)
  (format s "[")
  (pretty-print-separated-list (array-literal-elements elm) s)
  (format s "]"))

(defmethod pretty-print ((elm re-literal) s)
  (format s "/~A/~A" (escape-regexp (re-literal-pattern elm)) (re-literal-options elm)))

;; TODO May want some extra smarts here for things like class defns
;; ie, add newlines after each property if any of the properties are of type function-expression
(defmethod pretty-print ((elm object-literal) s)
  (format s "{")
  (loop for idx upfrom 0
        for name/value in (object-literal-properties elm)
        do
        (unless (zerop idx)
          (format s ",~a" *opt-space*))

        ;; Special case: print string field names in identifier form
        (if (string-literal-p (car name/value))
          (pretty-print (string-literal-value (car name/value)) s)
          (pretty-print (car name/value) s))

        (format s ":~a" *opt-space*)
        (pretty-print (cdr name/value) s))
  (format s "}"))

(defmethod pretty-print ((elm new-expr) s)
  (format s "new ")
  (pretty-print-arg (new-expr-constructor elm) elm s)
  (when (new-expr-args elm)
    (format s "(")
    (pretty-print-separated-list (new-expr-args elm) s)
    (format s ")")))

(defmethod pretty-print ((elm fn-call) s)
  (with-slots (fn args) elm
    ;; With zero-argument new expressions, the issue is less one of operator
    ;; precedence and more one of ambiguity about what the "(" represents.  If
    ;; we always printed the "()" after a 0-argument new expression, then we
    ;; wouldn't need this special check, but since we omit it we need to add
    ;; disambiguating parentheses in the function-call case only.
    (if (and (new-expr-p fn)
             (zerop (length (new-expr-args fn))))
      (progn
        (format s "(")
        (pretty-print fn s)
        (format s ")"))
      (pretty-print-arg (fn-call-fn elm) elm s :left))

    (format s "(")
    (pretty-print-separated-list (fn-call-args elm) s)
    (format s ")")))

(defgeneric printable-as-dot (literal-elm)
  (:documentation
   "Return true if LITERAL-ELM is a string that could be printed as a valid identifier,
    and therefore can be used in dotted form for accessing properties in Javascript.
   Eg: (printable-as-dot #S(string-literal :value \"value\")) ==> T
       (printable-as-dot #S(numeric-literal :value 80)) ==> NIL
       (printable-as-dot #S(string-literal :value \"has spaces\")) ==> NIL
       (printable-as-dot #S(string-literal :value \"has/punctuation\")) ==> NIL"))

(defmethod printable-as-dot ((literal-elm string-literal))
  (not (null (scan "^[\\w\\$]+$" (string-literal-value literal-elm)))))

;; Only string literals are dot candidates
(defmethod printable-as-dot (literal-elm)
  nil)

(defmethod pretty-print ((elm property-access) s)
  (pretty-print-arg (property-access-target elm) elm s :left)
  (cond
    ((printable-as-dot (property-access-field elm))
     (format s ".~A" (string-literal-value (property-access-field elm))))
    (t
     (format s "[")
     (pretty-print (property-access-field elm) s)
     (format s "]"))))

(defun pretty-print-arg (arg-elm parent-elm s &optional associativity)
  "Pretty print an argument subexpression, parenthesizing if:
   1. The sub-expression has a lower precedence than the parent expression, or
   2. The sub-expression and the parent expression have the same precedence
      and this arg is on the non-associative branch."
  (if (or (> (elm-precedence arg-elm)
             (elm-precedence parent-elm))
          (and associativity
               (= (elm-precedence arg-elm)
                  (elm-precedence parent-elm))
               (not (eq associativity
                        (elm-associativity arg-elm)))))
    (progn
      (format s "(")
      (pretty-print arg-elm s)
      (format s ")"))
    (pretty-print arg-elm s)))

(defmethod pretty-print ((elm unary-operator) s)
  (let* ((op-symbol (unary-operator-op-symbol elm))
         (op-string (or (gethash op-symbol *symbols-to-tokens*)
                        (if (find op-symbol *keyword-symbols*)
                          (string-downcase (symbol-name op-symbol))))))
  (ecase op-symbol
    ((:post-decr :post-incr)
     (pretty-print-arg (unary-operator-arg elm) elm s)
     (format s "~A" op-string))
    ((:pre-decr :pre-incr :unary-minus :unary-plus :bitwise-not :logical-not)
     (format s "~A" op-string)
     (pretty-print-arg (unary-operator-arg elm) elm s))
    ((:delete :void :typeof)
     (format s "~A " op-string)
     (pretty-print-arg (unary-operator-arg elm) elm s)))))

(defmethod pretty-print ((elm binary-operator) s)
  (let* ((op-symbol (binary-operator-op-symbol elm))
         (op-string (or (gethash op-symbol *symbols-to-tokens*)
                        (if (find op-symbol *keyword-symbols*)
                          (string-downcase (symbol-name op-symbol))))))
    
    (pretty-print-arg (binary-operator-left-arg elm) elm s :left)
    (format s "~a~A~a" *opt-space* op-string *opt-space*)
    (pretty-print-arg (binary-operator-right-arg elm) elm s :right)))

(defmethod pretty-print ((elm conditional) s)
  (pretty-print-arg (conditional-condition elm) elm s)
  (format s "~a?~a" *opt-space* *opt-space*)
  (pretty-print-arg (conditional-true-arg elm) elm s)
  (format s "~a:~a" *opt-space* *opt-space*)
  (pretty-print-arg (conditional-false-arg elm) elm s))

(defmethod pretty-print ((elm comma-expr) s)
  (pretty-print-separated-list (comma-expr-exprs elm) s))

(defmethod pretty-print ((elm var-decl-statement) s)
  (format s "var ")
  (pretty-print-separated-list (var-decl-statement-var-decls elm) s))

(defmethod pretty-print ((elm var-decl) s)
  (format s (var-decl-name elm))
  (when (var-decl-initializer elm)
    (format s "~a=~a" *opt-space* *opt-space*)
    (pretty-print (var-decl-initializer elm) s)))

(defmethod pretty-print ((elm-list list) s)
  (loop for elm in elm-list
        do
        (fresh-line-indented s)
        (pretty-print elm s)
        (when (or (expression-p elm)
                  (var-decl-statement-p elm))
          (format s ";"))))
  
(defmethod pretty-print ((elm statement-block) s)
  (fresh-line-indented s)
  (format s "{")
  (with-indent
    (pretty-print (statement-block-statements elm) s))
  (fresh-line-indented s)
  (format s "}"))

(defmethod pretty-print ((elm if-statement) s)
  (format s "if(")
  (pretty-print (if-statement-condition elm) s)
  (format s ")")
  (pretty-print-subordinate (if-statement-then-statement elm) s)
  (when (if-statement-else-statement elm)
    (fresh-line-indented s)
    (format s "else")
    (unless (statement-block-p (if-statement-else-statement elm))
      (force-space s))
    (pretty-print-subordinate (if-statement-else-statement elm) s)))

(defmethod pretty-print ((elm do-statement) s)
  (format s "do")
  (pretty-print-subordinate (do-statement-body elm) s)
  (fresh-line-indented s)
  (format s "while(")
  (pretty-print (do-statement-condition elm) s)
  (format s ");"))

(defmethod pretty-print ((elm while) s)
  (format s "while(")
  (pretty-print (while-condition elm) s)
  (format s ")")
  (pretty-print-subordinate (while-body elm) s))

(defmethod pretty-print ((elm null) s)
  (declare (ignore elm s)))

(defmethod pretty-print ((elm string) s)
  (format s "~A" elm))

(defmethod pretty-print ((elm for) s)
  (format s "for(")
  (pretty-print (for-initializer elm) s)
  (format s ";~a" *opt-space*)
  (pretty-print (for-condition elm) s)
  (format s ";~a" *opt-space*)
  (pretty-print (for-step elm) s)
  (format s ")")
  (pretty-print-subordinate (for-body elm) s))

(defmethod pretty-print ((elm for-in) s)
  (format s "for(")
  (pretty-print (for-in-binding elm) s)
  (format s " in ")
  (pretty-print (for-in-collection elm) s)
  (format s ")")
  (pretty-print-subordinate (for-in-body elm) s))

(defmethod pretty-print ((elm continue-statement) s)
  (format s "continue")
  (when (continue-statement-target-label elm)
    (format s " ")
    (pretty-print (continue-statement-target-label elm) s))
  (format s ";"))

(defmethod pretty-print ((elm break-statement) s)
  (format s "break")
  (when (break-statement-target-label elm)
    (format s " ")
    (pretty-print (break-statement-target-label elm) s))
  (format s ";"))

(defmethod pretty-print ((elm return-statement) s)
  (format s "return")
  (when (return-statement-arg elm)
    (format s " ")
    (pretty-print (return-statement-arg elm) s))
  (format s ";"))

(defmethod pretty-print ((elm throw-statement) s)
  (format s "throw ")
  (pretty-print (throw-statement-value elm) s)
  (when (throw-statement-target elm)
    (format s "~A->~A" *opt-space* *opt-space*)
    (pretty-print (throw-statement-target elm) s))
  (format s ";"))

(defmethod pretty-print ((elm switch) s)
  (format s "switch(")
  (pretty-print (switch-value elm) s)
  (format s ")")
  (fresh-line-indented s)
  (format s "{")
  (loop for clause in (switch-clauses elm)
        do (pretty-print clause s))
  (fresh-line-indented s)
  (format s "}"))

(defmethod pretty-print ((elm case-clause) s)
  (fresh-line-indented s)
  (format s "case ")
  (pretty-print (case-clause-value elm) s)
  (format s ":")
  (with-indent
    (pretty-print (case-clause-body elm) s)))

(defmethod pretty-print ((elm default-clause) s)
  (fresh-line-indented s)
  (format s "default:")
  (with-indent
    (pretty-print (default-clause-body elm) s)))

(defmethod pretty-print ((elm with) s)
  (format s "with(")
  (pretty-print (with-scope-object elm) s)
  (format s ")")
  (pretty-print-subordinate (with-body elm) s))

(defmethod pretty-print ((elm try) s)
  (format s "try")
  (fresh-line-indented s)
  (format s "{")
  (with-indent
    (pretty-print (try-body elm) s))
  (fresh-line-indented s)
  (format s "}")
  (when (try-catch-clause elm)
    (fresh-line-indented s)
    (pretty-print (try-catch-clause elm) s))
  (when (try-finally-clause elm)
    (fresh-line-indented s)
    (pretty-print (try-finally-clause elm) s)))

(defmethod pretty-print ((elm catch-clause) s)
  (format s "catch(~A)" (catch-clause-binding elm))
  (fresh-line-indented s)
  (format s "{")
  (with-indent
    (pretty-print (catch-clause-body elm) s))
  (fresh-line-indented s)
  (format s "}"))

(defmethod pretty-print ((elm finally-clause) s)
  (format s "finally")
  (fresh-line-indented s)
  (format s "{")
  (with-indent
    (pretty-print (finally-clause-body elm) s))
  (fresh-line-indented s)
  (format s "}"))

(defmethod pretty-print ((elm function-decl) s)
  (format s "function ~A(" (function-decl-name elm))
  (pretty-print-separated-list (function-decl-parameters elm) s)
  (format s ")")
  (fresh-line-indented s)
  (format s "{")
  (with-indent
    (pretty-print (function-decl-body elm) s))
  (fresh-line-indented s)
  (format s "}"))

(defmethod pretty-print ((elm function-expression) s)
  (if (function-expression-name elm)
    (format s "function ~A(" (function-expression-name elm))
    (format s "function("))
  (pretty-print-separated-list (function-expression-parameters elm) s)
  (format s ")")

  (cond
    ;; If there's only one statement in the body, try to fit everything onto one line
    ((and nil ;TEST Disable for now
          (<= (length (function-expression-body elm)) 1))
     (let ((first-stmt (first (function-expression-body elm))))
       (format s "~a{~a" *opt-space* *opt-space*)
       (pretty-print first-stmt s)
       (when (or (expression-p first-stmt)
                 (var-decl-statement-p first-stmt))
         (format s ";"))
       (format s "~a}" *opt-space*)))
    (t
     (fresh-line-indented s)
     (format s "{")
     (with-indent
       (pretty-print (function-expression-body elm) s))
     (fresh-line-indented s)
     (format s "}"))))

;; Print a label before each statement that has one
(defmethod pretty-print :before ((elm source-element) s)
  (when (source-element-label elm)
    (format s "~A:" (source-element-label elm))
    (fresh-line-indented s)))

;;;; ------- jwacs-only extensions -----------------------------------------------------------------
(defmethod pretty-print ((elm suspend-statement) s)
  (format s "suspend;"))

(defmethod pretty-print ((elm resume-statement) s)
  (format s "resume ")
  (pretty-print (resume-statement-target elm) s)
  (when (resume-statement-arg elm)
    (format s "~a<-~a" *opt-space* *opt-space* )
    (pretty-print (resume-statement-arg elm) s))
  (format s ";"))

(defmethod pretty-print ((elm import-decl) s)
  (format s "import ")
  (when (import-decl-type-symbol elm)
    (format s "~A " (string-downcase (symbol-name (import-decl-type-symbol elm)))))
  (format s "\"~A\";" (import-decl-uripath elm)))

;;;; ------- administrative elements ---------------------------------------------------------------
(defmethod pretty-print ((elm add-handler) s)
  (format s "$addHandler(")
  (pretty-print-separated-list (list (add-handler-handler elm)
                                     (make-function-expression :body (add-handler-thunk-body elm)))
                               s)
  (format s ");"))

(defmethod pretty-print ((elm remove-handler) s)
  (format s "$removeHandler(")
  (pretty-print-separated-list (list (remove-handler-handler elm)
                                     (make-function-expression :body (remove-handler-thunk-body elm)))
                               s)
  (format s ");"))

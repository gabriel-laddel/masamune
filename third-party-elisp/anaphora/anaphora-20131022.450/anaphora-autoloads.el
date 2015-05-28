;;; anaphora-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (anaphoric-setq anaphoric-set anaphoric-/ anaphoric-*
;;;;;;  anaphoric-- anaphoric-+ anaphoric-let anaphoric-etypecase
;;;;;;  anaphoric-typecase anaphoric-ecase anaphoric-case anaphoric-block
;;;;;;  anaphoric-lambda anaphoric-cond anaphoric-and anaphoric-while
;;;;;;  anaphoric-when anaphoric-prog2 anaphoric-prog1 anaphoric-if
;;;;;;  anaphora-use-long-names-only anaphora) "anaphora" "anaphora.el"
;;;;;;  (21238 48267 340903 239000))
;;; Generated autoloads from anaphora.el

(let ((loads (get 'anaphora 'custom-loads))) (if (member '"anaphora" loads) nil (put 'anaphora 'custom-loads (cons '"anaphora" loads))))

(defvar anaphora-use-long-names-only nil "\
Use only long names such as `anaphoric-if' instead of traditional `aif'.")

(custom-autoload 'anaphora-use-long-names-only "anaphora" t)

(defun anaphora--install-traditional-aliases (&optional arg) "\
Install traditional short aliases for anaphoric macros.

With negative numeric ARG, remove traditional aliases." (let ((syms (quote ((if . t) (prog1 . t) (prog2 . t) (when . when) (while . t) (and . t) (cond . cond) (lambda . lambda) (block . block) (case . case) (ecase . ecase) (typecase . typecase) (etypecase . etypecase) (let . let) (+ . t) (- . t) (* . t) (/ . t))))) (cond ((and (numberp arg) (< arg 0)) (dolist (cell syms) (when (ignore-errors (eq (symbol-function (intern-soft (format "a%s" (car cell)))) (intern-soft (format "anaphoric-%s" (car cell))))) (fmakunbound (intern (format "a%s" (car cell))))))) (t (dolist (cell syms) (let* ((builtin (car cell)) (traditional (intern (format "a%s" builtin))) (long (intern (format "anaphoric-%s" builtin)))) (defalias traditional long) (put traditional (quote lisp-indent-function) (get builtin (quote lisp-indent-function))) (put traditional (quote edebug-form-spec) (cdr cell))))))))

(unless anaphora-use-long-names-only (anaphora--install-traditional-aliases))

(autoload 'anaphoric-if "anaphora" "\
Like `if', but the result of evaluating COND is bound to `it'.

The variable `it' is available within THEN and ELSE.

COND, THEN, and ELSE are otherwise as documented for `if'.

\(fn COND THEN &rest ELSE)" nil t)

(put 'anaphoric-if 'lisp-indent-function '2)

(autoload 'anaphoric-prog1 "anaphora" "\
Like `prog1', but the result of evaluating FIRST is bound to `it'.

The variable `it' is available within BODY.

FIRST and BODY are otherwise as documented for `prog1'.

\(fn FIRST &rest BODY)" nil t)

(put 'anaphoric-prog1 'lisp-indent-function '1)

(autoload 'anaphoric-prog2 "anaphora" "\
Like `prog2', but the result of evaluating FORM2 is bound to `it'.

The variable `it' is available within BODY.

FORM1, FORM2, and BODY are otherwise as documented for `prog2'.

\(fn FORM1 FORM2 &rest BODY)" nil t)

(put 'anaphoric-prog2 'lisp-indent-function '2)

(autoload 'anaphoric-when "anaphora" "\
Like `when', but the result of evaluating COND is bound to `it'.

The variable `it' is available within BODY.

COND and BODY are otherwise as documented for `when'.

\(fn COND &rest BODY)" nil t)

(put 'anaphoric-when 'lisp-indent-function '1)

(autoload 'anaphoric-while "anaphora" "\
Like `while', but the result of evaluating TEST is bound to `it'.

The variable `it' is available within BODY.

TEST and BODY are otherwise as documented for `while'.

\(fn TEST &rest BODY)" nil t)

(put 'anaphoric-while 'lisp-indent-function '1)

(autoload 'anaphoric-and "anaphora" "\
Like `and', but the result of the previous condition is bound to `it'.

The variable `it' is available within all CONDITIONS after the
initial one.

CONDITIONS are otherwise as documented for `and'.

Note that some implementations of this macro bind only the first
condition to `it', rather than each successive condition.

\(fn &rest CONDITIONS)" nil t)

(autoload 'anaphoric-cond "anaphora" "\
Like `cond', but the result of each condition is bound to `it'.

The variable `it' is available within the remainder of each of CLAUSES.

CLAUSES are otherwise as documented for `cond'.

\(fn &rest CLAUSES)" nil t)

(autoload 'anaphoric-lambda "anaphora" "\
Like `lambda', but the function may refer to itself as `self'.

ARGS and BODY are otherwise as documented for `lambda'.

\(fn ARGS &rest BODY)" nil t)

(put 'anaphoric-lambda 'lisp-indent-function 'defun)

(autoload 'anaphoric-block "anaphora" "\
Like `block', but the result of the previous expression is bound to `it'.

The variable `it' is available within all expressions of BODY
except the initial one.

NAME and BODY are otherwise as documented for `block'.

\(fn NAME &rest BODY)" nil t)

(put 'anaphoric-block 'lisp-indent-function '1)

(autoload 'anaphoric-case "anaphora" "\
Like `case', but the result of evaluating EXPR is bound to `it'.

The variable `it' is available within CLAUSES.

EXPR and CLAUSES are otherwise as documented for `case'.

\(fn EXPR &rest CLAUSES)" nil t)

(put 'anaphoric-case 'lisp-indent-function '1)

(autoload 'anaphoric-ecase "anaphora" "\
Like `ecase', but the result of evaluating EXPR is bound to `it'.

The variable `it' is available within CLAUSES.

EXPR and CLAUSES are otherwise as documented for `ecase'.

\(fn EXPR &rest CLAUSES)" nil t)

(put 'anaphoric-ecase 'lisp-indent-function '1)

(autoload 'anaphoric-typecase "anaphora" "\
Like `typecase', but the result of evaluating EXPR is bound to `it'.

The variable `it' is available within CLAUSES.

EXPR and CLAUSES are otherwise as documented for `typecase'.

\(fn EXPR &rest CLAUSES)" nil t)

(put 'anaphoric-typecase 'lisp-indent-function '1)

(autoload 'anaphoric-etypecase "anaphora" "\
Like `etypecase', but result of evaluating EXPR is bound to `it'.

The variable `it' is available within CLAUSES.

EXPR and CLAUSES are otherwise as documented for `etypecase'.

\(fn EXPR &rest CLAUSES)" nil t)

(put 'anaphoric-etypecase 'lisp-indent-function '1)

(autoload 'anaphoric-let "anaphora" "\
Like `let', but the content of VARLIST is bound to `it'.

VARLIST as it appears in `it' is not evaluated.  The variable `it'
is available within BODY.

VARLIST and BODY are otherwise as documented for `let'.

\(fn VARLIST &rest BODY)" nil t)

(put 'anaphoric-let 'lisp-indent-function '1)

(autoload 'anaphoric-+ "anaphora" "\
Like `+', but the result of evaluating the previous expression is bound to `it'.

The variable `it' is available within all expressions after the
initial one.

NUMBERS-OR-MARKERS are otherwise as documented for `+'.

\(fn &rest NUMBERS-OR-MARKERS)" nil t)

(autoload 'anaphoric-- "anaphora" "\
Like `-', but the result of evaluating the previous expression is bound to `it'.

The variable `it' is available within all expressions after the
initial one.

NUMBER-OR-MARKER and NUMBERS-OR-MARKERS are otherwise as
documented for `-'.

\(fn &optional NUMBER-OR-MARKER &rest NUMBERS-OR-MARKERS)" nil t)

(autoload 'anaphoric-* "anaphora" "\
Like `*', but the result of evaluating the previous expression is bound to `it'.

The variable `it' is available within all expressions after the
initial one.

NUMBERS-OR-MARKERS are otherwise as documented for `*'.

\(fn &rest NUMBERS-OR-MARKERS)" nil t)

(autoload 'anaphoric-/ "anaphora" "\
Like `/', but the result of evaluating the previous divisor is bound to `it'.

The variable `it' is available within all expressions after the
first divisor.

DIVIDEND, DIVISOR, and DIVISORS are otherwise as documented for `/'.

\(fn DIVIDEND DIVISOR &rest DIVISORS)" nil t)

(autoload 'anaphoric-set "anaphora" "\
Like `set', except that the value of SYMBOL is bound to `it'.

The variable `it' is available within VALUE.

SYMBOL and VALUE are otherwise as documented for `set'.

Note that if this macro followed traditional naming for
anaphoric expressions, it would conflict with the existing
\(quite different) function `aset'.

\(fn SYMBOL VALUE)" nil t)

(autoload 'anaphoric-setq "anaphora" "\
Like `setq', except that the value of SYM is bound to `it'.

The variable `it' is available within each VAL.

ARGS in the form [SYM VAL] ... are otherwise as documented for `setq'.

No alias `asetq' is provided, because it would easily mistaken
for the pre-existing `aset', and because `anaphoric-setq' is not
likely to find frequent use.

\(fn &rest ARGS)" nil t)

;;;***

;;;### (autoloads nil nil ("anaphora-pkg.el") (21238 48267 450224
;;;;;;  445000))

;;;***

(provide 'anaphora-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; anaphora-autoloads.el ends here

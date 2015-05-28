;;; M-x Masamune Mode
;;; 
;;; https://github.com/ShingoFukuyama/helm-swoop
;;; http://danmidwood.com/content/2014/11/21/animated-paredit.html

;;; Commands specific to the Lisp syntax for Climacs.

(in-package :drei-lisp-syntax)

(make-command-table 'lisp-table :errorp nil)

(defmethod additional-command-tables append ((frame climacs-gui:climacs) (command-table lisp-table))
  '(lisp-table))

(define-command (com-package :name t :command-table lisp-table) ()
  (let ((package (package-at-mark (current-syntax) (point))))
    (esa:display-message (format nil "~A" (if (packagep package)
                                              (package-name package)
                                              package)))))

(define-command (com-set-base :name t :command-table lisp-table)
    ((base '(integer 2 36)))
  "Set the base for the current buffer."
  (setf (base (current-syntax)) base))

(define-command (com-set-package :name t :command-table lisp-table)
    ((package 'package))
  "Set the package for the current buffer."
  (setf (option-specified-package (current-syntax)) package))

(define-command (com-macroexpand-1 :name t :command-table lisp-table)
    ()
  "Macroexpand-1 the expression at point.

The expanded expression will be displayed in a
\"*Macroexpansion*\"-buffer."
  (let* ((token (expression-at-mark (current-syntax) (point))))
    (if token
        (macroexpand-token (current-syntax) token)
        (esa:display-message "Nothing to expand at point."))))

(define-command (com-macroexpand-all :name t :command-table lisp-table)
    ()
  "Completely macroexpand the expression at point.

The expanded expression will be displayed in a
\"*Macroexpansion*\"-buffer."
  (let ((token (expression-at-mark (current-syntax) (point))))
    (if token
        (macroexpand-token (current-syntax) token t)
        (esa:display-message "Nothing to expand at point."))))

(define-command (com-print-last-expression :name t :command-table lisp-table)
    ()
  "Evaluate the expression before point in the local Lisp image
and print the resulting value to the \"*Results*\"-buffer."
  (let* ((token (form-before (current-syntax) (offset (point)))))
    (if token
        (let ((*read-base* (base (current-syntax)))
              (exp (form-to-object (current-syntax) token :read t)))
          (let ((values (multiple-value-list
                         (handler-case (eval exp)
                           (error (condition)
                             (progn (beep)
                                    (display-message "~a" condition)
                                    (return-from
                                     com-print-last-expression nil)))))))
            (let* ((current-view (esa-current-window *esa-instance*))
                   (view (climacs-core:switch-or-move-to-view (current-window) "*Results*")))
              (set-syntax view "Lisp")
              (end-of-buffer (point))
              (unless (beginning-of-buffer-p (point))
                (insert-object (point) #\Newline))
              (insert-sequence (point)
                               (format nil "~{~A~%~}" values))
              (insert-object (point) #\Newline)
              (climacs-gui:other-window current-view))))
        (esa:display-message "Nothing to evaluate at point."))))

(define-command (com-compile-and-load-file :name t :command-table lisp-table)
    ()
  "Compile and load the current file.

Compiler notes will be displayed in a seperate view."
  (compile-file-interactively (current-view) t))

(define-command (com-compile-file :name t :command-table lisp-table)
    ()
  "Compile the file open in the current view.

This command does not load the file after it has been compiled."
  (compile-file-interactively (current-view) nil))

(define-command (com-goto-location :name t :command-table lisp-table)
    ((note 'compiler-note))
  "Move point to the part of a given file that caused the
compiler note.

If the file is not already open, a new buffer will be opened with
that file."
  (goto-location (location note)))

(define-presentation-to-command-translator compiler-note-to-goto-location-translator
    (compiler-note com-goto-location lisp-table)
    (presentation)
  (list (presentation-object presentation)))

(define-command (com-goto-xref :name t :command-table lisp-table)
    ((xref 'xref))
  "Go to the referenced location of a code cross-reference."
  (goto-location xref))

(define-presentation-to-command-translator xref-to-goto-location-translator
    (xref com-goto-xref lisp-table)
    (presentation)
    (list (presentation-object presentation)))

(define-command (com-edit-definition :name t :command-table lisp-table)
    ((symbol 'symbol))
  "Edit definition of the symbol."
  (edit-definition symbol))

(define-command (com-edit-this-definition :command-table lisp-table)
    ()
  "Edit definition of the symbol at point.
If there is no symbol at point, this is a no-op."
  (let* ((token (this-form (current-syntax) (point)))
         (this-symbol (form-to-object (current-syntax) token)))
    (when (and this-symbol (symbolp this-symbol))
      (let ((local-definition (find-local-definition (current-syntax) token)))
        (if local-definition
            (setf (offset (point)) (start-offset local-definition))
            (edit-definition this-symbol))))))

(define-command (com-return-from-definition :name t :command-table lisp-table)
    ()
  "Return point to where it was before the previous Edit
Definition command was issued."
  (pop-find-definition-stack))

(define-command (com-compile-definition :name t :command-table pane-lisp-table)
    ()
  "Compile and load definition at point."
  (evaluating-interactively
    (compile-definition-interactively (current-view) (point))))

(loop for (command table gestures)
	in '((com-eval-defun             lisp-table ((#\x :control :meta)))
	     (com-print-last-expression  lisp-table ((#\c :control) (#\p :control)))
	     (com-macroexpand-1          lisp-table ((#\c :control) (#\Newline)))
	     (com-macroexpand-all        lisp-table ((#\c :control) (#\m :control)))
	     (com-compile-and-load-file  lisp-table ((#\c :control) (#\k :control)))
	     (com-compile-file           lisp-table ((#\c :control) (#\k :meta)))
	     (com-edit-this-definition   lisp-table ((#\. :meta)))
	     (com-return-from-definition lisp-table ((#\, :meta)))
	     (com-compile-definition     pane-lisp-table    ((#\c :control) (#\c :control))))
      do (esa:set-key command table gestures))

(in-package :climacs-masamune)

(define-syntax-mode masamune-mode ()
    ()
    (:documentation "common lisp specific structure editing")
    (:applicable-syntaxes lisp-syntax))

(define-mode-toggle-commands com-masamune-mode (masamune-mode "Masamune")
  :command-table lisp-table)

(make-command-table 'masamune-table
 :errorp nil)

(defmethod syntax-command-tables append ((syntax masamune-mode))
  '(masamune-table))

(defun delete-form (buffer form)
  "Delete `form' from `buffer'."
  (delete-buffer-range
   buffer (start-offset form) (size form)))

(define-command (com-open-list :name t :command-table masamune-table)
    ((n 'integer :default 0))
  "Insert a balanced parenthesis pair.
With an argument N, put the closing parentheses after N
S-expressions forward.  If in string or comment, insert a single
opening parenthesis.  If in a character literal, replace the
character literal with #\(."
  (cond ((in-string-p (current-syntax) (point))
         (insert-character #\())
        ((in-comment-p (current-syntax) (point))
         (insert-character #\())
        ((in-character-p (current-syntax) (point))
         (delete-form (current-buffer) (form-around (current-syntax) (offset (point))))
         (insert-sequence (point) "#\\("))
        (t
         (when (and (not (zerop n))
                    (forward-expression (point) (current-syntax) 1 nil))
           (backward-expression (point) (current-syntax) 1 nil))
         (insert-character #\()
         (forward-expression (point) (current-syntax) n nil)
         (insert-character #\))
         (backward-object (point))
         (backward-expression (point) (current-syntax) n nil))))

(define-command (com-wrap-expression :name t :command-table masamune-table)
    ((n 'integer :default 1))
  "Wrap the following N S-expressions in a list.
Automatically indent the newly wrapped S-expressions.  As a
special case, if the point is at the end of a list, simply insert
a pair of parentheses, rather than insert a lone opening
parenthesis and then signal an error, in the interest of
preserving structural validity."
  (com-open-list n))

(define-command (com-close-list-and-newline :name t :command-table masamune-table)
    ()
  "Move past one closing delimiter, add a newline, and reindent."
  (cond ((or (in-string-p (current-syntax) (point))
             (in-comment-p (current-syntax) (point)))
         (insert-character #\)))
        ((in-character-p (current-syntax) (point))
         (delete-form (current-buffer) (form-around (current-syntax) (offset (point))))
         (insert-sequence (point) "#\\)"))
        ((forward-up (point) (current-syntax) 1 nil)
         (insert-object (point) #\Newline)
         (indent-current-line (current-view) (point)))))

(defun delete-object-structurally (delete-fn move-fn immediate-form-fn
                                   border-offset-fn
                                   at-border-fn)
  "Delete an object at `(point)' structurally. `Delete-fn' is
either `forward-delete-object' or `backward-delete-object',
`move-fn' is either `forward-object' or `backward-object',
`immediate-form-fn' is some form selector, `border-offset-fn' is
either `end-offset' or `begin-offset', `at-border-fn' is a
function used to determine whether or not `(point)' is at the end
of a structural object."
  (let ((immediate-form (funcall immediate-form-fn (current-syntax) (offset (point))))
        (form-around (form-around (current-syntax) (offset (point))))
        (list-at-mark (list-at-mark (current-syntax) (point)))
        (string-at-mark (form-of-type-at-mark (current-syntax) (point) #'form-string-p)))
    (cond ((and (or (form-string-p immediate-form)
                    (form-list-p immediate-form))
                (= (funcall border-offset-fn immediate-form)
                   (offset (point))))
           (funcall move-fn (point)))
          ((and (funcall at-border-fn (current-syntax) (point))
                form-around)
           (when (or (and list-at-mark
                          (null (form-children list-at-mark)))
                     (and string-at-mark
                          (= (size string-at-mark) 2)))
             (delete-form (current-buffer) form-around)))
          ((and (form-character-p immediate-form)
                (= (funcall border-offset-fn immediate-form)
                   (offset (point))))
           (delete-form (current-buffer) immediate-form))
          (t (funcall delete-fn (point))))))

(define-command (com-forward-delete-object-structurally
                 :name t :command-table masamune-table)
    ((force 'boolean :default nil))
  "Delete a character forward or move forward over a delimiter.
If on an opening S-expression delimiter, move forward into the
S-expression. If on a closing S-expression delimiter, refuse to
delete unless the S-expression is empty, in which case delete the
whole S-expression. If `force' is true, simply delete a character
forward, without regard for delimiter balancing."
  (if force
      (forward-delete-object (point))
      (delete-object-structurally #'forward-delete-object #'forward-object
                                  #'form-after #'start-offset
                                  #'location-at-end-of-form)))

(define-command (com-backward-delete-object-structurally
                 :name t :command-table masamune-table)
    ((force 'boolean :default nil))
  "Delete a character backward or move backward over a delimiter.
If on an ending S-expression delimiter, move backward into the
S-expression. If on an opening S-expression delimiter, refuse to
delete unless the S-expression is empty, in which case delete the
whole S-expression. If `force' is true, simply delete a
character backward, without regard for delimiter balancing."
  (if force
      (backward-delete-object (point))
      (delete-object-structurally #'backward-delete-object #'backward-object
                                  #'form-before #'end-offset
                                  #'location-at-beginning-of-form)))

(define-command (com-insert-double-quote-structurally
                 :name t :command-table masamune-table)
    ((n 'integer :default 0))
  "Insert a pair of double-quotes.
With a prefix argument N, wrap the following N S-expressions in
  double-quotes, escaping intermediate characters if necessary.
Inside a comment, insert a literal double-quote.
At the end of a string, move past the closing double-quote.
In the middle of a string, insert a backslash-escaped double-quote.
If in a character literal, replace the character literal with #\\\"."
  (cond ((in-comment-p (current-syntax) (point))
         (insert-character #\"))
        ((at-end-of-string-p (current-syntax) (point))
         (forward-object (point)))
        ((in-string-p (current-syntax) (point))
         (insert-sequence (point) "\\\""))
        ((in-character-p (current-syntax) (point))
         (delete-form (current-buffer) (form-around (current-syntax) (offset (point))))
         (insert-sequence (point) "#\\\""))
        (t
         (let ((old-offset (offset (point))))
           (forward-expression (point) (current-syntax) n nil)
           (insert-buffer-object (current-buffer) old-offset #\")
           (insert-character #\")
           (backward-object (point))
           (backward-expression (point) (current-syntax) (min 1 n) nil)))))

(define-command (com-wrap-expression-in-doublequote :name t :command-table masamune-table)
    ((n 'integer :default 1))
  "Move to the end of the string, insert a newline, and indent.
If not in a string, act as `Insert Double Quote Structurally'; if
no prefix argument is specified, the default is to wrap one
S-expression, however, not zero."
  (if (in-string-p (current-syntax) (point))
      (setf (offset (point))
            (1+ (end-offset (form-around (current-syntax) (point)))))
      (com-insert-double-quote-structurally n)))

(define-command (com-splice-list :name t :command-table masamune-table)
    ((kill-backward 'boolean :default nil))
  "Splice the list that the point is on by removing its delimiters.
With a prefix argument as in `C-u', kill all S-expressions
backward in the current list before splicing all S-expressions
forward into the enclosing list."
  (let ((list (list-at-mark (current-syntax) (point))))
    (when list
      (let ((begin-mark (make-buffer-mark (current-buffer) (start-offset list)))
            (end-mark (make-buffer-mark (current-buffer) (end-offset list))))
        (when kill-backward
          (loop until (eq (list-at-mark (current-syntax) (offset (point)))
                          (or (form-before (current-syntax) (offset (point)))
                              (form-around (current-syntax) (offset (point)))))
             do (backward-delete-expression (point) (current-syntax) 1 nil)))
        (delete-buffer-range (current-buffer) (offset begin-mark) 1)
        (delete-buffer-range (current-buffer) (1- (offset end-mark)) 1)))))

(define-command (com-kill-line-structurally :name t :command-table masamune-table)
    ()
  "Kill a line as if with \"Kill Line\", but respecting delimiters.
In a string, act exactly as \"Kill Line\" but do not kill past
the closing string delimiter.  On a line with no S-expressions on
it starting after the point or within a comment, act exactly as
\"Kill Line\".  Otherwise, kill all S-expressions that start
after the point."
  (let ((form-around (form-around (current-syntax) (offset (point))))
        (form-after (form-after (current-syntax) (offset (point))))
        (comment (comment-at-mark (current-syntax) (point))))
    (cond ((empty-line-p (point))
           (forward-delete-object (point)))
          ((in-string-p (current-syntax) (point))
           (if (= (buffer-line-number (current-buffer) (end-offset form-around))
                  (line-number (point)))
               ;; Delete from point until the end of the string, but
               ;; keep the ending delimiter.
               (kill-region (point) (1- (end-offset form-around)))
               ;; Delete from point until end of line.
               (kill-region (point) (end-of-line (clone-mark (point))))))
          ((in-line-comment-p (current-syntax) (point))
           ;; Delete until end of line
           (kill-region (point) (end-of-line (clone-mark (point)))))
          ((in-long-comment-p (current-syntax) (point))
           (if (= (buffer-line-number (current-buffer) (end-offset comment))
                  (line-number (point)))
               ;; End of comment on same line as point, if a complete
               ;; long comment, don't delete the ending delimiter
               (kill-region (point) (- (end-offset comment)
                                       (if (form-complete-p comment)
                                           2 0)))
               ;; Delete from point until end of line.
               (kill-region (point) (end-of-line (clone-mark (point))))))
          ((and form-after
                (= (buffer-line-number (current-buffer) (start-offset form-after))
                   (line-number (point))))
           (forward-kill-expression (point) (current-syntax))
           (loop for form-after = (form-after (current-syntax) (offset (point)))
                 while (and form-after
                            (= (buffer-line-number (current-buffer) (start-offset form-after))
                               (line-number (point))))
                 do (forward-kill-expression (point) (current-syntax) 1 t)))
          (t (forward-kill-line (point) (current-syntax) 1 t nil)))))

(loop for (command-or-l gesture) 
	in `(((com-open-list ,*numeric-argument-marker* ,*numeric-argument-marker*) (#\())
	     ((com-wrap-expression ,*numeric-argument-marker*) ((#\( :meta)))
	     (com-close-list-and-newline (#\)))
	     ((com-forward-delete-object-structurally ,*numeric-argument-marker*)((#\d :control)))
	     ((com-backward-delete-object-structurally ,*numeric-argument-marker*) ((#\Backspace)))
	     ((com-insert-double-quote-structurally ,*numeric-argument-marker*) ((#\")))
	     ((com-wrap-expression-in-doublequote ,*numeric-argument-marker*) ((#\" :meta)))
	     ((com-splice-list ,*numeric-argument-marker*) ((#\s :meta)))
	     (com-kill-line-structurally ((#\k :control))))
      do (set-key command-or-l 'masamune-table gesture))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; structure editor

;; (cl:defpackage #:clee
;;   (:nicknames #:clee)
;;   (:use #:cl)
;;   (:export #:free-vars-for-emacs
;;            #:values-for-emacs
;;            #:find-free-variables
;;            #:find-variables
;;            #:tree-walk))

;; (in-package #:clee)

;; #+sbcl
;; (eval-when (:compile-toplevel :load-toplevel :execute)
;;   (require :sb-cltl2))

;; (defun tree-walk (tree fn &key key)
;;   (subst-if nil (constantly nil) tree
;;             :key (lambda (sub-tree)
;;                    (funcall fn (funcall (or key #'identity) sub-tree)))))

;; (defun find-variables (form &optional env)
;;   (let ((vars '()))
;;     (flet ((record-variable (x)
;;              (when (and (symbolp x)
;;                         (not (constantp x env)))
;;                (pushnew x vars))))
;;       (tree-walk form #'record-variable))
;;     vars))

;; (defun macroexpand-all (form &optional env)
;;   (declare (ignorable env))
;;   #+sbcl (sb-cltl2:macroexpand-all form env)
;;   #-sbcl (swank::macroexpand-all form))

;; (defun special-variable-p (symbol &optional env)
;;   (declare (ignorable symbol env))
;;   (eql (or #+sbcl (sb-cltl2:variable-information symbol env))
;;        :special))

;; (defmacro %extract-variable (variable specials)
;;   (declare (special *free-variables*))
;;   (when (or specials
;;             (not (special-variable-p variable)))
;;     (pushnew variable *free-variables*))
;;   (gensym))

;; (defun find-free-variables (form &key env (specials t))
;;   (let ((bindings (loop for v in (find-variables form env)
;;                         collect (list v `(%extract-variable ,v ,specials))))
;;         (*free-variables* '()))
;;     (declare (special *free-variables*))
;;     ;; macro-expanding picks up free variables as side effect
;;     (macroexpand-all `(symbol-macrolet ,bindings ,form) env)
;;     *free-variables*))

;; (defun values-for-emacs (list &optional package)
;;   (with-standard-io-syntax
;;    (let ((*print-case* :downcase)
;;          (*print-readably* nil)
;;          (*print-pretty* nil)
;;          (*package* (or package *package*)))
;;      (mapcar #'prin1-to-string list))))

;; (defun free-vars-for-emacs (form-string package &key env specials)
;;   (let* ((form (swank::from-string form-string))
;;          (free-vars (reverse (find-free-variables form :env env
;;                                                   :specials specials))))
;;     (values-for-emacs free-vars (find-package (string-upcase package)))))

;; `align-slot-specs-in-form':
;; * Does not work if slot forms contain newlines
;; * Does not work well with #+ and #- reader conditionals
;; * Long slot options cause large columns (:documentation ...)

;; (defun accessor-name/ref (slot-name)
;;   "SLOT-REF style accessor names."
;;   (concat (accessor-name/% slot-name) "-ref"))

;; (defun accessor-name (slot-name)
;;   (if (functionp accessor-name-function)
;;       (funcall accessor-name-function slot-name)
;;     (accessor-name/get slot-name)))

;; (defun canonical-slot-name/% (slot-name)
;;   "%SLOT style slots names."
;;   (if (string-match "^%" slot-name)
;;       slot-name
;;     (concat "%" slot-name)))

;; (defun canonical-slot-name (slot-name)
;;   "Returns canonicalized slot name.  You can use this hook to
;; ensure certain style in naming your slots, for instance
;; %SLOT."
;;   (if (functionp canonical-slot-name-function)
;;       (funcall canonical-slot-name-function slot-name)
;;     slot-name))

;; (defun initarg-name (slot-name)
;;   (if (functionp initarg-name-function)
;;       (funcall initarg-name-function slot-name)
;;     (initarg-name/keyword slot-name)))

;; (defun initarg-name/keyword (slot-name)
;;   (concat ":" (accessor-name/% slot-name)))

;; (defun initarg-name/symbol (slot-name)
;;   (concat "'" (accessor-name/% slot-name)))

;; (defun -align-slot-form-regexp ()
;;   (mapconcat 'identity align-slot-forms-list "\\|"))

;; (defun maybe-splice-progn ()
;;   "Splice PROGN form at point into its surrounding form.
;; Nothing is done if point is not preceding a PROGN form."
;;   (interactive "*")
;;   (paredit-point-at-sexp-start)
;;   (when (-looking-at-or-inside "progn")
;;     (paredit-forward-kill-word)
;;     (delete-region (prog1 (point) (paredit-skip-whitespace t))
;;                    (point))
;;     (paredit-splice-sexp-killing-backward)
;;     (paredit-point-at-sexp-start)))

;; (defun point-at-enclosing-let-form ()
;;   "Move point to enclosing LET/LET* form if existing.
;; Point is not moved across other binding forms \(e.g., DEFUN,
;; LABELS or FLET.)"
;;   (interactive)
;;   (let ((here.point (point)))
;;     (or (ignore-errors
;;           (block nil
;;             (backward-up-list)
;;             (while (not (looking-at "(let\\*?\\S_"))
;;               (when (looking-at "(\\(def\\s_*\\|labels\\|flet\\)\\S_")
;;                 (return nil))
;;               (backward-up-list))
;;             (point)))
;;         (prog1 nil
;;           (goto-char here.point)))))

;; (defun -symbol-namep (symbol)
;;   (and (stringp symbol)
;;        (not (string= symbol ""))))

;; (defun canonical-package-name (package-name)
;;   (and package-name (not (string= "" package-name))
;;        ;; very naive
;;        (lexical-let ((package-name (-trim-whitespace package-name)))
;;          (if (or (string-match "^#?:\\(.*\\)$" package-name)
;;                  (string-match "^\"\\(.*\\)\"$" package-name))
;;              (match-string-no-properties 1 package-name)
;;            package-name))))

;; (defun canonical-package-designator (package-name)
;;   (and package-name (not (string= "" package-name))
;;        (funcall canonical-package-designator-function
;;                 (canonical-package-name package-name))))

;; (defun package-designator/uninterned-symbol (package-name)
;;   (concat "#:" (downcase package-name)))

;; (defun package-designator/keyword (package-name)
;;   (concat ":" (downcase package-name)))

;; (defun package-designator/symbol (package-name)
;;   (downcase package-name))

;; (defun package-designator/string (package-name)
;;   (prin1-to-string (upcase package-name)))

;; (defun -end-of-sexp-column ()
;;   "Move point to end of current form, neglecting trailing whitespace."
;;   (forward-sexp)
;;   (while (forward-comment +1))
;;   (skip-chars-backward "[:space:]"))

;; (defun -sexp-column-widths ()
;;   "Return list of column widths for s-expression at point."
;;   (down-list)
;;   (loop do (while (forward-comment 1))
;;         until (or (looking-at ")") (eobp))
;;         collect (- (- (point)
;;                       (progn
;;                         (-end-of-sexp-column)
;;                         (point))))
;;         finally (up-list)))

;; (defun -max* (&rest args)
;;   (reduce #'max args :key (lambda (arg) (or arg 0))))

;; (defun align-sexp-columns (column-widths)
;;   "Align expressions in S-expression at point.
;; COLUMN-WIDTHS is expected to be a list."
;;   (down-list)
;;   (loop initially (while (forward-comment +1))
;;         for width in column-widths
;;         until (looking-at ")")
;;         do (let ((beg (point)))
;;              (-end-of-sexp-column)
;;              (let ((used (- (point) beg)))
;;                (just-one-space (if (looking-at "[[:space:]]*)") 0
;;                                  (1+ (- width used))))))
;;         finally (up-list)))

;; (defun -slot-form-at-point-p ()
;;   (ignore-errors
;;     (save-excursion
;;       (backward-up-list +3)
;;       (-looking-at-or-inside (-align-slot-form-regexp)))))

;; (defun -region-active-p ()
;;   "Returns true if `transient-mark-mode' is used and region is active."
;;   (and (boundp 'transient-mark-mode)
;;        transient-mark-mode
;;        (boundp 'mark-active)
;;        mark-active))

;; (defun ignore-event (event)
;;   "Ignores a (mouse) event.
;; This is used to override mouse bindings in a minor mode keymap,
;; but does otherwise nothing."
;;   (interactive "e"))

;; (defmacro -with-doublequotes (&rest body)
;;   `(progn
;;      (paredit-doublequote)
;;      (insert (or (progn ,@body) ""))
;;      (paredit-doublequote)
;;      nil))

;; ;; lenient variant of `slime-read-package-name'
;; (defun read-package-name (prompt &optional initial-value)
;;   "Read a package name from the minibuffer, prompting with PROMPT."
;;   (let ((completion-ignore-case t))
;;     (canonical-package-name
;;      (completing-read prompt (when (and (featurep 'slime)
;;                                         (connected-p))
;;                                (slime-bogus-completion-alist
;;                                 (slime-eval
;;                                  `(swank:list-all-package-names t))))
;;                       nil nil initial-value nil initial-value))))

;; (defun find-potential-buffer-package ()
;;   (canonical-package-name
;;    (or slime-buffer-package
;;        (and (fboundp 'slime-find-buffer-package)
;;             (slime-find-buffer-package))
;;        (let ((case-fold-search t)
;;              (regexp (concat "^(\\(cl:\\|common-lisp:\\)?defpackage\\>[ \t']*"
;;                              "\\([^()]+\\)")))
;;          (save-excursion
;;            (when (or (re-search-backward regexp nil t)
;;                      (re-search-forward  regexp nil t))
;;              (match-string-no-properties 2)))))))

;; (defun -assoc-match (key alist)
;;   (loop for entry in alist do
;;         (cond ((stringp (car entry))
;;                (when (eq t (compare-strings (car entry) 0 nil
;;                                             key 0 nil
;;                                             case-fold-search))
;;                  (return entry)))
;;               ((functionp (car entry))
;;                (when (funcall (car entry) key)
;;                  (return entry)))
;;               ((eq t (car entry))
;;                (return entry)))))

;; ;;; ASDF
;; (defun walk-filesystem (spec enter-fn leave-fn)
;;   (when (and (funcall enter-fn
;;                       (file-name-directory spec)
;;                       (file-name-nondirectory spec))
;;              (file-directory-p spec))
;;     (let ((contents (sort (directory-files spec nil nil nil)
;;                           #'string<)))
;;       (dolist (file contents)
;;         (unless (member file '("." ".."))
;;           (walk-filesystem (concat (file-name-as-directory spec) file)
;;                                     enter-fn leave-fn)))
;;       (when leave-fn
;;         (funcall leave-fn
;;                  (file-name-directory spec)
;;                  (file-name-nondirectory spec))))))

;; (defun asdf-make-spec/file-type (filename)
;;   (list (file-name-sans-extension filename)
;;         :type (file-name-extension filename)))

;; (defun asdf-make-spec/filename (filename)
;;   (list* filename (when (file-name-extension filename)
;;                     (list :pathname filename))))

;; (defun asdf-classify-component (directory filename)
;;   (dolist (mapping asdf-component-mapping)
;;     (destructuring-bind (regex tag &optional filename-fn) mapping
;;       (when (string-match regex (concat directory filename))
;;         (return `(,tag ,@(if filename-fn
;;                              (funcall filename-fn filename)
;;                            (list (file-name-sans-extension filename)))))))))

;; (defun letify-form (var)
;;   "Extract the form at point into a new LET binding.
;; The binding variable's name is requested in the mini-buffer."
;;   (interactive "*sVariable name: ")
;;   (when (-symbol-namep var)
;;     (paredit-point-at-sexp-start)
;;     (paredit-wrap-sexp +1)              ; wrap with (LET ...)
;;     (insert "let ")
;;     (paredit-wrap-sexp +1)              ; wrap binders
;;     (let ((binder.start (point)))
;;       (paredit-wrap-sexp +1)
;;       (insert var " ")
;;       (up-list)
;;       (highlight-binder binder.start (point)))
;;     (up-list)                           ; point at LET body
;;     (paredit-newline)
;;     (save-excursion                     ; insert variable name
;;       (insert var))))

;; (defun letify-form-up (var &optional arg)
;;   "Extract the form at point into a (possibly enclosing) LET binding.
;; The binding variable's name is requested in the mini-buffer.
;; With prefix argument, or if no suitable binding can be found,
;; `letify-form' is executed instead."
;;   (interactive "*sVariable name: \nP")
;;   (let ((let.start (save-excursion
;;                      (point-at-enclosing-let-form))))
;;     (cond ((and (-symbol-namep var)
;;                 (not arg)
;;                 let.start)
;;            (paredit-point-at-sexp-start)
;;            (let* ((form.start (prog1 (point) (forward-sexp)))
;;                   (form (delete-and-extract-region form.start (point))))
;;              (save-excursion
;;                (insert var)
;;                (goto-char let.start)
;;                (down-list)              ; move point from |(let ...
;;                (forward-sexp +2)        ; to behind last binder form
;;                (backward-down-list)
;;                (paredit-newline)        ; insert new binder
;;                (let ((binder.start (point)))
;;                  (insert "(" var " " form ")")
;;                  (highlight-binder binder.start (point)))
;;                (backward-sexp)          ; ... and reindent it
;;                (indent-sexp))))
;;           (t (letify-form var)))))

;; (defun extract-to-defun (start end name &optional package)
;;   "Extracts region from START to END as new defun NAME.
;; The marked region is replaced with a call, the actual function
;; definition is placed on the kill ring.

;; A best effort is made to determine free variables in the marked
;; region and make them parameters of the extracted function.  This
;; involves macro-expanding code, and as such might have side effects."
;;   (interactive "*r\nsName for extracted function: ")
;;   (let* ((form-string (buffer-substring-no-properties start end))
;;          (free-vars (slime-eval `(redshank:free-vars-for-emacs
;;                                   ,(concat "(locally " form-string ")")
;;                                   ,(or package (slime-pretty-package-name
;;                                                 (slime-current-package))))
;;                                 package)))
;;     (flet ((princ-to-string (o)
;;              (with-output-to-string
;;                (princ (if (null o) "()" o)))))
;;       (with-temp-buffer
;;         (lisp-mode)                     ; for proper indentation
;;         (insert "(defun " name " " (princ-to-string free-vars) "\n")
;;         (insert form-string ")\n")
;;         (goto-char (point-min))
;;         (indent-sexp)
;;         (paredit-hack-kill-region (point-min) (point-max))
;;         (message (substitute-command-keys
;;                   "Extracted function `%s' now on kill ring; \\[yank] to insert at point.") ;
;;                  name))
;;       (delete-region start end)
;;       (princ (list* name free-vars) (current-buffer)))))

;; (defun enclose-form-with-lambda (arglist)
;;   "Enclose form with lambda expression with parameter VAR.
;; With prefix argument ARG, enclose ARG upward forms.

;; Example:
;;   \(foo x (bar y| z) qux)

;; \\[enclose-form-with-lambda] RET RET yields:

;;   \(foo x (lambda (y) (bar y| z)) qux)"
;;   (interactive
;;    (let ((arglist (thing-at-point 'symbol)))
;;      (when (and (stringp arglist)
;;                 (string-match "[(]" arglist))
;;        (setq arglist ""))
;;      (list (read-string "Lambda arglist: " arglist))))
;;   (save-excursion
;;     (call-interactively 'backward-up-list)
;;     (paredit-wrap-sexp +1)
;;     (insert "lambda (" arglist ")")
;;     (if (> (- (line-end-position) (line-beginning-position))
;;            (current-fill-column))
;;         (newline)
;;       (insert " "))
;;     (backward-up-list)
;;     (indent-sexp)))

;; (defun upgrade-form ()
;;   "when->if->cond")
;; (defun negate-form ())
;; (defun align-forms-as-columns (beg end)
;;   "Align S-expressions in region as columns.
;; Example:
;;   \(define-symbol-macro MEM (mem-of *cpu*))
;;   \(define-symbol-macro IP (ip-of *cpu*))
;;   \(define-symbol-macro STACK (stack-of *cpu*))

;; is formatted as:

;;   \(define-symbol-macro MEM   (mem-of *cpu*))
;;   \(define-symbol-macro IP    (ip-of *cpu*))
;;   \(define-symbol-macro STACK (stack-of *cpu*))"
;;   (interactive "*r")
;;   (save-restriction
;;     (narrow-to-region beg end)
;;     (goto-char beg)
;;     (let* ((columns
;;             (loop do (while (forward-comment +1))
;;                   until (or (looking-at ")") (eobp))
;;                   collect (-sexp-column-widths)))
;;            (max-column-widths
;;             (loop for cols = columns then (mapcar #'cdr cols)
;;                   while (some #'consp cols)
;;                   collect (apply #'-max* (mapcar #'car cols)))))
;;       (goto-char beg)
;;       (loop do (while (forward-comment +1))
;;             until (or (looking-at ")") (eobp))
;;             do (align-sexp-columns max-column-widths)))))

;; (in-package CLIMACS-COMMANDS)

;; (defun find-accessor (class-name slot-name)
;;   "accepts a class, and returns the accessor information for it"
;;   (let* ((class-slots (mm::->> (find-class class-name)
;; 			       (c2mop:class-precedence-list)
;; 			       (remove-if (lambda (o) (member (type-of o) '(standard-object t) :test #'equal)))
;; 			       (mapcar #'c2mop:class-direct-slots)
;; 			       (apply #'append)))
;; 	 (matches (filter (lambda (direct-slot) 
;; 			    (scan (typecase slot-name
;; 				    (string (string-upcase slot-name))
;; 				    (symbol (symbol-name slot-name))) 
;; 				  (symbol-name (sb-mop:slot-definition-name direct-slot))))
;; 			  class-slots)))
;;     (mapcar (lambda (slot-definition) (list (sb-mop:slot-definition-readers slot-definition)
;; 				       (sb-mop:slot-definition-writers slot-definition))) matches)))

 ;; (defun qsearch ())
;; (defun new-system ())
;; (defun visit-current-url ())
;; (defun cut-string ())
;; (defun toggle-comment-sexp-forwards ())
;; (defun call-or-initalize-form-at-point ())
;; (defun insert-comment ())
;; (defun document-buffer ())
;; (defun document-package ())
;; (defun document-system ())
;; (defun export-symbols-for-class ())

;; (in-package :climacs-masamune)

;; (defalias regex-matches cl-ppcre:all-matches-as-strings)

;; (define-command (com-create-new-buffer :name "new buffer" :command-table lisp-table) ()
;;   (labels ((f (s) (read-from-string (apply #'cat (regex-matches "[0-9]" s)))))
;;     (let* ((scratch-files (filter (lambda (p) (and (cl-ppcre:scan "scratch" (pathname-name p))
;; 					      (cl-ppcre:scan "[0-9]" (llast (pathname-name p)))))
;; 				  (ls "/tmp/")))
;; 	   (new-scratch-file (if scratch-files
;; 				 (1+ (f (car (sort scratch-files (lambda (p1 p2) (> (f p1) (f p2)))))))
;; 				 "/tmp/scratch-0.lisp")))
;;       (esa-io:frame-find-file *application-frame* new-scratch-file))))

;; (use-package 'mm)

;;; plist -> class definition -> class instance -> plist

;; (defun object-slots->plist (object))
;; (defun plist->class-definition (plist))
;; (defun plist->object (plist class))

;;; slots and defclass

;; (defun insert-seq (seq)
;;   (loop for o across seq do (insert-object (point) o)))

;; (defun exec-command (command-name)
;;   (EXECUTE-FRAME-COMMAND *application-frame* (list command-name)))

;; (defun newline ()
;;   (insert-object (point) #\newline))

;; (define-command (com-slot-skeletons :name "slot skeletons" :command-table lisp-table
;; 				    :keystroke ((:meta :control #\s))) ()
;;   (let* ((name (accept 'symbol :prompt "slot name" :default nil)))
;;     (when name
;;       (insert-seq (format nil "(:accessor ~a " name))
;;       (insert-seq (format nil ":initform ~a :initarg ~a " (accept 'clim-internals::form :prompt "initform" :default nil) name))
;;       (awhen (accept 'string :prompt "documentation" :default nil) (insert-seq (format nil ":documentation ~s" it)))
;;       (insert-seq ")")
;;       (newline)
;;       (funcall 'com-slot-skeletons))))

;; (define-command (com-defclass-skeleton :name "defclass skeleton" :command-table lisp-table
;; 				       :keystroke ((#\c :control :meta))) ()
;;   ;; TODO 2014-10-31T01:20:49-07:00 Gabriel Laddel
;;   ;; - alphabetize slots 
;;   ;; - autocomplete metaclasses
;;   ;; - default initargs
;;   ;; - types
;;   (insert-seq (format nil "(defclass ~a (" (accept 'symbol :prompt "classname")))
;;   ;; slots
;;   (loop for input = (accept 'symbol :prompt "superclass" :default nil)
;; 	while input
;; 	do (insert-seq (format nil "~a " input))
;; 	finally (progn (insert-seq ")") (newline)))
;;   (insert-seq "(")
;;   (exec-command 'com-slot-skeletons)
;;   (insert-seq ")")
;;   ;; documentation
;;   (awhen (accept 'string :prompt "class documentation string") (insert-seq (format nil "(:documentation ~s)" it))))



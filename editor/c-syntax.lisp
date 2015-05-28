;;; Syntax module for analysing C

(in-package :climacs-c-syntax)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; The command table.

(define-syntax-command-table c-table
    :errorp nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; the syntax object

(define-syntax c-syntax (lr-syntax-mixin fundamental-syntax)
  ()
  (:name "C")
  (:pathname-types "c" "h")
  (:command-table c-table)
  (:default-initargs :initial-state |initial-state |))

(defmethod name-for-info-pane ((syntax c-syntax) &key pane)
  (declare (ignore pane))
  (format nil "C"))

(defmethod display-syntax-name ((syntax c-syntax)
				(stream extended-output-stream) &key pane)
  (declare (ignore pane))
  (princ "C" stream))

;;; Lexing

(define-lexer-state lexer-preprocessor-state ()
  ()
  (:documentation "In this state, the lexer is working inside a
    preprocessing directive."))

(define-lexer-state lexer-escaped-preprocessor-state (lexer-preprocessor-state)
  ()
  (:documentation "In this state, the lexer is working inside a
    preprocessing directive and an escaped newline has been seen."))

(define-lexer-state lexer-string-state ()
  ()
  (:documentation "In this state, the lexer is working inside a string
    delimited by double quote characters."))

(define-lexer-state lexer-line-comment-state ()
  ()
  (:documentation "In this state, the lexer is working inside a line
    comment starting with //."))

(define-lexer-state lexer-long-comment-state ()
  ()
  (:documentation "In this state, the lexer is working inside a long
    comment delimited by /* and */."))

(define-lexer-state lexer-character-state ()
  ()
  (:documentation "In this state, the lexer is working inside a
    character constant delimited by single quote characters."))

(defclass c-nonterminal (nonterminal) ())

(defclass form (c-nonterminal) ())
(defclass complete-form-mixin () ())
(defclass incomplete-form-mixin () ())

(defclass comment (c-nonterminal) ())
(defclass line-comment (c-comment) ())
(defclass long-comment (c-comment) ())

(defclass preprocessor-directive (c-nonterminal) ())

(defclass error-symbol (c-nonterminal) ())

(defclass c-lexeme (lexeme)
  ((ink)
   (face)))

(defclass form-lexeme (form c-lexeme) ())

(defclass keyword-lexeme (form-lexeme) ())

(defclass storage-class-specifier () ())
(defclass type-specifier () ())
(defclass type-qualifier () ())
(defclass function-specifier () ())
(defclass operator () ())

(eval-when (:compile-toplevel :load-toplevel :execute)
(defun spelling-to-symbol (name)
  (intern (concatenate 'string name "-LEXEME") #.*package*)))

(defmacro define-keywords (&rest keyword-names)
  `(progn
     ,@(loop for (name . supers) in keyword-names
	     for real-name = (spelling-to-symbol name)
	     collecting `(defclass ,real-name (,@ supers keyword-lexeme) ())
	       into defclasses
	     collecting name into names
	     finally (return (cons `(defparameter *keyword-spellings* ',names)
				   defclasses)))))
(define-keywords 
    ("auto" storage-class-specifier) 
    ("break" operator) 
    ("case" operator)
    ("char" type-specifier)
    ("const" type-qualifier)
    ("continue" operator)
    ("default" operator)
    ("do" operator)
    ("double" type-specifier) 
    ("else" operator)
    ("enum" type-specifier)
    ("extern" storage-class-specifier)
    ("float" type-specifier)
    ("for" operator)
    ("goto" operator)
    ("if" operator)
    ("inline" function-specifier)
    ("int" type-specifier) 
    ("long" type-specifier)
    ("register" storage-class-specifier) 
    ("restrict" type-qualifier)
    ("return" operator)
    ("short" type-specifier)
    ("signed" type-specifier)
    ("sizeof" operator) 
    ("static" storage-class-specifier) 
    ("struct" type-specifier)
    ("switch" operator)
    ("typedef" storage-class-specifier)
    ("union" type-specifier)
    ("unsigned" type-specifier) 
    ("void" type-specifier)
    ("volatile" type-qualifier)
    ("while" operator) 
    ("_Bool" type-specifier) 
    ("_Complex" type-specifier) 
    ("_Imaginary" type-specifier))

(defclass identifier-lexeme (form-lexeme) ())
(defclass constant-lexeme (form-lexeme) ())
(defclass string-literal-lexeme (form-lexeme) ())
(defclass punctuator-lexeme (form-lexeme) ())

#|
[ ] ( ) { } . ->
++ -- & * + - ~ !
/ % << >> < > <= >= == != ^ | && ||
? : ; ...
= *= /= %= += -= <<= >>= &= ^= |=
, # ##
<: :> <% %> %: %:%:
|#

(defmacro define-punctuators (&rest punctuator-names)
  `(progn
     ,@(loop for name in punctuator-names
	     for real-name = 
			   (intern (concatenate 'string 
						 (string name) "-LEXEME")
                                   #.*package*)
	     collecting `(defclass ,real-name (punctuator-lexeme) ()))))

(define-punctuators 
;;     left-bracket right-bracket left-parenthesis
;;     right-parenthesis left-brace right-brace 
    dot dereference
    increment decrement ampersand asterisk plus minus tilde
    exclamation slash percent left-shift right-shift
    left-angle-bracket right-angle-bracket leq geq eq neq
    circumflex pipe and-and or-or question colon semi-colon ellipsis
    equal asterisk-equal slash-equal percent-equal plus-equal minus-equal
    left-shift-equal right-shift-equal ampersand-equal circumflex-equal
    pipe-equal comma hash hash-hash)

(defclass delimiter-mixin () ())
(defclass opening-delimiter-mixin (delimiter-mixin) ())
(defclass closing-delimiter-mixin (delimiter-mixin) ())

(defclass left-bracket-lexeme (punctuator-lexeme opening-delimiter-mixin) ())
(defclass right-bracket-lexeme (punctuator-lexeme closing-delimiter-mixin) ())
(defclass left-parenthesis-lexeme (punctuator-lexeme opening-delimiter-mixin) ())
(defclass right-parenthesis-lexeme (punctuator-lexeme closing-delimiter-mixin) ())
(defclass left-brace-lexeme (punctuator-lexeme opening-delimiter-mixin) ())
(defclass right-brace-lexeme (punctuator-lexeme closing-delimiter-mixin) ())

(defclass integer-constant-lexeme (constant-lexeme) ())
(defclass floating-constant-lexeme (constant-lexeme) ())
;; (defclass enumeration-constant-lexeme (constant-lexeme) ())
;; (defclass character-constant-lexeme (constant-lexeme) ())

(defclass error-lexeme (c-lexeme) ())

(defclass line-comment-start-lexeme (c-lexeme) ())
(defclass long-comment-start-lexeme (c-lexeme) ())
(defclass comment-end-lexeme (c-lexeme) ())
(defclass string-start-lexeme (c-lexeme) ())
(defclass wide-string-start-lexeme (c-lexeme) ())
(defclass string-end-lexeme (c-lexeme) ())
(defclass preprocessor-start-lexeme (c-lexeme) ())
(defclass preprocessor-end-lexeme (c-lexeme) ())
(defclass escaped-newline-lexeme (c-lexeme) ())
(defclass word-lexeme (c-lexeme) ())
(defclass delimiter-lexeme (c-lexeme) ())
(defclass text-lexeme (c-lexeme) ())
(defclass character-start-lexeme (c-lexeme) ())
(defclass wide-character-start-lexeme (c-lexeme) ())
(defclass character-end-lexeme (c-lexeme) ())

(defun alpha-or-underscore-p (ch)
  (and (characterp ch)
       (or (alpha-char-p ch)
	   (char= ch #\_))))

;; todo - other chars in identifiers etc.
(defun c-constituentp (ch)
  (and (characterp ch)
       (or (alphanumericp ch)
	   (char= ch #\_))))

(defmethod skip-inter ((syntax c-syntax) state scan)
  (macrolet ((fo () `(forward-object scan)))
    (loop when (end-of-buffer-p scan)
	    do (return nil)
	  until (not (whitespacep syntax (object-after scan)))
	  do (fo)
	  finally (return t))))

(defmethod lex ((syntax c-syntax) (state lexer-toplevel-state) scan)
  (macrolet ((fo () `(forward-object scan)))
    (let ((object (object-after scan)))
      (case object
	(#\" (fo) (make-instance 'string-start-lexeme))
	(#\' (fo) (make-instance 'character-start-lexeme))
	(#\# (let ((bolp (beginning-of-line-p scan)))
	       (fo) 
	       (if bolp
		   (make-instance 'preprocessor-start-lexeme)
		   (make-instance 'error-lexeme))))
	(#\[ (fo) (make-instance 'left-bracket-lexeme))
	(#\] (fo) (make-instance 'right-bracket-lexeme))
	(#\( (fo) (make-instance 'left-parenthesis-lexeme))
	(#\) (fo) (make-instance 'right-parenthesis-lexeme))
	(#\{ (fo) (make-instance 'left-brace-lexeme))
	(#\} (fo) (make-instance 'right-brace-lexeme))
	(#\. (fo) (if (end-of-buffer-p scan)
		      (make-instance 'dot-lexeme)
		      (cond ((eql (object-after scan) #\.)
			     (fo)
			     (cond ((or (end-of-buffer-p scan)
					(not (eql (object-after scan) #\.)))
				    (backward-object scan)
				    (make-instance 'dot-lexeme))
				   (t (fo) (make-instance 'ellipsis-lexeme))))
			    ((and (characterp (object-after scan))
				  (digit-char-p (object-after scan)))
			     (backward-object scan)
			     (lex-token syntax scan))
			    (t (make-instance 'dot-lexeme)))))
	(#\- (fo) (if (end-of-buffer-p scan)
		      (make-instance 'minus-lexeme)
		      (case (object-after scan)
			(#\- (fo) (make-instance 'decrement-lexeme))
			(#\= (fo) (make-instance 'minus-equal-lexeme))
			(#\> (fo) (make-instance 'dereference-lexeme))
			(t (make-instance 'minus-lexeme)))))
	(#\+ (fo) (if (end-of-buffer-p scan)
		      (make-instance 'plus-lexeme)
		      (case (object-after scan)
			(#\+ (fo) (make-instance 'increment-lexeme))
			(#\= (fo) (make-instance 'plus-equal-lexeme))
			(t (make-instance 'plus-lexeme)))))
	(#\& (fo) (if (end-of-buffer-p scan)
		      (make-instance 'ampersand-lexeme)
		      (case (object-after scan)
			(#\& (fo) (make-instance 'and-and-lexeme))
			(#\= (fo) (make-instance 'ampersand-equal-lexeme))
			(t (make-instance 'ampersand-lexeme)))))
	(#\* (fo) (if (end-of-buffer-p scan)
		      (make-instance 'asterisk-lexeme)
		      (cond ((eql (object-after scan) #\=)
			     (fo)
			     (make-instance 'asterisk-equal-lexeme))
			    (t (make-instance 'asterisk-lexeme)))))
	(#\~ (fo) (make-instance 'tilde-lexeme))
	(#\! (fo) (if (end-of-buffer-p scan)
		      (make-instance 'exclamation-lexeme)
		      (cond ((eql (object-after scan) #\=)
			     (fo)
			     (make-instance 'neq-lexeme))
			    (t (make-instance 'exclamation-lexeme)))))
	(#\/ (fo) (if (end-of-buffer-p scan)
		      (make-instance 'slash-lexeme)
		      (case (object-after scan)
			(#\= (fo) (make-instance 'slash-equal-lexeme))
			(#\* (fo) (make-instance 'long-comment-start-lexeme))
			(#\/ (fo) (make-instance 'line-comment-start-lexeme))
			(t (make-instance 'slash-lexeme)))))
	(#\% (fo) (if (end-of-buffer-p scan)
		      (make-instance 'percent-lexeme)
		      (case (object-after scan)
			(#\= (fo) (make-instance 'percent-equal-lexeme))
			(#\> (fo) (make-instance 'right-brace-lexeme))
			(#\: (fo)
			     (cond ((eql (object-after scan) #\%)
				    (fo)
				    (cond ((eql (object-after scan) #\:)
					   (make-instance 'hash-hash-lexeme))
					  (t
					   (backward-object scan)
					   (make-instance 'preprocessor-start-lexeme))))
				   (t (make-instance 'preprocessor-start-lexeme
))))
			(t (make-instance 'percent-lexeme)))))
	(#\< (fo) (if (end-of-buffer-p scan)
		      (make-instance 'left-angle-bracket-lexeme)
		      (case (object-after scan)
			(#\= (fo) (make-instance 'leq-lexeme))
			(#\: (fo) (make-instance 'left-bracket-lexeme))
			(#\% (fo) (make-instance 'left-brace-lexeme))
			(#\< (fo) 
			     (cond ((eql (object-after scan) #\=)
				    (fo)
				    (make-instance 'left-shift-equal-lexeme))
				   (t (make-instance 'left-shift-lexeme))))
			(t (make-instance 'left-angle-bracket-lexeme)))))
	(#\> (fo) (if (end-of-buffer-p scan)
		      (make-instance 'right-angle-bracket-lexeme)
		      (case (object-after scan)
			(#\= (fo) (make-instance 'geq-lexeme))
			(#\> (fo) 
			     (cond ((eql (object-after scan) #\=)
				    (fo)
				    (make-instance 'right-shift-equal-lexeme))
				   (t (make-instance 'right-shift-lexeme))))
			(t (make-instance 'right-angle-bracket-lexeme)))))
	(#\= (fo) (if (end-of-buffer-p scan)
		      (make-instance 'equal-lexeme)
		      (cond ((eql (object-after scan) #\=)
			     (fo)
			     (make-instance 'eq-lexeme))
			    (t (make-instance 'equal-lexeme)))))
	(#\^ (fo) (if (end-of-buffer-p scan)
		      (make-instance 'circumflex-lexeme)
		      (cond ((eql (object-after scan) #\=)
			     (fo)
			     (make-instance 'circumflex-equal-lexeme))
			    (t (make-instance 'circumflex-lexeme)))))
	(#\| (fo) (if (end-of-buffer-p scan)
		      (make-instance 'pipe-lexeme)
		      (case (object-after scan)
			(#\| (fo) (make-instance 'or-or-lexeme))
			(#\= (fo) (make-instance 'pipe-equal-lexeme)) 
			(t (make-instance 'pipe-lexeme)))))
	(#\? (fo) (make-instance 'question-lexeme))
	(#\: (fo) (if (end-of-buffer-p scan)
		      (make-instance 'colon-lexeme)
		      (cond ((eql (object-after scan) #\>)
			     (fo)
			     (make-instance 'right-bracket-lexeme))
			    (t (make-instance 'colon-lexeme)))))
	(#\; (fo) (make-instance 'semi-colon-lexeme))
	(#\, (fo) (make-instance 'comma-lexeme))
	(t (cond ((or (alphanumericp object)
                      (eql object #\_))
		  (cond ((eql object #\L)
			 (fo)
			 (cond ((end-of-buffer-p scan)
				(make-instance 'identifier-lexeme))
			       ((eql (object-after scan) #\')
				(fo)
				(make-instance 'wide-character-start-lexeme))
			       ((eql (object-after scan) #\")
				(fo)
				(make-instance 'wide-string-start-lexeme))
			       (t
				(backward-object scan)
				(lex-token syntax scan))))
			(t (lex-token syntax scan))))
                 (t (fo) (make-instance 'error-lexeme))))))))

(defmethod skip-inter ((syntax c-syntax) (state lexer-preprocessor-state) scan)
  (macrolet ((fo () `(forward-object scan)))
    (loop until (or (end-of-line-p scan)
		    (not (whitespacep syntax (object-after scan))))
	  do (fo)
	  finally (return t))))

(defmethod skip-inter ((syntax c-syntax) 
		       (state lexer-escaped-preprocessor-state)
		       scan)
  (macrolet ((fo () `(forward-object scan)))
    (loop with newline-seen = nil
	  until (or (end-of-buffer-p scan)
		    (and newline-seen (end-of-line-p scan))
		    (not (whitespacep syntax (object-after scan))))
	  when (eql (object-after scan) #\Newline)
	    do (setf newline-seen t)
	  do (fo)
	  finally (return t))))

(defmethod lex ((syntax c-syntax) (state lexer-preprocessor-state) scan)
  (macrolet ((fo () `(forward-object scan)))
    (cond ((end-of-line-p scan)
	   (make-instance 'preprocessor-end-lexeme))
	  ((eql (object-after scan) #\\)
	   (fo)
	   (if (and (not (end-of-buffer-p scan))
		    (end-of-line-p scan))
	       (make-instance 'escaped-newline-lexeme)
	       (make-instance 'delimiter-lexeme)))
	  ((eql (object-after scan) #\#)
	   (fo)
	   (if (end-of-buffer-p scan)
	       (make-instance 'hash-lexeme)
	       (cond ((eql (object-after scan) #\#)
		      (fo)
		      (make-instance 'hash-hash-lexeme))
		     (t (make-instance 'hash-lexeme)))))
	  ((c-constituentp (object-after scan))
	   (loop until (or (end-of-buffer-p scan)
			   (not (c-constituentp (object-after scan))))
		 do (fo))
	   (make-instance 'word-lexeme))
	  (t (fo) (make-instance 'delimiter-lexeme)))))

(defmethod lex ((syntax c-syntax) (state lexer-string-state) scan)
  (macrolet ((fo () `(forward-object scan)))
    (let ((object (object-after scan)))
      (cond ((eql object #\") (fo) (make-instance 'string-end-lexeme))
	    ((eql object #\\)
	     ;; TODO: string escapes
	     (fo)
	     (unless (end-of-buffer-p scan)
	       (fo))
	     (make-instance 'delimiter-lexeme))
	    ;; TODO: c-constituentp
	    ((c-constituentp object)
	     (loop until (or (end-of-buffer-p scan)
			     (not (c-constituentp (object-after scan))))
		   do (fo))
	     (make-instance 'word-lexeme))
	    (t (fo) (make-instance 'delimiter-lexeme))))))

(defmethod lex ((syntax c-syntax) (state lexer-character-state) scan)
  (macrolet ((fo () `(forward-object scan)))
    (let ((object (object-after scan)))
      (cond ((eql object #\') 
	     (fo) 
	     (make-instance 'character-end-lexeme))
	    ((eql object #\\)
	     ;; TODO: character escapes
	     (fo)
	     (unless (end-of-buffer-p scan)
	       (fo))
	     (make-instance 'delimiter-lexeme))
	    ((c-constituentp object)
	     (loop until (or (end-of-buffer-p scan)
			     (not (c-constituentp (object-after scan))))
		   do (fo))
	     (make-instance 'word-lexeme))
	    (t (fo) (make-instance 'delimiter-lexeme))))))

(defmethod lex ((syntax c-syntax) (state lexer-long-comment-state) scan)
  (flet ((fo () (forward-object scan)))
    (let ((object (object-after scan)))
      (cond ((eql object #\*)
	     (fo)
	     (cond ((or (end-of-buffer-p scan)
			(not (eql (object-after scan) #\/)))
		    (make-instance 'delimiter-lexeme))
		   (t (fo) (make-instance 'comment-end-lexeme))))
	    ;; TODO optionalise nesting
	    ((eql object #\/)
	     (fo)
	     (cond ((or (end-of-buffer-p scan)
			(not (eql (object-after scan) #\*)))
		    (make-instance 'delimiter-lexeme))
		   (t (fo) (make-instance 'long-comment-start-lexeme))))
	    ((c-constituentp object)
	     (loop until (or (end-of-buffer-p scan)
			     (not (c-constituentp (object-after scan))))
		   do (fo))
	     (make-instance 'word-lexeme))
	    (t (fo) (make-instance 'delimiter-lexeme))))))

(defmethod skip-inter ((syntax c-syntax) (state lexer-line-comment-state) scan)
  (macrolet ((fo () `(forward-object scan)))
    (loop until (or (end-of-line-p scan)
		    (not (whitespacep syntax (object-after scan))))
	  do (fo)
	  finally (return t))))

(defmethod lex ((syntax c-syntax) (state lexer-line-comment-state) scan)
  (macrolet ((fo () `(forward-object scan)))
    (cond ((end-of-line-p scan)
	   (make-instance 'comment-end-lexeme))
	  ((c-constituentp (object-after scan))
	   (loop until (or (end-of-buffer-p scan)
			   (not (c-constituentp (object-after scan))))
		 do (fo))
	   (make-instance 'word-lexeme))
	  (t (fo) (make-instance 'delimiter-lexeme)))))

(defun eat-pp-number (scan)
  (let ((dots-seen 0))
    (macrolet ((fo () `(forward-object scan)))
      (when (eql (object-after scan) #\.)
	(fo)
	(incf dots-seen))
      (loop until (end-of-buffer-p scan)
	    for next = (object-after scan)
	    while (or (digit-char-p next)
		      (alpha-or-underscore-p next)
		      (eql next #\.))
	    when (eql next #\.)
	      do (incf dots-seen)
	    when (member next '(#\E #\P) :test #'equalp)
	      do (fo)
		 (unless (end-of-buffer-p scan)
		   (when (member (object-after scan) '(#\+ #\-))
		     (fo)))
	    do (fo)
	    finally (return dots-seen)))))

(defun lex-token (syntax scan)
  (declare (ignore syntax))
  (labels ((fo () (forward-object scan)))
    (cond ((alpha-or-underscore-p (object-after scan))
	   (let ((token (make-array 32 :element-type 'character
				    :adjustable t :fill-pointer 0)))
	     (loop until (or (end-of-buffer-p scan)
			     (not (or (alphanumericp (object-after scan))
				      (eql (object-after scan) #\_))))
		   do (vector-push-extend (object-after scan) token)
		      (fo)) 
	     (if (find token *keyword-spellings* :test #'string=)
		 (make-instance (spelling-to-symbol token))
		 (make-instance 'identifier-lexeme))))
	  (t
	   ;; TODO: real numbers
	   (if (zerop (eat-pp-number scan))
	       (make-instance 'integer-constant-lexeme)
	       (make-instance 'floating-constant-lexeme))))))

(defmethod lex ((syntax c-syntax) (state lexer-error-state) scan)
  (macrolet ((fo () `(forward-object scan)))
    (loop until (end-of-line-p scan)
	  do (fo))
    (make-instance 'error-lexeme)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; parser

(defmacro define-c-action ((state lexeme) &body body)
  `(defmethod action ((syntax c-syntax) (state ,state) (lexeme ,lexeme))
     ,@body))

(defmacro define-new-c-state ((state parser-symbol) &body body)
  `(defmethod new-state ((syntax c-syntax) (state ,state) (tree ,parser-symbol))
     ,@body))

(define-c-action (error-reduce-state (eql nil))
  (throw 'done nil))

;;; the default action for any lexeme is shift
(define-c-action (t c-lexeme)
  lexeme)

;;; the action on end-of-buffer is to reduce to the error symbol
(define-c-action (t (eql nil))
  (reduce-all error-symbol))

;;; the default new state is the error state
(define-new-c-state (t parser-symbol) error-state)

;;; the new state when an error-state
(define-new-c-state (t error-symbol) error-reduce-state)

;;;;;;;;;;;;;;;; Top-level

#| rules
   form* ->
   form* -> form* form
|#

;;; parse trees
(defclass form* (c-nonterminal) ())

(define-parser-state |form* | (lexer-toplevel-state parser-state) ())
(define-parser-state form-may-follow (lexer-toplevel-state parser-state) ())
(define-parser-state |initial-state | (form-may-follow) ())

(define-new-c-state (|initial-state | form) |initial-state |)
(define-new-c-state (|initial-state | comment) |initial-state |)

(define-c-action (|initial-state | (eql nil))
  (reduce-all form*))

(define-new-c-state (|initial-state | form*) |form* | )

(define-c-action (|form* | (eql nil))
  (throw 'done nil))

;;;;;;;;;;;;;;;; String

;;; parse trees
(defclass string-form (form) ())
(defclass complete-string-form (string-form complete-form-mixin) ())
(defclass incomplete-string-form (string-form incomplete-form-mixin) ())
(defclass wide-string-form (string-form) ())
(defclass complete-wide-string-form (wide-string-form complete-string-form) ())
(defclass incomplete-wide-string-form (wide-string-form incomplete-string-form) ())

(define-parser-state |" word* | (lexer-string-state parser-state) ())
(define-parser-state |L" word* | (lexer-string-state parser-state) ())
(define-parser-state |" word* " | (lexer-toplevel-state parser-state) ())
(define-parser-state |L" word* " | (lexer-toplevel-state parser-state) ())

(define-new-c-state (|" word* | word-lexeme) |" word* |)
(define-new-c-state (|L" word* | word-lexeme) |L" word* |)
(define-new-c-state (|" word* | delimiter-lexeme) |" word* |)
(define-new-c-state (|L" word* | delimiter-lexeme) |L" word* |)
(define-new-c-state (form-may-follow string-start-lexeme) |" word* |)
(define-new-c-state (form-may-follow wide-string-start-lexeme) |L" word* |)
(define-new-c-state (|" word* | string-end-lexeme) |" word* " |)
(define-new-c-state (|L" word* | string-end-lexeme) |L" word* " |)

;;; reduce according to the rule form -> " word* "
(define-c-action (|" word* " | t)
  (reduce-until-type complete-string-form string-start-lexeme))
(define-c-action (|L" word* " | t)
  (reduce-until-type complete-wide-string-form wide-string-start-lexeme))

;;; reduce at the end of the buffer
(define-c-action (|" word* | (eql nil))
  (reduce-until-type incomplete-string-form string-start-lexeme))
(define-c-action (|L" word* | (eql nil))
  (reduce-until-type incomplete-wide-string-form wide-string-start-lexeme))

;;;;;;;;;;;;;;;; Character

;;; parse trees
(defclass character-form (form) ())
(defclass complete-character-form (character-form complete-form-mixin) ())
(defclass incomplete-character-form (character-form incomplete-form-mixin) ())
(defclass wide-character-form (character-form) ())
(defclass complete-wide-character-form (wide-character-form complete-character-form) ())
(defclass incomplete-wide-character-form (wide-character-form incomplete-character-form) ())

(define-parser-state |' word* | (lexer-character-state parser-state) ())
(define-parser-state |L' word* | (lexer-character-state parser-state) ())
(define-parser-state |' word* ' | (lexer-toplevel-state parser-state) ())
(define-parser-state |L' word* ' | (lexer-toplevel-state parser-state) ())

(define-new-c-state (|' word* | word-lexeme) |' word* |)
(define-new-c-state (|L' word* | word-lexeme) |L' word* |)
(define-new-c-state (|' word* | delimiter-lexeme) |' word* |)
(define-new-c-state (|L' word* | delimiter-lexeme) |L' word* |)
(define-new-c-state (form-may-follow character-start-lexeme) |' word* |)
(define-new-c-state (form-may-follow wide-character-start-lexeme) |L' word* |)
(define-new-c-state (|' word* | character-end-lexeme) |' word* ' |)
(define-new-c-state (|L' word* | character-end-lexeme) |L' word* ' |)

;;; reduce according to the rule form -> ' word* '
(define-c-action (|' word* ' | t)
  (reduce-until-type complete-character-form character-start-lexeme))
(define-c-action (|L' word* ' | t)
  (reduce-until-type complete-wide-character-form wide-character-start-lexeme))

;;; reduce at the end of the buffer
(define-c-action (|' word* | (eql nil))
  (reduce-until-type incomplete-character-form character-start-lexeme))
(define-c-action (|L' word* | (eql nil))
  (reduce-until-type incomplete-wide-character-form wide-character-start-lexeme))

;;;;;;;;;;;;;;;; Preprocessor directive

;;; parse trees
(defclass preprocessor-directive-form (form) ())

(define-parser-state |# word* | (lexer-preprocessor-state parser-state) ())
(define-parser-state |# word* NL | (lexer-toplevel-state parser-state) ())
(define-parser-state |# word* eNL | (lexer-escaped-preprocessor-state parser-state) ())

(define-new-c-state (form-may-follow preprocessor-start-lexeme) |# word* |)
(define-new-c-state (|# word* | word-lexeme) |# word* |)
(define-new-c-state (|# word* | delimiter-lexeme) |# word* |)
(define-new-c-state (|# word* | hash-lexeme) |# word* |)
(define-new-c-state (|# word* | hash-hash-lexeme) |# word* |)
(define-new-c-state (|# word* | escaped-newline-lexeme) |# word* eNL |)
(define-new-c-state (|# word* eNL | word-lexeme) |# word* |)
(define-new-c-state (|# word* eNL | delimiter-lexeme) |# word* |)
(define-new-c-state (|# word* eNL | hash-lexeme) |# word* |)
(define-new-c-state (|# word* eNL | hash-hash-lexeme) |# word* |)
(define-new-c-state (|# word* eNL | escaped-newline-lexeme) |# word* eNL |)
(define-new-c-state (|# word* | preprocessor-end-lexeme) |# word* NL |)
(define-new-c-state (|# word* eNL | preprocessor-end-lexeme) |# word* NL |)

;;; reduce according to the rule form -> # word* NL
(define-c-action (|# word* NL | t)
  (reduce-until-type preprocessor-directive-form preprocessor-start-lexeme))

;;;;;;;;;;;;;;;; Line comment

;;; parse trees
(defclass line-comment-form (comment) ())

(define-parser-state |// word* | (lexer-line-comment-state parser-state) ())
(define-parser-state |// word* NL | (lexer-toplevel-state parser-state) ())

(define-new-c-state (form-may-follow line-comment-start-lexeme) |// word* |)
(define-new-c-state (|// word* | word-lexeme) |// word* |)
(define-new-c-state (|// word* | delimiter-lexeme) |// word* |)
(define-new-c-state (|// word* | comment-end-lexeme) |// word* NL |)

;;; reduce according to the rule form -> // word* NL
(define-c-action (|// word* NL | t)
  (reduce-until-type line-comment-form line-comment-start-lexeme))

;;;;;;;;;;;;;;;; Long comment

;;; parse trees
(defclass long-comment-form (comment) ())
(defclass complete-long-comment-form (long-comment-form complete-form-mixin) ())
(defclass incomplete-long-comment-form (long-comment-form incomplete-form-mixin) ())

(define-parser-state |/* word* | (lexer-long-comment-state parser-state) ())
(define-parser-state |/* word* */ | (lexer-toplevel-state parser-state) ())

(define-new-c-state (|/* word* | word-lexeme) |/* word* |)
(define-new-c-state (|/* word* | delimiter-lexeme) |/* word* |)
(define-new-c-state (|/* word* | long-comment-start-lexeme) |/* word* |)
(define-new-c-state (|/* word* | long-comment-form) |/* word* |)
(define-new-c-state (form-may-follow long-comment-start-lexeme) |/* word* |)
(define-new-c-state (|/* word* | comment-end-lexeme) |/* word* */ |)

;;; reduce according to the rule form -> /* word* */
(define-c-action (|/* word* */ | t)
  (reduce-until-type complete-long-comment-form long-comment-start-lexeme))

;;; reduce at the end of the buffer
(define-c-action (|/* word* | (eql nil))
  (reduce-until-type incomplete-long-comment-form long-comment-start-lexeme))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; display

(defun form-string (syntax form)
  "Return the string that correspond to `form' in the buffer of
`syntax'."
  (buffer-substring (buffer syntax) (start-offset form) (end-offset form)))

(define-syntax-highlighting-rules default-c-highlighting
  (error-symbol (*error-drawing-options*))
  (string-form (*string-drawing-options*))
  (operator (*special-operator-drawing-options*))
  (type-specifier (*keyword-drawing-options*))
  (type-qualifier (*keyword-drawing-options*))
  (storage-class-specifier (:face :ink +dark-green+))
  (function-specifier (:face :ink +dark-green+))
  (comment (*comment-drawing-options*))
  (integer-constant-lexeme (:face :ink +gray50+))
  (floating-constant-lexeme (:face :ink +gray50+)))

(defparameter *syntax-highlighting-rules* 'default-c-highlighting
  "The syntax highlighting rules used for highlighting C
syntax.")

(defmethod syntax-highlighting-rules ((syntax c-syntax))
  *syntax-highlighting-rules*)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; exploit the parse

(defun form-string-p (form)
  (typep form 'string-form))

(defun commentp (form)
  (typep form 'comment))

(defun top-level-vector (syntax)
  (coerce (children (slot-value syntax 'stack-top)) 'simple-vector))

(defun top-level-form-before-in-vector (tlv 
					offset 
					&optional ignore-comments-p)
  "Return top-level form in top-level-vector `tlv' around or before `offset'
together with index of form in `tlv', or nil. If `ignore-comments-p', don't 
treat comments as forms."
  (loop for count from (1- (length tlv)) downto 0
	for tlf = (aref tlv count)
	when (and (or (not ignore-comments-p) (not (commentp tlf)))
		  (< (start-offset tlf) offset (end-offset tlf)))
	  return (values tlf count)
	when (and (or (not ignore-comments-p) (not (commentp tlf)))
		  (<= (end-offset tlf) offset))
	  return (values tlf count)
	finally (return nil)))

(defun top-level-form-after-in-vector (tlv 
				       offset
				       &optional ignore-comments-p)
  "Return top-level form in top-level-vector `tlv' around or after `offset'
together with index of form in `tlv', or nil. If `ignore-comments-p', don't
treat comments as forms."
  (loop for tlf across tlv
	for count from 0
	when (and (or (not ignore-comments-p) (not (commentp tlf)))
		  (< (start-offset tlf) offset (end-offset tlf)))
	  return (values tlf count)
	when (and (or (not ignore-comments-p) (not (commentp tlf)))
		  (>= (start-offset tlf) offset))
	  return (values tlf count)
	finally (return nil)))

(defun top-level-form-around-in-vector (tlv 
					offset
					&optional ignore-comments-p)
  "Return top-level form in top-level-vector `tlv' around `offset'
together with index of form in `tlv', or nil. If `ignore-comments-p', don't
treat comments as forms."
  (loop for tlf across tlv
	for count from 0
	when (and (or (not ignore-comments-p) (not (commentp tlf)))
		  (< (start-offset tlf) offset (end-offset tlf)))
	  return (values tlf count)
	when (and (or (not ignore-comments-p) (not (commentp tlf)))
		  (>= (start-offset tlf) offset))
	  return nil
	finally (return nil)))

(defun form-around (syntax offset &optional ignore-comments-p)
  (top-level-form-around-in-vector
   (top-level-vector syntax)
   offset
   ignore-comments-p))

(defgeneric opening-delimiter-p (token)
  (:documentation "Is `token' an opening delimiter."))

(defmethod opening-delimiter-p (token)
  nil)

(defmethod opening-delimiter-p ((token opening-delimiter-mixin))
  t)

(defgeneric closing-delimiter-p (token)
  (:documentation "Is `token' a closing delimiter."))

(defmethod closing-delimiter-p (token)
  nil)

(defmethod closing-delimiter-p ((token closing-delimiter-mixin))
  t)

(defgeneric matching-delimiter-p (token match)
  (:documentation "Is `match' a matching delimiter of `token'."))

(defmethod matching-delimiter-p (token match)
  nil)

(defmethod matching-delimiter-p ((token closing-delimiter-mixin)
				 (match opening-delimiter-mixin))
  (matching-delimiter-p match token))

(defmethod matching-delimiter-p ((token left-parenthesis-lexeme)
				(match right-parenthesis-lexeme))
  t)

(defmethod matching-delimiter-p ((token left-bracket-lexeme)
				 (match right-bracket-lexeme))
  t)

(defmethod matching-delimiter-p ((token left-brace-lexeme)
				 (match right-brace-lexeme))
  t)

(defmethod backward-one-expression (mark (syntax c-syntax))
  (let ((tlv (top-level-vector syntax)))
    (multiple-value-bind (form count)
	(top-level-form-before-in-vector tlv (offset mark) t)
      (when form
	(if (closing-delimiter-p form)
	    (loop for index from count downto 0
		  for match = (aref tlv index)
		  with delims = 0
		  when (eql (class-of match)
			    (class-of form))
		    do (incf delims)
		  when (matching-delimiter-p form match)
		    do (decf delims)
		  until (zerop delims)
		  finally (cond ((zerop delims)
				 (setf (offset mark) (start-offset match))
				 (return t))
				(t (return nil))))
	    (setf (offset mark) (start-offset form)))))))

(defmethod forward-one-expression (mark (syntax c-syntax))
  (let ((tlv (top-level-vector syntax)))
    (multiple-value-bind (form count)
	(top-level-form-after-in-vector tlv (offset mark) t)
      (when form
	(if (opening-delimiter-p form)
	    (loop for index from count below (length tlv)
		  for match = (aref tlv index)
		  with delims = 0
		  when (eql (class-of match)
			    (class-of form))
		    do (incf delims)
		  when (matching-delimiter-p form match)
		    do (decf delims)
		  until (zerop delims)
		  finally (cond ((zerop delims)
				 (setf (offset mark) (end-offset match))
				 (return t))
				(t (return nil))))
	    (setf (offset mark) (end-offset form)))))))

(defmethod forward-one-list ((mark mark) (syntax c-syntax))
  (let ((tlv (top-level-vector syntax)))
    (multiple-value-bind (form count)
	(top-level-form-after-in-vector tlv (offset mark))
      (when form
	(loop for index from count below (length tlv)
	      for match = (aref tlv index)
	      with delims = ()
	      when (opening-delimiter-p match)
		do (push match delims)
	      when (closing-delimiter-p match)
		do (cond ((null delims)
			  (return nil))
			 (t (cond ((matching-delimiter-p match 
							 (car delims))
				   (pop delims)
				   (when (null delims)
				     (setf (offset mark) (end-offset match))
				     (return t)))
				  (t (return nil)))))
	      finally (return nil))))))

(defmethod backward-one-list ((mark mark) (syntax c-syntax))
  (let ((tlv (top-level-vector syntax)))
    (multiple-value-bind (form count)
	(top-level-form-before-in-vector tlv (offset mark))
      (when form
	(loop for index from count downto 0
	      for match = (aref tlv index)
	      with delims = ()
	      when (closing-delimiter-p match)
		do (push match delims)
	      when (opening-delimiter-p match)
		do (cond 
		     ((null delims)
		      (return nil))
		     (t (cond ((matching-delimiter-p match 
						     (car delims))
			       (pop delims)
			       (when (null delims)
				 (setf (offset mark) (start-offset match))
				 (return t)))
			      (t (return nil))))) 
	      finally (return nil))))))

(drei-motion:define-motion-fns list)

(defmethod backward-one-down ((mark mark) (syntax c-syntax))
  (let ((tlv (top-level-vector syntax)))
    (multiple-value-bind (form count)
	(top-level-form-before-in-vector tlv (offset mark))
      (when form
	(loop for index from count downto 0
	      for match = (aref tlv index)
	      when (closing-delimiter-p match)
		do (setf (offset mark) (start-offset match))
		   (return t)
	      finally (return nil))))))

(defmethod backward-one-up (mark (syntax c-syntax))
  (let ((tlv (top-level-vector syntax)))
    (multiple-value-bind (form count)
	(top-level-form-before-in-vector tlv (offset mark))
      (when form
	(loop for index from count downto 0
	      for match = (aref tlv index)
	      with delims = ()
	      when (closing-delimiter-p match)
		do (push match delims)
	      when (opening-delimiter-p match)
		do (cond ((null delims)
			  (setf (offset mark) (start-offset match))
			  (return t))
			 ((matching-delimiter-p match 
						(car delims))
			  (pop delims))
			 (t (return nil)))
	      finally (return nil))))))

(defmethod forward-one-down ((mark mark) (syntax c-syntax))
  (let ((tlv (top-level-vector syntax)))
    (multiple-value-bind (form count)
	(top-level-form-after-in-vector tlv (offset mark))
      (when form
	(loop for index from count below (length tlv)
	      for match = (aref tlv index)
	      when (opening-delimiter-p match)
		do (setf (offset mark) (end-offset match))
		   (return t)
	      finally (return nil))))))

(defmethod forward-one-up (mark (syntax c-syntax))
  (let ((tlv (top-level-vector syntax)))
    (multiple-value-bind (form count)
	(top-level-form-after-in-vector tlv (offset mark))
      (when form
	(loop for index from count below (length tlv)
	      for match = (aref tlv index)
	      with delims = ()
	      when (opening-delimiter-p match)
		do (push match delims)
	      when (closing-delimiter-p match)
		do (cond ((null delims)
			  (setf (offset mark) (end-offset match))
			  (return t))
			 ((matching-delimiter-p match 
						(car delims))
			  (pop delims))
			 (t (return nil)))
	      finally (return nil))))))

;; (defmethod backward-one-definition ((mark mark) (syntax c-syntax))
;;   )

;; (defmethod forward-one-definition ((mark mark) (syntax c-syntax))
;;   )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; indentation

(defun real-column-number (mark tab-width)
  (let ((mark2 (clone-mark mark)))
    (beginning-of-line mark2)
    (loop with column = 0
	  until (mark= mark mark2)
	  do (if (eql (object-after mark2) #\Tab)
		 (loop do (incf column)
		       until (zerop (mod column tab-width)))
		 (incf column))
	  do (incf (offset mark2))
          finally (return column))))

(defmethod syntax-line-indentation (mark tab-width (syntax c-syntax))
  (setf mark (clone-mark mark))
  (let ((this-indentation (line-indentation mark tab-width)))
    (beginning-of-line mark)
    (loop until (beginning-of-buffer-p mark)
	  do (previous-line mark 0)
	  when (line-indentation mark tab-width)
	    return it
	  finally (return this-indentation))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Commenting

(defmethod syntax-line-comment-string ((syntax c-syntax))
  "// ")

(defmethod comment-region ((syntax c-syntax) mark1 mark2)
  (line-comment-region syntax mark1 mark2))

(defmethod uncomment-region ((syntax c-syntax) mark1 mark2)
  (line-uncomment-region syntax mark1 mark2))

;;;;;;;;;;;

;;; TESTING

(defun collect-forms (top)
  (loop for child in (children top)
	collect (collect-forms child)
	  into things
	finally (return (cons top things))))

(define-command (com-dump-forms :name t :command-table c-table)
    ()
  "Dump the parse trees to trace output."
  (let* ((buffer (current-buffer))
	 (syntax (syntax buffer)))
    (pprint (collect-forms (slot-value syntax 'stack-top)) *trace-output*)
    (terpri *trace-output*)
    (finish-output *trace-output*)))

(set-key 'com-dump-forms
	 'c-table
	 '((#\c :control) (#\c :control)))

(defun toplevel-forms (syntax)
  (children (slot-value syntax 'stack-top)))

(define-command (com-dump-preprocessor :name t :command-table c-table)
    ()
  "Dump the toplevel parse trees to trace output."
  (let* ((buffer (current-buffer))
	 (syntax (syntax buffer))
	 (pp-forms (remove-if-not 
			  (lambda (form)
			    (typep form 'preprocessor-directive-form))
			  (toplevel-forms syntax)))
	 (pp-types (mapcar (lambda (form)
			     (form-string syntax (second (children form))))
			   pp-forms)))
    
    (pprint pp-types *trace-output*)
    (terpri *trace-output*)
    (finish-output *trace-output*)))

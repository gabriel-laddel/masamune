;;;; lex-javascript.lisp
;;;
;;; Contains the definition of the Javascript lexer used by the parser,
;;; as well as some lookup structures for dealing with tokens.
;;; Unit tests are in tests/test-lexer.lisp.
;;;
;;; Copyright (c) 2005 James Wright
;;; See LICENSE for full licensing details.
;;;
(in-package :js-parser)

;;;; Token definitions
(defmacro deftoken (symbol &optional key token-type)
  "Add a token's symbol and possibly string to the appropriate lookups.

   Different actions will be taken depending upon the TOKEN-TYPE:
   OPERATOR-TOKENs are infix operators, which are recognized as atomic
   tokens regardless of whether they are surrounded by whitespace (unlike
   identifier tokens, for example).
   KEYWORDs are reserved strings that can never be used as an identifier.
   OPERATIONs will never be returned by the lexer, but they are used in the
   source model in the same place as tokens (ie, in :op-symbol slots), so
   they are treated as tokens for now."
  (cond
    ((eq token-type :operator-token)
     `(progn
       (setf (gethash ,key *tokens-to-symbols*) ,symbol)
       (setf (gethash ,symbol *symbols-to-tokens*) ,key)
       (push ,key *operator-tokens*)))
    ((eq token-type :operation)
     `(setf (gethash ,symbol *symbols-to-tokens*) ,key))
    ((eq token-type :keyword)
     `(progn
       (setf (gethash ,key *tokens-to-symbols*) ,symbol)
       (push ,symbol *keyword-symbols*)))
    (key
     `(setf (gethash ,key *tokens-to-symbols*) ,symbol))
    (t
     `(setf (gethash ,symbol *tokens-to-symbols*) ,symbol))))

;;;; Token lookups

(defparameter *tokens-to-symbols* (make-hash-table :test 'equal)
  "Map from string to token symbol.
   Every symbol that the tokenizer will return is in this map.")

(defparameter *symbols-to-tokens* (make-hash-table :test 'eq)
  "Map from token symbol to token string.  This contains an entry for every token
   in *tokens-to-symbols*, plus additional entries for the 'operation' symbols.")

(defparameter *operator-tokens* nil
  "These are operators (as distinct from general tokens) that will be built into
   a special regular expression.

   We can't just use the keys from *symbols-to-tokens*, because the
   order that the tokens appear in this list is significant.
   Specifically, each 'long' operator must occur before any operator that
   it is a supersequence of.  Eg, '<<<' must occur before '<<', which
   must occur before '<'.  '!=' must occur before both '!' and '='.")

(defparameter *keyword-symbols* nil
  "A list of the keyword symbols.")

(defparameter *restricted-tokens* (make-hash-table :test 'eq)
  "Tokens that participate in 'restricted productions'.  Value should be either
   :PRE or :POST.  For each of these tokens, the lexer will emit either a
   :NO-LINE-TERMINATOR or a :LINE-TERMINATOR token depending upon whether the token
   was preceded/followed by a line-break.")

(setf (gethash :plus2 *restricted-tokens*) :pre)
(setf (gethash :minus2 *restricted-tokens*) :pre)
(setf (gethash :break *restricted-tokens*) :post)
(setf (gethash :continue *restricted-tokens*) :post)
(setf (gethash :return *restricted-tokens*) :post)
(setf (gethash :throw *restricted-tokens*) :post)

;; Compound assignment operators
(deftoken :times-equals   "*=" :operator-token)
(deftoken :divide-equals  "/=" :operator-token)
(deftoken :mod-equals     "%=" :operator-token)
(deftoken :plus-equals    "+=" :operator-token)
(deftoken :minus-equals   "-=" :operator-token)
(deftoken :lshift-equals  "<<=" :operator-token)
(deftoken :rshift-equals  ">>=" :operator-token)
(deftoken :urshift-equals ">>>=" :operator-token)
(deftoken :and-equals     "&=" :operator-token)
(deftoken :xor-equals     "^=" :operator-token)
(deftoken :or-equals      "|=" :operator-token)

;; Operators and punctuators
(deftoken :semicolon    ";" :operator-token)
(deftoken :comma        "," :operator-token)
(deftoken :hook         "?" :operator-token)
(deftoken :colon        ":" :operator-token)
(deftoken :conditional)
(deftoken :bar2         "||" :operator-token)
(deftoken :ampersand2   "&&" :operator-token)
(deftoken :bar          "|" :operator-token)
(deftoken :caret        "^" :operator-token)
(deftoken :ampersand    "&" :operator-token)
(deftoken :equals3      "===" :operator-token)
(deftoken :not-equals2  "!==" :operator-token)
(deftoken :equals2      "==" :operator-token)
(deftoken :equals       "=" :operator-token)
(deftoken :not-equals   "!=" :operator-token)
(deftoken :left-arrow   "<-" :operator-token)
(deftoken :right-arrow  "->" :operator-token)
(deftoken :less-than-equals "<=" :operator-token)
(deftoken :greater-than-equals ">=" :operator-token)
(deftoken :urshift      ">>>" :operator-token)
(deftoken :lshift       "<<" :operator-token)
(deftoken :rshift       ">>" :operator-token)
(deftoken :less-than    "<" :operator-token)
(deftoken :greater-than ">" :operator-token)

(deftoken :asterisk     "*" :operator-token)
(deftoken :slash        "/" :operator-token)
(deftoken :percent      "%" :operator-token)
(deftoken :bang         "!" :operator-token)
(deftoken :tilde        "~" :operator-token)
(deftoken :plus2        "++" :operator-token)
(deftoken :minus2       "--" :operator-token)
(deftoken :plus         "+" :operator-token)
(deftoken :minus        "-" :operator-token)
(deftoken :dot          "." :operator-token)
(deftoken :left-bracket "[" :operator-token)
(deftoken :right-bracket "]" :operator-token)
(deftoken :left-curly   "{" :operator-token)
(deftoken :right-curly  "}" :operator-token)
(deftoken :left-paren   "(" :operator-token)
(deftoken :right-paren  ")" :operator-token)

;; These represent operations (addition, etc.) rather than just text tokens.
;; The lexer will never output them, so possibly the whole *symbols-to-tokens*
;; setup wants to be in js-source-model instead of here.
(deftoken :assign       "=" :operation)
(deftoken :unary-plus   "+" :operation)
(deftoken :unary-minus  "-" :operation)
(deftoken :pre-decr     "--" :operation)
(deftoken :pre-incr     "++" :operation)
(deftoken :post-decr    "--" :operation)
(deftoken :post-incr    "++" :operation)
(deftoken :logical-and  "&&" :operation)
(deftoken :logical-or   "||" :operation)
(deftoken :logical-not  "!" :operation)
(deftoken :bitwise-and  "&" :operation)
(deftoken :bitwise-or   "|" :operation)
(deftoken :bitwise-not  "~" :operation)
(deftoken :bitwise-xor  "^" :operation)
(deftoken :equals       "==" :operation)
(deftoken :strict-equals "===" :operation)
(deftoken :strict-not-equals "!==" :operation)
(deftoken :add          "+" :operation)
(deftoken :subtract     "-" :operation)
(deftoken :multiply     "*" :operation)
(deftoken :divide       "/" :operation)
(deftoken :modulo       "%" :operation)


;; Keywords
(deftoken :break        "break" :keyword)
(deftoken :case         "case"  :keyword)
(deftoken :catch        "catch" :keyword)
(deftoken :const        "const" :keyword) ;???
(deftoken :continue     "continue" :keyword)
(deftoken :debugger     "debugger" :keyword) ;???
(deftoken :default      "default"  :keyword)
(deftoken :delete       "delete" :keyword)
(deftoken :do           "do"     :keyword)
(deftoken :else         "else"   :keyword)
(deftoken :enum         "enum"   :keyword) ;???
(deftoken :false        "false"  :keyword)
(deftoken :finally      "finally" :keyword)
(deftoken :for          "for"    :keyword)
(deftoken :function     "function" :keyword)
;(deftoken :function_continuation "function_continuation" :keyword) ; jwacs-only syntax
(deftoken :if           "if"     :keyword)
;(deftoken :import      "import"  :keyword) ; jwacs-only syntax
(deftoken :in           "in"     :keyword)
(deftoken :instanceof   "instanceof" :keyword) ;???
(deftoken :new          "new"    :keyword)
(deftoken :null         "null"   :keyword)
;(deftoken :resume       "resume" :keyword) ; jwacs-only syntax
(deftoken :return       "return" :keyword)
;(deftoken :suspend      "suspend" :keywork) ; jwacs-only syntax
(deftoken :switch       "switch" :keyword)
(deftoken :this         "this"   :keyword)
(deftoken :throw        "throw"  :keyword)
(deftoken :true         "true"   :keyword)
(deftoken :try          "try"    :keyword)
(deftoken :typeof       "typeof" :keyword)
(deftoken :var          "var"    :keyword)
(deftoken :void         "void"   :keyword) ;???
(deftoken :while        "while"  :keyword)
(deftoken :with         "with"   :keyword)

;; Other terminal types
(deftoken :number)
(deftoken :identifier)
(deftoken :re-literal)
(deftoken :string-literal)
(deftoken :inserted-semicolon)
(deftoken :line-terminator)
(deftoken :no-line-terminator)

;;;; Regular expressions
(defparameter floating-re (create-scanner
                            '(:sequence
                              :start-anchor
                              (:alternation
                                (:sequence
                                  (:greedy-repetition 1 nil :digit-class)
                                  #\.
                                  (:greedy-repetition 0 nil :digit-class)
                                  (:greedy-repetition 0 1
                                    (:sequence (:alternation #\e #\E)
                                               (:greedy-repetition 0 1 (:alternation #\+ #\-))
                                               (:greedy-repetition 1 nil :digit-class))))
                                (:sequence
                                  #\.
                                  (:greedy-repetition 1 nil :digit-class)
                                  (:greedy-repetition 0 1
                                    (:sequence (:alternation #\e #\E)
                                               (:greedy-repetition 0 1 (:alternation #\+ #\-))
                                               (:greedy-repetition 1 nil :digit-class)))))))

  "Regular expression for recognizing floating-point literals")

(defparameter string-re (create-scanner 
                          '(:sequence
                            :start-anchor 
                            (:alternation
                              (:sequence 
                                #\"
                                (:greedy-repetition 0 nil
                                  (:alternation
                                    (:sequence #\\ :everything)
                                    (:inverted-char-class #\")))
                                #\")
                              (:sequence 
                                #\'
                                (:greedy-repetition 0 nil
                                  (:alternation
                                    (:sequence #\\ :everything)
                                    (:inverted-char-class #\')))
                                #\'))))

  "Regular expression for recognizing string literals")


(define-parse-tree-synonym non-terminator
    (:inverted-char-class #\Newline #\Return))

(defparameter regexp-re (create-scanner
                         '(:sequence
                           :start-anchor
                           #\/
                           (:register
                            (:sequence

                             ;; First char
                             (:alternation
                              (:sequence #\\ non-terminator)
                              (:inverted-char-class #\* #\\ #\/ #\Newline #\Return))

                             ;; Subsequent chars
                             (:greedy-repetition 0 nil
                               (:alternation
                                (:sequence #\\ non-terminator)
                                (:inverted-char-class #\\ #\/ #\Newline #\Return)))))
                           #\/
                           (:register
                            (:greedy-repetition 0 nil
                             (:char-class #\g #\i)))))

  "(Lisp) regular expression for recognizing (Javascript) regular expression literals")

(defparameter operator-re (create-scanner 
                           (list :sequence 
                                 :start-anchor 
                                 (cons :alternation
                                       (reverse *operator-tokens*))))

  "Regular expression for recognizing operators")

(defparameter whitespace-and-comments-re (create-scanner
                                          '(:sequence
                                            :start-anchor
                                            (:greedy-repetition 1 nil
                                             (:alternation
					      ;; whitespace
                                              (:greedy-repetition 1 nil
                                               :whitespace-char-class)
					      ;; single-line comment
                                              (:sequence
                                               "//"
                                               (:greedy-repetition 0 nil
                                                (:inverted-char-class #\Newline))
                                               (:alternation #\Newline :end-anchor))
                                              (:sequence
                                               "/*"
                                               (:greedy-repetition 0 nil
                                                (:branch (:positive-lookahead "*/")
                                                         (:alternation :void
                                                                       (:alternation :everything
                                                                                     :whitespace-char-class))))
                                                "*/")))))
  "Regular expression for consuming (and thereby skipping) whitespace and comments")

(defparameter integer-re (create-scanner
                          '(:sequence
                            :start-anchor
                            (:alternation
                             (:sequence "0x" (:greedy-repetition 1 nil 
                                              (:char-class 
                                               (:range #\a #\f) 
                                               (:range #\A #\F) 
                                               :digit-class)))
                             (:sequence "0" (:greedy-repetition 1 nil (:char-class (:range #\0 #\7))))
                             (:greedy-repetition 1 nil :digit-class))))

  "Regular expression for recognizing integer literals")
  
;;;; Data structures
(defstruct token
  "Represents a token returned by the lexer"
  (terminal nil :type symbol)
  (value   nil :type (or number string (cons string string) null))
  (start   nil :type (or number null))
  (end     nil :type (or number null)))

#+(or sbcl cmu)
(defmethod make-load-form ((self token) &optional environment)
  (make-load-form-saving-slots self :environment environment))

;;;; Helper functions and macros
(defun parse-javascript-integer (integer-str &key (start 0) end)
  "Parse integer literals, taking account of 0x and 0 radix-specifiers"
  (cond
    ((and (> (- (length integer-str) start) 2)
          (eql #\0 (aref integer-str start))
          (eql #\x (aref integer-str (1+ start))))
     (parse-integer integer-str :start (+ start 2) :end end :radix 16 :junk-allowed nil))
    ((scan "^0[0-7]+" integer-str :start start)
     (parse-integer integer-str :start (1+ start) :end end :radix 8 :junk-allowed nil))
    (t
     (parse-integer integer-str :start start :end end :radix 10 :junk-allowed nil))))

(defun unescape-regexp (re-string)
  (regex-replace "\\\\/" re-string "/"))

(defun escape-regexp (re-string)
  (regex-replace "/" re-string "\\/"))

(defun line-terminator-p (c)
  "Return non-NIL if C is a line-terminator character according to the Javascript spec"
  (find (char-code c) '(#x0a #x0d #x2028 #x2029)))

;;;; Top-level logic

(defclass javascript-lexer ()
  ((cursor :initform 0 :accessor cursor)
   (text :initarg :text :reader text)
   (unget-stack :initform nil :accessor unget-stack)
   (encountered-line-terminator :initform nil :accessor encountered-line-terminator))
  (:documentation
   "Represents the current state of a lexing operation"))

(defun next-token (lexer)
  "Returns a token structure containing the next token in the lexing operation
   represented by LEXER.  This token will have a terminal of NIL at end of stream.
   The next token will be fetched from the unget-stack if that stack is non-empty."

  ;; If we've pushed other input, then return that instead
  (when-let (cell (pop (unget-stack lexer)))
    (setf (encountered-line-terminator lexer) (cdr cell))
    (return-from next-token (car cell)))

  ;; Skip the whitespace...
  (consume-whitespace lexer)

  ;; .. and grab a token from the text
  (let* ((token (consume-token lexer))
         (terminal (when token (token-terminal token))))

    ;; Restricted token handling
    (case (gethash terminal *restricted-tokens*)
      (:pre
       (cond
         ((encountered-line-terminator lexer)
          (push-token lexer token)
          (make-token :terminal :line-terminator :value ""
                      :start (encountered-line-terminator lexer)
                      :end (1+ (encountered-line-terminator lexer))))
         (t
          (push-token lexer token)
          (make-token :terminal :no-line-terminator :value ""
                      :start nil :end nil))))
      (:post
       (let ((saved-terminator (encountered-line-terminator lexer))
             (next-terminator (consume-whitespace lexer)))
         (if next-terminator
           (push-token lexer (make-token :terminal :line-terminator
                                         :value ""
                                         :start next-terminator :end (1+ next-terminator))
                       next-terminator)
           (push-token lexer (make-token :terminal :no-line-terminator
                                         :value "" :start nil :end nil)
                       nil))
         (setf (encountered-line-terminator lexer) saved-terminator)
         token))
      (otherwise
       token))))

(defun push-token (lexer token &optional (encountered-line-terminator (encountered-line-terminator lexer)))
  "Push a token onto the top of LEXER's unget stack.  The next token fetched by NEXT-TOKEN will be
   the pushed token.  The state of the ENCOUNTERED-LINE-TERMINATOR flag is also saved and will be
   restored by the next NEXT-TOKEN call."
  (push (cons token encountered-line-terminator)
        (unget-stack lexer)))

(defun set-cursor (lexer new-pos)
  "Sets the cursor of the lexer to the position specified by NEW-POS"
  (setf (cursor lexer) new-pos))

(defun coerce-token (lexer terminal)
  "Force LEXER to interpret the next token as TERMINAL.  An error will be raised
   if that's not actually possible."
  (let ((token-string (gethash terminal *symbols-to-tokens*)))

    ;; Only coerce constant tokens (ie, :SLASH good, :STRING-LITERAL bad)
    (unless (stringp token-string)
      (error "~S is not a coerceable terminal type" terminal))

    ;; Check that this is a feasible request
    (unless (string= (subseq (text lexer)
                             (cursor lexer)
                             (+ (cursor lexer) (length token-string)))
                     token-string)
      (error "Cannot interpret ~S as a ~S"
             (subseq (text lexer)
                     (cursor lexer)
                     (+ (cursor lexer) (length token-string)))
             terminal))

    ;; Okay, let's do this thing
    
    (push-token lexer (make-token :terminal terminal
                                  :value token-string
                                  :start (cursor lexer)
                                  :end (+ (cursor lexer) (length token-string))))
    (set-cursor lexer (+ (cursor lexer) (length token-string)))))
             

(defun consume-whitespace (lexer)
  "Consumes whitespace and comments.  Lexer's cursor is updated to the next
   non-whitespace character, and its ENCOUNTERED-LINE-TERMINATOR slot is set
   based upon whether or not the skipped whitespace included a newline.
   Returns non-NIL if line-terminators were encountered."
  (with-slots (cursor text encountered-line-terminator) lexer
    (multiple-value-bind (ws-s ws-e)
        (scan whitespace-and-comments-re text :start cursor)
      (setf encountered-line-terminator nil)
      (when ws-s
        (set-cursor lexer ws-e)
        (setf encountered-line-terminator
              (position-if 'line-terminator-p text :start ws-s :end ws-e))))))

(defun consume-token (lexer)
  "Reads the next token from LEXER's source text (where 'next' is determined by
   the value of LEXER's cursor).  The cursor is assumed to point to a non-whitespace
   character on entry; on exit points to the first character after the consumed token.
   Returns a token structure.  The token's terminal will be NIL on end of input"
  (with-slots (text cursor) lexer
    (re-cond (text :start cursor)
       ("^$"
        (make-token :terminal nil :start (length text) :end (length text)))
       (floating-re
        (set-cursor lexer %e)
        (make-token :terminal :number :start %s :end %e
                    :value (read-from-string text nil nil :start %s :end %e)))
       (integer-re
        (set-cursor lexer %e)
        (make-token :terminal :number :start %s :end %e
                    :value (parse-javascript-integer text :start %s :end %e)))
       ("^(\\$|\\w)+"
        (set-cursor lexer %e)
        (let* ((text (subseq text %s %e))
               (terminal (or (gethash text *tokens-to-symbols*)
                             :identifier)))
          (make-token :terminal terminal :start %s :end %e :value text)))
       (regexp-re
        (set-cursor lexer %e)
        (make-token :terminal :re-literal :start %s :end %e
                    :value (cons (unescape-regexp (subseq text (aref %sub-s 0) (aref %sub-e 0)))
                                 (subseq text (aref %sub-s 1) (aref %sub-e 1)))))
       (string-re
        (set-cursor lexer %e)
        (make-token :terminal :string-literal :start %s :end %e
                    :value (subseq text (1+ %s) (1- %e))))
       (operator-re
        (set-cursor lexer %e)
        (let* ((text (subseq text %s %e))
               (terminal (gethash text *tokens-to-symbols*)))
          (make-token :terminal terminal :start %s :end %e
                      :value text)))
       ("^\\S+"
        (error "unrecognized token: '~A'" (subseq text %s %e)))
       (t
        (error "coding error - we should never get here")))))

;; TODO Tab handling
(defun position-to-line/column (text index)
  "Returns a cons cell containing the 1-based row and column for the character
  at INDEX in TEXT."
  (let ((newline-count (count #\Newline text :end index))
        (final-newline (position #\Newline text :end index :from-end t)))
    (if final-newline
      (cons (1+ newline-count) (- index final-newline))
      (cons 1 (1+ index)))))

;;;; Interface function
(defun make-lexer-function (lexer)
  "Returns a function of 0 arguments that calls NEXT-TOKEN on LEXER whenever it is called.
   This is normally the function that gets passed into a parser."
  (lambda ()
    (let ((token (next-token lexer)))
      (when token
        (values (token-terminal token) token)))))

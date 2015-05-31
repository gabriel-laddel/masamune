;;; Comments 
;;; ============================================================================
;;; You've probably already grokked that ; denotes a comment. M-; will insert
;;; whatever level of commenting is appropriate for the situation in question.

#| 
Block comments
can span multiple lines

!!!
|#

t  ; is an atom denoting true

;;; In (common) lisp any value other than `nil' and the empty list () are
;;; considered to be truthy

(if t 1 2) ; => 1
(if 12 1 2) ; => 1
(if () 1 2) ; => 2
(if nil 1 2) ; => 2

;;; Primitive datatypes and operators 
;;; ============================================================================
;;; 
;;; Symbols

'foo ; => FOO  Notice that the symbol is upper-cased automatically. CL is
     ; generally not case sensitive, but it is possible to create case-sensitive
     ; keywords and symbols.

;;; Intern manually creates a symbol from a string

(intern "AAAA") ; => AAAA
(intern "aAa")  ; => |aAa| <- note the |...| indicating case-sensitivity.

;;; Numbers
9999999999999999999999 ; integers
#b111                  ; binary => 7
#o111                  ; octal => 73
#x111                  ; hexadecimal => 273
3.14159s0              ; single
3.14159d0              ; double
1/2                    ; ratios
#C(1 2)                ; complex numbers

;;; Function application is written (f x y z ...)  where f is a function and x,
;;; y, z, ... are operands. If you want to create a literal list of data, use '
;;; to stop it from being evaluated - literally, "quote" the data.

'(+ 1 2) ; => (+ 1 2)

;;; You can also call a function manually:

(funcall #'+ 1 2 3) ; => 6
		    ; the #' notation allows one to access the function object a
		    ; symbol refers to. CL has two namespaces, one for functions,
		    ; one for variables.

(defparameter foo "ALGOL SUCKS")
(defun foo () (format t "Java is for morons"))

#'foo ; => #<FUNCTION FOO>
foo   ; => "ALGOL SUCKS"

;;; Some arithmetic operations
(+ 1 1)              ; => 2
(- 8 1)              ; => 7
(* 10 2)             ; => 20
(expt 2 3)           ; => 8
(mod 5 2)            ; => 1
(/ 35 5)             ; => 7
(/ 1 3)              ; => 1/3
(+ #C(1 2) #C(6 -4)) ; => #C(7 -2)

;;; Booleans
t                    ; for true (any not-nil value is true)
nil                  ; for false
()                   ; an empty list is the same as nil
(not nil)            ; => t
(and 0 t)            ; => t
(or 0 nil)           ; => 0

;;; Strings are fixed-length arrays of characters.
"Hello, world!"
"Benjamin \"Bugsy\" Siegel"   ; backslash is an escaping character

;;; Strings can be concatenated too!
(concatenate 'string "Hello " "world!") ; => "Hello world!"

;;; A string can be treated like a sequence of characters
(elt "Apple" 0) ; => #\A
(aref "lole" 3) ; => #\e

;;; format can be used to format strings:
(format nil "~a can be ~a" "strings" "formatted")
;;; note that using `t' as the output stream redirects to standard output 
(format t "~a can be ~a" "strings" "formatted") 

;;; format actually has its own mini language for formatting output - the
;;; Practical Common Lisp page on the matter is a good starting point for those
;;; interested in this sort of thing.
;;;
;;; (mmb::open-uri "http://www.gigamonkeys.com/book/a-few-format-recipes.html" t)

;;; Variables 
;;; ============================================================================
;;; You can create a global (dynamically scoped) using defparameter
;;; a variable name can use any character except: ()",'`;#|\

(defparameter *some-var* 5)
*some-var* ; => 5

;;; `defvar' has slightly different semantics - compiling a defvar form will
;;; only bind the symbol to the passed value if it is not already bound.

(defvar *some-var* 1)
*some-var* ;=> 5

;;; Local binding: `me` is bound to "dance with you" only within the
;;; (let* ...). Let always returns the value of the last `form` in the
;;; let form.

(let* ((me "dance with you"))
  me)
;; => "dance with you"

;;; Lists 
;;; ============================================================================
;;; Lists are linked-list data structures, made of `cons' pairs and end
;;; with a `nil' (or '()) to mark the end of the list

(cons 1 (cons 2 (cons 3 nil))) ; => '(1 2 3)

;;; `list' is a convenience variadic constructor for lists

(list 1 2 3) ; => '(1 2 3)

;;; and a quote can also be used for a literal list value
'(1 2 3) ; => '(1 2 3)

(cdr '(1 2 3)) ;=>  (2 3)
(rest '(1 2 3)) ; (2 3)

;;; Regarding `car' & `cdr'
;;; -----------------------
;;; These names are accidents of history. They stand for ``Contents of the
;;; Address part of Register'' and ``Contents of the Decrement part of Register''
;;; of the IBM 704 computer, which was used for the first implementation of Lisp
;;; in the late 1950s. Scheme is a dialect of Lisp. -- SICM, appendix A
;;; 
;;; Can still use `cons' to add an item to the beginning of a list
(cons 4 '(1 2 3)) ; => '(4 1 2 3)

;;; Use `append' to - surprisingly - append lists together
(append '(1 2) '(3 4)) ; => '(1 2 3 4)

;;; Or use concatenate - 

(concatenate 'list '(1 2) '(3 4))
(mapcar #'1+ '(1 2 3))             ; => '(2 3 4)
(mapcar #'+ '(1 2 3) '(10 20 30))  ; => '(11 22 33)
(remove-if-not #'evenp '(1 2 3 4)) ; => '(2 4)
(every #'evenp '(1 2 3 4))         ; => nil
(some #'oddp '(1 2 3 4))           ; => T
(butlast '(subject verb object))   ; => (SUBJECT VERB)

;;; `Vector's `array's and `hash-table's exist, see CLHS for details.
;;; 
;;; Naively, sets are just lists:

(set-difference '(1 2 3 4) '(4 5 6 7)) ; => (3 2 1)
(intersection '(1 2 3 4) '(4 5 6 7)) ; => 4
(union '(1 2 3 4) '(4 5 6 7))        ; => (3 2 1 4 5 6 7)
(adjoin 4 '(1 2 3 4))     ; => (1 2 3 4)

;;; functions 
;;; ============================================================================
;;; 
;;; Use `lambda' to create anonymous functions.
;;; A function always returns the value of its last expression.
;;; The exact printable representation of a function will vary...

(lambda () "Hello World") ; => #<FUNCTION (LAMBDA ()) {1004E7818B}>

;; Use funcall to call lambda functions
(funcall (lambda () "Hello World")) ; => "Hello World"

;; Or Apply
(apply (lambda () "Hello World") nil) ; => "Hello World"

;; De-anonymize the function
(defun hello-world ()
   "Hello World")
(hello-world) ; => "Hello World"

;; The () in the above is the list of arguments for the function
(defun hello (name)
   (format nil "Hello, ~a " name))

(hello "Steve") ; => "Hello, Steve"

;; Functions can have optional arguments; they default to nil

(defun hello (name &optional from)
    (if from
        (format t "Hello, ~a, from ~a" name from)
        (format t "Hello, ~a" name)))

 (hello "Jim" "Alpacas") ;; => Hello, Jim, from Alpacas

;; And the defaults can be set...
(defun hello (name &optional (from "The world"))
   (format t "Hello, ~a, from ~a" name from))

(hello "Steve")
; => Hello, Steve, from The world

(hello "Steve" "the alpacas")
; => Hello, Steve, from the alpacas

;; And of course, keywords are allowed as well... usually more
;;   flexible than &optional.

(defun generalized-greeter (name &key (from "the world") (honorific "Mx"))
    (format t "Hello, ~a ~a, from ~a" honorific name from))

(generalized-greeter "Jim")   ; => Hello, Mx Jim, from the world

(generalized-greeter "Jim" :from "the alpacas you met last summer" :honorific "Mr")
; => Hello, Mr Jim, from the alpacas you met last summer

;; Let's handle the multiple return values here in code.

(multiple-value-bind
      (a b)
    (gethash 'd *m*)
  (list a b))
; => (NIL NIL)

(multiple-value-bind
      (a b)
    (gethash 'a *m*)
  (list a b))
; => (1 T)

;;; Equality 
;;; ============================================================================
;;; 
;;; Common Lisp has a sophisticated equality system. A couple are covered here.
;;; 
;;; for numbers use `='

(= 3 3.0) ; => t
(= 2 1) ; => nil

;;; for object identity (approximately) use `eql`

(eql 3 3) ; => t
(eql 3 3.0) ; => nil
(eql (list 3) (list 3)) ; => nil

;;; for lists, strings, and bit-vectors use `equal'

(equal (list 'a 'b) (list 'a 'b)) ; => t
(equal (list 'a 'b) (list 'b 'a)) ; => nil

;;; Control Flow 
;;; ============================================================================

;;; Conditionals

(if t                ; test expression
    "this is true"   ; then expression
    "this is false") ; else expression
; => "this is true"

;; In conditionals, all non-nil values are treated as true
(member 'Groucho '(Harpo Groucho Zeppo)) ; => '(GROUCHO ZEPPO)
(if (member 'Groucho '(Harpo Groucho Zeppo))
    'yep
    'nope)
; => 'YEP

;; `cond' chains a series of tests to select a result
(cond ((> 2 2) (error "wrong!"))
      ((< 2 2) (error "wrong again!"))
      (t 'ok)) ; => 'OK

;; Typecase switches on the type of the value
(typecase 1
  (string :string)
  (integer :int))

; => :int

;;; Iteration

;; Of course recursion is supported:

(defun walker (n)
  (if (zerop n)
      :walked
      (walker (1- n))))

(walker 5) ; => :walked

;; Most of the time, we use DOLIST or LOOP


(dolist (i '(1 2 3 4))
  (format t "~a" i))

; => 1234

(loop for i from 0 below 10
      collect i)

;;; loop details....
; => (0 1 2 3 4 5 6 7 8 9)

;;; Mutation
;;; ============================================================================
;;; Use `setf' to assign a new value to an existing variable. 

(let* ((variable 10))
  (setf variable 2))
 ; => 2

;;; Good Lisp style is to minimize destructive functions and to avoid
;;; mutation when reasonable.
;;; 
;;; CLOS (Common Lisp Object System)
;;; ============================================================================
;;; 
;;; No more Animal classes, let's have Human-Powered Mechanical
;;; Conveyances.

(defclass human-powered-conveyance ()
  ((velocity
    :accessor velocity
    :initarg :velocity)
   (average-efficiency
    :accessor average-efficiency
   :initarg :average-efficiency))
  (:documentation "A human powered conveyance"))

;;; defclass, followed by name, followed by the superclass list,
;;; followed by slot list, followed by optional qualities such as
;;; :documentation.
;;; 
;;; When no superclass list is set, the empty list defaults to the
;;; standard-object class. This *can* be changed, but not until you
;;; know what you're doing. Look up the Art of the Metaobject Protocol
;;; for more information.

(defclass bicycle (human-powered-conveyance)
  ((wheel-size
    :accessor wheel-size
    :initarg :wheel-size
    :documentation "Diameter of the wheel.")
   (height
    :accessor height
    :initarg :height)))

(defclass recumbent (bicycle)
  ((chain-type
    :accessor chain-type
    :initarg  :chain-type)))

(defclass unicycle (human-powered-conveyance) nil)

(defclass canoe (human-powered-conveyance)
  ((number-of-rowers
    :accessor number-of-rowers
    :initarg :number-of-rowers)))

;;; Calling DESCRIBE on the human-powered-conveyance class in the REPL gives:

(describe 'human-powered-conveyance)

; COMMON-LISP-USER::HUMAN-POWERED-CONVEYANCE
;  [symbol]
;
; HUMAN-POWERED-CONVEYANCE names the standard-class #<STANDARD-CLASS
;                                                    HUMAN-POWERED-CONVEYANCE>:
;  Documentation:
;    A human powered conveyance
;  Direct superclasses: STANDARD-OBJECT
;  Direct subclasses: UNICYCLE, BICYCLE, CANOE
;  Not yet finalized.
;  Direct slots:
;    VELOCITY
;      Readers: VELOCITY
;      Writers: (SETF VELOCITY)
;    AVERAGE-EFFICIENCY
;      Readers: AVERAGE-EFFICIENCY
;      Writers: (SETF AVERAGE-EFFICIENCY)

;; To define a method, let's find out what our circumference of the
;; bike wheel turns out to be using the equation: C = d * pi

(defmethod circumference ((object bicycle))
  (* pi (wheel-size object)))

;; pi is defined in Lisp already for us!

;; Let's suppose we find out that the efficiency value of the number
;; of rowers in a canoe is roughly logarithmic. This should probably be set
;; in the constructor/initializer.

;; Here's how to initialize your instance after Common Lisp gets done
;; constructing it:

(defmethod initialize-instance :after ((object canoe) &rest args)
  (setf (average-efficiency object)  (log (1+ (number-of-rowers object)))))

;; Then to construct an instance and check the average efficiency...

(average-efficiency (make-instance 'canoe :number-of-rowers 15))
; => 2.7725887

;;; Macros 
;;; ============================================================================
;;; Common Lisp doesn't come with a WHILE loop- let's add one.
;;; If we obey our assembler instincts, we wind up with:

(defmacro while (condition &body body)
    "While `condition` is true, `body` is executed.

`condition` is tested prior to each execution of `body`"
    (let ((block-name (gensym)) (done (gensym)))
        `(tagbody
           ,block-name
           (unless ,condition
               (go ,done))
           (progn
           ,@body)
           (go ,block-name)
           ,done)))

;; Let's look at the high-level version of this:

(defmacro while (condition &body body)
    "While `condition` is true, `body` is executed.

`condition` is tested prior to each execution of `body`"
  `(loop while ,condition
         do
         (progn
	   ,@body)))

;;; People often argue that becuase macros generate code they're impossible to
;;; understand. These people work in impoverished editors.
;;;
;;; Compile one of the `while' definitions above and macroexpand the let form
;;; using C-c C-m
;;;
;;; the *slime-macroexpansion* buffer will list a form containing `while'.
;;; By default C-c C-m is bound to slime-macroexpand-1, which does exactly
;;; what it sounds like. You can expand arbitrary sub-forms (such as the
;;; `while') using the same keybinding in the macroexpansion buffer.
;;;
;;; Try expanding `while' into `loop' and then into a `block'
;;;
;;; M-x slime-macroexpand-all will fully expand the form. 

(let* ((a 0))
  (while (> 10 a)
    (format t "the value of a is ~a" a)
    (incf a)))

;;; However, with a modern compiler, this is not required; the LOOP
;;; form compiles equally well and is easier to read.
;;; 
;;; Note that  is used, as well as `,` and `@`.  is a quote-type operator
;;; known as quasiquote; it allows the use of `,` . `,` allows "unquoting"
;;; variables. @ interpolates lists.
;;; 
;;; Gensym creates a unique symbol guaranteed to not exist elsewhere in
;;; the system. This is because macros are expanded at compile time and
;;; variables declared in the macro can collide with variables used in
;;; regular code.
;;; 
;;; To really learn common lisp, evaluate the following form and read
;;; one of the books.

(progn
  (mmb::open-uri "http://learnlispthehardway.org/book/")
  (mmb::open-uri "http://www.psg.com/~dlamkins/sl/contents.html")
  (mmb::open-uri "http://www.gigamonkeys.com/book/" t))

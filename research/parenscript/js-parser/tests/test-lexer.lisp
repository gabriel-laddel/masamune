;;;; test-lexer.lisp
;;;
;;; Unit tests for the Javascript lexer.
;;;
;;; Copyright (c) 2005 James Wright
;;; See LICENSE for full licensing details.
;;;
(in-package :js-parser-tests)

;;;; Helper functions
(defun non-null (x)
  "Return T if X is non-null.  This is a convenience function that
   frees us from the necessity of having to know exactly which non-null
   value a test should expect."
  (not (null x)))

;;;; Test categories
(defnote lexer "tests for the lexer")
(defnote regexp "tests for individual regular expressions")

;;;; Tests
(deftest regexp-re/1 :notes (lexer regexp)
  (non-null (scan regexp-re "/hello/"))
  t)

(deftest regexp-re/2 :notes (lexer regexp)
  (non-null (scan regexp-re "/.\\n/"))
  t)

(deftest regexp-re/3 :notes (lexer regexp)
  (non-null (scan regexp-re "/(this)/g"))
  t)

(deftest regexp-re/4 :notes (lexer regexp)
  (non-null (scan regexp-re "/(this)/gi"))
  t)

(deftest regexp-re/5 :notes (lexer regexp)
  (scan regexp-re "\"hi\"")
  nil)

(deftest regexp-re/6 :notes (lexer regexp)
  (scan regexp-re "/\"hi\"")
  nil)

(defun read-all-tokens (js-string)
  "Return a list of cons cells representing the tokens
   of JS-STRING.  The CAR of each cell is the type of
   token, and the CDR is the source text."
  (loop with l = (make-lexer-function (make-instance 'javascript-lexer :text js-string))
        for x = (multiple-value-list (funcall l))
        until (null (first x))
        collect (list (first x) (token-value (second x)))))

(deftest lexer/1 :notes lexer
  (read-all-tokens
   "/* test string */
    function (f)
    {
      // Ignore this stuff
      var m = 010;
      doStuff('stuff', \"nonsense\", 0xff, 45.0, f(m));
    }")
  ((:function "function")
   (:left-paren "(")
   (:identifier "f")
   (:right-paren ")")
   (:left-curly "{")
   (:var "var")
   (:identifier "m")
   (:equals "=")
   (:number 8)
   (:semicolon ";")
   (:identifier "doStuff")
   (:left-paren "(")
   (:string-literal "stuff")
   (:comma ",")
   (:string-literal "nonsense")
   (:comma ",")
   (:number 255)
   (:comma ",")
   (:number 45.0)
   (:comma ",")
   (:identifier "f")
   (:left-paren "(")
   (:identifier "m")
   (:right-paren ")")
   (:right-paren ")")
   (:semicolon ";")
   (:right-curly "}")))
    
(deftest lexer/2 :notes lexer
  (read-all-tokens
   "var re1 = /hello/g;
    var re2 = /hello\\/goodbye/ig;")
  ((:var "var")
   (:identifier "re1")
   (:equals "=")
   (:re-literal ("hello" . "g"))
   (:semicolon ";")
   (:var "var")
   (:identifier "re2")
   (:equals "=")
   (:re-literal ("hello/goodbye" . "ig"))
   (:semicolon ";")))

(deftest lexer/3 :notes lexer
  (read-all-tokens
   "x >= 10, y<=20, <=>=<>")
  ((:identifier "x")
   (:greater-than-equals ">=")
   (:number 10)
   (:comma ",")
   (:identifier "y")
   (:less-than-equals "<=")
   (:number 20)
   (:comma ",")
   (:less-than-equals "<=")
   (:greater-than-equals ">=")
   (:less-than "<")
   (:greater-than ">")))

(deftest lexer/4 :notes lexer
  (read-all-tokens
   "foo _foo $foo foo1 foo$ foo_")
  ((:identifier "foo")
   (:identifier "_foo")
   (:identifier "$foo")
   (:identifier "foo1")
   (:identifier "foo$")
   (:identifier "foo_")))

(deftest lexer/restricted-tokens/1 :notes lexer
  (read-all-tokens "break foo")
  ((:break "break") (:no-line-terminator "") (:identifier "foo")))

(deftest lexer/restricted-tokens/2 :notes lexer
  (read-all-tokens "break
                    foo")
  ((:break "break") (:line-terminator "") (:identifier "foo")))

(deftest lexer/restricted-tokens/3 :notes lexer
  (read-all-tokens "continue;")
  ((:continue "continue") (:no-line-terminator "") (:semicolon ";")))

(deftest lexer/restricted-tokens/4 :notes lexer
  (read-all-tokens "continue
                    ;")
  ((:continue "continue") (:line-terminator "") (:semicolon ";")))

(deftest lexer/restricted-tokens/5 :notes lexer
  (read-all-tokens "b ++ c")
  ((:identifier "b") (:no-line-terminator "") (:plus2 "++") (:identifier "c")))

(deftest lexer/restricted-tokens/6 :notes lexer
  (read-all-tokens "b
                    ++ c")
  ((:identifier "b") (:line-terminator "") (:plus2 "++") (:identifier "c")))

(deftest lexer/set-cursor/1 :notes lexer
  (let ((lexer (make-instance 'javascript-lexer :text "x/5 + x/10;"))
        (tok nil))
    (next-token lexer)  ; ==> X
    (setf tok (next-token lexer))  ; ==> /5 + x/
    (set-cursor lexer (token-start tok))
    (next-token lexer))
  #s(token :terminal :re-literal :value ("5 + x" . "")
           :start 1 :end 8))

(deftest lexer/coerce-token/1 :notes lexer
  (let ((lexer (make-instance 'javascript-lexer :text "x/5 + x/10;"))
        (tok nil))
    (next-token lexer)  ; ==> X
    (setf tok (next-token lexer))  ; ==> /5 + x/
    (set-cursor lexer (token-start tok))
    (coerce-token lexer :slash)
    (next-token lexer)  ; ==> /
    (next-token lexer))
  #s(token :terminal :number :value 5 :start 2 :end 3))

(deftest lexer/position/1 :notes lexer
  (let ((lexer (make-instance 'javascript-lexer :text "Line 1
Line number 2

Line 4")))
    (loop for token = (next-token lexer)
          until (null (token-terminal token))
          collect (position-to-line/column (js-parser::text lexer) (token-start token))))
  ((1 . 1) (1 . 6)
   (2 . 1) (2 . 6) (2 . 13)
   (4 . 1) (4 . 6)))

(deftest lexer/position/2 :notes lexer
  (let ((lexer (make-instance 'javascript-lexer :text "Line 1
Line number 2

Line 4")))
    (loop for token = (next-token lexer)
          until (null (token-terminal token))
          collect (position-to-line/column (js-parser::text lexer) (token-end token))))
  ((1 . 5) (1 . 7)
   (2 . 5) (2 . 12) (2 . 14)
   (4 . 5) (4 . 7)))

(deftest lexer/encountered-line-terminator/1 :notes lexer
  (let ((lexer (make-instance 'javascript-lexer :text "})
                                                       return 10;")))
    (loop for token = (next-token lexer)
          until (null (token-terminal token))
          collect (not (null (encountered-line-terminator lexer)))))
  (nil nil t nil nil nil))
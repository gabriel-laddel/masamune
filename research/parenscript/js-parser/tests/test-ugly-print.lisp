;;;; test-ugly-print.lisp
;;;
;;; Tests for the ugly printer
;;;
;;; Copyright (c) 2005 Greg Smolyn
;;; See LICENSE for full licensing details.
;;;
(in-package :js-parser-tests)

;;;; Helper functions
(defun ugly-string (elm)
  "Uglyprint LM to a string value instead of a stream"
  (with-output-to-string (s)
    (ugly-print elm s)))

(defmacro with-fresh-genvar (&body body)
  "Make sure that GENVAR variable names will start from 0 and that
   continuation arguments will have a known value"
  `(let* ((*genvar-counter* 0))
    ,@body))

;;;; Test categories
(defnote ugly-print "tests for the ugly printer")
(defnote uniquify "tests for the uniquify transformation")

;;;; Ugly-printer tests
(deftest ugly-print/var-decl/1 :notes ugly-print
  (with-fresh-genvar
    (in-local-scope
      (ugly-string (test-parse "var x = 3;"))))
    "var JW0=3;")

(deftest ugly-print/function-decl/1 :notes ugly-print
  (with-fresh-genvar
    (in-local-scope
      (ugly-string (test-parse "function FOO(){}"))))
  "function JW0(){}")

(deftest ugly-print/function-decl/2 :notes ugly-print
  (with-fresh-genvar
    (in-local-scope
      (ugly-string (test-parse "function FOO(x){}"))))
  "function JW0(JW1){}")

(deftest ugly-print/function-decl/3 :notes ugly-print
  (with-fresh-genvar
    (ugly-string (test-parse "function FOO(x){ var y = x; }")))
  "function FOO(JW0){var JW1=JW0;}")

(deftest ugly-print/function-decl/4 :notes ugly-print
  (with-fresh-genvar
    (in-local-scope
      (ugly-string (test-parse "function FOO(){ FOO(); }"))))
  "function JW0(){JW0();}")

(deftest ugly-print/function-decl/5 :notes ugly-print
  (with-fresh-genvar
    (in-local-scope
      (let ((js-parser::*pretty-mode* nil))
        (js-parser::uglify-vars (test-parse "
          function recursiveCount(i, n)
          {
            if(i > n)
              return i - 1;
            else
            {
              WScript.echo(i + '/' + n);
              return recursiveCount(i + 1, n);
            }
          }")))))
  #.(test-parse "
      function JW0(JW1, JW2)
      {
        if(JW1 > JW2)
          return JW1 - 1;
        else
        {
          WScript.echo(JW1 + '/' + JW2);
          return JW0(JW1 + 1, JW2);
        }
      }"))

;; ensure vardecls in blocks shadow function vars
;;
;;    function foo(x) <-- this x could be JW0
;;    {
;;        var x = 3;  <-- this x should be JW1 not 0
;;        bar(x);     <-- this x should be JW1
;;    }
;;
(deftest ugly-print/function-decl-arg-shadow/1 :notes ugly-print
  (with-fresh-genvar
    (ugly-string (test-parse "function FOO(x){ var x = 3; }")))
  "function FOO(JW0){var JW1=3;}")
    

(deftest ugly-print/function-decl-arg-shadow/2 :notes ugly-print
  (with-fresh-genvar
    (in-local-scope
      (ugly-string (test-parse "function FOO(x){ var x = 3; FOO(x);}"))))
  "function JW0(JW1){var JW2=3;JW0(JW2);}")

(deftest ugly-print/function-in-function/1 :notes ugly-print
  (with-fresh-genvar
    (ugly-string (test-parse "function FOO(x) {
                          function BAR(z) {
                             return z + y;
                          }
                          var y = 3;
                          BAR(3); 
                         }")))
  "function FOO(JW0){function JW2(JW3){return JW3+JW1;}var JW1=3;JW2(3);}")

(deftest ugly-print/function-in-function/2 :notes ugly-print
  (with-fresh-genvar
    (ugly-string (test-parse "function FOO(x) {
                          var y = 3;
                          function BAR(z) {
                             return z + y;
                          }
                          BAR(3); 
                         }")))
  "function FOO(JW0){var JW1=3;function JW2(JW3){return JW3+JW1;}JW2(3);}")

(deftest ugly-print/function-in-function-in-function/1 :notes ugly-print
  (with-fresh-genvar
    (ugly-string (test-parse "function FOO(x) {
                          function BAR(z) {
                            function BAZ(xz) {
                                return 3 + y;
                             }
                             return z + y + BAZ(3);
                          }
                          var y = 3;
                          BAR(3); 
                         }")))
"function FOO(JW0){function JW2(JW3){function JW4(JW5){return 3+JW1;}return JW3+JW1+JW4(3);}var JW1=3;JW2(3);}")

(deftest ugly-print/blocks/1 :notes ugly-print
 (with-fresh-genvar
   (in-local-scope
     (ugly-string (test-parse "{ var y = 3;
                             {
                                var x = 1;
                             }
                            x + y;
                          }"))))
   "{var JW0=3;{var JW1=1;}JW1+JW0;}")

(deftest ugly-print/free-variables/1 :notes ugly-print
  (with-fresh-genvar
    (in-local-scope
      (ugly-string (test-parse "var x = 10;
                           var y = x + z;"))))
    "var JW0=10;var JW1=JW0+z;")

(deftest ugly-print/free-variables/2 :notes ugly-print
  (with-fresh-genvar
    (ugly-string (test-parse "var x = foo;
                         function bar(m)
                         {
                           var y=m*2;
                           if(y > x)
                             return bar(m--);
                           else
                             return m;
                         }")))
  "var x=foo;function bar(JW0){var JW1=JW0*2;if(JW1>x)return bar(JW0--);else return JW0;}")

(deftest ugly-print/pretty-variable/1 :notes ugly-print
  (with-fresh-genvar
    (let ((js-parser::*pretty-mode* t))
      (js-parser::uglify-vars (test-parse "
        function fn(arg1, arg2)
        {
          function bar() { return 7; }
          var foo = 10;
          WScript.echo(foo + arg2);
        }"))))
  #.(test-parse "
        function fn(arg1$0, arg2$1)
        {
          function bar$3() { return 7; }
          var foo$2 = 10;
          WScript.echo(foo$2 + arg2$1);
        }"))

(deftest ugly-print/for-loop-does-not-create-new-scope/1 :notes ugly-print
  (with-fresh-genvar
    (ugly-string (test-parse "
        var top = 10;
        for(var top = 0; top < 100; top++)
        {
          echo(top);
        }")))
   "var top=10;for(var top=0;top<100;top++)echo(top);")

(deftest ugly-print/for-loop-does-not-create-new-scope/2 :notes ugly-print
  (with-fresh-genvar
    (ugly-string (test-parse "
        var top = 10;
        for(var top in topVars)
        {
          echo(top);
        }")))
   "var top=10;for(var top in topVars)echo(top);")

(deftest ugly-print/case-sensitivity/1 :notes ugly-print
  (with-fresh-genvar
    (ugly-string (test-parse "
        function Counter() {}
        function foo()
        {
          var counter = new Counter;
        }")))
  "function Counter(){}function foo(){var JW0=new Counter;}")

(deftest ugly-print/case-sensitivity/2 :notes ugly-print
  (with-fresh-genvar
    (ugly-string (test-parse "
        function Counter() {}
        var counter = new Counter;")))
  "function Counter(){}var counter=new Counter;") ; Toplevel identifiers (including the `counter` var) should not be changed

;;;; Uniquify tests

(deftest uniquify/position-preservation/1 :notes uniquify
  (with-fresh-genvar
    (transform 'uniquify (parse "function foo(x) { var x = 10; x = 5;}")))
  (#s(function-decl :name "foo" :parameters ("x$0")
                    :body (#s(var-decl-statement :var-decls (#s(var-decl :name "x$1"
                                                                         :initializer #s(numeric-literal :value 10 :start 26 :end 28)
                                                                         :start 22 :end 28))
                                                 :start 18 :end 29)
                           #s(binary-operator :op-symbol :assign
                                              :left-arg #s(identifier :name "x$1"
                                                                      :start 30 :end 31)
                                              :right-arg #s(numeric-literal :value 5 :start 34 :end 35)
                                              :start 30 :end 35))
                    :start 0 :end 35)))
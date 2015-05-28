;;;; test-pretty-print.lisp
;;;
;;; Tests for the pretty-printer.
;;;
;;; Copyright (c) 2005 James Wright
;;; See LICENSE for full licensing details.
;;;
(in-package :js-parser-tests)

;;;; Helper functions
(defun pretty-string (elm)
  "Pretty-print ELM to a string value instead of a stream."
  (with-output-to-string (s)
    (pretty-print elm s)))

;;;; Test categories
(defnote pretty-print "tests for the pretty-printer")

;;;; Helpful constants
;;; We define these as parameters even though they're really constants in order to get
;;; SBCL to SHUT UP.  Defconstant under SBCL complains unless the old value is EQL to
;;; the new one.  Sadly, there is no way for two structure values to be EQL (unless they
;;; are EQ).  We could write some sort of crazy macro to do this right, but for now
;;; let's just not worry about it.
(defparameter foo-id (make-identifier :name "foo"))
(defparameter bar-id (make-identifier :name "bar"))
(defparameter baz-id (make-identifier :name "baz"))

;;;; Tests
(deftest pretty-print/re-literal/1 :notes pretty-print
  (pretty-string (make-re-literal :pattern "ahoy" :options ""))
  "/ahoy/")

(deftest pretty-print/re-literal/2 :notes pretty-print
  (pretty-string (make-re-literal :pattern "avast" :options "gi"))
  "/avast/gi")

(deftest pretty-print/new-expr/1 :notes pretty-print
  (pretty-string (make-new-expr :constructor foo-id :args nil))
  "new foo")

(deftest pretty-print/new-expr/2 :notes pretty-print
  (pretty-string (make-new-expr :constructor foo-id :args (list bar-id baz-id)))
  "new foo(bar, baz)")

(deftest pretty-print/new-expr/3 :notes pretty-print
  (pretty-string (make-new-expr :constructor (make-fn-call :fn foo-id)))
  "new (foo())")

(deftest pretty-print/new-expr/4 :notes pretty-print
  (pretty-string (make-new-expr :constructor (make-fn-call :fn foo-id :args (list bar-id))
                                :args (list baz-id)))
  "new (foo(bar))(baz)")

(deftest pretty-print/new-expr/5 :notes pretty-print
  (pretty-string (test-parse "new new new foo;"))
  "new new new foo;")

(deftest pretty-print/new-expr/6 :notes pretty-print
  (pretty-string (test-parse "new new foo(20);"))
  "new new foo(20);")

(deftest pretty-print/new-expr/7 :notes pretty-print
  (pretty-string (test-parse "(new new foo)(20);"))
  "(new new foo)(20);")

(deftest pretty-print/special-value/1 :notes pretty-print
  (pretty-string (make-special-value :symbol :this))
  "this")

(deftest pretty-print/special-value/2 :notes pretty-print
  (pretty-string (make-special-value :symbol :true))
  "true")

(deftest pretty-print/special-value/3 :notes pretty-print
       (pretty-string (make-special-value :symbol :false))
       "false")

(deftest pretty-print/special-value/4 :notes pretty-print
  (pretty-string (make-special-value :symbol :null))
  "null")

(deftest pretty-print/special-value/5 :notes pretty-print
  (pretty-string (make-special-value :symbol :arguments))
  "arguments")

(deftest pretty-print/property-access/1 :notes pretty-print
  (pretty-string (make-property-access :target foo-id :field (make-string-literal :value "bar")))
  "foo.bar")

(deftest pretty-print/property-access/2 :notes pretty-print
  (pretty-string (make-property-access :target foo-id :field bar-id))
  "foo[bar]")

(deftest pretty-print/property-access/3 :notes pretty-print
  (pretty-string (make-property-access :target foo-id :field (make-string-literal :value "space out")))
  "foo[\"space out\"]")

(deftest pretty-print/unary-operator/1 :notes pretty-print
  (pretty-string (make-unary-operator :op-symbol :void :arg foo-id))
  "void foo")

(deftest pretty-print/unary-operator/2 :notes pretty-print
  (pretty-string (make-unary-operator :op-symbol :delete :arg foo-id))
  "delete foo")

(deftest pretty-print/unary-operator/3 :notes pretty-print
  (pretty-string (make-unary-operator :op-symbol :post-incr :arg foo-id))
  "foo++")

(deftest pretty-print/unary-operator/4 :notes pretty-print
  (pretty-string (make-unary-operator :op-symbol :post-decr :arg foo-id))
  "foo--")

(deftest pretty-print/unary-operator/5 :notes pretty-print
  (pretty-string (make-unary-operator :op-symbol :pre-incr :arg foo-id))
  "++foo")

(deftest pretty-print/unary-operator/6 :notes pretty-print
  (pretty-string (make-unary-operator :op-symbol :pre-decr :arg foo-id))
  "--foo")

(deftest pretty-print/unary-operator/7 :notes pretty-print
  (pretty-string (make-unary-operator :op-symbol :unary-plus :arg foo-id))
  "+foo")

(deftest pretty-print/unary-operator/8 :notes pretty-print
  (pretty-string (make-unary-operator :op-symbol :unary-minus :arg foo-id))
  "-foo")

(deftest pretty-print/binary-operator/1 :notes pretty-print
  (pretty-string (make-binary-operator :op-symbol :add :left-arg foo-id :right-arg bar-id))
  "foo + bar")

(deftest pretty-print/binary-operator/2 :notes pretty-print
  (pretty-string (make-binary-operator :op-symbol :lshift :left-arg foo-id :right-arg bar-id))
  "foo << bar")

(deftest pretty-print/binary-operator/3 :notes pretty-print
  (pretty-string (make-binary-operator :op-symbol :instanceof :left-arg foo-id :right-arg bar-id))
  "foo instanceof bar")

(deftest pretty-print/operator-precedence/1 :notes pretty-print
  (pretty-string
   (make-binary-operator :op-symbol :multiply
                         :left-arg (make-binary-operator :op-symbol :add
                                                         :left-arg (make-numeric-literal :value 1)
                                                         :right-arg (make-numeric-literal :value 2))
                         :right-arg (make-binary-operator :op-symbol :add
                                                          :left-arg (make-numeric-literal :value 3)
                                                          :right-arg (make-numeric-literal :value 4))))
  "(1 + 2) * (3 + 4)")

(deftest pretty-print/operator-precedence/2 :notes pretty-print
  (pretty-string
   (make-unary-operator :op-symbol :post-incr
                        :arg (make-binary-operator :op-symbol :subtract
                                                   :left-arg foo-id
                                                   :right-arg (make-numeric-literal :value 10))))
  "(foo - 10)++")

(deftest pretty-print/operator-precedence/3 :notes pretty-print
  (pretty-string
   (make-unary-operator :op-symbol :delete
                        :arg (make-binary-operator :op-symbol :logical-or
                                                   :left-arg foo-id
                                                   :right-arg bar-id)))
  "delete (foo || bar)")

(deftest pretty-print/operator-precedence/4 :notes pretty-print
  (pretty-string
   (make-property-access :target (make-new-expr :constructor foo-id)
                         :field bar-id))
  "(new foo)[bar]")

(deftest pretty-print/operator-precedence/5 :notes pretty-print
  (pretty-string
   (make-property-access :target (make-new-expr :constructor foo-id
                                                :args (list baz-id))
                         :field bar-id))
  "new foo(baz)[bar]")

(deftest pretty-print/operator-precedence/6 :notes pretty-print
  (pretty-string
   (make-new-expr :constructor (make-property-access
                                :target (make-fn-call :fn foo-id
                                                      :args (list baz-id))
                                :field bar-id)))
  "new (foo(baz)[bar])")

(deftest pretty-print/operator-precedence/7 :notes pretty-print
  (pretty-string
   (make-new-expr :constructor (make-property-access
                                :target (make-fn-call :fn foo-id
                                                      :args (list baz-id))
                                :field bar-id)
                  :args (list (make-numeric-literal :value 50))))
  "new (foo(baz)[bar])(50)")

(deftest pretty-print/operator-associativity/1 :notes pretty-print
  (pretty-string
   (make-binary-operator :op-symbol :add
                         :left-arg foo-id
                         :right-arg (make-binary-operator :op-symbol :add
                                                          :left-arg bar-id
                                                          :right-arg baz-id)))
  "foo + (bar + baz)")

(deftest pretty-print/operator-associativity/2 :notes pretty-print
  (pretty-string
   (make-binary-operator :op-symbol :add
                         :left-arg foo-id
                         :right-arg (make-binary-operator :op-symbol :subtract
                                                          :left-arg bar-id
                                                          :right-arg baz-id)))
  "foo + (bar - baz)")

(deftest pretty-print/operator-associativity/3 :notes pretty-print
  (pretty-string
   (make-binary-operator :op-symbol :add
                         :left-arg (make-binary-operator :op-symbol :add
                                                         :left-arg foo-id
                                                         :right-arg bar-id)
                         :right-arg baz-id))
  "foo + bar + baz")

(deftest pretty-print/operator-associativity/4 :notes pretty-print
  (pretty-string
   (make-binary-operator :op-symbol :add
                         :left-arg (make-binary-operator :op-symbol :subtract
                                                         :left-arg foo-id
                                                         :right-arg bar-id)
                         :right-arg baz-id))
  "foo - bar + baz")

(deftest pretty-print/conditional/1 :notes pretty-print
  (pretty-string
   (make-conditional :condition foo-id
                     :true-arg bar-id
                     :false-arg (make-special-value :symbol :null)))
  "foo ? bar : null")

(deftest pretty-print/conditional/2 :notes pretty-print
  (pretty-string
   (make-conditional :condition (make-binary-operator :op-symbol :and-equals
                                                      :left-arg foo-id
                                                      :right-arg bar-id)
                     :true-arg bar-id
                     :false-arg baz-id))
  "(foo &= bar) ? bar : baz")

(deftest pretty-print/numeric-literal/1 :notes pretty-print
  (pretty-string (make-numeric-literal :value 50))
  "50")

(deftest pretty-print/var-decl-statement/1 :notes pretty-print
  (pretty-string
   (make-var-decl-statement :var-decls 
                       (list (make-var-decl :name "x")
                             (make-var-decl :name "y" :initializer (make-numeric-literal :value 50)))))
  "var x, y = 50")

(deftest pretty-print/block/1 :notes pretty-print
  (pretty-string
   (make-statement-block :statements
               (list (make-var-decl-statement :var-decls (list (make-var-decl :name "x" :initializer (make-numeric-literal :value 55))))
                     (make-binary-operator :op-symbol :times-equals :left-arg (make-identifier :name "x") :right-arg foo-id))))
  "{
  var x = 55;
  x *= foo;
}")

(deftest pretty-print/block/2 :notes pretty-print
  (with-indent
    (pretty-string
     (make-statement-block :statements
                 (list (make-var-decl-statement :var-decls (list (make-var-decl :name "x" :initializer (make-numeric-literal :value 55))))
                       (make-binary-operator :op-symbol :times-equals :left-arg (make-identifier :name "x") :right-arg foo-id)))))
  "  {
    var x = 55;
    x *= foo;
  }")

(deftest pretty-print/if/1 :notes pretty-print
  (pretty-string (make-if-statement :condition foo-id :then-statement bar-id))
  "if(foo)
  bar;")

(deftest pretty-print/if/2 :notes pretty-print
  (pretty-string (make-if-statement :condition foo-id :then-statement bar-id :else-statement baz-id))
  "if(foo)
  bar;
else
  baz;")

(deftest pretty-print/if/3 :notes pretty-print
  (pretty-string (make-if-statement :condition foo-id :then-statement (make-statement-block :statements (list bar-id))))
  "if(foo)
{
  bar;
}")

(deftest pretty-print/if/4 :notes pretty-print
  (pretty-string
   (make-if-statement :condition foo-id :then-statement (make-statement-block :statements (list bar-id)) :else-statement baz-id))
  "if(foo)
{
  bar;
}
else
  baz;")

(deftest pretty-print/if/5 :notes pretty-print
  (pretty-string
   (make-if-statement :condition foo-id :then-statement bar-id
            :else-statement (make-statement-block :statements (list baz-id))))
   "if(foo)
  bar;
else
{
  baz;
}")


(deftest pretty-print/do/1 :notes pretty-print
  (pretty-string (make-do-statement :condition
                                    (make-binary-operator :op-symbol :greater-than
                                                          :left-arg foo-id
                                                          :right-arg (make-numeric-literal :value 55.0))
                                    :body
                                    (make-statement-block :statements
                                                          (list (make-unary-operator :op-symbol :post-incr :arg foo-id)))))
  "do
{
  foo++;
}
while(foo > 55.0);")

(deftest pretty-print/do/2 :notes pretty-print
  (pretty-string (make-do-statement :condition (make-special-value :symbol :true)
                                    :body (make-unary-operator :op-symbol :post-incr
                                                               :arg foo-id)))
  "do
  foo++;
while(true);")

(deftest pretty-print/while/1 :notes pretty-print
  (pretty-string (make-while :condition (make-unary-operator :op-symbol :typeof :arg foo-id)
                             :body (make-unary-operator :op-symbol :delete :arg foo-id)))
  "while(typeof foo)
  delete foo;")

(deftest pretty-print/while/2 :notes pretty-print
  (pretty-string (make-while :condition (make-unary-operator :op-symbol :typeof :arg foo-id)
                             :body (make-statement-block :statements (list (make-unary-operator :op-symbol :delete :arg foo-id)))))
  "while(typeof foo)
{
  delete foo;
}")

(deftest pretty-print/for/1 :notes pretty-print
  (pretty-string (make-for :body (make-statement-block)))
  "for(; ; )
{
}")

(deftest pretty-print/for/2 :notes pretty-print
  (pretty-string (make-for :initializer (make-var-decl-statement :var-decls (list (make-var-decl :name "foo" :initializer bar-id)))
                        :condition (make-binary-operator :op-symbol :not-equals :left-arg foo-id :right-arg baz-id)
                        :step (make-unary-operator :op-symbol :pre-incr :arg foo-id)
                        :body (make-statement-block :statements (list (make-unary-operator :op-symbol :post-decr :arg foo-id)))))
  "for(var foo = bar; foo != baz; ++foo)
{
  foo--;
}")

(deftest pretty-print/for-in/1 :notes pretty-print
  (pretty-string (make-for-in :binding (make-var-decl-statement :var-decls (list (make-var-decl :name "foo")))
                           :collection bar-id
                           :body (make-fn-call :fn baz-id :args (list foo-id bar-id))))
  "for(var foo in bar)
  baz(foo, bar);")

(deftest pretty-print/for-in/2 :notes pretty-print
  (pretty-string (make-for-in :binding foo-id
                              :collection bar-id
                              :body (make-fn-call :fn baz-id :args (list foo-id bar-id))))
  "for(foo in bar)
  baz(foo, bar);")

(deftest pretty-print/switch/1 :notes pretty-print
  (pretty-string (make-switch :value foo-id :clauses
                 (list
                  (make-case-clause :value (make-numeric-literal :value 10)
                             :body (list
                                    (make-fn-call :fn bar-id :args (list (make-numeric-literal :value 1)))
                                    (make-fn-call :fn baz-id :args (list foo-id))
                                    (make-break-statement)))
                  (make-case-clause :value (make-numeric-literal :value 20))
                  (make-case-clause :value (make-numeric-literal :value 30)
                             :body (list
                                    (make-fn-call :fn bar-id :args (list (make-numeric-literal :value 3)))))
                  (make-default-clause :body (list
                                       (make-return-statement :arg foo-id))))))
  "switch(foo)
{
case 10:
  bar(1);
  baz(foo);
  break;
case 20:
case 30:
  bar(3);
default:
  return foo;
}")

(deftest pretty-print/with/1 :notes pretty-print
  (pretty-string (make-with :scope-object foo-id :body (make-statement-block :statements (list bar-id))))
  "with(foo)
{
  bar;
}")

(deftest pretty-print/label/1 :notes pretty-print
  (pretty-string (make-fn-call :fn foo-id :label "fhwqgads" :args (list bar-id baz-id)))
  "fhwqgads:
foo(bar, baz)")

(deftest pretty-print/try/1 :notes pretty-print
  (pretty-string (make-try :body (list (make-fn-call :fn foo-id :args (list bar-id)))
                           :catch-clause (make-catch-clause :binding "e" :body (list (make-fn-call :fn foo-id :args (list (make-identifier :name "e")))))
                           :finally-clause (make-finally-clause :body (list (make-fn-call :fn baz-id) (make-unary-operator :op-symbol :delete :arg foo-id)))))
  "try
{
  foo(bar);
}
catch(e)
{
  foo(e);
}
finally
{
  baz();
  delete foo;
}")

(deftest pretty-print/function-decl/1 :notes pretty-print
  (pretty-string
   (make-function-decl :name "yarb" :parameters '("x" "y") :body (list (make-fn-call :fn foo-id) (make-fn-call :fn bar-id) (make-fn-call :fn baz-id))))
  "function yarb(x, y)
{
  foo();
  bar();
  baz();
}")

(deftest pretty-print/inner-function-decl/1 :notes pretty-print
  (pretty-string
   (make-function-decl :name "foo"
                       :parameters '("x")
                       :body (list
                              (make-function-decl :name "bar"
                                                  :parameters '("a" "b")
                                                  :body (list (make-return-statement :arg #s(special-value :symbol :this)))))))
  "function foo(x)
{
  function bar(a, b)
  {
    return this;
  }
}")

(deftest pretty-print/inner-function-decl/2 :notes pretty-print
  (pretty-string
   (make-function-decl :name "foo"
                       :parameters '("x")
                       :body (list
                              (make-return-statement)
                              (make-function-decl :name "bar"
                                                  :parameters '("a" "b")
                                                  :body (list (make-return-statement :arg #s(special-value :symbol :this)))))))
  "function foo(x)
{
  return;
  function bar(a, b)
  {
    return this;
  }
}")

;;  TODO Add test case(s) for function expressions (at least one for single-line and one for multi-line)

(deftest pretty-print/object-literal/1 :notes pretty-print
  (pretty-string
   (make-object-literal :properties (list
                                     (cons (make-string-literal :value "foo")
                                           (make-numeric-literal :value 10))
                                     (cons (make-string-literal :value "bar")
                                           (make-numeric-literal :value 20)))))
  "{foo: 10, bar: 20}")

(deftest pretty-print/suspend-statement/1 :notes pretty-print
  (pretty-string
   (make-suspend-statement))
  "suspend;")

(deftest pretty-print/function_continuation/1 :notes pretty-print
  (pretty-string
   (make-special-value :symbol :function_continuation))
  "function_continuation")

(deftest pretty-print/resume-statement/1 :notes pretty-print
  (pretty-string
   (make-resume-statement :target (make-fn-call :fn foo-id :args (list bar-id))))
  "resume foo(bar);")

(deftest pretty-print/resume-statement/2 :notes pretty-print
  (pretty-string
   (make-resume-statement :target (make-property-access :target foo-id
                                                        :field #s(string-literal :value "bar"))
                          :arg baz-id))
  "resume foo.bar <- baz;")

(deftest pretty-print/null-in-list/1 :notes pretty-print
  (pretty-string (test-parse "function foo() {}"))
  "function foo()
{
}")

(deftest pretty-print/null-in-then/1 :notes pretty-print
  (pretty-string (test-parse "if(foo) ; else bar();"))
  "if(foo)
  ;
else
  bar();")

(deftest pretty-print-null-in-else/1 :notes pretty-print
  (pretty-string (test-parse "if(foo) bar();"))
  "if(foo)
  bar();")

(deftest pretty-print-null-in-while/1 :notes pretty-print
  (pretty-string (test-parse "while(x++ < 10);"))
  "while(x++ < 10)
  ;")

(deftest pretty-print/add-handler/1 :notes pretty-print
  (pretty-string (list (make-add-handler :handler foo-id
                                         :thunk-body (list (make-return-statement :arg (make-special-value :symbol :null))))
                       (make-suspend-statement)))
  "$addHandler(foo, function()
{
  return null;
});
suspend;")

(deftest pretty-print/remove-handler/1 :notes pretty-print
  (pretty-string (list (js-parser::make-remove-handler :handler foo-id
                                                :thunk-body (list (make-return-statement :arg (make-special-value :symbol :null))))
                       (js-parser::make-suspend-statement)))
  "$removeHandler(foo, function()
{
  return null;
});
suspend;")

(deftest pretty-print/function-expression-literal-call/1 :notes pretty-print
  (pretty-string (make-fn-call
                  :fn (make-function-expression
                       :body (list (make-return-statement :arg (make-special-value :symbol :null))))))
  "(function()
{
  return null;
})()")

(deftest pretty-print/throw/1 :notes pretty-print
  (pretty-string (make-throw-statement :value foo-id))
  "throw foo;")

(deftest pretty-print/throw/2 :notes pretty-print
  (pretty-string (make-throw-statement :value foo-id
                                       :target bar-id))
  "throw foo -> bar;")

(deftest pretty-print/string-literal/1 :notes pretty-print
  (pretty-string (make-string-literal :value "\\\"hello\\\""))
  "\"\\\"hello\\\"\"")

(deftest pretty-print/string-literal/2 :notes pretty-print
  (let ((*escape-script-end-tags* t))
    (pretty-string (make-string-literal :value "hello </script> ahoy")))
  "(\"hello <\"+\"/script> ahoy\")")

(deftest pretty-print/string-literal/3 :notes pretty-print
  (let ((*escape-script-end-tags* nil))
    (pretty-string (make-string-literal :value "hello </script> ahoy")))
  "\"hello </script> ahoy\"")

(deftest pretty-print/string-literal/4 :notes pretty-print
  (pretty-string (test-parse "'\"'"))
  "\"\\\"\";")

;;;; Some round-trip tests
(deftest parse-print/round-trip/1 :notes (pretty-print parser)
  (test-parse (pretty-string (test-parse "\"string \\\"literal\\\"\"")))
  #.(test-parse "\"string \\\"literal\\\"\""))

(deftest parse-print/round-trip/2 :notes (pretty-print parser)
  (test-parse (pretty-string (test-parse "var endTag = /<\\/[^>]+>/;")))
  #.(test-parse "var endTag = /<\\/[^>]+>/;"))
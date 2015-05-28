;;;; test-cps-transformation.lisp
;;;
;;; Tests for the cps transformation
;;;
;;; Copyright (c) 2005-2006 James Wright
;;; See LICENSE for full licensing details.
;;;
(in-package :jwacs-tests)

;;;; Test categories 
(defnote cps "tests for the cps transformation")

;;;; Tests 
(deftest cps/factorial/1 :notes cps
  (with-fresh-genvar
    (test-transform 'cps (parse "
       function factorial1(n)
       {
         if(n == 0)
           return 1;
         else
         {
           var r1 = factorial1(n-1);
           return n * r1;
         }
       }")))
  (#s(function-decl
      :name "factorial1" :parameters ("$k" "n")
      :body (#s(if-statement
                :condition
                #s(binary-operator :op-symbol :equals
                                   :left-arg #s(identifier :name "n")
                                   :right-arg #s(numeric-literal :value 0))
                :then-statement
                #s(return-statement :arg
                              #s(fn-call :fn #s(identifier :name "$k")
                                         :args (#s(numeric-literal :value 1))))
                :else-statement
                #s(return-statement
                   :arg
                   #s(fn-call :fn #s(identifier :name "factorial1")
                              :args
                              (#s(function-expression
                                  :parameters ("r1")
                                  :body (#s(return-statement
                                            :arg #s(fn-call :fn #s(identifier :name "$k")
                                                            :args (#s(binary-operator :op-symbol :multiply
                                                                                      :left-arg #s(identifier :name "n")
                                                                                      :right-arg #s(identifier :name "r1")))))))
                                 #s(binary-operator :op-symbol :subtract
                                                    :left-arg #s(identifier :name "n")
                                                    :right-arg #s(numeric-literal :value 1))))))))))
(deftest cps/symmetric-dangling-tail/1 :notes cps
  (with-fresh-genvar
    (test-transform 'cps (parse "
      function doStuff(branch)
      {
        if(branch)
          foo();
        else
          bar();
        baz();
        }")))
  #.(test-parse "
      function doStuff($k, branch)
      {
        var ifK$0 = function()
        {
          return baz(function() { return $k(); });
        };
        if(branch)
          return foo(function() { resume ifK$0; });
        else
          return bar(function() { resume ifK$0; });
      }"))

(deftest cps/asymmetric-dangling-tail/1 :notes cps
  (with-fresh-genvar
    (test-transform 'cps (parse "
      function factorial2(n)
      {
        var retVal;
        if(n == 0)
          retVal = 1;
        else
        {
          var r1 = factorial2(n-1);
          var r2 = n * r1;
          retVal = r2;
        }
        return retVal;
      }")))
  #.(test-parse "
      function factorial2($k, n)
      {
        var retVal;
        var ifK$0 = function()
        {
          return $k(retVal);
        };
        if(n == 0)
        {
          retVal = 1;
          resume ifK$0;
        }
        else
          return factorial2(function (r1)
                            {
                              var r2 = n * r1;
                              retVal = r2;
                              resume ifK$0;
                            }, n-1);
      }"))
  
(deftest cps/if/unreachable-tail/1 :notes cps
  (with-fresh-genvar
    (test-transform 'cps (parse "
      function foo(userK)
      {
        if(x)
          return 40;
        else
          resume userK;
        output('this will never be seen');
      }")))
  #.(test-parse "
      function foo($k, userK)
      {
        if(x)
          return $k(40);
        else
          resume userK;
        return output(function() { return $k(); }, 'this will never be seen');
      }"))

(deftest cps/if/terminated-then-null-else/1 :notes cps
  (with-fresh-genvar
    (test-transform 'cps (parse "
      function foo(userK)
      {
        if(x)
        {
          if(y)
            return 80;
          else
            resume userK <- 80;
        }
        output('might see this');
      }")))
  #.(test-parse "
      function foo($k, userK)
      {
        if(x)
          if(y)
            return $k(80);
          else
            resume userK <- 80;
        return output(function() { return $k(); }, 'might see this');
      }"))

(deftest cps/if/terminated-then-null-else/2 :notes cps
  (with-fresh-genvar
    (test-transform 'cps (parse "
      function foo(x)
      {
        if(x)
        {
          output('yes');
          return true;
        }
        output('no');
      }")))
  #.(test-parse "
      function foo($k, x)
      {
        if(x)
          return output(function() { return $k(true); }, 'yes');
        return output(function() { return $k(); }, 'no');
      }"))


(deftest cps/if/terminated-then-unterminated-else/1 :notes cps
  (with-fresh-genvar
    (test-transform 'cps (parse "
      function foo(userK)
      {
        if(x)
        {
          if(y)
            return 80;
          else
            resume userK <- 80;
        }
        else
          output('also this');
        output('might see this');
      }")))
  #.(test-parse "
      function foo($k, userK)
      {
        if(x)
          if(y)
            return $k(80);
          else
            resume userK <- 80;
        else
          return output(function() {
                                return output(function() { return $k(); },
                                              'might see this');
                        }, 'also this');
      }"))
      
(deftest cps/if/unterminated-then-terminated-else/1 :notes cps
  (with-fresh-genvar
    (test-transform 'cps (parse "
      function foo(userK)
      {
        if(x)
          output('also this');
        else
        {
          if(y)
            return 80;
          else
            resume userK <- 80;
        }
          
        output('might see this');
      }")))
  #.(test-parse "
      function foo($k, userK)
      {
        if(x)
          return output(function() {
                                return output(function() { return $k(); },
                                              'might see this');
                        }, 'also this');
        else
          if(y)
            return $k(80);
          else
            resume userK <- 80;
      }"))     

(deftest cps/post-function-dangling-tail/1 :notes cps
  (with-fresh-genvar
    (in-local-scope
      (test-transform 'cps (parse "
      function foo(branch)
      {
        if(branch)
          return foo(false);
        else
        {
          WScript.echo('hi');
          return foo(true);
        }
      }
      foo(false);"))))
  #.(test-parse "
      function foo($k, branch)
      {
        if(branch)
          return foo($k, false);
        else
          return WScript.echo(function() { return foo($k, true); }, 'hi');
      }
      return foo(function() { return $k(); }, false);"))

(deftest cps/post-function-dangling-tail/2 :notes cps
  (with-fresh-genvar
    (test-transform 'cps (parse "
      function foo()
      {
        onEvent = function(e) { process(e); };
        bar();
      }")))
  #.(test-parse "
      function foo($k)
      {
        onEvent = function($k, e)
                  {
                    return process(function() { return $k(); }, e);
                  };
        return bar(function() { return $k(); });
      }"))

(deftest cps/no-tail-after-if/1 :notes cps
  (with-fresh-genvar
    (test-transform 'cps (parse "
      function foo(x)
      {
        if(x)
          return x * 10;
        else
          return 20;
      }")))
  #.(test-parse "
      function foo($k, x)
      {
        if(x)
          return $k(x * 10);
        else
          return $k(20);
      }"))
        
(deftest cps/switch-statement/1 :notes cps
  (with-fresh-genvar
    (test-transform 'cps (parse "
      function foo(x)
      {
        switch(x)
        {
          case 1:
            bar();
            break;

          default:
            return 88;
        }

        return baz();
      }")))
  #.(test-parse "
      function foo($k, x)
      {
        var switchK$0 = function()
        {
          return baz($k);
        };
        switch(x)
        {
          case 1:
            return bar(function() { resume switchK$0; });
          default:
            return $k(88);
        }
      }"))

(deftest cps/switch-statement/2 :notes cps
  (with-fresh-genvar
    (test-transform 'cps (parse "
      function foo(x)
      {
        switch(x)
        {
          case 10:
            bar();
            break;
        }
        return baz(x + 2);
      }")))
  #.(test-parse "
      function foo($k, x)
      {
        var switchK$0 = function()
        {
          return baz($k, x + 2);
        };
        switch(x)
        {
          case 10:
            return bar(function() { resume switchK$0; });
          default:
            resume switchK$0;
        }
      }"))

(deftest cps/switch-statement/3 :notes cps
  (with-fresh-genvar
    (test-transform 'cps (parse "
      function foo(x)
      {
        switch(x)
        {
          case 10:
            bar();
        }
        return baz(x + 2);
      }")))
  #.(test-parse "
      function foo($k, x)
      {
        var switchK$0 = function()
        {
          return baz($k, x + 2);
        };
        switch(x)
        {
          case 10:
            return bar(function() { resume switchK$0; });
          default:
            resume switchK$0;
        }
      }"))

(deftest cps/switch-statement/4 :notes cps
  (with-fresh-genvar
    (test-transform 'cps (parse "
      function foo(x)
      {
        switch(x)
        {
          case 10:
            foo();
          case 15:
          case 20:
            bar();
        }
        return baz(x - 1);
      }")))
  #.(test-parse "
      function foo($k, x)
      {
        var switchK$0 = function()
        {
          return baz($k, x - 1);
        };
        switch(x)
        {
          case 10:
            return foo(function() { return bar(function() { resume switchK$0; }); });
          case 15:
          case 20:
            return bar(function() { resume switchK$0; });
          default:
            resume switchK$0;
        }
      }"))
        
(deftest cps/labelled-switch/1 :notes cps
  (with-fresh-genvar
    (test-transform 'cps (parse "
      function foo(x, y)
      {
        outer:
        switch(x)
        {
          case 10:
          inner:
          switch(y)
          {
            case 20:
            doSomething();
            break inner;
          }
          doSomethingElse();
          break;
        }
        return doAlways();
      }")))
  #.(test-parse "
      function foo($k, x, y)
      {
        var switchK$0 = function()
        {
          return doAlways($k);
        };
        switch(x)
        {
          case 10:
            var switchK$1 = function()
            {
              return doSomethingElse(function() { resume switchK$0; });
            };
            switch(y)
            {
              case 20:
                return doSomething(function() { resume switchK$1; });
              default:
                resume switchK$1;
            }
          default:
            resume switchK$0;
        }
      }"))

(deftest cps/labelled-switch/2 :notes cps
  (with-fresh-genvar
    (test-transform 'cps (parse "
      function foo(x, y)
      {
        outer:
        switch(x)
        {
          case 10:
          inner:
          switch(y)
          {
            case 20:
            doSomething();
            break outer;
          }
          doSomethingElse();
          break;
        }
        return doAlways();
      }")))
  #.(test-parse "
      function foo($k, x, y)
      {
        var switchK$0 = function()
        {
          return doAlways($k);
        };
        switch(x)
        {
          case 10:
            var switchK$1 = function()
            {
              return doSomethingElse(function() { resume switchK$0; });
            };
            switch(y)
            {
              case 20:
                return doSomething(function() { resume switchK$0; });
              default:
                resume switchK$1;
            }
          default:
            resume switchK$0;
        }
      }"))
  
(deftest cps/simple-loop/1 :notes cps
  (with-fresh-genvar
    (in-local-scope
      (test-transform 'cps (parse "
      while(true)
      {
        if(x++ > 10)
          break;
        output(x);
        continue;
      }
      return 10;"))))
  #.(test-parse "
      var break$0 = function()
      {
        return $k(10);
      };
      var continue$1 = function()
      {
        if(x++ > 10)
          resume break$0;
        return output(function() { resume continue$1; }, x);
      };
      resume continue$1;"))

(deftest cps/nested-loop/1 :notes cps
  (with-fresh-genvar
    (in-local-scope
      (test-transform 'cps (parse "
      outer:
      while(true)
      {
        if(x++ > 10)
          break;
        inner:
        while(true)
        {
          if(y++ > 10)
            break;
          continue;
        }
        continue;
      }
      return 10;"))))
  #.(test-parse "
      var break$0 = function()
      {
        return $k(10);
      };
      var continue$1 = function()
      {
        if(x++ > 10)
          resume break$0;
        var break$2 = function() { resume continue$1; };
        var continue$3 = function()
        {
          if(y++ > 10)
            resume break$2;
          resume continue$3;
        };
        resume continue$3;
      };
      resume continue$1;"))
     
(deftest cps/nested-loop/2 :notes cps
  (with-fresh-genvar
    (in-local-scope
      (test-transform 'cps (parse "
      outer:
      while(true)
      {
        if(x++ > 10)
          break outer;
        inner:
        while(true)
        {
          if(y++ > 10)
            break inner;
          if(y % 2 == 0)
            break outer;
          continue inner;
        }
        continue outer;
      }
      return 10;"))))
  #.(test-parse "
      var break$0 = function()
      {
        return $k(10);
      };
      var continue$1 = function()
      {
        if(x++ > 10)
          resume break$0;
        var break$2 = function() { resume continue$1; };
        var continue$3 = function()
        {
          if(y++ > 10)
            resume break$2;
          if(y % 2 == 0)
            resume break$0;
          resume continue$3;
        };
        resume continue$3;
      };
      resume continue$1;"))      

(deftest cps/nested-loop/3 :notes cps
  (with-fresh-genvar
    (in-local-scope
      (test-transform 'cps (parse "
      outer:
      while(true)
      {
        if(x++ > 10)
          break outer;
        inner:
        while(true)
        {
          if(y++ > 10)
            break inner;
          if(y % 2 == 0)
            continue outer;
          continue inner;
        }
        continue outer;
      }
      return 10;"))))
  #.(test-parse "
      var break$0 = function()
      {
        return $k(10);
      };
      var continue$1 = function()
      {
        if(x++ > 10)
          resume break$0;
        var break$2 = function() { resume continue$1; };
        var continue$3 = function()
        {
          if(y++ > 10)
            resume break$2;
          if(y % 2 == 0)
            resume continue$1;
          resume continue$3;
        };
        resume continue$3;
      };
      resume continue$1;"))

(deftest cps/tail-fn-call/1 :notes cps
  (with-fresh-genvar
    (in-local-scope
      (test-transform 'cps (parse "
        return factorial(JW0);"))))
  #.(test-parse "
      return factorial($k, JW0);"))

(deftest cps/inline-call-with-tail/1 :notes cps
  (with-fresh-genvar
    (test-transform 'cps (parse "
      function foo() {}
      function bar()
      {
        foo();
        x += 10;
      }")))
  #.(test-parse "
      function foo($k) { return $k(); }
      function bar($k)
      {
        return foo(function () {
                     x += 10;
                     return $k();
                   });
      }"))

(deftest cps/object-literal/1 :notes cps
  (with-fresh-genvar
    (test-transform 'cps (parse "
      var obj =
      {
        field: 44, 
        method: function()
        {
          var x = factorial(this.field);
          return x * 2;
        }
      };
      function fn()
      {
        var y = obj.field + 1;
        obj.method(y);
      }")))
  #.(test-parse "
      var obj =
      {
        field: 44,
        method: function ($k)
        {
          return factorial(function(x) {
                             return $k(x * 2);
                           }, this.field);
        }
      };
      function fn($k)
      {
        var y = obj.field + 1;
        return obj.method(function() { return $k(); }, y);
      }"))

(deftest cps/implicit-return/1 :notes cps
  (test-transform 'cps (parse "
    function foo()
    {
    }"))
  #.(test-parse "
    function foo($k)
    {
      return $k();
    }"))

(deftest cps/implicit-return/2 :notes cps
  (test-transform 'cps (parse "
    function foo()
    {
      x = 10;
    }"))
  #.(test-parse "
    function foo($k)
    {
      x = 10;
      return $k();
    }"))

(deftest cps/implicit-return/3 :notes cps
  (with-fresh-genvar
    (test-transform 'cps (parse "
    function foo()
    {
      if(x)
        bar();
      else
        y = 10;
      z = 20;
    }")))
  #.(test-parse "
    function foo($k)
    {
      var ifK$0 = function()
      {
        z = 20;
        return $k();
      };
      if(x)
        return bar(function() { resume ifK$0; });
      else
      {
        y = 10;
        resume ifK$0;
      }
    }"))

;; `suspend` and `resume` are handled by the TRAMPOLINE transformation.
;; Capturing the current function's current continuation is handled by
;; the CPS transformation.
(deftest cps/function_continuation/1 :notes cps
  (in-local-scope
    (test-transform 'cps
                    (parse "x = function_continuation;")))
  #.(test-parse "x = $k;"))

(deftest cps/void-new-expr-with-tail/1 :notes cps
  (in-local-scope
    (test-transform 'cps
                    (parse "
        new Foo;
        bar(10);")))
  #.(test-parse "
        return new Foo(function() { return bar(function() { return $k(); }, 10); });"))

(deftest cps/void-new-expr-with-tail/2 :notes cps
  (test-transform 'cps
                  (parse "
        new Foo;
        bar(10);"))
  #.(test-parse "
        return new Foo(function() { return bar(function() { suspend; }, 10); });"))

(deftest cps/new-expr-with-tail/1 :notes cps
  (in-local-scope
    (test-transform 'cps
                    (parse "
        var x = new Foo(50);
        bar(x[10]);")))
  #.(test-parse "
        return new Foo(function(x) { return bar(function() { return $k();}, x[10]); }, 50);"))

(deftest cps/new-expr-with-tail/2 :notes cps
  (test-transform 'cps
                  (parse "
        var x = new Foo(50);
        bar(x[10]);"))
  #.(test-parse "
        return new Foo(function(x) { return bar(function() { suspend; }, x[10]); }, 50);"))

(deftest cps/new-expr-tail-return/1 :notes cps
  (test-transform 'cps
                  (parse "
      function foo()
      {
        return new bar(50);
      }"))
  #.(test-parse "
      function foo($k)
      {
        return new bar($k, 50);
      }"))

(deftest cps/new-expr-tail-return/2 :notes cps
  (test-transform 'cps
                  (parse "
      function foo()
      {
        return new (function(n) { return n; }) (50);
      }"))
  #.(test-parse "
      function foo($k)
      {
        return new (function($k, n) { return $k(n); }) ($k, 50);
      }"))

(deftest cps/void-function-call-with-tail/1 :notes cps
  (in-local-scope
    (test-transform 'cps
                    (parse "
        foo(10);
        bar(20);")))
  #.(test-parse "
        return foo(function() { return bar(function() { return $k(); }, 20); }, 10);"))

(deftest cps/void-function-call-with-tail/2 :notes cps
  (test-transform 'cps
                  (parse "
        foo(10);
        bar(20);"))
  #.(test-parse "
        return foo(function() { return bar(function() { suspend; }, 20); }, 10);"))

(deftest cps/toplevel-function-call-with-tail/1 :notes cps
  (test-transform 'cps
                  (parse "
        var x = foo(10);
        bar(x);"))
  #.(test-parse "
        return foo(function(x) { return bar(function() { suspend; }, x); }, 10);"))

(deftest cps/strip-var-decls/function-decl/1 :notes cps
  (with-fresh-genvar
    (test-transform 'cps (parse "
      function foo(narf)
      {
        if(narf)
          var intermediate = narf.charAt(0); 
        if(narf && intermediate == 'f')
          foo('m');
      }")))
  #.(test-parse "
      function foo($k, narf)
      {
        var intermediate;
        var ifK$0 = function() {
          var ifK$1 = function() {
            return $k();
          };
          if(narf && intermediate == 'f')
            return foo(function() { resume ifK$1; }, 'm');
          else
            resume ifK$1;
        };

        if(narf)
          return narf.charAt(function(JW2) { intermediate = JW2; resume ifK$0; }, 0);
        else
          resume ifK$0;
      }"))

(deftest cps/strip-var-decls/function-decl/2 :notes cps
  (with-fresh-genvar
    (test-transform 'cps (parse "
      if(monthOK)
      {
        var JW74 = day < 1;
        if(!JW74)
          var JW73 = lastDay();
      }
      if(monthOK && (JW74 || day > JW73))
        resume foo;")))
  #.(test-parse "
      var JW74;
      var JW73;
      var ifK$0 = function()
      {
        if(monthOK && (JW74 || day > JW73))
          resume foo;
        suspend;
      };
      if(monthOK)
      {
        JW74 = day < 1;
        var ifK$2 = function()
        {
          resume ifK$0;
        };
        if(!JW74)
          return lastDay(function(JW3)
          {
            JW73 = JW3;
            resume ifK$2;
          });
        else
          resume ifK$2;
      }
      else
        resume ifK$0;"))   

(deftest cps/strip-var-decls/new-expr/1 :notes cps
  (with-fresh-genvar
    (test-transform 'cps (parse "
      function foo()
      {
        try
        {
          var http = new XMLHttpRequest;
          if(http) return http;
        }
        catch(e) { }

        try
        {
          var handExplicitize = new ActiveXObject('Microsoft.XML');
          http = handExplicitize;
          if(http) return http;
        }
        catch(e) { }
      }")))
  #.(test-parse "
      function foo($k)
      {
        var http;
        var tryK$1 = function()
        {
          var tryK$3 = function()
          {
            return $k();
          };
          var catchK$2 = function(e)
          {
            resume tryK$3;
          };
          $addHandler(catchK$2, function() {
            return new ActiveXObject(function(handExplicitize)
            {
              http = handExplicitize;
              if(http)
                $removeHandler(catchK$2, function()
                {
                  return $k(http);
                });
              $removeHandler(catchK$2, function()
              {
                resume tryK$3;
              });
            }, 'Microsoft.XML');
          });
        };
        var catchK$0 = function(e)
        {
          resume tryK$1;
        };
        $addHandler(catchK$0, function() {
          return new XMLHttpRequest(function(JW4)
          {
            http = JW4;
            if(http)
              $removeHandler(catchK$0, function()
              {
                return $k(http);
              });
            $removeHandler(catchK$0, function()
            {
              resume tryK$1;
            });
          });
        });
      }"))



(deftest cps/strip-var-decls/function-expression/1 :notes cps
  (with-fresh-genvar
    (test-transform 'cps (parse "
      var foo = function(narf)
      {
        if(narf)
          var intermediate = narf.charAt(0); 
        if(narf && intermediate == 'f')
          foo('m');
      };")))
  #.(test-parse "
      var foo = function($k, narf)
      {
        var intermediate;
        var ifK$0 = function() {
          var ifK$1 = function() {
            return $k();
          };
          if(narf && intermediate == 'f')
            return foo(function() { resume ifK$1; }, 'm');
          else
            resume ifK$1;
        };

        if(narf)
          return narf.charAt(function(JW2) { intermediate = JW2; resume ifK$0; }, 0);
        else
          resume ifK$0;
      };"))

(deftest cps/strip-var-decls/function-expression/2 :notes cps
  (with-fresh-genvar
    (test-transform 'cps (parse "
      function bar()
      {
        var foo = function(narf)
        {
          if(narf)
            var intermediate = narf.charAt(0); 
          if(narf && intermediate == 'f')
            foo('m');
        };
      }")))
  #.(test-parse "
      function bar($k)
      {
        var foo = function($k, narf)
        {
          var intermediate;
          var ifK$0 = function() {
            var ifK$1 = function() {
              return $k();
            };
            if(narf && intermediate == 'f')
              return foo(function() { resume ifK$1; }, 'm');
            else
              resume ifK$1;
          };

          if(narf)
            return narf.charAt(function(JW2) { intermediate = JW2; resume ifK$0; }, 0);
          else
            resume ifK$0;
        };
        return $k();
      }"))

(deftest cps/strip-var-decls/switch/1 :notes cps
  (with-fresh-genvar
    (test-transform 'cps (transform 'explicitize (parse "
      function foo()
      {
        switch(narf && narf.charAt(0))
        {
          case 'a':
          global = 10;
          break;
          default:
          global = 20;
        }
        bar();
      }"))))
  #.(test-parse "
      function foo($k)
      {
        var JW0;
        var ifK$1 = function() {
          var switchK$2 = function() {
            return bar(function() {
                return $k();
            });
          };
          switch(narf && JW0)
          {
            case 'a':
            global = 10;
            resume switchK$2;
            default:
            global = 20;
            resume switchK$2;
          }
        };
        if(narf)
          return narf.charAt(function(JW3) {
            JW0 = JW3;
            resume ifK$1;
          }, 0);
        else
          resume ifK$1;
      }"))
            
(deftest cps/strip-var-decls/while/1 :notes cps
  (with-fresh-genvar
    (test-transform 'cps (transform 'explicitize (parse "
      function loopy()
      {
        var x = 0;
        while(true)
        {
          if(!(foo(x) && bar(x)))
            break;
          x++;
          continue;
        }
        return baz();
      }"))))
  #.(test-parse "
      function loopy($k)
      {
        var JW1;
        var x = 0;
        var break$2 = function() {
          return baz($k);
        };
        var continue$3 = function() {
          return foo(function(JW0) {
            var ifK$4 = function() {
              if(!(JW0 && JW1))
                resume break$2;
              x++;
              resume continue$3;
            };
            if(JW0)
              return bar(function(JW5) {
                JW1 = JW5;
                resume ifK$4;
              }, x);
            else
              resume ifK$4;
          }, x);
        };
        resume continue$3;
      }"))

(deftest cps/strip-var-decls/toplevel/1 :notes cps
  (with-fresh-genvar
    (test-transform 'cps (parse "
      if(narf)
        var intermediate = narf_charAt(0); 
      if(narf && intermediate == 'f')
        foo('m');")))
  #.(test-parse "
      var intermediate;
      var ifK$0 = function() {
        var ifK$1 = function() {
          suspend;
        };

        if(narf && intermediate == 'f')
          return foo(function() { resume ifK$1; }, 'm');
        else
          resume ifK$1;
      };

      if(narf)
        return narf_charAt(function(JW2) { intermediate = JW2; resume ifK$0; }, 0);
      else
        resume ifK$0;"))

(deftest cps/find-free-variables/1 :notes cps
  (find-free-variables (parse "
      x = 10;
      x += 2;
      var x;
      function foo(y)
      {
        if(y)
          foo(10);
        else
          bar(y);
      }"))
  ("bar"))

(deftest cps/find-free-variables/2 :notes cps
  (sort 
   (find-free-variables (parse "
      x = 10;
      x += 2;
      var x = JW0 && x;
      function foo(y)
      {
        var JW0 = baz(x);
        if(x)
          foo(y);
        else
          bar(y);
      }"))
   #'string<)
  ("JW0" "bar" "baz"))

(deftest cps/try-catch/1 :notes cps
  (with-fresh-genvar
    (test-transform 'cps (parse "
      function boo()
      {
        try
        {
          foo();
          bar();
        }
        catch(e)
        {
          baz();
        }
        return quux();
      }")))
  #.(test-parse "
      function boo($k)
      {
        var tryK$1 = function() {
          return quux($k);
        };
        var catchK$0 = function(e) {
          return baz(function() {
            resume tryK$1;
          });
        };
        $addHandler(catchK$0, function() {
          return foo(function() {
            return bar(function() {
              $removeHandler(catchK$0, function() {
                resume tryK$1;
              });
            });
          });
        });
      }"))

(deftest cps/try-catch/2 :notes cps
  (with-fresh-genvar
    (test-transform 'cps (parse "
      function boo()
      {
        try
        {
          foo();
          var x = bar();
          if(x)
            return x;
        }
        catch(e)
        {
          var y = baz();
          if(y)
            return y;
        }
        return quux();
      }")))
  #.(test-parse "
      function boo($k)
      {
        var tryK$1 = function() {
          return quux($k);
        };
        var catchK$0 = function(e) {
          return baz(function(y) {
            if(y)
              return $k(y);
            resume tryK$1;
          });
        };
        $addHandler(catchK$0, function() {
          return foo(function() {
            return bar(function(x) {
              if(x)
                $removeHandler(catchK$0, function() {
                  return $k(x);
                });
              $removeHandler(catchK$0, function() {
                resume tryK$1;
              });
            });
          });
        });
      }"))

(deftest cps/try-catch/3 :notes cps
  (with-fresh-genvar
    (test-transform 'cps (parse "
      function boo()
      {
        try
        {
          foo();
          var x = bar();
          if(x)
            return x;
        }
        catch(e)
        {
          return baz();
        }
        return quux();
      }")))
  #.(test-parse "
      function boo($k)
      {
        var catchK$0 = function(e) {
          return baz($k);
        };
        $addHandler(catchK$0, function() {
          return foo(function() {
            return bar(function(x) {
              if(x)
                $removeHandler(catchK$0, function() {
                  return $k(x);
                });
              $removeHandler(catchK$0, function() {
                return quux($k);
              });
            });
          });
        });
      }"))

(deftest cps/try-catch/4 :notes cps
  (with-fresh-genvar
    (test-transform 'cps (parse "
      function boo()
      {
        try
        {
          foo();
          try
          {
            var y = bar();
            if(y)
              return null;
            narf = 10;
          }
          catch(x)
          {
            throw 60;
          }
        }
        catch(e)
        {
          baz();
        }
        return quux();
      }")))
  #.(test-parse "
      function boo($k)
      {
        var tryK$1 = function() {
          return quux($k);
        };
        var catchK$0 = function(e) {
          return baz(function() {
            resume tryK$1;
          });
        };
        $addHandler(catchK$0, function() {
          return foo(function() {
            var catchK$2 = function(x) {
              throw 60;
            };
            $addHandler(catchK$2, function() {
              return bar(function(y) {
                if(y)
                  $removeHandler(catchK$2, function() {
                    $removeHandler(catchK$0, function() {
                      return $k(null);
                    });
                  });
                narf = 10;
                $removeHandler(catchK$2, function() {
                  $removeHandler(catchK$0, function() {
                    resume tryK$1;
                  });
                });
              });
            });
          });
        });
      }"))

(deftest cps/try-catch/5 :notes cps
  (with-fresh-genvar
    (test-transform 'cps (parse "
      function boo()
      {
        outer:
        while(true)
        {
          if(!go)
            break outer;
          try
          {
            inner:
            while(true)
            {
              if(!go2)
                break inner;
 
              go--;
              if(go == 10)
                continue;
              else
                continue outer;
              continue inner;
            }
          }
          catch(e)
          {
            return bar(25);
          }
          continue outer;
        }
      }")))
  #.(test-parse "
      function boo($k)
      {
        var break$0 = function() {
          return $k();
        };
        var continue$1 = function() {
          if(!go)
            resume break$0;
          var catchK$2 = function(e) {
            return bar($k, 25);
          };
          $addHandler(catchK$2, function() {
            var break$3 = function() {
              $removeHandler(catchK$2, function() {
                resume continue$1;
              });
            };
            var continue$4 = function() {
              if(!go2)
                resume break$3;
              go--;
              if(go == 10)
                resume continue$4;
              else
                resume continue$1;
              resume continue$4;
            };
            resume continue$4;
          });
        };
        resume continue$1;
      }"))

(deftest cps/try-catch/6 :notes cps
  (with-fresh-genvar
    (test-transform 'cps (parse "
     function boo(x)
     {
       try
       {
         if(x)
           return bar(20);
       }
       catch(e)
       {
         return null;
       }
       return null;
     }")))
  #.(test-parse "
     function boo($k, x)
     {
       var catchK$0 = function(e) {
         return $k(null);
       };
       $addHandler(catchK$0, function() {
         if(x)
           return bar(function(JW1) {
             $removeHandler(catchK$0, function() {
               return $k(JW1);
             });
           }, 20);
         $removeHandler(catchK$0, function() {
           return $k(null);
         });
       });
     }"))

(deftest cps/try-catch/7 :notes cps
  (with-fresh-genvar
    (test-transform 'cps (parse "
     function boo(x)
     {
       try
       {
         if(x)
           return bar(20);
         else
           return null;
       }
       catch(e)
       {
         return null;
       }
     }")))
  #.(test-parse "
     function boo($k, x)
     {
       var catchK$0 = function(e) {
         return $k(null);
       };
       $addHandler(catchK$0, function() {
         if(x)
           return bar(function(JW1) {
             $removeHandler(catchK$0, function() {
               return $k(JW1);
             });
           }, 20);
         else
           $removeHandler(catchK$0, function() {
             return $k(null);
           });
       });
     }"))

(deftest cps/try-catch/8 :notes cps
  (with-fresh-genvar
    (test-transform 'cps (parse "
      function foo()
      {
        try
        {
          try
          {
            return bar(10);
          }
          catch(e)
          {
            throw e;
          }
        }
        catch(x)
        {
          return x;
        }
      }")))
  #.(test-parse "
      function foo($k)
      {
        var catchK$0 = function(x) {
          return $k(x);
        };

        $addHandler(catchK$0, function() {
          var catchK$1 = function(e) {
            throw e;
          };
          $addHandler(catchK$1, function() {
            return bar(function(JW2) {
              $removeHandler(catchK$1, function() {
                $removeHandler(catchK$0, function() {
                  return $k(JW2);
                });
              });
            }, 10);
          });
        });
      }"))

(deftest cps/position-preservation/1 :notes cps
  (with-fresh-genvar
    (transform 'cps (parse "function foo() { var x = bar(); baz(); }")))
  (#s(function-decl :name "foo" :parameters ("$k")
                    :body (#s(return-statement :arg #s(fn-call :start 25 :end 28 :fn #s(identifier :start 25 :end 28 :name "bar")
                                                               :args (#s(continuation-function :parameters ("x")
                                                                                               :body (#s(return-statement :arg #s(fn-call :start 32 :end 35 :fn #s(identifier :start 32 :end 35 :name "baz")
                                                                                                                                          :args (#s(continuation-function :body (#s(return-statement :arg #s(fn-call :fn #s(identifier :name "$k") :args nil))))))
                                                                                                                          :start 32 :end 35)))))
                                               :start 25 :end 28))
                    :start 0 :end 35)))

(deftest cps/position-preservation/2 :notes cps
  (with-fresh-genvar
    (transform 'cps (parse "function foo() { bar(); return baz(); }")))
  (#s(function-decl :start 0 :end 37 :name "foo" :parameters ("$k")
                    :body (#s(return-statement :arg #s(fn-call :start 17 :end 20 :fn #s(identifier :start 17 :end 20 :name "bar")
                                                               :args (#s(continuation-function :body (#s(return-statement :start 24 :end 37
                                                                                                                          :arg #s(fn-call :fn #s(identifier :start 31 :end 34 :name "baz")
                                                                                                                                          :args (#s(identifier :name "$k"))))))))
                                               :start 17 :end 20)))))

(deftest cps/simple-function-expression/1 :notes cps
  (with-fresh-genvar
    (test-transform 'cps (parse "x = function() { return baz(); }")))
  #.(test-parse "x = function($k) { return baz($k); }"))

(deftest cps/function-expression-as-fn-arg/1 :notes cps
  (with-fresh-genvar
    (test-transform 'cps (parse "
      var JW0 = bar(function()
      {
        return baz();
      });
      foo = JW0")))
  #.(test-parse "
      return bar( function(JW0) {
        foo = JW0;
        suspend;
      }, function($k) { return baz($k); });"))

(deftest cps/function-expression-as-new-expr-arg/1 :notes cps
  (with-fresh-genvar
    (test-transform 'cps (parse "
      var JW0 = new bar(function()
      {
        return baz();
      });
      foo = JW0")))
  #.(test-parse "
      return new bar( function(JW0) {
        foo = JW0;
        suspend;
      }, function($k) { return baz($k); });"))
      

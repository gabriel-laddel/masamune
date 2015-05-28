;;;; test-shift-function-decls.lisp
;;;
;;; Tests for the shift-decls transformation
;;;
;;; Copyright (c) 2005 James Wright
;;; See LICENSE for full licensing details.
;;;
(in-package :jwacs-tests)

(defnote shift-decls "Tests for the shift-decls transformation")

(deftest shift-decls/1 :notes shift-decls
  (transform 'shift-decls (test-parse "
    var global1, global2 = 20;
    WScript.echo(global2);
    function foo()
    {
      bar();
      var a = 10;
      function inner(h)
      {
        return h * 10;
      }
      var b = 20;
    }
    var global3 = /h/g;
    function bar() { return -88; }"))
  #.(test-parse "
    var global1;
    var global2;
    var global3;
    function foo()
    {
      function inner(h)
      {
        return h * 10;
      }
      bar();
      var a = 10;
      var b = 20;
    }
    function bar() { return -88; }
    global2 = 20;
    WScript.echo(global2);
    global3 = /h/g;"))


(deftest shift-decls/2 :notes shift-decls
  (transform 'shift-decls (test-parse "
    function foo()
    {
      var a = 1;
      if(x)
      {
        var b;
        var fex = function functionExpression() // Doesn't move; only function decls move
        {
          return inner(2);
          function inner(arg) { return arg; } // Moves up within functionExpression's body only
        };
        var c = 100;
      }
    }"))
  #.(test-parse "
    function foo()
    {
      var a = 1;
      if(x)
      {
        var b;
        var fex = function functionExpression()
        {
          function inner(arg) { return arg; }
          return inner(2);
        };
        var c = 100;
      }
    }"))

(deftest shift-decls/3 :notes shift-decls
  (transform 'shift-decls (test-parse "
      var obj = { field: 44, method: function() { return this.field * 2; }};
      function fn()
      {
        obj.method(obj.field);
      }"))
  #.(test-parse "
      var obj;
      function fn()
      {
        obj.method(obj.field);
      }
      obj = { field: 44, method: function() { return this.field * 2; }};"))

(deftest shift-decls/nested-var-decls/1 :notes shift-decls
  (transform 'shift-decls (test-parse "
      foo();
      var x = 10;
      try
      {
        var y = 20;
      }
      catch(e)
      {
        bar(e);
      }"))
  #.(test-parse "
      var x;
      var y;
      foo();
      x = 10;
      try
      {
        y = 20;
      }
      catch(e)
      {
        bar(e);
      }"))

(deftest shift-decls/nested-var-decls/2 :notes shift-decls
  (transform 'shift-decls (test-parse "
      foo();
      var x = 10;
      try
      {
        var y = 20;
      }
      catch(e)
      {
        var z;
        bar(e);
      }"))
  #.(test-parse "
      var x;
      var y;
      var z;
      foo();
      x = 10;
      try
      {
        y = 20;
      }
      catch(e)
      {
        bar(e);
      }"))

(deftest shift-decls/position-preservation/1 :notes shift-decls
  (transform 'shift-decls (parse "foo(); var x = bar();"))
  (#s(var-decl-statement :var-decls (#s(var-decl :name "x")))
   #s(fn-call :fn #s(identifier :name "foo" :start 0 :end 3)
              :args nil
              :start 0 :end 3)
   #s(binary-operator :left-arg #s(identifier :name "x")
                      :op-symbol :assign
                      :right-arg #s(fn-call :fn #s(identifier :name "bar" :start 15 :end 18)
                                            :args nil
                                            :start 15 :end 18)
                      :start 11 :end 18)))
;;;; test-source-transformation.lisp
;;;
;;; Tests for the source transformations.
;;;
;;; Copyright (c) 2005 James Wright
;;; See LICENSE for full licensing details.
;;;
(in-package :jwacs-tests)

;;;; Test categories 
(defnote source-transformations "tests for the source-transformations")
    
;;;; Tests 

;;;; General traversal behaviour 
(defmethod transform ((xform (eql 'hello)) (elm string))
    "hello there!")

(defmethod transform ((xform (eql 'hello)) (elm break-statement))
  (list (make-break-statement :target-label "break out!")))

(deftest source-transformations/general-behaviour/1 :notes source-transformations
  (transform 'hello #S(continue-statement :target-label "go away!"))
  #S(continue-statement :target-label "hello there!"))

(deftest source-transformations/general-behaviour/2 :notes source-transformations
  (transform 'hello '("string 1" symbol ("string 2")))
  ("hello there!" symbol ("hello there!")))

(deftest source-transformations/flattenning-behaviour/1 :notes source-transformations
  (transform 'hello '(#S(continue-statement :target-label "beat it!")
                      #S(break-statement)))
  (#S(continue-statement :target-label "hello there!")
   #S(break-statement :target-label "break out!")))
                      
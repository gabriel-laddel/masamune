;;;; test-utils.lisp
;;;
;;; Some helper functions for testing in general (i.e., that are
;;; not specific to testing a certain file).
;;;
;;; Copyright (c) 2005 James Wright
;;; See LICENSE for full licensing details.
;;;
(in-package :js-parser-tests)

(defun flag-expected-failure (test-name)
  "Add a test to the list of expected failures"
  (pushnew test-name rtest::*expected-failures*))

(defmacro expect-error (form &optional (condition-type 'error))
  "If evaluating FORM results in an error being raised, returns non-NIL.
   If the CONDITION-TYPE argument is provided, non-NIL is raised only if
   an error of that specific type is raised."
  (let ((gret (gensym))
        (gerr (gensym)))
    `(multiple-value-bind (,gret ,gerr)
        (ignore-errors ,form)
      (declare (ignore ,gret))
      (or (typep ,gerr ',condition-type)
          ,gerr))))

;;; The REMOVE-POSITIONS transformation strips source positions from a source
;;; tree.  TEST-PARSE and TEST-TRANSFORM both use it so that we can check
;;; generated Javascript code in unit tests without having to ensure that we
;;; provide the correct source positions (which is in many cases impossible,
;;; since transformation often moves source elements to different parts of the
;;; code while preserving their original position information).
(defmethod transform ((xform (eql 'remove-positions)) (elm source-element))
  (declare (optimize debug))
  (apply
   (get-constructor elm)
   (loop for slot in (structure-slots elm)
         collect (make-keyword slot)
         collect (if (or (eq slot 'js-parser::start) (eq slot 'js-parser::end))
                     nil
		     (let ((slot-value (slot-value elm slot)))
		       (transform xform slot-value))))))

(defmethod transform ((xform (eql 'remove-positions)) (elm string))
  elm)

(defmethod transform ((xform (eql 'remove-positions)) (elm cons))
  (cons (transform xform (car elm))
	(transform xform (cdr elm))))

(defmethod transform ((xform (eql 'remove-positions)) (elm symbol))
  elm)

(defmethod transform ((xform (eql 'remove-positions)) (elm number))
  elm)

(defmethod transform ((xform (eql 'remove-positions)) (elm (eql nil)))
  nil)

(defmethod transform ((xform (eql 'remove-positions)) (elm object-literal))
  (make-object-literal
   :properties
   (loop for (prop-name . prop-value) in (js-parser::object-literal-properties elm)
         collect (cons
                  (transform xform prop-name)
                  (transform xform prop-value)))))

(defun test-parse (str)
  "Parse STR into a source model representation that does not include source positions"
  (let ((elm (parse str)))
    (transform 'remove-positions elm)))

;    (transform 'remove-positions (car elm))))

;;; The REMOVE-ADMINISTRATIVES transformation translates administrative
;;; source-elements (such as CONTINUATION-FUNCTIONs) to their non-administrative
;;; equivalents (eg FUNCTION-EXPRESSION).  This transformation is used by the
;;; TEST-TRANSFORM function to ensure that the results of a transformation are
;;; the same as what would be parsed from their pretty-printed representation
;;; (so that we can write unit tests by providing JWACS code instead of ASTs).
(defmethod transform ((xform (eql 'remove-administratives)) (elm thunk-function))
  (make-function-expression :name (js-parser::function-expression-name elm)
                            :parameters (js-parser::function-expression-parameters elm)
                            :body (transform xform (js-parser::function-expression-body elm))))

(defmethod transform ((xform (eql 'remove-administratives)) (elm continuation-function))
  (make-function-expression :name (js-parser::function-expression-name elm)
                            :parameters (js-parser::function-expression-parameters elm)
                            :body (transform xform (js-parser::function-expression-body elm))))

(defmethod transform ((xform (eql 'remove-administratives)) (elm continuation-call))
  (make-fn-call :fn (transform xform (js-parser::fn-call-fn elm))
                :args (transform xform (js-parser::fn-call-args elm))))

(defmethod transform ((xform (eql 'remove-administratives)) (elm special-value))
  (if (eq :arguments (js-parser::special-value-symbol elm))
    (make-identifier :name js-parser::*arguments-name*)
    (call-next-method)))

(defmethod transform ((xform (eql 'remove-administratives)) (elm add-handler))
  (make-fn-call :fn (make-identifier :name "$addHandler")
                :args (list (transform xform (js-parser::add-handler-handler elm))
                            (transform xform (js-parser::make-function-expression
                                              :body (js-parser::add-handler-thunk-body elm))))))

(defmethod transform ((xform (eql 'remove-administratives)) (elm remove-handler))
  (make-fn-call :fn (make-identifier :name "$removeHandler")
                :args (list (transform xform (js-parser::remove-handler-handler elm))
                            (transform xform (js-parser::make-function-expression
                                              :body (js-parser::remove-handler-thunk-body elm))))))

(defun test-transform (xform elm)
  "Return the results of applying XFORM to ELM with any administrative source-elements
   converted to their non-administrative equivalents and with source positions removed."
  (transform 'remove-administratives
             (transform 'remove-positions
                        (transform xform elm))))

;;; compilation helpers

(defun compile-lang-tests (&key debug-mode)
  "Compile the language tests"
  (let* ((js-parser::*debug-mode* debug-mode)
         (module (asdf:find-component (asdf:find-system :jwacs-tests) "tests"))
         (component (asdf:find-component module "lang-tests")))
    (js-parser::build-app (asdf:component-pathname component))))

#+nil
(defun compile-examples (&key (compress-mode t) (combine-mode t))
  "Compiles all the examples"
  (let* ((sys-pathname (truename (asdf:system-definition-pathname (asdf:find-system :jwacs))))
         (lib-pathname (merge-pathnames (make-pathname :directory '(:relative "lib")
                                                       :name :unspecific :type :unspecific :version :unspecific)
                                        sys-pathname))
         (examples-pathname (merge-pathnames (make-pathname :directory '(:relative "examples")
                                                            :name :unspecific :type :unspecific :version :unspecific)
                                        sys-pathname)))
    (flet ((build-ex (name)
             (js-parser:build-app (merge-pathnames name examples-pathname)
                           :prefix-lookup `(("/lib/" . ,lib-pathname))
                           :compress-mode compress-mode
                           :combine-mode combine-mode)))

      (list
       (build-ex "CalendarMark2.jw")
       (build-ex "Counter.jw")
       (build-ex "TrivialHttpRequest.jw")))))

;;TODO Automated benchmarks?
;;TODO Randomized testing?  (a la Quickcheck)
;;TODO Coverage checks?

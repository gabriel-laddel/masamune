;;;; jwacs.asd
;;;
;;; This is the system definition file for the jwacs project.
;;; It defines the asdf system plus any extra asdf operations
;;; (eg test-op).

(defpackage :js-parser-system
  (:use :cl :asdf)
  (:export
   #:*version*
   #:*executable-name*))

(in-package :js-parser-system)

;;;; ======= Build parameters ======================================================================

(defparameter *version* "alpha2"
  "The current version")

;;;; ======= Compilation configuration =============================================================
(defparameter *muffle-conflicts* nil
  "When T, yacc warnings about Shift/Reduce and Reduce/Reduce conflicts will be muffled.
   When NIL, all such conflicts will be reported.
   When non-NIL, non-T, a single summary warning will be reported when conflicts exist.

   This value should be set to NIL or non-T during grammar
   development/debugging (so that we find out about the conflicts), but T
   at all other times (so that SBCL won't drop into the debugger when
   we're trying to load parse-javascript.lisp).")

;;;; ======= System definition =====================================================================
(asdf:defsystem js-parser 
  :version *version*
  :author "Red Daly"
  :licence "MIT License <http://www.opensource.org/licenses/mit-license.php>"
  :serial t
  :components ((:module "external"
                        :components
                        ((:file "yacc")))
	       (:module
		"src"
		:serial t
		:components
		((:file "package")
		 (:file "general-utilities")
		 (:file "conditions")
		 (:file "lexer-macros")
		 (:file "lex-javascript")
		 (:file "js-source-model")
		 (:file "parse-javascript-yacc")
		 (:file "parse-javascript")
		 (:file "pretty-print")
		 (:file "source-transformations")
		 (:file "shift-decls-transformation")
		 (:file "ugly-print"))))
;		 (:file "static-analysis")
;		 (:file "type-analysis")
;		 (:file "explicitize-transformation")
;		 (:file "shadow-values-transformation")
;		 (:file "cps-transformation")
;		 (:file "loop-transformation")
;		 (:file "trampoline-transformation")
;		 (:file "runtime-transformation")
;		 (:file "compiler")
;		 (:file "js-to-parenscript"))))
;		 #+(or sbcl lispworks) (:file "main"))
  :depends-on (cl-ppcre))

;;;; ======= Test operation ========================================================================
(defmethod perform ((o test-op) (c (eql (find-system 'js-parser))))
  (operate 'load-op 'js-parser-tests)
  (operate 'test-op 'js-parser-tests))

(defmethod operation-done-p ((o test-op) (c (eql (find-system 'js-parser-tests))))
  nil)
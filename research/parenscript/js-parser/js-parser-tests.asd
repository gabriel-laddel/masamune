;;;; jwacs-tests.asd
;;;
;;; Defines an asdf system containing unit tests for jwacs.

(defpackage :js-parser-tests-system
  (:use :cl :asdf))
(in-package :js-parser-tests-system)

;;;; ======= System definition =====================================================================
(asdf:defsystem js-parser-tests
    :version "0.1"
    :author "Red Daly <reddaly@gmail.com>"
    :licence "BSD License <http://www.opensource.org/licenses/bsd-license.php>"
    :serial t
    :components
    ((:module "external"
              :components
              ((:file "rt")))
     (:module "tests"
              :serial t
              :components
              ((:file "package")
               (:file "test-utils")
               (:file "test-lexer")
               (:file "test-parser")
;               (:file "test-pretty-print")
;               (:file "test-ugly-print")
;               (:file "test-static-analysis")
;               (:file "test-type-analysis")

;               (:file "test-source-transformations")
;               (:file "test-shift-decls-transformation")
;               (:file "test-explicitize")
;               (:file "test-shadow-values-transformation")
;               (:file "test-cps-transformation")
;               (:file "test-loop-transformation")
;               (:file "test-trampoline-transformation")
;               (:file "test-runtime-transformation")
;               (:js-parser-file "lang-tests"))))
	       )))
    :depends-on (js-parser))

;;;; ======= Test operation ========================================================================
(defmethod perform ((o test-op) (c (eql (find-system 'js-parser-tests))))
  (operate 'load-op :js-parser)
  (funcall (intern (symbol-name '#:do-tests) (find-package :js-parser-tests))))

(defmethod operation-done-p ((o test-op) (c (eql (find-system 'js-parser-tests))))
  nil)
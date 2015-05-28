(in-package #:climi)

;;; I want to be able to look at any one of these, see if it has been changed,
;;; tested, what level of optimization, how much development is going on etc.
;;; test them and run the applicable operations:

;; asdf:BASIC-COMPILE-BUNDLE-OP
;; asdf:BINARY-OP
;; asdf:BUILD-OP
;; asdf:BUNDLE-OP
;; asdf:COMPILE-BUNDLE-OP
;; asdf:COMPILE-CONCATENATED-SOURCE-OP
;; asdf:COMPILE-SYSTEM
;; asdf:COMPONENT-NAME
;; asdf:DEFSYSTEM
;; asdf:DELIVER-ASD-OP
;; asdf:EXPLAIN
;; asdf:LOAD-COMPILED-CONCATENATED-SOURCE-OP
;; asdf:LOAD-CONCATENATED-SOURCE-OP
;; asdf:LOCATE-SYSTEM
;; asdf:OPERATE
;; asdf:PARENT-COMPONENT
;; asdf:SYSTEM
;; asdf:SYSTEM-DEFSYSTEM-DEPENDS-ON
;; asdf:SYSTEM-DEPENDS-ON
;; asdf:SYSTEM-DESCRIPTION
;; asdf:SYSTEM-HOMEPAGE
;; asdf:SYSTEM-LONG-DESCRIPTION
;; asdf:TEST-OP
;; asdf:TEST-SYSTEM

;; (asdf:registered-systems)
;; (asdf:already-loaded-systems)

;;; also, view manual, documentation or Masamune lesson for any one of these
;;; systems.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; system browser

(define-presentation-type system ())

(defun plot-systems ()
  (let* ((root-system (car (sort (mapcar #'asdf:find-system (asdf:registered-systems))
				 (lambda (system-1 system-2) (> (length (asdf:system-depends-on system-1))
							   (length (asdf:system-depends-on system-2))))))))
    (with-room-for-graphics (mm::*hack*)
      (format-graph-from-root root-system
			      (lambda (system sheet)
				(multiple-value-bind (x y) (stream-cursor-position sheet)
				  (with-output-as-presentation (sheet system 'system)
				    (surrounding-output-with-border (mm::*hack* :shape :ellipse :ink +blue+ :line-thickness 3)
				      (format sheet "~s" system)))))
			      (lambda (system) (remove-if #'null (mapcar (lambda (s) (handler-case (asdf:find-system s) (error nil)))
								    (asdf:system-depends-on system))))
			      :stream mm::*hack*
			      :merge-duplicates t
			      :move-cursor t
			      :graph-type :dag
			      :orientation :vertical
			      :generation-separation 200
			      :within-generation-separation 50))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; package browser

(define-presentation-type package ())

(defun plot-packages ()
  (with-room-for-graphics (mm::*hack*)
    (format-graph-from-root (find-package 'cl)
			    (lambda (package sheet)
			      (multiple-value-bind (x y) (stream-cursor-position sheet)
				(with-output-as-presentation (sheet package 'package)
				  (surrounding-output-with-border (mm::*hack* :shape :ellipse :ink +blue+ :line-thickness 3)
				    (format sheet "~A" (package-name (find-package package)))))))
			    #'package-used-by-list
			    :stream mm::*hack*
			    :merge-duplicates t
			    :move-cursor t
			    :graph-type :dag
			    :orientation :vertical
			    :generation-separation 400
			    :within-generation-separation 50)))

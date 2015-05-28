;;;; deliver.lisp
;;;
;;; Delivery script for producing a js-on-cl binary using SBCL.
;;;
;;; Copyright (c) 2006 James Wright
;;; See LICENSE for full licensing details.
;;;
(require :asdf)
(require :js-parser)
(in-package :js-parser)

(sb-ext:save-lisp-and-die jw-system:*executable-name*
                          :executable t
                          :toplevel #'main)

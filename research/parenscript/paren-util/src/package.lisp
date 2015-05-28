(defpackage org.iodb.paren-util
  (:nicknames #:paren-util)
  (:use :common-lisp :parenscript)
  (:export
   #:in-package
   #:use-package
   #:defaultf
   #:funcall
   #:methcall
   #:with-arguments-array
   #:length
   #:subseq
   #:to-array
   #:merge-into
   #:lispy-apply
   #:lispy-map
   #:collect
   #:every
   #:some
   #:remove-duplicates
   #:lexicographic-compare
   #:object-to-keyargs
   #:escape-html
   #:unescape-html))

(defpackage #:js-global
    (:export #:console #:console.warn #:window #:window.console #:document #:arguments
	     #:splice
	     #:-Array #:-Object))


(defpackage org.iodb.paren-util.js-package
  (:use :parenscript :common-lisp))
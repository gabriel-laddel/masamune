(defpackage org.iodb.paren-util
  (:nicknames #:paren-util)
  (:use :common-lisp :parenscript)
  (:export
   ;; Misc
   #:merge-into
   #:lispy-apply
   #:lexicographic-compare
   #:identity

   ;; macros
   #:in-package
   #:use-package
   #:defaultf
   #:with-arguments-array

   ;; macro pseudofunctions
   #:funcall
   #:methcall

   ;; string/escaping functions
   #:stringp
   #:escape-html
   #:unescape-html
   #:re-escape
   #:query-serialize
   #:query-decode

   ;; AJAX
   #:ajax-request  

   ;; DOM
   #:write-attributes
   #:toplevel-window
   #:create-elem
   #:elem-add-class
   #:elem-remove-class
   #:elem-hide
   #:elem-show
   #:elem-visible?
   #:elem-insert
   #:elem-wrap
   #:insertion-fn
   #:html-content-to-dom-nodes
   #:elem-by-id
   #:remove-elem

   ;; UIish
   #:calculate-window-size
   #:calculate-page-size
   #:calculate-page-scroll
   #:calculate-max-page-scroll
   
   ;; mapping functions
   #:find
   #:lispy-map
   #:collect
   #:every
   #:some
   #:remove
   #:remove-if-not
   #:remove-duplicates

   ;; set operations
   #:rough-set-union
   #:rough-set-difference

   ;; other array functions
   #:push-on-end
   #:copy-array
   #:to-array
   #:concat
   #:length
   #:subseq
   #:remove-from-array
   #:join
   #:nreverse
   #:reverse-in-place
   #:reverse

   ;; keyword arguments
   #:object-values
   #:object-to-keyargs   
   #:key-object-to-array
   #:keys-array-fn-to-key-object-fn

   ))

(defpackage #:js-global
    (:nicknames #:jsg #:jsns)
    (:export #:console #:console.warn  #:window.console #:document #:arguments
	     #:window
	     #:document
	     #:arguments
	     #:splice
	     #:this
	     #:-Array #:-Object))


(defpackage org.iodb.paren-util.js-package
  (:use :parenscript :common-lisp))

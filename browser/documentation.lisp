;;; Documentation
;;; ============================================================================
;;;
;;; mozrepl allows one to hop around contexts and evaluate arbitrary code in
;;; them. a 'context' is a webpage + the repl evaluation environment plus at 
;;; least one more environment for firefox itself.
;;;
;;; The mozrepl wiki on github is a useful resource
;;; (mmb::open-uri "https://github.com/bard/mozrepl/wiki" t)
;;;
;;; - the "master object" is, afaik, conkeror. You can inspect it, and anything
;;;   else with (mps (chain repl (inspect conkeror))). Other useful objects,
;;;
;;;   -cc
;;;   -ci
;;;   -cr
;;;
;;; To really make sense of conkeror, first finish the js->ps transpiler,
;;; convert it to PS so one has working M-. then figure out why eval isn't
;;; working, see if it can be exposed etc. 
;;; 
;;; TODO
;;; ============================================================================
;;; - eval doesn't work in conkeror - WHY!? 

;; (mps (@ conkeror browser_object_top)) ;; and what is this?

;;; This will clean all the extra crud from a wikipedia page
;; (add_hook "buffer_dom_content_loaded_hook"
;; 	  (lambda () (when (smember "wiki" current-url)
;; 		  (setf (@ (chain (@ (aref (@ (aref ((@ conkeror get-windows)) 0) 
;; 					      buffers buffer_history) 0) document)
;; 				  (get-element-by-id "content"))
;; 			   style margin-left)
;; 			0)
;; 		  (loop for dom-id in '("mw-panel" "mw-page-base" "mw-head")
;; 			do (mps (chain (@ (chain (@ (aref (@ (aref ((@ conkeror get-windows)) 0) buffers buffer_history) 0) document)
;; 						 (get-element-by-id dom-id)) parent-node) 
;; 				       (remove-child (chain (@ (aref (@ (aref ((@ conkeror get-windows)) 0) buffers buffer_history) 0) document)
;; 							    (get-element-by-id dom-id))))))))
;; 	  t t)

;; (mps (chain repl (inspect (chain (@ -cc "@mozilla.org/appshell/window-mediator;1")
;; 				      (get-service (@ -ci ns-i-window-mediator))
;; 				      (get-most-recent-window "navigator:browser")
;; 				      (get-browser)))))

;; (loop for b across (@ (aref (get-windows) 0) buffers buffer_history)
;;       collect (@ b display_uri_string))

;; (mps (chain repl (inspect (aref (@ (aref ((@ conkeror get-windows)) 0) buffers buffer_history) 0))))

;; repl.doc(elm)
;; repl.doc(document.getElementById('minibuffer')))

;;; will display the type and nodename of the object and open online
;;; documentation if applicable. TODO this currently opens in a new conkeror
;;; window instead a new tab.

;; repl.enter(place) ;; will put you into a new context.
;; repl.whereAmI() ;; current context

;;; TODO apparent for Firefox 3 you'll need: repl.enter(content.wrappedJSObject)
;;; what version is conkeror running

;; repl.back() ; to return to the previous context.
;; repl.home() ; to return to the context in which the repl was started. by default, the Conkeror window.

;; var scratch = {} ; create your own context
;; repl.enter(scratch)

;; repl.enter(repl) ;;; the repl itself is a context

;;; The key functions for finding your way around

;; repl.load("file:///home/francis/quicklisp/local-projects/masamune/browser/custom-commands.js") 
;; load will allow you to specify any 
;; 
;; repl.look
;; repl.inspect
;; repl.doc
;; repl.search

;;; some other misc functions that may or may not be useful

;; repl.setenv
;; repl.getenv
;; repl.pushenv
;; repl.popenv
;; repl.print
;; repl.quit()

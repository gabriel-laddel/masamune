;;; Javascript to parenscript compiler.
;;; ============================================================================
;;; Thanks to bhyde for creating backwards parenscript. It didn't / doesn't
;;; compile, so I've translate it into this.

(in-package #:mmb)

(named-readtables:in-readtable :fare-quasiquote)

(defun js-name-to-symbol (string)
  (intern
   (with-output-to-string (s)
     (loop
        for c across string
          do
          (when (upper-case-p c)
            (princ #\- s))
	  (princ (char-upcase c) s)))))


(defun help-let-forms (form)
  (match form
    (`(progn ,@forms)
      (labels ((r (forms)
                 (ematch forms
                   (`((let ,binds :helpme) ,@more-forms)
                     `((let ,binds ,@(r more-forms))))
                   (`(,form ,@more-forms)
		     `(,form ,@(r more-forms)))
                   (nil nil))))
        (let ((rewrite (r forms)))
          (if (cdr rewrite)
              `(progn ,@rewrite)
              (car rewrite)))))
    (_ form)))

(defun help-progn (form)
  "Eliminate unnecessary progn"
  ;; (progn 1 (progn 2 (f (progn 3)) 4) 5 (progn (progn 6) 7))
  ;; -> (progn 1 2 (f (progn 3)) 4 5 6 7)
  (match form
      (`(progn ,x) (help-progn x))
      (`(progn ,@forms)           
        (labels ((r (forms)
                   (match forms
                     (`((progn ,@a) ,@b)  `(,@(r a) ,@(r b)))
                     (`(,a ,@b)           `(,a ,@(r b)))
                     (nil                 nil))))
          `(progn ,@(r forms))))
    (x x)))

(defvar *last-conversion* nil)
(defvar *context-is-top-level* t)

;;; TODO 2015-01-27T05:48:17+00:00 Gabriel Laddel
;;; move docstring into correct position
(defun js-ast->ps (parse)
  (let* ((*print-case* :downcase)
	 (*print-level* nil)
	 (*print-length* nil))
    (labels
	((TBD (&rest args)
	   (declare (ignore args))
	   (error "no rest of the wiki'd"))
	 (cant (x)
	   (error "Parenscript has no construct for ~a." x))
	 (op (op)
	   (ecase op
	     (:+ '+)
	     (:- '-)
	     (:* '*)
	     (:< '<)
	     (:-- '--)
	     (:++ '++)
	     (:!= '!=)
	     (:/ '/)
	     (:% '%)
	     (:<< '<<)
	     (:>> '>>)
	     (:>>> '>>>)
	     (:> '>)
	     (:EQL 'EQL)
	     (:== 'equal)
	     (:=== '===)
	     (:!== '!==)
	     (:& '&)
	     (:^ '^)
	     (:&& 'and)
	     (:AND 'AND)
	     (:OR 'OR)
	     (:|\|\|| 'OR)
	     (:! 'not)
	     (:typeof 'typeof)
	     (:instanceof 'instanceof)
	     (:void 'void)
	     (:delete 'delete)))
	 (binary (form &optional ps-op?)
	   (ematch form
	     (`(,_ ,op ,left ,right)
	       `(,(if ps-op? ps-op? (op op)) ,(r left) ,(r right)))))
	 (unary (form)
	   (ematch form
	     (`(,_ ,op ,arg)
	       `(,(op op) ,(r arg)))))
	 (wrap-forms-in-progn (forms)
	   (help-let-forms
	    (help-progn
	     `(progn ,@forms))))
	 (form-to-forms (form)
	   (match form
	     (`(progn ,@forms)   forms)
	     (x                  (list x))))
	 (r! (forms) (mapcar #'r forms))
	 (r!-progn (forms)
	   (wrap-forms-in-progn (r! forms)))
	 (tidy (x)
	   "clean up various @/chain forms"
	   (match x
	     ;; merge nested cases
	     (`(@ (@ ,@a) ,@b)         `(@ ,@(append a b)))
	     (`(chain (@ ,@a) ,@b)     `(chain ,@(append a b)))
	     (`(chain (chain ,@a) ,@b) `(chain ,@(append a b)))
	     ;; revise calls on chain
	     (`((chain ,@a) ,@args)
	       `(chain ,@(loop 
			   for (i . remainder) on a
			   collect (if remainder
				       i
				       `(,i ,@args)))))
	     ;; Introduce chain if working on a complex object.
	     (`(@ ,(guard call (listp call)) ,@b)
	       (tidy
		`(chain ,call ,@b)))
	     ;; otherwise untouched.
	     (_ x)))
	 (r (x)
	   (match x
	     (`(:name ,txt)           (js-name-to-symbol txt))
	     (`(:num ,num)            num)
	     (`(:string ,s)           s)
	     (`(:stat ,form)          (r form))

	     (`(:call ,func ,args)    (tidy `(,(r func) ,@(r! args))))
	     (`(:return ,form)        `(return ,(r form)))
	     (`(:binary ,_ ,_ ,_)     (binary x))
	     (`(:assign :- ,_ ,_)     (binary x 'decf))

	     (`(:assign t ,_ ,_)      (binary x 'setf))
	     (`(:assign :+ ,_ ,_)     (binary x 'incf))
	     (`(:assign := ,_ ,_)     (binary x 'setf))
	     (`(:unary-prefix ,_ ,_)  (unary x))

	     (`(:array ,elements)     `(list ,@(r! elements)))
	     (`(:toplevel ,forms)     (let ((*context-is-top-level* t))
					(r!-progn forms)))
	     (`(:seq ,forms)          (r!-progn forms))
	     (`(:block ,forms)        (r!-progn forms))

	     (`(:sub ,array ,index)   `(aref ,(r array) ,(r index)))
	     (`(:regexp ,expr "")     `(regex ,expr))
	     (`(:regexp ,expr ,flags) `(regex ,(format nil "/~A/~A" expr flags)))
	     ('(:debugger)            'debugger)

	     (`(:new ,func ,args)     `(new ,(tidy `(,(r func) ,@(r! args)))))
	     (`(:atom :true)          't)
	     (`(:atom :false)         'f)
	     (`(:atom :null)          nil)
	     (`(:atom ,_)             (TBD))

	     (`(:dot ,x ,(guard slot (stringp slot)))
	       (tidy
		`(@ ,(r x) ,(js-name-to-symbol slot))))

	     (`(:function nil ,args ,forms)
	       `(lambda ,(mapcar #'js-name-to-symbol args) 
		  ,@(form-to-forms
		     (let ((*context-is-top-level* nil))
		       (r!-progn forms)))))
	     (`(:defun ,name ,args ,forms)
	       `(defun ,(js-name-to-symbol name) ,(mapcar #'js-name-to-symbol args)
		  ,@(form-to-forms
		     (let ((*context-is-top-level* nil))
		       (r!-progn forms)))))

	     (`(:object ,field-alist)
	       `(create ,@(loop
			    for (key . value) in field-alist
			    nconc `(,(js-name-to-symbol key) ,(r value)))))
	     (`(:var ,bindings)
	       (let ((bindings (loop 
				 for (var . init) in bindings
				 collect 
				 `(,(js-name-to-symbol var) ,(r init)))))
		 (if *context-is-top-level*
		     (wrap-forms-in-progn
		      (loop for (var binding) in bindings
			    collect `(defvar ,var ,binding)))
		     `(let ,bindings :helpme))))

	     (`(:throw ,expr)                   `(throw ,(r expr)))
	     (`(:try ,body ,catch nil)          `(try ,(r body)
						      (:catch ,(r catch))))
	     (`(:try ,body ,catch ,finally)     `(try ,(r body) 
						      (:catch ,(r catch))
						      (:finally ,(r finally))))


	     (`(:if ,q ,a ,b)                   `(if ,(r q) ,(r a) ,(r b)))
	     (`(:if ,q ,a)                      `(when ,(r q) ,(r a)))
	     (`(:conditional ,test ,then ,else) `(if ,(r test) ,(r then) ,(r else)))
	     (`(:while ,cond (:block ,forms))   `(while ,(r cond)
						   ,(r!-progn forms)))

	     (`(:for-in (:var ((,name))) (:name ,_) ,obj ,body)
	       `(for-in (,(js-name-to-symbol name) ,(r obj))
			,@(form-to-forms (r body))))

	     (`(:label ,_ ,_)                   (cant "label")) 
	     (`(:continue nil)                  '(continue))
	     (`(:continue ,label)               `(continue ,(js-name-to-symbol label)))
	     (`(:break nil)                     '(break))
	     (`(:break ,label)                  `(break ,(js-name-to-symbol label)))
	     (`(:do ,_ ,_)                      (cant "do"))
	     (`(:for (:var ,bindings) ,cond ,step ,body)
	       (let ((ps-bindings (loop for (name . init) in bindings
					collect `(,(js-name-to-symbol name) ,(r init)))))
		 `(for ,ps-bindings (,(r cond)) (,(r step))
		       ,@(form-to-forms (r body)))))
	     (`(:switch ,val ,@cases)
	       `(switch ,(r val) ,@(r! cases)))
	     ;; That's all so far, pending:
	     (`(:unary-postfix ,op ,place) `(,(r op) ,(r place)))
	     (`(:with ,obj ,body)             (TBD obj body)))))
      (r parse))))

;;; MozRepl Conversion (WIP / scratch)
;;; ============================================================================

(defun js-ast (input)
  (parse-js (etypecase input
	      (string input)
	      (pathname (mm::slurp-file input))
	      (file-stream (mm::slurp-stream input))
	      (string-stream (mm::slurp-stream input)))))

(defmethod js->psf ((pathname pathname))
  (with-open-file (s (mm::alter-pathname-type pathname "paren")
		     :direction :output
		     :if-exists :supersede)
    (loop with *print-case* = :downcase
	  for k in (js->ps pathname)
	  do (unless (eq 'progn k)
	       (progn (print k s)
		      (terpri s))))))

(defun js->ps (js &optional outf)
  "the js parser doesn't work on everything by default. some useful translations

Const -> var 
let   -> var

BEWARE! let cannot simply be changed to var all the time - it just happens to
work where I've encountered it in the conkeror codebase.

http://stackoverflow.com/questions/762011/javascript-let-keyword-vs-var-keyword"
  ;; if we encounter strings that probably represent pathnames, treat them as such.
  (let* ((ast (js-ast->ps (js-ast (etypecase js
				    (string js)
				    (pathname (mm::slurp-file js))
				    (file-stream (mm::slurp-stream js))
				    (string-stream (mm::slurp-stream js)))))))
    (if outf (write-to-file outf ast :supersede) ast)))

(defmacro ast-q (q type)
  `(let* ((out))
     (dolist (tree js-asts)
       (mm::walk-tree (lambda (k) (when (and (listp k) (eq ,type (car k)))
			       (push k out)))
		      tree))
     ,q))

(defparameter js-asts nil
  "for asking questions about the parser")
(defparameter browser-files
  (mm::remove-if (lambda (p) (or (mm::emacs-backup? p) (not (string= (pathname-type p) "js")))) 
		 (mm::recursive-contents (mm::qlpp "/masamune/browser/repl/"))))


;; (loop with *print-case* = :downcase
;;   ;; NOTE 2015-04-14T01:48:42+00:00 Gabriel Laddel
;;   ;; ignore files that have  already been modified to work.
;;       for i in (remove-if (lambda (p) (or (string=  "overlay" (pathname-name p))
;; 				     (string=  "ui" (pathname-name p)))) browser-files)
;;       if (handler-case (js->ps i) (error nil nil))
;; 	do (with-open-file (s (mm::alter-pathname-type i "paren")
;; 			      :direction :output
;; 			      :if-exists :supersede)
;; 	     (loop for k in (rest (js->ps i))
;; 		   do (progn (print k s)
;; 			     (terpri s))))
;;       else collect i)

;; (let* ((i #P"~/quicklisp/local-projects/masamune/browser/repl/chrome/content/repl.js")
;;        (*print-case* :downcase))
;;   (with-open-file (s (mm::alter-pathname-type i "paren")
;; 		     :direction :output
;; 		     :if-exists :supersede)
;;     (print (js->ps i) s)))

;;; finished files 
;;; - overlay.paren
;;; - ui.paren
;;; - overlay_imply.paren
;;; - util.paren
;;; - server.paren
;;; - repl.js needs work - check the commented out functions

;;; "Sun Apr 19 23:58:21 2015" 
;;; ============================================================================
;;; possible issues
;;; - install.rdf doesn't match in some bizzare way

;; (mm::recursive-contents-of-type #P"~/quicklisp/local-projects/masamune/browser/repl/"
;; 				"paren")

;; (loop for i in '(#P"/root/quicklisp/local-projects/masamune/browser/repl/chrome/content/overlay.paren"
;; 		 ;; #P"/root/quicklisp/local-projects/masamune/browser/repl/chrome/content/overlay_impl.paren"
;; 		 ;; #P"/root/quicklisp/local-projects/masamune/browser/repl/chrome/content/server.paren"
;; 		 #P"/root/quicklisp/local-projects/masamune/browser/repl/chrome/content/ui.paren"
;; 		 ;; #P"/root/quicklisp/local-projects/masamune/browser/repl/chrome/content/util.paren"
;; 		 #P"/root/quicklisp/local-projects/masamune/browser/repl/defaults/preferences/mozrepl.paren"
;; 		 )
;;       do (ps-compile-to-file i))

;;; Old. 
;;; ============================================================================

;; (mapcar (lambda (k) )
;; 	(mapcar (lambda (i) (mm::alter-pathname-type i "ps")) browser-files)
;; 	(loop for i in '(#P"/root/quicklisp/local-projects/masamune/browser/repl/chrome/content/overlay_impl.ps"
;; 			 #P"/root/quicklisp/local-projects/masamune/browser/repl/chrome/content/ui.ps"  #P"/root/quicklisp/local-projects/masamune/browser/repl/components/CommandLine.ps"
;; 			 #P"/root/quicklisp/local-projects/masamune/browser/repl/components/MozRepl.ps"
;; 			 #P"/root/quicklisp/local-projects/masamune/browser/repl/defaults/preferences/mozrepl.ps")
;; 	      collect (mm::alter-pathname-type i "paren")))

;; (dolist (k)
;;   (js->ps k (mm::alter-pathname-type k "paren")))

;;       ;; convert the js-to paren
;; (dolist (path (filter (lambda (p) (member (mm::filename p) '("mozrepl.js" "ui.js" "overlay.js") :test #'equal))
;; 		      (mm::recursive-contents-of-type (qlpp "/masamune/browser/repl/") "js")))
;;   (js->ps path (mm::alter-pathname-type path  "paren")))

;; (ps-compile-to-file #P"/root/quicklisp/local-projects/masamune/browser/repl/chrome/content/overlay.paren")

;; (loop for ps-file in ;; (mm::recursive-contents-of-type (qlpp "/masamune/browser/repl/") "paren")
;;       '( #P"/root/quicklisp/local-projects/masamune/browser/repl/chrome/content/overlay.paren"
;; 	#P"/root/quicklisp/local-projects/masamune/browser/repl/chrome/content/ui.paren"
;; 	#P"/root/quicklisp/local-projects/masamune/browser/repl/defaults/preferences/mozrepl.paren"
;; 	)
;;       do (ps-compile-to-file ps-file))

;; (filter (lambda (p) (member (mm::filename p) '("mozrepl.js" "ui.js" "overlay.js") :test #'equal))
;; 	(mm::recursive-contents-of-type (qlpp "/masamune/browser/repl/") "js"))

;; '(#P"/root/quicklisp/local-projects/masamune/browser/repl/chrome/content/overlay.js"
;;    #P"/root/quicklisp/local-projects/masamune/browser/repl/chrome/content/ui.js"
;;    #P"/root/quicklisp/local-projects/masamune/browser/repl/defaults/preferences/mozrepl.js")

;; #P"/root/quicklisp/local-projects/masamune/browser/mozrepl/chrome/content/overlay_impl.js"
;; #P"/root/quicklisp/local-projects/masamune/browser/mozrepl/chrome/content/repl.js"
;; #P"/root/quicklisp/local-projects/masamune/browser/mozrepl/chrome/content/server.js"
;; #P"/root/quicklisp/local-projects/masamune/browser/mozrepl/chrome/content/util.js"
;; #P"/root/quicklisp/local-projects/masamune/browser/mozrepl/components/CommandLine.js"
;; #P"/root/quicklisp/local-projects/masamune/browser/mozrepl/components/MozRepl.js"

;; '(#P"/root/quicklisp/local-projects/masamune/browser/repl/chrome/content/overlay.js"
;;   #P"/root/quicklisp/local-projects/masamune/browser/repl/chrome/content/overlay_impl.js"
;;   #P"/root/quicklisp/local-projects/masamune/browser/repl/chrome/content/repl.js" 
;;   ;; TODO
;;   ;; `push-env'
;;   ;; `highlight'
;;   #P"/root/quicklisp/local-projects/masamune/browser/repl/chrome/content/server.js"
;;   #P"/root/quicklisp/local-projects/masamune/browser/repl/chrome/content/ui.js"
;;   #P"/root/quicklisp/local-projects/masamune/browser/repl/chrome/content/util.js"
;;   #P"/root/quicklisp/local-projects/masamune/browser/repl/components/CommandLine.js"
;;   #P"/root/quicklisp/local-projects/masamune/browser/repl/components/MozRepl.js"
;;   #P"/root/quicklisp/local-projects/masamune/browser/repl/defaults/preferences/mozrepl.js")

;;; RPC
;;; ============================================================================
;;;
;;; TODO
;;; - when groveling the library, check symbols being exported for (un)qualified,
;;;   symbol matches.
;;; 
;;; open questions / unimlemented:
;;; ==============================
;;; 
;;; - PGP security
;;; - allow / disallow reader macros?
;;; - use trivial-pretty-print or slime-pretty-print for formatting newly
;;;   generated code
;;;
;;; usage
;;; =====
;;; * exported functions must not return multiple values.
;;; * see the tests in rpc-tests.lisp

(in-package #:mm)

(defun export-defun-or-macro (defun-sexp)
  "currently dependent on *socket being named thusly"
  (destructuring-bind (name lambda-list maybe-docstring) 
      (take 3 (rest defun-sexp))
    (if (member '&rest lambda-list)
	(error "rest args are currently not handled")
	(let* ((body `((write (list (quote ,name) 
				    ,@(remove-if (lambda (s) (member s '(&rest &optional &keys)))
						 lambda-list)) :stream (usocket:socket-stream *socket*))
		       (finish-output (usocket:socket-stream *socket*))
		       (read (usocket:socket-stream *socket*)))))
	  `(defun ,name ,lambda-list
	     ,@(if (stringp maybe-docstring)
		   (cons maybe-docstring body)
		   body))))))

(defun generate-client-library
    (output-dir input-pathname client-library-name host port
     symbols-to-export &optional skip-on-error)
  "INPUT-PATHNAME names the dir containing the project you're exporting from"
  (let* ((client-library-pathname (merge-pathnames output-dir "client-library.lisp")))
    (with-open-file (s client-library-pathname
		       :direction :output
		       :if-exists :supersede
		       :if-does-not-exist :create)
      (labels ((h (control-string &rest args)
		 (format-symbol *package* control-string args))
	       (valid-sexps (file) (loop for sexp in (read-file file)
					 when (and (member (cadr sexp) symbols-to-export) 
						   (member (car sexp) '(defmacro defun)))
					   collect sexp))
	       (k (l) (format s "~{~s~%~%~}" l)))

	(let* ((*print-case* :downcase)
	       (skip-on-error skip-on-error)
	       (sexps-to-export (loop for file in (filter (lambda (p) (string= "lisp" (pathname-type p)))
							  (recursive-contents input-pathname))
				      for sexps = (if skip-on-error
						      (handler-case (valid-sexps file)
							(error () nil))
						      (restart-case (valid-sexps file)
							(skip () :report "don't read this file")
							(skip-all () :report "skip any file which throws an error"
							  (setf skip-on-error t)
							  nil)))
				      when sexps append sexps))
	       (detected-functions-or-macros (mapcar #'second sexps-to-export))
	       (undetected-symbols (set-difference symbols-to-export detected-functions-or-macros)))
	  (restart-case (if undetected-symbols
			    (error "The symbols ~s were not detected as functions or macros and thus cannot be exported"
				   undetected-symbols))
	    (continue-anyways () :report "disregard the missing symbols"))

	  (k `((in-package ,client-library-name)
	     
	       (defparameter *socket* nil)
	       (defparameter *host* ,host)
	       (defparameter *port* ,port)

	       (defun connect ()
	  	 (setf *socket*(usocket:socket-connect *host* *port* 
						       :element-type 'character
						       :protocol :stream)))

	       (defun disconnect ()
		 (write '(disconnect) :stream (usocket:socket-stream *socket*))
		 (finish-output (usocket:socket-stream *socket*))
	  	 (usocket:socket-close *socket*))))

	  (format s "~{~s~%~%~}" (mapcar #'export-defun-or-macro sexps-to-export)))))
    (format t "~%created client library at ~a" client-library-pathname)))

(defun generate-client-asd-file (output-dir system-name)
  (let* ((*print-case* :downcase)
	 (asd-pathname (merge-pathnames output-dir 
					(format nil "~a.asd" system-name))))
    (with-open-file (stream asd-pathname
			    :direction :output
			    :if-exists :supersede
			    :if-does-not-exist :create)
      (format stream "~s~%~%~s" '(in-package #:asdf)
	      `(defsystem ,system-name
		 :serial t
		 :depends-on (#:masamune)
		 :components ((:file "packages.lisp") (:file "client-library")))))
    (format t "~%created client library .asd file, ~a" asd-pathname)))

(defun generate-client-package
    (output-dir package-name package-nicknames package-exports)
  ;; TODO 2014-12-14T04:47:15+00:00 Gabriel Laddel
  ;; * derive `package-name' from package being 'exported'
  ;; * remove regex hack
  (assert (and (symbolp package-name)
	       (listp package-nicknames)
	       (listp package-exports)))
  (let* ((*print-case* :downcase)
	 (package-sexp-string (format nil "~s"
				      `(defpackage ,package-name
					 (:nicknames ,@package-nicknames)
					 (:export ,@package-exports)
					 (:use #:cl #:masamune))))
	 (output-pathname (merge-pathnames output-dir "packages.lisp")))
    (with-open-file (stream output-pathname 
			    :direction :output
			    :if-exists :supersede
			    :if-does-not-exist :create)
      (format stream (regex-replace-all "\\|" package-sexp-string "")))
    (format t "~%created client library package ~a" output-pathname)))

(defun defsystem-sexp-p (l) 
  (when (member (car l) '(asdf/defsystem:defsystem asdf:defsystem defsystem) :test #'equal) l))

(defun generate-server
    (output-dir input-library-path host port symbols-to-export)
  (let* ((server-codebase-asd-file (some (lambda (p) (when (string= "asd" (pathname-type p)) p)) 
					 (ls input-library-path)))
	 (*print-case* :downcase)
	 (rpc-server-path (if output-dir
			      (merge-pathnames output-dir "rpc-server.lisp")
			      (merge-pathnames input-library-path "rpc-server.lisp")))
	 (server-asd-path (merge-pathnames output-dir "server.asd"))) 
    (with-open-file (s rpc-server-path
		       :direction :output
		       :if-exists :supersede
		       :if-does-not-exist :create)
      (format s "~a" 
	      (cat (format nil "(in-package #:mm)~%~%~{~s~%~%~}"
			   `((defparameter *host* ,host)
			     (defparameter *port* ,port)
			     (defparameter *exported-symbols*
			       (quote ,(append '(help %describe disconnect) symbols-to-export)))))
		   (slurp-file (qlpp "/masamune/rpc/rpc-server-template.lisp"))))
      ;; update .asd file
      (with-open-file (s server-codebase-asd-file
			 :direction :input
			 :if-exists :supersede)
	(with-open-file (stream server-asd-path
				:direction :output
				:if-exists :supersede
				:if-does-not-exist :create) 
	  (format stream "~{~s~%~%~}"
		  ;; (->> (ls input-library-path)
		  ;;      (some (lambda (p) (when (string= "asd" (pathname-type p)) p)))
		  ;;      (read-file))
		  (let* ((asd-file-contents (read-file "~/quicklisp/local-projects/masamune/masamune.asd"))
			 ;; XXX 2014-12-30T15:42:12+00:00 Gabriel Laddel
			 ;; 
			 ;; fetching `defsystem-sexp' via `length' is completely
			 ;; wrong, but as it stands I'm at a complete loss for
			 ;; why `defsytsem-sexp-p' isn't working. to understand
			 ;; my confusion, try running the following code.
			 ;; 
			 ;; (some #'defsystem-sexp-p (read-file "~/quicklisp/local-projects/masamune/masamune.asd"))
			 ;;
			 ;; then replace the current definition of defsystem-sexp with this version
			 ;;
			 ;; (defsystem-sexp (find-if #'defsystem-sexp-p asd-file-contents))
			 ;;
			 ;; then try this
			 ;;
			 ;; (export-project-rpcs "127.0.0.1" 6000 '(rpc-function-to-export) 
			 ;; 		      'masamune-client '(:mmc)
			 ;; 		      "/root/quicklisp/local-projects/masamune" "/tmp/"
			 ;; 		      :skip-on-error t)
			 ;;
			 ;; on my machine, it fails with the condition that it couldn't find the sexp.
			 (defsystem-sexp (if (= 1 (length asd-file-contents)) 
					     (car asd-file-contents) 
					     (second asd-file-contents)))
			 (defsystem-sexp-pos (position defsystem-sexp asd-file-contents :test #'eq))
			 (asd-file-leading (take defsystem-sexp-pos asd-file-contents))
			 (asd-file-trailing (drop (1+ defsystem-sexp-pos) asd-file-contents))
			 (components (getf defsystem-sexp :components)) 
			 (rpc-file '(:file "rpc-server")))
		    (unless (member :masamune (getf defsystem-sexp :depends-on))
		      (push :masamune (getf defsystem-sexp :depends-on)))
		    (unless (member rpc-file (getf defsystem-sexp :components) :test #'equal)
		      (setf (getf defsystem-sexp :components)
			    (append components (list rpc-file))))
		    (if (= 0 defsystem-sexp-pos) (list defsystem-sexp) 
			(append asd-file-leading (cons defsystem-sexp asd-file-trailing))))))))
    (format t "~%created server asd file ~a and rpc-server at ~a" 
	    server-asd-path rpc-server-path)))

(defun export-project-rpcs
    (host port symbols-to-export client-library-name client-package-nicknames
     input-library-dir output-dir &key skip-on-error)
  (generate-client-library output-dir input-library-dir client-library-name host port 
			   symbols-to-export skip-on-error)
  (generate-client-package output-dir client-library-name client-package-nicknames symbols-to-export)
  (generate-client-asd-file output-dir client-library-name)
  (generate-server output-dir input-library-dir host port symbols-to-export))

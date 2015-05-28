(fiasco:define-test-package #:rpc-tests
  (:use #:masamune #:fiasco))

(in-package #:mm)

(defvar *rpc-test-file* nil)

(defun rpc-function-to-export ()
  (let* ((rpc-test-file (format nil "/tmp/test-output-~a" (get-universal-time))))
    (setf *rpc-test-file* rpc-test-file)
    (with-open-file (stream rpc-test-file
			    :direction :output
			    :if-exists :error
			    :if-does-not-exist :create)
      (format stream "t"))))

(in-package #:rpc-tests)

;; (export-project-rpcs "127.0.0.1" 6001 '(rpc-function-to-export) 
;; 		     'masamune-client '(:mmc)
;; 		     "/root/quicklisp/local-projects/masamune" "/tmp/"
;; 		     :skip-on-error t)

;; (load (merge-pathnames "/tmp/" "rpc-server.lisp"))

;; (format nil "sbcl --load ~a --eval ~a"
;; 	    (merge-pathnames client-output-dir "client-library.lisp")
;; 	    '(progn (funcall 'rpc-function-to-export) (quit)))
;;     (is (car (mm::read-file mm::*rpc-test-file*)))
;;     (setf mm::*rpc-test-file* nil)

;; (deftest local-rpc ()
;;   (let* ((client-output-dir (format nil "/tmp/test-~a" (get-universal-time)))
;; 	 (port (valid-open-port)))
;;     (rp (format nil "mkdir -p ~a" client-output-dir))
;;     (load (merge-pathnames client-output-dir "rpc-server.lisp"))
;;     ;; new sbcl proc
;;     (format nil "sbcl --load ~a --eval ~a"
;; 	    (merge-pathnames client-output-dir "client-library.lisp")
;; 	    '(progn (funcall 'rpc-function-to-export) (quit)))
;;     (is (car (mm::read-file mm::*rpc-test-file*)))
;;     (setf mm::*rpc-test-file* nil)))

;;; create a client library and server
(let* ((output-dir "/tmp/"))
  (export-project-rpcs "127.0.0.1" 6001 '(rpc-function-to-export) 
		       'masamune-client '(:mmc)
		       "/root/quicklisp/local-projects/masamune"
		       output-dir
		       :skip-on-error t)

  (load (merge-pathnames output-dir "rpc-server.lisp")))

(load (merge-pathnames "/tmp/rpc-server.lisp"))

;;; load it and run

(setf *client-connection* (usocket:socket-accept *socket*))

;;; start a new sbcl process in emacs as an async shell, start a new swank

(ql:quickload 'swank)
(swank:create-server :port 4006 :style swank:*communication-style* :dont-close t)

;;; M-x slime-connect from emacs (don't close the current connection! but if you
;;; happen to, just reconnect and try again). Run the following in repl2.

(ql:quickload 'masamune) ;; use the ACCEPT restart on all failures.

(load "/tmp/packages.lisp")
(load "/tmp/client-library.lisp")
(masamune::connect)

;;; note that the `socket-accept' call returned. note that when sending messages
;;; you must `finish-output'

;;; the two tests that i need to perform is
;;; 
;;; 1. try sending, then reading
;;; 2. try reading, then sending


;;; for some unknown reason the socket connection must be 'primed' before use,
;;; but somehow, when sedingin

;;; server
(progn (setf mm::*swank-connection-hack* *standard-output*)
       (bt:make-thread
	(lambda () 
	  (let* ((*standard-output* mm::*swank-connection-hack*))
	    (loop (format t "~%socket message: ~a" 
			  (read (usocket:socket-stream *client-connection*))))))
	:name "socket-listener"))

;;; client
(progn (setf mm::*swank-connection-hack* *standard-output*)
       (bt:make-thread
	(lambda () 
	  (let* ((*standard-output* mm::*swank-connection-hack*))
	    (loop (format t "~%socket message: ~a" 
			  (read (usocket:socket-stream masamune::*socket*))))))
	:name "socket-listener"))


;;; so the problem is what exactly? I stream these across until it streams me a
;;; connected! message? I see no reason that this won't work, but don't know.
;;;
;;; this is awfully strange.

(bt:make-thread
 (lambda () 
   (let* ((*standard-output* mm::*swank-connection-hack*) (input))
     (loop (if *client-connection*
	       (labels ((valid-input? () (member (car input) *exported-symbols*)))
		 (setf input (read (usocket:socket-stream *client-connection*)))
		 (cond ((and (eq 'disconnect (car input)) (valid-input?)) (disconnect))
		       ((valid-input?) (send (eval input)))
		       (t (send '(error "attempted to call restricted function")))))
	       (setf *client-connection* (usocket:socket-accept *socket*))))))
 :name "server rpc loop")
 
;; (send '(MASAMUNE::%DESCRIBE 'mm::%describe))

;;; is the pattern that if I read prior to sending somethineg it takes 2? 

;;; we're always lagging one behind what the actual thing is?

;;; all symbols must be resolved into the masamune namespace for now because I'm
;;; not yet sure what the correct course of action is. this means
;;;
;;;
;;; I could use a step debugger and explore things that way.

;;; the pattern appears to be the number of times that I've disconected. or something.

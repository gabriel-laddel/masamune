(in-package #:common-lisp-user)

;;; XXX 2015-05-23T13:07:31+00:00 Gabriel Laddel
;;; this script *will overwrite* your ~/.emacs, ~/.stumpwmrc and ~/.swank.lisp
;;; files. If these are in anyway valuable please back them up.
;;;
;;; The code in your ~/.emacs can move to
;;; ~/quicklisp/local-projects/masamune/emacs-customizations.el
;;; 
;;; the code from your ~/.stumpwmrc,
;;; ~/quicklisp/local-projects/masamune/lisp-customization.lisp
;;;
;;; ~/.swank.lisp ? IDK.
;;; 
;;; I'm assuming you've got SBCL, quicklisp, stumpwm, and X installed already
;;; and that X boots into stumpwm by default. I also assume that quicklisp will
;;; be loaded into SBCL by defualt.
;;;
;;; Using this file to install Masamune amounts to
;;;
;;; 0. visit http://gabriel-laddel.github.io/system.html#sec-2-1-5
;;;
;;; 1. emerge all systems not installed on your box (starting at 'virtual/jpeg')
;;;
;;; 2. install xulrunner and conkeror as specified
;;;
;;; 3. sbcl --load ~/quicklisp/local-projects/masamune/build/trinque.lisp
;;;
;;; evaluating the forms at the bottom of this file that look useful

(in-package #:common-lisp-user)

(sb-ext:restrict-compiler-policy 'debug 3)

(ql:quickload '(cl-fad alexandria swank cl-ppcre clx))

(defun cat (&rest objs)
  (apply #'concatenate 'string
	 (mapcar (lambda (o) (if (stringp o) o (write-to-string o))) objs)))

(defun rp (shell-string &optional (output-stream :string))
  (uiop:run-program shell-string :output output-stream))

(defun rp-in-dir (commands dir &optional (output-stream :string))
  (dolist (shell-command commands)
    (rp (format nil "cd ~A && ~A" dir shell-command) output-stream)))

(defun interpose (o l)
  (loop for i in l append (list i o)))

(defun lg (message)  (format t (format nil "~%~a" message)))
(defun k (j)  (rp (format nil "emerge ~a" j) *standard-output*))

(defun download-hyperspec ()
  (rp-in-dir
   '("curl ftp://ftp.lispworks.com/pub/software_tools/reference/HyperSpec-7-0.tar.gz > /tmp/HyperSpec-7-0.tar.gz"
     "tar xzf /tmp/HyperSpec-7-0.tar.gz")
   "/tmp/")
  (rename-file "/tmp/HyperSpec" "~/lisp/HyperSpec"))

(defun write-dotfiles ()
  (macrolet ((f (file form) `(with-open-file (stream ,file
						     :direction :output
						     :if-exists :supersede
						     :if-does-not-exist :create)
			       (format stream "~A" ,form))))
    (f "~/.swank.lisp" ";;; -*- Mode: Lisp -*-~%(in-package #:cl)~%(setf swank::globally-redirect-io t)")  
    (f "~/.emacs" "(load \"~/quicklisp/local-projects/masamune/init.el\")") ;; XXX
    (f "~/.stumpwmrc" (format nil ";;; -*- Mode: Lisp -*-~%(in-package #:stumpwm)~%(redirect-all-output \"~~/.masamune/stumpwm-debug-output\")~%(ql:quickload 'swank)~%(swank:create-server :port 4005 :dont-close t)~%~S~%~S"
			      '(setq
				*input-window-gravity* :center
				*message-window-gravity* :center
				*normal-border-width* 0
				*window-border-style* :none
				*transient-border-width* 0
				*top-level-error-action* :break
				*mouse-focus-policy* :sloppy
				*startup-message* "Please wait for Masamune to start - this might take a minute")
			      '(run-or-raise "emacs --debug-init" (quote (:class "Emacs")))))))



#|
(loop for k in '("~/.masamune/pclos-datastore/"
		 "~/screenshots/"
		 "~/algol/"
		 "~/lisp/") 
      unless (probe-file k)
      do (rp (format nil "mkdir -p ~a" k) *standard-output*))

(unless (probe-file "~/lisp/HyperSpec/") (download-hyperspec))

(write-dotfiles)

(stumpwm::delete-window 
 (car (remove-if-not (lambda (w) (search "emacs" (window-name w))) 
		     (stumpwm::all-windows))))

(stumpwm::emacs)
|#

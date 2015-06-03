(in-package #:common-lisp-user)

(format t "please be patient and don't click on anything quite yet.") 

(sb-ext:restrict-compiler-policy 'debug 3)

(ql:quickload '(cl-fad alexandria cl-ppcre))

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

(defun move-sbcl-sources ()
  "XXX /usr/lib64/sbcl/ contains the sbcl.core file and some contrib modules by
default that are essential to SBCL - don't overwrite them"
  (let* ((sbcl-sources (find-if (lambda (p) (let* ((namestring (namestring p)))
					 (and (scan "sbcl" namestring)
					      (not (scan "binary" namestring))))) 
				(cl-fad:list-directory "/usr/portage/distfiles")))
	 (temp-pathname (merge-pathnames #P"/tmp/" (filename sbcl-sources))))
    (labels ((sbcl-temp-dir () (find-if (lambda (p) (and (scan "sbcl" (namestring p))
						    (not (scan ".tar.bz2" (namestring p)))))
					(cl-fad:list-directory "/tmp/")))
	     (contrib-dir? (pathname) (string= "contrib" (car (last (cl-ppcre:split "/" (namestring pathname)))))))
      (unless (probe-file temp-pathname)
	(cl-fad:copy-file sbcl-sources temp-pathname))
      (unless (probe-file (sbcl-temp-dir))
	(rp (format nil "cd /tmp/ && bunzip2 -f -c * | tar xvf ~A" temp-pathname)))
      (loop for i in (remove-if 'contrib-dir? (cl-fad:list-directory (sbcl-temp-dir)))
	    do (rp (format nil "mv ~A /usr/lib64/sbcl/" i)))
      (loop for i in (cl-fad:list-directory (find-if 'contrib-dir? (cl-fad:list-directory (sbcl-temp-dir))))
	    do (rp (format nil "mv ~A /usr/lib64/sbcl/contrib/" ))))))

(defun move-maxima-sources ()
  (let* ((maxima-sources-pathname (find-if (lambda (p) (and (null (pathname-type p))
						       (search "maxima" (namestring p))
						       (not (char= #\i (aref (filename p) 0)))))
					   (ls "/usr/portage/distfiles/")))
	 (maxima-outpath "/var/tmp/portage/sci-mathematics/maxima-5.18.1/work/"))
    (unless (probe-file maxima-outpath)
      (rp "mkdir -p ~A" maxima-outpath))
    (rp (format nil "cp -R ~A ~A" maxima-sources-pathname maxima-outpath))))

(move-sbcl-sources)
(move-maxima-sources)
(write-dotfiles)
(lg "wrote dotfiles")

(cerror "my mouse and keyboard work as demonstrated by pressing this restart"
	"If the mouse and keyboard don't work you're in undocumented territory, try 
'emerge x11-drivers/xf86-input-mouse && emerge x11-drivers/xf86-input-keyboard && emerge x11-base/xorg-drivers'

See http://www.funtoo.org/X_Window_System for more information. If you could
report this as a bug on http://github.com/gabriel-laddel/masamune and include
as much information about the box in question you're comfortable sharing
it would be greatly appreciated.")

(stumpwm::delete-window 
 (car (remove-if-not (lambda (w) (search "emacs" (window-name w))) 
		     (stumpwm::all-windows))))
(stumpwm::emacs)

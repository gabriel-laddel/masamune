(in-package #:parenscript)

(defprinter ps-js:defun (name args docstring body-block)
	    (print-fun-def name args body-block)
	    (when docstring
	      (format *psw-stream* 
		      "~%~A.doc = ~S" (symbol-to-js-string name) docstring)))

;; TODO 2015-04-22T14:50:35+00:00 Gabriel Laddel
;; this automatically adds an uneeded ";" 
(define-expression-operator inline-js (inline-js-string)
  `(ps-js:escape ,inline-js-string))

(in-package #:stumpwm)

(defun shift-windows-forward (frames win)
  "Exchange windows through cycling frames."
  (when frames
    (let ((frame (car frames)))
      (shift-windows-forward (cdr frames)
			     (frame-window frame))
      (when win
	(pull-window win frame))))) 

(defun mode-line-on ()
  (let* ((screen (stumpwm::current-screen))
	 (head (stumpwm::current-head)))
    (stumpwm::enable-mode-line screen head :visible)
    (dolist (group (screen-groups screen))
      (group-sync-head group head))))

(defun mode-line-off ()
  (let* ((screen (stumpwm::current-screen))
	 (head (stumpwm::current-head))
	 (ml (stumpwm::head-mode-line head)))
    ;; sometimes you'll still be able to see the modeline and the request has
    ;; not yet taken
    (when ml
      (run-hook-with-args *destroy-mode-line-hook* ml)
      (xlib:destroy-window (mode-line-window ml))
      (xlib:free-gcontext (mode-line-gc ml))
      (setf (head-mode-line head) nil)
      (maybe-cancel-mode-line-timer)
      (dolist (group (screen-groups screen))
	(group-sync-head group head)))))

(defun pause-to-read (message)
  "returns T if the user presses \"y\" to continue."
  (message (mm::format-message-for-stumpwm 
	    (mm::cat message "~%(press y to continue, though any key will suffice)")))
  (char= (read-one-char (current-screen)) #\y))

(defun keyboard-layout ()
  (some (lambda (s) (when (string= "variant" (mm:take 7 s))
		 (mm::llast (mm::split #\space s))))
	(mm::split #\Newline (mm:run-program "setxkbmap -query" :output :string))))

(defcommand masamune-tutorial () ()
  "Starts the introductory Masamune tutorial"
  (cond ((and (emacs-window) (browser-window))
	 (mmb::open-uri "file:///root/quicklisp/local-projects/masamune/introduction.html" t))
	((emacs-window)
	 (progn (emacs)
		(message "please start the browser and parenscript REPL via `(progn (mm:::start-conkeror)(sleep 3)(mmb::start-ps-repl))' or the like")))
	((browser-window)
	 (message "please start emacs"))
	(t (message "without both emacs and the browser being open, there simply isn't"))))

(defcommand shutdown () ()
  ""
  (when (fboundp 'mm::record-state)
    (mm::record-state :shutdown))
  (mm::rp "shutdown -h now"))

(defcommand reboot () ()
  ""
  (when (fboundp 'mm::record-state)
    (mm::record-state :shutdown))
  (mm::rp "reboot"))

(defcommand rotate-keyboard-layout () ()
  "toggle through various keyboard configurations"
  (let* ((layout (keyboard-layout)))
    (cond ((null layout)
	   (mm:run-program "setxkbmap us -variant colemak -option ctrl:nocaps")
	   (message-no-timeout "The current keyboard layout is COLEMAK"))
	  ((string= "colemak" layout) 
	   (mm:run-program "setxkbmap us -variant dvorak -option ctrl:nocaps")
	   (message-no-timeout "The current keyboard layout is DVORAK"))
	  ((string= "dvorak" layout)
	   (mm:run-program "setxkbmap us -option ctrl:nocaps")
	   (message-no-timeout "The current keyboard layout is QWERTY")))))

(defcommand invert-screen () ()
  "inverts the screen"
  (uiop:run-program "xcalib -invert -alter"))

(defcommand network () () ""
  (run-commands "exec xterm -e nmtui"))

(defcommand minute-increase-volume () ()
  "Increase master volume"
  (run-shell-command "amixer -c 0 sset Master 1dB+ unmute"))

(defcommand increase-volume () ()
  "Increase master volume"
  (run-shell-command "amixer -c 0 sset Master 5dB+ unmute"))

(defcommand minute-decrease-volume () ()
  "Decrease master volume"
  (run-shell-command "amixer -c 0 sset Master 1dB- unmute"))

(defcommand decrease-volume () ()
  "Decrease master volume"
  (run-shell-command "amixer -c 0 sset Master 5dB- unmute"))

(defcommand toggle-mute () ()
  "Mute master volume"
  (run-shell-command "amixer -c 0 sset Master toggle"))

(defcommand rotate-windows () ()
  "from https://gist.github.com/shes-a-skeeze/1419407"
  (let* ((frames (group-frames (current-group)))
	 (win (frame-window (car (last frames)))))
    (shift-windows-forward frames win)))

(defcommand screenshot () ()
  "Takes a screenshot"
  (mm::save-screenshot-as-png
   (mm::take-screenshot) 
   (format nil "~~/Pictures/screenshots/screenshot-~d.png" (get-universal-time))))

;; (defcommand select-browser () ()
;;   ""
;;   (select-browser))

(define-key *top-map* (kbd "F1") "rotate-keyboard-layout")
(define-key *top-map* (kbd "F2") "invert-screen")
(define-key *top-map* (kbd "XF86AudioMute") "toggle-mute")
(define-key *top-map* (kbd "XF86AudioLowerVolume") "decrease-volume")
(define-key *top-map* (kbd "XF86AudioRaiseVolume") "increase-volume")
;; (define-key *top-map* (kbd "C-b") "select-browser")
(define-key *top-map* (kbd "C-XF86AudioLowerVolume") "minute-decrease-volume")
(define-key *top-map* (kbd "C-XF86AudioRaiseVolume") "minute-increase-volume")
;; (define-key *top-map* (kbd "t") "masamune-tutorial")

;; Paste X selection
;; (defcommand paste-x-selection () (:rest)
;;   "Universal rat-less X paste."
;;   (let ((cmd (concatenate 'string "insert " (get-x-selection))))
;;     (eval-command cmd))
;;   (define-key *top-map* (kbd "Insert") "paste-x-selection"))
;; 
;; place-existing-windows
;; restore-from-file
;; restore-window-placement-rules

;;; I wouldn't touch this if I were you...  
;;; ============================================================================

;; copy for uber mode
;; https://github.com/stumpwm/stumpwm/wiki/HandlingTheGimp

;;; TODO 2014-11-10T11:49:44-08:00 Gabriel Laddel
;;; review (ppath "../stumpwm-contrib/")

;; (defcommand echo-colors-brief () ()
;;   "Output a brief list of currently defined colors."
;;   (echo-string (current-screen) (eval "
;; BOLD ^B^0*black ^1*red ^2*green ^3*yellow ^4*blue ^5*magenta ^6*cyan ^7*white ^8*user ^9*user^n
;; NONE ^0*black ^1*red ^2*green ^3*yellow ^4*blue ^5*magenta ^6*cyan ^7*white ^8*user ^9*user^n")))

;; (defcommand echo-date () ()
;;   "Display date highlighting the most important parts"
;;   (message "^1*~a" (format-expand *time-format-string-alist*
;;                                   "^B%l^b:%M:%S %p ^B%a^b %Y-%m-^B%e^b")))

;;; hot modeline

;; (load-module "cpu")
;; (load-module "disk")
;; ;; (load-module "mpd")
;; (load-module "mem")
;; (load-module "net")
;; (load-module "battery-portable")
;; (load-module "notifications")
;; (define-key *root-map* (kbd "N") '*notifications-map*)

;; ;; (defun top-programs)
;; (setf *time-modeline-string* "%a %m-%e ^4*^B%l:%M^b^n %p") ; zero-pad day
;; (setf *screen-mode-line-format*
;;       (list
;;        "[^B%n^b] " ; group num
;;        '(:eval (color-ping (read-ml-file ".ml-wifi")))
;;        "%B " ; battery
;;        ;; "%g" ;groups
;;        ;; "^B%w^b" ; window list
;;        ;; voicemail, sms, email
;;        '(:eval (read-ml-file ".ml-email"))
;;        ;; quotes
;;        '(:eval (read-ml-file ".ml-quotes"))
;;        ;; notifications
;;        " %Z"
;;        ;; FIXME add weather forecast
;;        ;; TODO add google reader unread
;;        ;; TODO add linphone status/incoming calls
;;        ;; TODO add irc alert
;;        ;; TOOD add current todo (from emacs/org, clocked in item)
;;        ;; " DRP: " '(:eval (read-ml-file ".ml-dropbox"))
;;        "^>" ; right align
;;        ;; pomodoro
;;        '(:eval (read-ml-file ".ml-pomodoro-msg")) " "
;;        '(:eval (read-ml-file ".ml-pomodoro-time")) " "
;;        '(:eval (read-ml-file ".ml-weather")) "°F " ;; "°F "
;;        ;; ;; volume
;;        ;; '(:eval (read-file ".mode-line-volume")) " "
;;        "%f "
;;        "%c" ; cpu
;;        ;; '(:eval (read-ml-file ".ml-sensors")) " "
;;        ;; "%M" ; mem
;;        "NET: %l" ; net
;;        ;; "%D" ; disk
;;        ;; "Media: " '(:eval (read-ml-file ".ml-media")) " "
;;        ;; "Home: " '(:eval (read-ml-file ".ml-home")) " "
;;        "Trash: " '(:eval (read-ml-file ".ml-trash")) " "
;;        ;; "^> %M %c" ;; I like %c but not working last time I tries it's cpu.lisp
;;        ;; "»»»"
;;        "%d" ;; crappy default date
;;        ;; '(:eval (string-right-trim '(#\Newline) (run-shell-command
;;        ;; "date +'%a %m-%d ^4*^B%l:%M^b^n %p'|tr -d '\\n'"
;;        ;; uses date command so time can be bold
;;        ;; "date +'%a %m-%d ^4*^B%l:%M^b^n %p'" t)))
;;        ))

;; (defcommand uaml () ()
;;   ""
;;   (update-all-mode-lines))

;; (dolist (head
;; 	 (list (first (screen-heads (current-screen)))) ; first
;; 	 ;; (screen-heads (current-screen)) ; all
;; 	 )
;;   (enable-mode-line (current-screen) head
;; 	 	    t *screen-mode-line-format*))

;;; misc
;;; ============================================================================

(defun maybe-kill-shell-window ()
  (anaphora:awhen (some (lambda (w) (when (and (not (search "emacs" (window-name w)))
					  (search "lambda" (window-name w)))
				 w)) (all-windows))
    (pull-window anaphora:it)
    (delete-window anaphora:it)))

(defun window-by-name (name)
  "case insensitive"
  (find-if (lambda (w) (search (string-downcase name) (string-downcase (window-name w)) :test 'string=))
	   (all-windows)))

(defun browser-window ()
  (car (remove-if-not (lambda (w) (search "Conkeror" (window-name w) :test 'string=)) (all-windows))))

(defun select-browser ()
  (select-window (window-name (browser-window))))

(defun emacs-window ()
  (car (remove-if-not (lambda (w) (search "emacs" (window-name w))) (all-windows))))

(defun select-emacs ()
  (select-window (window-name (emacs-window))))

(defun emacs-selected? ()
  (search "emacs" (window-name (current-window))))

(defun emacs-fullscreen? ()
  (and (emacs-selected?) (not (listp (car (tile-group-frame-tree (current-group)))))))

(defun something-fullscreen? ()
  (let* ((o (car (tile-group-frame-tree (current-group)))))
    (not (listp o))))

(defun fullscreen-emacs ()
  (loop while (not (emacs-fullscreen?))
     do (cond ((something-fullscreen?) (pull-hidden-next))
	      ((emacs-selected?) (fnext))
	      (t (remove-split)))))

(defun select-nil-frame ()
  "Selects initial frame if a nil frames does not exist"
  (let ((initial-window (current-window))
	(initial-loop t))
    (loop while (or (not (null (current-window)))
		    (and (eq initial-window (current-window)) (not initial-loop)))
          do (progn (fnext) (setq initial-loop nil)))))

(defcommand dump-group-to-file (file) ((:rest "Dump To File: "))
  "Dumps the frames of the current group of the current screen to the named file."
  (dump-to-file (dump-group (current-group)) file))

(defcommand dump-screen-to-file (file) ((:rest "Dump To File: "))
  "Dumps the frames of all groups of the current screen to the named file"
  (dump-to-file (dump-screen (current-screen)) file))

(defcommand dump-desktop-to-file (file) ((:rest "Dump To File: "))
  "Dumps the frames of all groups of all screens to the named file"
  (dump-to-file (dump-desktop) file))

(defcommand restore-from-file (file) ((:rest "Restore From File: "))
  "Restores screen, groups, or frames from named file, depending on file's contents."
  (let ((dump (read-dump-from-file file)))
    (typecase dump
      (gdump
       (restore-group (current-group) dump))
      (sdump
       (restore-screen (current-screen) dump))
      (ddump
       (restore-desktop dump))
      (t
       (message "Don't know how to restore ~a" dump)))))

(in-package #:drei-lisp-syntax)

(defmethod goto-location ((location file-location))
  "If you M-. in drei you'll be sent to emacs"
  (let* ((elisp `(progn (find-file ,(file-name location))
			(goto-char ,(char-position (source-position location)))
			nil)))
    (mm::eval-in-emacs elisp)
    (stumpwm::select-emacs)))

;;; TODO 2015-05-01T09:52:08+00:00 Gabriel Laddel
;;; there is currently a problem with binding the buffer package and readtable

;; (defun emacs-ed-function (thing &rest args)
;;   (format t "~&emacs-ed-function  was passed ~A and ~A" thing args)
;;   (let* ((swank::*buffer-package* (find-package 'mm)))
;;     (drei-lisp-syntax::goto-location 
;;      (DREI-LISP-SYNTAX::MAKE-XREF 
;;       (car 
;;        (SWANK:FIND-DEFINITIONS-FOR-EMACS 
;; 	(etypecase thing
;; 	  (string thing)
;; 	  (symbol (format nil "~A" thing)))))))))

;; (SWANK:FIND-DEFINITIONS-FOR-EMACS "ed")
;; SWANK:*FIND-DEFINITIONS-LEFT-TRIM* (bound)
;; SWANK:*FIND-DEFINITIONS-RIGHT-TRIM* (bound)
;; SWANK:DESCRIBE-DEFINITION-FOR-EMACS (fbound)
;; SWANK:FIND-DEFINITION-FOR-THING (fbound)
;; SWANK::FIND-DEFINITIONS-FIND-SYMBOL-OR-PACKAGE (fbound)
;; SWANK:FIND-DEFINITIONS-FOR-EMACS (fbound)
;; SWANK:TOGGLE-PROFILE-FDEFINITION (fbound)

;;; whatever, modify the editor fucking whatever

;; (push 'drei-lisp-syntax::emacs-ed-function sb-impl::*ed-functions*)

;; (defun edit-file (thing)
;;   (typecase thing
;;     (null nil)
;;     (symbol (list 'drei-lisp-syntax::com-edit-definition thing))
;;     ((or string pathname)
;;      (truename thing)			; raise file-error if file doesn't exist
;;      (list 'esa-io::com-find-file thing))
;;     (t (error 'type-error :datum thing
;; 			  :expected-type '(or null string pathname symbol)))))

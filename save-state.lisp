;;; Save state
;;; ============================================================================
;;;
;;; many thanks to www.gentei.org/~yuuji/ who made this much easier
;;;
;;; Emacs, Stumpwm and Conkeror are all capable of saving and restoring a
;;; previous state. CLIM applictions are generally structured such that state
;;; can be saved in the appliction object itself via `define-appliction-frame'.
;;; 
;;; desktop.el is traditionally used to save state for Emacs, Stumpwm comes with
;;; its own set of related primitives and Conkeror has the ability to, but I'm
;;; not aware of anyone who has made a 'mode' for it.
;;;
;;; In my own hacking sessions Emacs generally manages 100s of buffers, Conkeror
;;; has 10-30 (at which point it really starts to slow down) and one or two
;;; CLIM windows, possibly a xterm that ncurses some music. Saving state for all
;;; but Xterm is the goal.
;;;
;;; TODO
;;; ============================================================================
;;; - docview
;;; - browser
;;; - global winner mode
;;; - Save mark rings.
;;; - REPL history?
;;; 
;;; We load state into emacs via the `after-init-hook'
;;;
;;; Unlike desktop.el etc. we ignore major and minor modes. Emacs is being cut
;;; down to a shim for hacking lisp and nothing more.

;; (buffer-list)
;; buffer-list-update-hook

(in-package #:mm)

(defvar storage-dir #P"~/.masamune/desktops/")

(unless (probe-file storage-dir) (mkdir storage-dir))

(defvar state
  '(:mode-line nil
    :emacs nil
    ;; restore emacs state via `restore-window-configuration'
    :browser nil
    :clim nil
    :stumpwm nil)
  "the :browser's buffer order is significant - `car' is the focused buffer")

(defun record-state (&optional (name (get-universal-time)))
  (assert (or (keywordp name) (numberp name)))
  (write-to-file (merge-pathnames storage-dir (format nil "~A" name)) mm::state :supersede))

(defun state-record-exists? (&optional id-keyword-or-pathname)
  (probe-file (etypecase id-keyword-or-pathname
		(keyword (merge-pathnames storage-dir (format nil "~A" id-keyword-or-pathname)))
		(number (merge-pathnames storage-dir (format nil "~A" id-keyword-or-pathname)))
		(pathname id-keyword-or-pathname))))

(defun state-record (state-plist-or-keyword-or-pathname)
  (etypecase state-plist-or-keyword-or-pathname
    (keyword (car (read-file (merge-pathnames storage-dir (format nil "~A" state-plist-or-keyword-or-pathname)))))
    (pathname (car (read-file state-plist-or-keyword-or-pathname)))
    (cons state-plist-or-keyword-or-pathname)))

(defun restore-state (state-plist-or-keyword-or-pathname)
  (let* ((swank::*emacs-connection* (car swank::*connections*))
	 (state (state-record state-plist-or-keyword-or-pathname)))
    (with-getfs (:mode-line :emacs :stumpwm :browser) state
      (if mode-line (stumpwm::mode-line-on) (stumpwm::mode-line-off))
      (when emacs (ignore-errors (swank::eval-in-emacs (list 'restore-window-configuration (list 'quote emacs)) t)))
      (when browser
	(format t "~&We currently do not restore scroll locations in browser buffers")
	(mmb::load-buffer-set (mapcar (lambda (l) (getf l :uri)) (getf browser :buffers))))
      ;; (when clim (format t "~&Failed to restore CLIM state: ~A" clim))
      (when stumpwm (stumpwm::restore-desktop stumpwm)))))

;;; Stumpwm 
;;; ============================================================================

(in-package #:stumpwm)

(add-hook stumpwm:*destroy-mode-line-hook* 
	  (lambda (&rest arguments) (setf (getf mm::state :mode-line) nil)))
(add-hook STUMPWM:*NEW-MODE-LINE-HOOK*
	  (lambda (&rest arguments) (setf (getf mm::state :mode-line) t)))

(defmacro add-hooks ()
  (cons 'progn
	(loop for i in '(stumpwm:*destroy-window-hook*
			 stumpwm:*focus-frame-hook*
			 stumpwm::*focus-group-hook*
			 STUMPWM:*FOCUS-WINDOW-HOOK*
			 STUMPWM::*MAP-WINDOW-HOOK*
			 STUMPWM:*NEW-FRAME-HOOK*
			 STUMPWM:*NEW-WINDOW-HOOK*
			 STUMPWM::*MAP-WINDOW-HOOK*
			 STUMPWM::*UNMAP-WINDOW-HOOK*
			 STUMPWM:*SPLIT-FRAME-HOOK*
			 STUMPWM:*PLACE-WINDOW-HOOK*)
	      collect `(add-hook ,i (lambda (&rest arguments)
				      (setf (getf mm::state :stumpwm) 
					    (stumpwm::dump-desktop)))))))

(add-hooks)

;;; Browser  
;;; ============================================================================

(in-package #:mmb)

(defun buffers () 
  (getf (getf mm::state :browser) :buffers))

(defun buffer-by-uri (uri)
  (find-if (lambda (l) (string= uri (getf l :uri))) (buffers)))

(defmacro kill-buffer (uri)
  (assert (buffer-by-uri uri) (uri) "Masamune doesn't know that ~A exists" uri)
  `(mps (let* ((window-enum (chain (aref (@ conkeror -cc)
					 "@mozilla.org/appshell/window-mediator;1")
				   (get-service (@ conkeror -ci ns-i-window-mediator))
				   (get-enumerator "")))
	       (single-x-window (chain window-enum (get-next)))
	       (buffers (@ single-x-window buffers buffer_history))
	       (buff (loop for b in buffers
			   do (when (string= ,uri (@ b document location href)) (return b)))))
	  (chain conkeror (kill_buffer buff))
	  (chain conkeror (tab_bar_kill_buffer buff)))))

(defun load-buffer-set (buffer-set &rest dont-kill)
  ;; TODO 2015-05-03T06:16:09+00:00 Gabriel Laddel
  ;; for now, buffer-sets are lists of URIs, we can't currently recover scroll-y positions
  "kill other buffers, and load the current buffer set in order"
  (let* ((to-kill (set-difference (mapcar (lambda (l) (getf l :uri)) (buffers)) buffer-set))
	 (to-kill (if dont-kill (remove-if (lambda (s) (member s dont-kill :test #'string=))
	 				   to-kill)
	 	      to-kill)))
    (loop for uri in (reverse buffer-set)
	  do (mmb::open-uri uri nil))
    ;; `(progn
    ;;    ;; `reverse' to ensure the same buffer is focused
    ;;    ,@(loop for uri in (reverse buffer-set)
    ;; 	       collect `(mmb::open-uri ,uri nil))
    ;;    ;; TODO 2015-05-03T07:17:09+00:00 Gabriel Laddel
    ;;    ;; this ends up killing conkeror for some unknown reason, until I've time
    ;;    ;; to *really* fix it, along with M-. for parenscript etc.
    ;;    ;; we simply don't kill buffers
    ;;    ;; (loop for buffer in to-kill collect `(kill-buffer ,buffer))
    ;;    )
    ))

(defparameter old-file-length 0)
(defparameter browser-output-stream (open #P"~/.masamune/browser-output"))

(defun update-browser-state ()
  ;; XXX 2015-05-01T02:31:18+00:00 Gabriel Laddel
  ;; this only exists because sending the information over the parenscript REPL
  ;; socket wasn't working. Why? Idkwtf.
  "We accept as a given that the buffer state, (:buffers (...)) is located on
its own line"
  ;; (format mm::*swank-connection-hack* "~&called update browser state")
  (let* ((new-file-length (file-length browser-output-stream)))
    (when (> old-file-length new-file-length)
      ;; if we detect the browser has restarted, ie ~/.masamune/browser-output
      ;; has been cleared, restart.
      (close browser-output-stream)
      (setf browser-output-stream (open #P"~/.masamune/browser-output")
  	    old-file-length (file-length browser-output-stream)))
    ;; no new information?
    (when (= old-file-length new-file-length) (return-from update-browser-state))
    ;; then read away!
    (loop while t
  	  for line = (read-line browser-output-stream nil :end-of-stream)
  	  do (cond ((eq :end-of-stream line) 
  		    (setf old-file-length (file-length browser-output-stream))
  		    (return-from update-browser-state))
  		   ((string= "(:buffers (" (take 11 line)) 
  		    (setf (getf mm::state :browser)
  			  (read-from-string line)))))))

(ignore-errors 
 (swank::eval-in-emacs '(run-with-idle-timer .1 t #'update-emacs-and-browser-state)))

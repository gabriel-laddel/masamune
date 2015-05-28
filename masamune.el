;;; -*- lexical-binding: t -*-

(defvar alphabet '("a" "b" "c" "d" "e" "f" "g" "h" "i" "j" "k" "l" "m" "n" "o" "p" "q" "r" "s" "t" "u" "v" "w" "x" "y" "z"))

(defun* change-hardware-mac-address ()
  (interactive)
  ;; TODO 2014-08-22T07:38:07-07:00 Gabriel Laddel
  ;; this needs some work: http://en.wikipedia.org/wiki/MAC_address
  (save-window-excursion
    (async-shell-command (cat "ifconfig eth0 down && macchanger -m "
			      (if (zerop (random* 2))
				  (nth (random* 26) alphabet)
				(random* 10))

			      (if (zerop (random* 2))
				  (nth (random* 26) alphabet)
				(random* 10))

			      ":"

			      (if (zerop (random* 2))
				  (nth (random* 26) alphabet)
				(random* 10))

			      (if (zerop (random* 2))
				  (nth (random* 26) alphabet)
				(random* 10))

			      ":66:4e:16:88"
			      " eth0 && ifconfig eth0 up")"ifconfig eth0 down && macchanger -m ")))

;;; misc 
;;; ============================================================================

(defmacro* swma (body &optional (cont '(lambda (_))) (package "stumpwm"))
  "[S]tump[W][M] [A]ccess - evaluate code on stumpwm proc"
  `(slime-eval-async (quote (swank:eval-and-grab-output ,(prin1-to-string body)))
     ,cont ,package))

(defun alert (s &optional no-timeout)
  (slime-eval-async `(swank:eval-and-grab-output
		      ,(cat (if no-timeout "(message-no-timeout \"" "(message \"") s "\" )"))
    (lambda (_)) "stumpwm"))

(defun message-me-in (time)
  (interactive "sTime: ")
  (run-at-time time nil #'alert (read-from-minibuffer "Message: ")))

(defun message-me-in-persistant-message (time)
  (interactive "sTime: ")
  (run-at-time time nil #'alert (read-from-minibuffer "Message: ") t))

(defun start-or-focus-masamune-irc ()
  (interactive)
  (if (rcirc-process-list)
      ;; TODO 2014-11-01T02:25:40-07:00 Gabriel Laddel
      ;; don't assume freenode is the only channel
      (progn (rcirc-join-channels (car (rcirc-process-list)) '("#bitcoin-assets"))
	     (pop-to-buffer "#bitcoin-assets@irc.freenode.net"))
    (progn (setq old-rcirc-server-alist rcirc-server-alist)
	   (setq rcirc-server-alist '(("irc.freenode.net" :channels
				       ("#bitcoin-assets"))))
	   (rcirc nil)
	   (setq rcirc-server-alist old-rcirc-server-alist)
	   (run-at-time "1 seconds" nil (lambda () (pop-to-buffer "#bitcoin-assets@irc.freenode.net")))))
  (delete-other-windows))

(defun* mm:dashboard ()
  (interactive)
  (slime-eval-async '(mmg::run-or-focus-dashboard)))

(defun* mm:kgraph ()
  (interactive)
  (slime-eval-async '(mmg::run-or-focus-kgraph)))

(defun ppath (string)
  "[p]roject [path]name"
  (cl-format nil "~~/quicklisp/local-projects/masamune/~a"
	     (if (string= "/" (subseq string 0 1)) (subseq string 1) string)))

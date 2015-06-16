;;; -*- lexical-binding: t -*-

(require 'cl-lib)
(require 'cl)
(require 'package)
(add-to-list 'load-path (expand-file-name "~/.emacs.d/elpa/"))
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/") 
			 ("marmalade" . "http://marmalade-repo.org/packages/")
			 ("melpa" . "http://melpa.milkbox.net/packages/")
			 ("org" . "http://orgmode.org/elpa/")))
(package-initialize)

;;; these packages are internal to emacs
(require 'auth-source)
(require 'dired-x)
(require 'doc-view)
(require 'easymenu)
(require 'eldoc)
(require 'ibuffer)
(require 'info)
(require 'json)
(require 'org)
(require 'org-agenda)
(require 'rcirc)
(require 'sh-script)
(require 'simple)
(require 'timer)
(require 'winner)

(loop with tpe = "~/quicklisp/local-projects/masamune/third-party-elisp/"
      for i in '(ac-helm anaphora auto-complete
			 auto-highlight-symbol autopair bookmark+
			 cl-format dash dash-functional desktop
			 dired+ dired-details dired-details+
			 dired-filter dired-hack-utils
			 dired-rainbow elisp-slime-nav
			 git-commit-mode git-link git-rebase
			 find-file-in-repository flx flx-ido helm
			 helm-dictionary helm-projectile
			 helm-projectile-all helm-themes
			 highlight-symbol ibuffer-vc imaxima
			 latex.el list-processes+ magit
			 magit-gh-pulls magit-gitflow
			 magit-topgit paredit pcache
			 persistent-soft popup pretty-lambdada
			 rainbow-delimiters redshank request s
			 shadchen tco thingatpt+.el uuid vkill
			 websocket wgrep)      
      do (add-to-list 'load-path (expand-file-name (concatenate 'string tpe (symbol-name i)))))

(require 'anaphora)
(require 'auto-complete)
(require 'auto-highlight-symbol)
(require 'autopair)
(require 'bookmark+)
(require 'cl-format)
(require 'cl-lib)
(require 'dash)
(require 'desktop)
(require 'dired+)
(require 'dired-details+)
(require 'dired-filter)
(require 'dired-rainbow)
(require 'elisp-slime-nav) 
(require 'find-file-in-repository)
(require 'flx-ido)
(require 'helm)
(require 'highlight-symbol)
(require 'ibuffer-vc)
(require 'list-processes+)
(require 'magit)
(require 'paredit)
(require 'pcache)
(require 'persistent-soft)
(require 'popup)
(require 'pretty-lambdada)
(require 'rainbow-delimiters)
(require 'rcirc)
(require 'redshank)
(require 'request)
(require 's)
(require 'shadchen)
(require 'skeleton)
(require 'tco)
(require 'uuid)
(require 'vkill)
(require 'websocket)
(require 'wgrep)
(require 'helm-config)
(require 'js)
;; (require 'ace-jump-mode)

(cl-defun cat (&rest args) 
  (apply #'concatenate 'string (mapcar (lambda (x) (if (stringp x) x (p1 x))) args)))

(defun latest-swank ()
  (or (and (file-exists-p "~/quicklisp/local-projects/slime/")
	   "~/quicklisp/local-projects/slime/")
      (let* ((quicklisp-dists-dir "~/quicklisp/dists/quicklisp/software/")
	     (slime-dir-name (find-if (lambda (s) (string-match-p "slime" s))
				      (directory-files quicklisp-dists-dir))))
	(when slime-dir-name
	  (cat quicklisp-dists-dir slime-dir-name)))
      (error "could not locate swank directory")))

(add-to-list 'load-path (latest-swank))  
(add-to-list 'load-path (cat (latest-swank) "/contrib"))

(defun ls-clean (dir)
  (remove-if (lambda (s) (or (s-prefix? "." s) (s-prefix? "#" s)
			(s-suffix? "#" s) (s-suffix? "~" s)))
	     (directory-files dir)))

(let* ((dir "~/quicklisp/local-projects/masamune/third-party-elisp/"))
  (dolist (path (mapcar (lambda (s) (cat dir  s "/")) (ls-clean dir)))
    (add-to-list 'load-path path)))

(setq slime-backend (expand-file-name (cat (latest-swank) "swank-loader.lisp"))
      slime-path (latest-swank)
      slime-contribs '(slime-editing-commands
		       slime-fancy
		       slime-fancy-inspector
		       slime-fontifying-fu
		       slime-fuzzy
		       slime-indentation
		       slime-media
		       slime-package-fu
		       slime-references
		       slime-repl
		       slime-sbcl-exts
		       slime-scratch
		       slime-sprof
		       slime-xref-browser
		       slime-asdf)
      slime-use-autodoc-mode         t
      slime-autodoc-use-multiline-p  t
      slime-enable-evaluate-in-emacs t
      slime-protocol-version 'ignore
      inferior-lisp-program (car (split-string (subseq (shell-command-to-string "whereis sbcl") 5) " " t))
      inferior-lisp-buffer           "*slime-repl sbcl*"
      slime-complete-symbol*-fancy t
      slime-complete-symbol-function 'slime-fuzzy-complete-symbol
      common-lisp-hyperspec-root (cat "file://" (expand-file-name "~/lisp/HyperSpec/")))

(require 'slime-autoloads)
(require 'slime)
(slime-setup)

(load-file "~/quicklisp/local-projects/masamune/util.el")
(load-file "~/quicklisp/local-projects/masamune/editing.el")	   ; Editing utilities
(load-file "~/quicklisp/local-projects/masamune/masamune.el")	   ; application code
(load-file "~/quicklisp/local-projects/masamune/clhs.el")
;; (load-file "~/quicklisp/local-projects/masamune/third-party-elisp/parenscript-mode.el")
(load-file "~/quicklisp/local-projects/masamune/save-state.el")

;; (defun steal-slime-keys-for-parenscript! ()
;;   ;; Don't affect all SLIME buffers, just where invoked
;;   (make-local-variable 'slime-mode-map)
;;   (let ((map slime-mode-map))
;;     (define-key map (kbd "C-x C-e") nil)
;;     (define-key map (kbd "C-c C-r") nil)
;;     (define-key map (kbd "C-M-x")   nil)
;;     (define-key map (kbd "C-c C-k") nil)
;;     (define-key map (kbd "C-c C-m") nil))
;;   (let ((map parenscript-mode-map))
;;     (define-key map (kbd "C-x C-e") 'parenscript-eval-last-expression)
;;     (define-key map (kbd "C-c C-r") 'parenscript-eval-region)
;;     (define-key map (kbd "C-M-x")   'parenscript-eval-defun)
;;     (define-key map (kbd "C-c C-k") 'parenscript-eval-buffer)
;;     (define-key map (kbd "C-c C-m") 'parenscript-expand-dwim)))

;; (add-hook 'parenscript-mode-hook 'steal-slime-keys-for-parenscript!)

;; (add-to-list 'auto-mode-alist (cons "\\.paren\\'" 'lisp-mode))
;; (add-hook 'lisp-mode-hook
;; 	  #'(lambda ()
;; 	      (when (and buffer-file-name
;; 			 (string-match-p "\\.paren\\>" buffer-file-name))
;; 		(unless (slime-connected-p)
;; 		  (save-excursion (slime)))
;; 		(parenscript-mode +1))))

(put 'dired-find-alternate-file 'disabled nil)
(setq inhibit-startup-message t      
      slime-eval-in-emacs t
      next-line-add-newlines nil
      default-justification 'left
      ido-use-faces nil
      enable-recursive-minibuffers t
      calendar-week-start-day 1		; mon
      doc-view-continuous t
      column-number-mode t
      package-load-list '(all)
      ring-bell-function (lambda () (message "*beep*"))
      default-fill-column 80
      org-startup-folded nil
      desktop-load-locked-desktop t
      bookmark-version-control t
      max-lisp-eval-depth 40000
      max-specpdl-size '100000
      highlight-symbol-idle-delay 0.2
      show-paren-delay 0
      tab-always-indent 'complete)
(elisp-slime-nav-mode 1)
(blink-cursor-mode -1)
(global-visual-line-mode 1); Proper line wrapping
(condition-case nil (display-battery-mode 1) (error nil))
(global-auto-revert-mode t)
(ido-mode t)
(ido-everywhere 1)
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(fringe-mode 0)
(winner-mode t)
(recentf-mode 1) 
(savehist-mode 1)
(flx-ido-mode 1)
(global-auto-complete-mode 0)
(global-pretty-lambda-mode)
(global-rainbow-delimiters-mode t)
(auto-compression-mode t)
(random t) ;; Seed the random-number generator

(defalias 'yes-or-no-p 'y-or-n-p)
(defalias 'file-as-sexps #'read-sexps-from)
(defalias 'slurp-sexps #'read-sexps-from)
(defalias 'p1 'prin1-to-string)
(defun browse-url (url) (mm:open-uri url t))

(defun gui-off ()
  (interactive)
  (menu-bar-mode 0)
  (tool-bar-mode 0)
  (scroll-bar-mode 0))

(defun gui-on ()
  (interactive)
  (menu-bar-mode 1)
  (tool-bar-mode 1)
  (scroll-bar-mode 1))

(defun toggle-gui ()
  (interactive)
  (if menu-bar-mode (gui-off) (gui-on))
  (fringe-mode 0))

(toggle-gui)

(defun true-fullscreen? ()
  (equal 'fullboth (frame-parameter nil 'fullscreen)))

(defun toggle-fullscreen (&optional f)
  (interactive)
  ;; I've not the slightest idea what `old-fullscreen' is doing here.
  (set-frame-parameter nil 'fullscreen
		       (if (true-fullscreen?)
			   (progn (fringe-mode 0) (when (boundp 'old-fullscreen) old-fullscreen))
			 (progn (fringe-mode 0) 
				(setq old-fullscreen (true-fullscreen?)) 
				'fullboth))))

(defun current-theme ()
  "Symbol name of the current theme"
  (car (some #'custom-theme-enabled-p (custom-available-themes))))

(defun load-theme-disable-current (theme)
  (disable-theme (current-theme))
  (load-theme theme)
  (set-font-size 100))

(defun rotate-snazzy-theme ()
  (interactive)
  (llet ((snazzy-themes '(leuven
			  twilight-bright
			  solarized-dark
			  cyberpunk
			  tsdh-light
			  grandshell
			  colorsarenice-dark
			  dakrone
			  gruvbox
			  ir-black
			  light-blue
			  subatomic-enhanced
			  hemisu-light
			  molokai))
	 (i (-find-index (lambda (s) (equal s (current-theme))) snazzy-themes))
	 (new-i (cond ((null i) 0)
		      ((equal (length snazzy-themes) (1+ i)) 0)
		      (t (1+ i))))
	 (old-theme (when i (nth i snazzy-themes)))
	 (new-theme (nth new-i snazzy-themes)))
    (disable-theme (current-theme))
    (load-theme new-theme))
  (set-font-size 100))

(defun set-font-size (n)
  (interactive "nSize: ")
  (set-face-attribute 'default nil :height n))

;;; magit 
;;; ============================================================================

(defadvice magit-status (around magit-fullscreen activate)
  (window-configuration-to-register :magit-fullscreen)
  ad-do-it
  (delete-other-windows))

(ad-activate 'magit-status)

(defun magit-quit-session ()
  "Restores the previous window configuration and kills the magit buffer"
  (interactive)
  (kill-buffer)
  (jump-to-register :magit-fullscreen))

(defun magit-goto-next-sibiling-section-like-sldb ()
  (interactive)
  (magit-section-hideshow (lambda (s) (magit-section-set-hidden s t)))
  (magit-goto-next-sibling-section)
  (magit-section-hideshow (lambda (s) (magit-section-set-hidden s nil))))

(defun magit-goto-previous-sibiling-section-like-sldb ()
  (interactive)
  (magit-section-hideshow (lambda (s) (magit-section-set-hidden s t)))
  (magit-goto-previous-sibling-section)
  (magit-section-hideshow (lambda (s) (magit-section-set-hidden s nil))))

(define-key magit-status-mode-map (kbd "M-n") 
  'magit-goto-next-sibiling-section-like-sldb)
(define-key magit-status-mode-map (kbd "M-p") 
  'magit-goto-previous-sibiling-section-like-sldb)
(define-key magit-status-mode-map (kbd "q") 'magit-quit-session)

(defun vc-annotate-quit ()
  "Restores the previous window configuration and kills the vc-annotate buffer"
  (interactive)
  (kill-buffer)
  (jump-to-register :vc-annotate-fullscreen))

(eval-after-load "vc-annotate"
  '(progn
     (defadvice vc-annotate (around fullscreen activate)
       (window-configuration-to-register :vc-annotate-fullscreen)
       ad-do-it
       (delete-other-windows))
     (define-key vc-annotate-mode-map (kbd "q") 'vc-annotate-quit)))

;; ignore whitespace

(defun magit-toggle-whitespace ()
  (interactive)
  (if (member "-w" magit-diff-options)
      (magit-dont-ignore-whitespace)
    (magit-ignore-whitespace)))

(defun magit-ignore-whitespace ()
  (interactive)
  (add-to-list 'magit-diff-options "-w")
  (magit-refresh))

(defun magit-dont-ignore-whitespace ()
  (interactive)
  (setq magit-diff-options (remove "-w" magit-diff-options))
  (magit-refresh))

(define-key magit-status-mode-map (kbd "W") 'magit-toggle-whitespace)

;;; Maxima! 
;;; ============================================================================

(add-to-list 'load-path (expand-file-name "~/quicklisp/local-projects/masamune/third-party-elisp/imaxima/"))
;; (add-to-list 'Info-directory-list "/usr/local/share/info/")
(autoload 'imaxima "imaxima" "Image support for Maxima." t)
(autoload 'imath-mode "imath" "Interactive Math minor mode." t)

(require 'imaxima)
(require 'imath)

;;; misc
;;; ============================================================================

(defun mm:open-uri (uri &optional focus-browser)
  (slime-eval-async `(mmb::open-uri ,uri ,focus-browser)))

(defun dired-browser-find-file ()
  (interactive)
  (mm:open-uri (cat "file://" (dired-get-file-for-visit)) t))

(add-hook 'dired-load-hook (lambda () (load "dired-x")))
(add-hook 'dired-mode-hook 'dired-omit-mode)
(setq dired-recursive-deletes 'always)

(defun buffer-around? (buffer-or-name)
  (awhen (if (stringp buffer-or-name)
	     (member buffer-or-name (buffer-name-list))
	   (member buffer-or-name (buffer-list)))
    (car it)))

(defun* finalize-boot ()
  (interactive)
  (if (buffer-around? "*slime-repl sbcl*")
      (with-current-buffer "*slime-repl sbcl*"
	(save-excursion (end-of-buffer)
			(insert "(progn (setf cl-user::*swank-connection-hack* *standard-output*)
       (handler-bind
	((error #'(lambda (c) (declare (ignore c)) (invoke-restart (find-restart 'cl::continue)))))
	(ql:quickload '(glop cl-opengl)))
       (ql:quickload 'clim-listener)
       (handler-bind
	((error #'(lambda (c) (declare (ignore c)) (invoke-restart (find-restart 'ASDF/LISP-ACTION:TRY-RECOMPILING)))))
	(ql:quickload 'masamune)))"))
	(end-of-buffer)
	(slime-repl-return)
	(slime-repl-return))
    (run-at-time ".5 seconds" nil 'finalize-boot)))

(defun swank-port ()
  (let* ((stumpwm-output-file "~/.masamune/stumpwm-debug-output"))
    (when (file-exists-p stumpwm-output-file)
      (let* ((ss ";; Swank started at port: ")
	     (k (car (last (remove-if-not (lambda (s) (search ss s))
					  (split-string (slurp stumpwm-output-file) "\n" nil)))))
	     (k (subseq k (length ss) (- (length k) 1))))
	(car (read-from-string k))))))

(add-hook 'slime-connected-hook 'finalize-boot)
(loop with p = (ppath "/third-party-elisp/themes/") 
      for dir in (ls-clean p)
      do (add-to-list 'custom-theme-load-path (cat p dir)))
(unless (slime-connected-p) (slime-connect "127.0.0.1" (or (swank-port) 4005)))
(enable-masamune-keybindings)
(when (file-exists-p "~/quicklisp/local-projects/masamune/emacs-customizations.el")
  (load-file "~/quicklisp/local-projects/masamune/emacs-customizations.el"))
(server-start) ;; for emacsclient to connect to

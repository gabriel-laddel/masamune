;;; -*- lexical-binding: t -*-
;;; Emacs lisp 
;;; ============================================================================

(defun flash-cursor ()
  (interactive)
  (highlight-symbol-at-point)
  (run-at-time ".4 seconds" nil #'highlight-symbol-at-point))

(defun pretty-curry-compose ()
  "Compact functional combination display"
  (mapc (lambda (pair)
          (let ((regexp (car pair))
                (symbol (cdr pair)))
            (font-lock-add-keywords 'emacs-lisp-mode
				    `((,regexp
				       (0 (progn (compose-region (match-beginning 1) (match-end 1)
								 ,symbol)
						 nil)))))))
        '(("(\\(compose\\)[ \t\n\r]" . ?\∘)
          ("(\\(curry\\)[ \t\n\r]"   . ?\»)
          ("(\\(rcurry\\)[ \t\n\r]"  . ?\«))))

(defface highlight-symbol-face-0
  '((((class color) (background dark))
     (:background "dark red"))
    (((class color) (background light))
     (:background "cyan")))
  "Face used by `highlight-symbol-mode'."
  :group 'highlight-symbol)

(defun highlight-symbol-temp-highlight ()
  "Highlight the current symbol until a command is executed."
  (when highlight-symbol-mode
    (let ((symbol (highlight-symbol-get-symbol)))
      (unless (or (equal symbol highlight-symbol)
                  (highlight-symbol-symbol-highlighted-p symbol))
        (highlight-symbol-mode-remove-temp)
        (when symbol
          (setq highlight-symbol symbol)
          (highlight-symbol-add-symbol-with-face symbol 'highlight-symbol-face-0)
          (font-lock-fontify-buffer))))))

(font-lock-add-keywords 'emacs-lisp-mode
			'(("\\<\\(FIXME\\)"          1 font-lock-warning-face prepend)
			  ("\\<\\(TODO\\)"           1 font-lock-warning-face prepend)
			  ("\\<\\(XXX\\)"            1 font-lock-warning-face prepend)
			  ("\\<\\(NOTE\\)"           1 font-lock-warning-face prepend)
			  ("(\\(compose\\)[ \t\n\r]" 1 font-lock-keyword-face)
                          ("(\\(curry\\)[ \t\n\r]"   1 font-lock-keyword-face)
                          ("(\\(rcurry\\)[ \t\n\r]"  1 font-lock-keyword-face)
			  ("\\(?:a\\(?:\\(?:nd\\|ssert\\)
			\\)\\|clambda\\|def\\(?:\\(?:macro\\|un\\)\\*\\)\\|\\(?:l\\(?:let\\|oop\\)\\|not\\|or\\)
			\\)" . font-lock-keyword-face)))


(add-to-list 'auto-mode-alist '("\\.el\\'" . emacs-lisp-mode))
(add-to-list 'completion-styles 'initials t)
(add-to-list 'emacs-lisp-mode-hook 'pretty-curry-compose)

(add-hook 'emacs-lisp-mode-hook 'paredit-mode)
(add-hook 'emacs-lisp-mode-hook 'highlight-symbol-nav-mode)
(add-hook 'emacs-lisp-mode-hook 'eldoc-mode)
(add-hook 'emacs-lisp-mode-hook 'highlight-symbol-mode)
(add-hook 'emacs-lisp-mode-hook 'show-paren-mode)
(add-hook 'emacs-lisp-mode-hook 'turn-on-elisp-slime-nav-mode)

;;; CL 
;;; ============================================================================

(font-lock-add-keywords 'lisp-mode
			'(("\\<\\(FIXME\\)" 1 font-lock-warning-face prepend)
			  ("\\<\\(TODO\\)"  1 font-lock-warning-face prepend)
			  ("\\<\\(XXX\\)"   1 font-lock-warning-face prepend)
			  ("\\<\\(NOTE\\)"  1 font-lock-warning-face prepend)
			  ("\\<\\(?:\\(?:a\\(?:if\\|nd\\|when\\)\\|it\\(?:er\\)?\\|l\\(?:et\\+\\|let\\)\\|not\\|or\\) \\)\\>" 
			   . font-lock-keyword-face)))

(add-hook 'slime-repl-mode-hook 'highlight-symbol-mode)
(add-hook 'slime-repl-mode-hook 'paredit-mode)
(add-hook 'lisp-mode-hook 'highlight-symbol-mode)
(add-hook 'lisp-mode-hook 'paredit-mode)
(add-hook 'lisp-mode-hook 'highlight-symbol-nav-mode)
(add-hook 'lisp-interaction-mode-hook 'paredit-mode)
(add-hook 'lisp-interaction-mode-hook 'highlight-symbol-mode)

(defun make-buffer-package-current ()
  (interactive)
  (let ((to-insert (cl-format nil "(in-package ~a)" (slime-current-package))))
    (with-current-buffer "*slime-repl sbcl*"
      (end-of-buffer)
      (end-of-line)
      (insert to-insert)
      (slime-repl-return))))

(defun mm-tests ()
  (interactive) 
  (with-current-buffer "*slime-repl sbcl*"
    (end-of-buffer)
    (insert "(masamune-tests::test-masamune)")
    (slime-repl-return)))

;;; shared 
;;; ============================================================================

(defun indent-pp-sexp-at-point ()
  (interactive)
  (save-excursion (slime-beginning-of-defun) (indent-pp-sexp)))

(defun tlf ()
  (acond ((comment-block-on-line-p)   'comment-block)
	 ((thing-at-point 'defun t) (first (read-from-whole-string it)))))

(defun mm:cut-string ()
  (interactive)
  (insert "\"  \"")
  (backward-char)
  (backward-char))

(defun mm:nest-call ()
  "moves 'up' one level and inserts a wrapping sexp"
  (interactive)
  (paredit-backward-up)
  (paredit-wrap-round))

(defun comment-sexp-forward-dwim ()
  (interactive)
  ;; TODO 2014-07-11T12:38:37-07:00 Gabriel Laddel
  ;; doesn't uncomment correctly.
  (if (paredit-comment-on-line-p)
      (save-excursion (call-interactively #'set-mark-command)
      		      (loop while (paredit-comment-on-line-p)
      			    do (next-line)
      			    finally (paredit-comment-dwim)))
    (progn (mark-sexp)
  	   (paredit-comment-dwim))))

(defun new-shell ()
  (interactive)
  (when (member "*shell*" (buffer-name-list))
    (with-current-buffer "*shell*"
      (rename-uniquely)))
  (shell))

(defun create-new-buffer ()
  (interactive)
  (llet ((m (case major-mode
	      ('text-mode           major-mode)
	      ('emacs-lisp-mode     major-mode)
	      ('common-lisp-mode    major-mode)
	      ('lisp-mode          'lisp-mode)
	      ('inferior-lisp-mode 'lisp-mode)
	      ('slime-repl-mode    'lisp-mode)
	      ('sldb-mode          'lisp-mode)
	      ('shell-mode          major-mode)
	      (t  'emacs-lisp-mode)))
	 (cl-package (when (equal m 'lisp-mode)
	 	       (intern (slime-current-package)))))
    (switch-to-buffer (generate-new-buffer-name "*new*"))
    (eval `(,m))
    (when cl-package
      (beginning-of-buffer)
      (insert (downcase (s-replace "\\" "" (cat "(in-package " cl-package ")"))))
      (forward-sexp))
    (paredit-newline)
    (paredit-newline)
    (when (equal 'emacs-lisp-mode m) (emacs-lisp-mode 1))))

(defun insert-lisp-comment (x)
  "FIXME  -  mark potential problematic code that requires special attention and
             or review.
NOTE      -  document inner workings of code and indicate potential pitfalls.
TODO      -  indicate planned enhancements.
XXX       -  warn other programmers of problematic or misguiding code. 
(h)eading -  denote section"
  (interactive "s(t)ODO, (f)IXME, (n)OTE, (x)XX, or (h)eading?")
  (if (equal x "h")
      (progn (save-excursion (paredit-comment-dwim)
			     (newline)
			     (paredit-comment-dwim)
			     (->> (loop for i from 0 to (- 79 (current-column)) collect "=")
			       (apply #'cat)
			       (insert)))
             (previous-line)
	     (loop repeat 4 do (forward-char) finally (insert " "))) 
    (let ((heading nil))
      (cond ((equal x "x") (setq heading "XXX"))
      	    ((equal x "t") (setq heading "TODO"))
      	    ((equal x "f") (setq heading "FIXME"))
      	    ((equal x "n") (setq heading "NOTE"))	  
      	    (t (message "TODO, implement restarts for interactive and, read-single-char.")))
      (progn (paredit-comment-dwim)
	     (insert heading " " (iso-now) " Gabriel Laddel" "\n")
	     (paredit-comment-dwim)))))

;; refactoring
;; =============================================================================

(defvar *rename-point* nil 
  "When finished renaming code, jump back to previoius point, i.e. this var")

(defadvice highlight-symbol-query-replace
    (before set-previous-rename-point first)
  (setf *rename-point* (point)))

(defadvice highlight-symbol-query-replace
    (after set-previous-rename-point last)
  (goto-char *rename-point*))

(ad-activate 'highlight-symbol-query-replace)

(defun* next-hl-sym-maybe-slime-next-note ()
  (interactive)
  (if (equal 'slime-repl-mode major-mode) (slime-repl-forward-input)
    (if (slime-find-next-note) (slime-next-note)
      (highlight-symbol-next))))

(defun* previous-hl-sym-maybe-slime-previous-note ()
  (interactive)
  (if (equal 'slime-repl-mode major-mode) (slime-repl-backward-input)
    (if (slime-find-next-note) (slime-previous-note)
      (highlight-symbol-prev))))

(defun dired-copy-filename-to-kill-ring ()
  (interactive)
  (kill-new (dired-file-name-at-point)))

(defun jump-to-compilation-buffer ()
  (interactive)
  (pop-to-buffer "*slime-compilation*"))

(defun define-word ()
  (interactive)
  (mm:open-uri (cat "g define " (let* ((default (acond ((region-no-properties) it)
						       ((thing-at-point 'sexp t) it)
						       (t nil))))
				  (if default
				      (let* ((input (read-string (cat "Define (default, " default "): "))))
					(if (string= "" (s-trim input))
					    default
					  input))
				    (read-string "Define: ")))) t))

(defun toggle-compose-mode ()
  (interactive)
  (if (loop for (k . v) in default-frame-alist
	    when (or (and (eq 'left-fringe k) (/= 0 v))
		     (and (eq 'right-fringe k) (/= 0 v))) 
	    return t)
      (progn (fringe-mode 0)
	     (jump-to-register :compose-mode))
    (progn (window-configuration-to-register :compose-mode)
	   (delete-other-windows)
	   (fringe-mode 400))))

(defun slime-visit-at-point ()
  (interactive)
  (let* ((url (thing-at-point 'url t))
	 (filename (thing-at-point 'filename t)))
    (cond ((and url filename)
	   (if (string= "http" (take 4 url))
	       (mm:open-uri url t)
	       (find-file filename)))
	  (filename (find-file filename))
	  (url (mm:open-uri url t))
	  (t (error "couldn't determine what I'm supposed to be visiting")))))

(defmacro mm:define-key (key-string fn)
  `(progn (define-key lisp-mode-map       ,(kbd key-string) ,fn)
	  (define-key slime-repl-mode-map ,(kbd key-string) ,fn)
	  (define-key slime-mode-map      ,(kbd key-string) ,fn)
	  (define-key emacs-lisp-mode-map ,(kbd key-string) ,fn)))

(defun enable-masamune-keybindings ()
  ;; other
  (define-key dired-mode-map (kbd "C-c n") 'create-new-buffer)
  (define-key org-mode-map (kbd "C-c n") 'create-new-buffer)
  (define-key org-agenda-mode-map (kbd "C-c n") 'create-new-buffer)
  (mm:define-key "C-c n" 'create-new-buffer)
  (mm:define-key "C-c SPC" 'ace-jump-mode)
  (define-key dired-mode-map (kbd "M-n") 'dired-copy-filename-to-kill-ring)
  (define-key text-mode-map (kbd "C-c SPC") 'ace-jump-mode)
  (global-set-key (kbd "M-o v")  'mm:open)
  (global-set-key [f5] 'toggle-compose-mode)
  (mm:define-key "M-o ;" 'flash-cursor)
  (mm:define-key "M-o t"   'mm-tests)
  (mm:define-key "M-o g"   'google)
  (mm:define-key "M-o M-g" 'define-word)
  (mm:define-key "M-o b"   'mm:document-buffer)
  (mm:define-key "M-o s"   'redshank-slot-spec-skeleton)
  (mm:define-key "M-o q"   'query-replace)
  (mm:define-key "M-o m"   'remember)
  (mm:define-key "M-o h"   'hyperspec-lookup)
  (mm:define-key "M-o n"   'mm:define)
  (mm:define-key "M-o M-n" 'goog-define)
  (mm:define-key "M-o v"   'mm:open)
  (mm:define-key "M-o w"   'wikipedia-lookup)
  (mm:define-key "M-o d"   'mm:dashboard)
  (mm:define-key "M-o k"   'mm:kgraph)
  (mm:define-key "M-o r"   'highlight-symbol-query-replace)
  (mm:define-key "C-M-;"   'comment-sexp-forward-dwim)
  (mm:define-key "C-M-k"   'kill-sexp)
  (mm:define-key "M-o f"     'slime-visit-at-point)
  (mm:define-key "C-c C-q" 'mm:cut-string)
  (mm:define-key "C-c a"   'redshank-align-slot-specs-in-form)
  (mm:define-key "M-RET"   'mm:nest-call)
  (mm:define-key "M-o c"   'redshank-defclass-skeleton)
  (define-key lisp-mode-map (kbd "M-n") 'highlight-symbol-next)
  (define-key emacs-lisp-mode-map (kbd "M-n") 'highlight-symbol-next)
  (define-key lisp-mode-map (kbd "M-p") 'highlight-symbol-previous)
  (define-key emacs-lisp-mode-map (kbd "M-p") 'highlight-symbol-previous)
  (mm:define-key "C-c n"   'create-new-buffer)
  (mm:define-key "C-c C-n" 'split-to-new-buffer)

  (define-key lisp-mode-map       (kbd "M-o p") 'make-buffer-package-current)
  (define-key compilation-mode-map   (kbd "C-c SPC")   'ace-jump-mode)
  (define-key lisp-mode-map (kbd "M-o C-c") 'jump-to-compilation-buffer)
  (define-key slime-repl-mode-map (kbd "M-o C-c") 'jump-to-compilation-buffer)
  (define-key slime-mode-map (kbd "M-o C-c") 'jump-to-compilation-buffer)
  (define-key slime-mode-map (kbd "C-c C-k") 'slime-compile-and-load-file)

  (global-set-key (kbd "C-x 5")   'mm:dashboard)
  (global-set-key (kbd "C-x C-b") 'ibuffer)
  (global-set-key (kbd "C-c 2")   'find-function)
  (global-set-key (kbd "C-c 3")   'highlight-symbol-nav-mode)
  (global-set-key (kbd "C-c 4")   'highlight-symbol-at-point)
  (global-set-key (kbd "C-c 5")   'highlight-symbol-query-replace)
  (global-set-key (kbd "C-c C-2") 'slime-edit-definition)
  (global-set-key (kbd "C-c C-z") 'slime-switch-to-output-buffer)
  (global-set-key (kbd "C-c C-a") (clambda () (interactive) 
					   (llet ((old (buffer-name (current-buffer))))
					     (pop-to-buffer "*Messages*")
					     (end-of-buffer)
					     (pop-to-buffer old))))

  (define-key org-mode-map        (kbd "C-c C-z") 'slime-switch-to-output-buffer)
  (define-key org-agenda-mode-map (kbd "C-c C-z") 'slime-switch-to-output-buffer)
  (define-key org-mode-map        (kbd "C-c SPC") 'ace-jump-mode)
  (define-key org-mode-map        (kbd "M-o M-d") 'define-word)
  ;; org
  (define-key org-mode-map        (kbd "M-o n") 'mm:define)
  (define-key org-mode-map (kbd "M-o ;") 'flash-cursor)
  (define-key org-mode-map        (kbd "M-o M-n") 'goog-define)
  (define-key org-mode-map        (kbd "C-c C-z") 'slime-switch-to-output-buffer)
  (define-key org-mode-map        (kbd "C-c C-e") 'slime-interactive-eval)
  ;; org agenda
  (define-key org-agenda-mode-map (kbd "C-c C-z") 'slime-switch-to-output-buffer)
  (define-key org-agenda-mode-map (kbd "C-c C-e") 'slime-interactive-eval)
  ;; dired
  (define-key dired-mode-map      (kbd "C-c C-e") 'slime-interactive-eval)
  (define-key dired-mode-map      (kbd "C-M-o")   'find-name-dired)
  (define-key dired-mode-map      (kbd "e")       'dired-browser-find-file)
  ;; elisp
  (define-key emacs-lisp-mode-map (kbd "C-c C-r") 'eval-region)
  (define-key emacs-lisp-mode-map (kbd "C-c C-k") 'eval-buffer)
  ;; slime
  (define-key slime-mode-map      (kbd "C-c C-z") 'slime-switch-to-output-buffer)
  (define-key slime-repl-mode-map (kbd "C-c C-d") 'slime-describe-symbol)
  (define-key slime-repl-mode-map (kbd "C-c C-e") 'slime-interactive-eval)
  (define-key slime-repl-mode-map (kbd "C-c C-z") 'slime-clear-presentations)
  ;; english editing
  (define-key text-mode-map (kbd "M-o d") 'mm:dashboard)
  (define-key text-mode-map (kbd "M-o ;") 'flash-cursor)
  (define-key text-mode-map (kbd "C-c n") 'create-new-buffer)
  (define-key text-mode-map (kbd "M-o n") 'mm:define)
  (define-key text-mode-map (kbd "M-o M-n") 'goog-define)
  (define-key text-mode-map (kbd "M-o w") 'wikipedia-lookup)
  (define-key text-mode-map (kbd "M-o m") 'remember)   
  ;; org agenda mode
  (define-key org-agenda-mode-map (kbd "g") 'mm:dashboard)
  ;; org-mode
  (define-key org-mode-map (kbd "C-M-<return>") (clambda () (interactive) (org-html-export-to-html)))
  ;; rcirc
  (define-key rcirc-mode-map (kbd "M-o d") 'mm:dashboard)
  (define-key rcirc-mode-map (kbd "M-o m") 'remember)
  (define-key rcirc-mode-map (kbd "C-c n") 'create-new-buffer)
  (define-key rcirc-mode-map (kbd "C-c C-n") 'split-to-new-buffer)
  ;; from my dot emacs
  (global-unset-key (kbd "C-x C-c"))
  (global-set-key (kbd "C-x C-m")   'execute-extended-command)
  (global-set-key (kbd "C-c C-o")   'rgrep)
  ;; (global-set-key (kbd "C-c SPC")   'ace-jump-mode)
  (global-set-key (kbd "C-x 7")     'revert-buffer)
  (global-set-key (kbd "C-x 8")     'kill-buffer-and-window)
  (global-set-key (kbd "C-x <tab>") 'other-window)
  (global-set-key (kbd "C-x C-u")   'magit-status)
  ;; (global-set-key (kbd "C-x SPC")   'ace-jump-mode-pop-mark)
  (global-set-key (kbd "S-<down>")  'windmove-down)
  (global-set-key (kbd "S-<left>")  'windmove-left)
  (global-set-key (kbd "S-<right>") 'windmove-right)
  (global-set-key (kbd "S-<up>")    'windmove-up)
  (global-set-key [f10]             'insert-lisp-comment)
  (global-set-key [f8]              'fill-paragraph)

  (define-key emacs-lisp-mode-map (kbd "C-M-r")     'eval-defun)
  (define-key emacs-lisp-mode-map (kbd "C-c C-e")   'slime-interactive-eval)

  ;; (define-key org-agenda-mode-map (kbd "C-SPC")     'ace-jump-mode)
  ;; (define-key org-agenda-mode-map (kbd "C-c SPC")   'ace-jump-mode)
  ;; (define-key org-agenda-mode-map (kbd "C-c SPC")   'ace-jump-mode)
  (define-key org-agenda-mode-map (kbd "S-<down>")  'windmove-down)
  (define-key org-agenda-mode-map (kbd "S-<left>")  'windmove-left)
  (define-key org-agenda-mode-map (kbd "S-<right>") 'windmove-right)
  (define-key org-agenda-mode-map (kbd "S-<up>")    'windmove-up)

  ;; (define-key org-mode-map        (kbd "C-c SPC")   'ace-jump-mode)
  (define-key org-mode-map        (kbd "S-<down>")  'windmove-down)
  (define-key org-mode-map        (kbd "S-<left>")  'windmove-left)
  (define-key org-mode-map        (kbd "S-<right>") 'windmove-right)
  (define-key org-mode-map        (kbd "S-<up>")    'windmove-up)

  (define-key dired-mode-map (kbd "C-o")   'dired-display-file)
  (define-key dired-mode-map (kbd "M-b")   'backward-word)
  (define-key dired-mode-map (kbd "C-c t")  (clambda () (interactive) (async-shell-command "tree")))
  ;; doc view mode
  (define-key doc-view-mode-map (kbd "i") 'doc-view-next-line-or-next-page)
  (define-key doc-view-mode-map (kbd "o") 'doc-view-previous-line-or-previous-page)
  (define-key doc-view-mode-map (kbd "r") 'doc-view-previous-line-or-previous-page)
  (define-key doc-view-mode-map (kbd "a") 'doc-view-next-line-or-next-page)
  (define-key doc-view-mode-map (kbd "s") 'doc-view-previous-page)
  (define-key doc-view-mode-map (kbd "t") 'doc-view-next-page)
  ;; sldb
  (define-key sldb-mode-map (kbd "M-o f") 'slime-visit-at-point)
  ;; js
  (define-key js-mode-map (kbd "C-c n") 'create-new-buffer)
  ;; maxima
  (define-key comint-mode-map (kbd "C-c n") 'create-new-buffer)
  ;; evaluation order dependent
  (define-key slime-mode-map (kbd "C-x C-e") 'slime-eval-last-expression)
  (define-key slime-mode-map (kbd "C-c C-m") 'slime-macroexpand-1)
  (define-key lisp-mode-map (kbd "TAB") 'slime-indent-and-complete-symbol))


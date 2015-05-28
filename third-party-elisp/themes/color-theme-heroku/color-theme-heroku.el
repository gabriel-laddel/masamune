;;; color-theme-heroku.el --- Heroku color theme
;;
;; Based on the aesthetics of Heroku articles such as
;; http://devcenter.heroku.com/articles/python
;;
;; Copyright 2013 Jonathan Chu
;;
;; Author: Jonathan Chu <jonathan.chu@me.com>
;; URL: https://github.com/jonathanchu/color-theme-heroku
;; Version: 1.0.0
;;
;; Installation (Emacs 23 and below)
;; 1. Install the color-theme package
;;   (http://www.emacswiki.org/cgi-bin/wiki/ColorTheme)
;; 2. Load this file i.e. (load-file "~/.emacs.d/vendor/custom-color-themes/color-theme-heroku.el")
;; 3. (color-theme-heroku)

(defun color-theme-heroku ()
  (interactive)
  (color-theme-install
   '(color-theme-heroku
      ((background-color . "#3f464c")
      (background-mode . light)
      (border-color . "#ffffff")
      (cursor-color . "#fce94f")
      (foreground-color . "#eeeeec")
      (mouse-color . "black"))
     (fringe ((t (:background "#3f464c"))))
     (header-line ((t (:foreground "#f0dfaf" :background "#2b2b2b"))))
     (highlight ((t (:background "#2b2b2b"))))
     (region ((t (:background "#6c91be"))))

     (mode-line ((t (:foreground "#030303" :background "#bdbdbd"
                                 :box (:line-width 1 :color "#000000" :style released-button)))))
     ;; uncomment and use the following mode-line for use with emacs-powerline
     ;; (https://github.com/jonathanchu/emacs-powerline.git)
     ;; (mode-line ((t (:foreground "#030303" :background "#bdbdbd"))))

     (minibuffer-prompt ((t (:foreground "#729fcf" :bold t))))

     ;; magit
     (magit-log-sha1 ((t (:foreground "#cf6a4c"))))
     (magit-log-head-label-local ((t (:foreground "#3387cc"))))
     (magit-log-head-label-remote ((t (:foreground "#65b042"))))
     (magit-branch ((t (:foreground "#fbde2d"))))
     (magit-section-title ((t (:foreground "#adc6ee"))))
     (magit-item-highlight ((t (:background "#6c91be"))))

     ;; hl-line-mode
     (hl-line-face ((t (:background "#2b2b2b"))))

     ;; font lock
     (font-lock-builtin-face ((t (:foreground "#ffffff"))))
     (font-lock-comment-face ((t (:foreground "#aeaeae"))))
     (font-lock-function-name-face ((t (:foreground "#ffffff"))))
     (font-lock-keyword-face ((t (:foreground "#fbde2d"))))
     (font-lock-string-face ((t (:foreground "#adc6ee"))))
     (font-lock-type-face ((t (:foreground"#ffffff"))))
     (font-lock-variable-name-face ((t (:foreground "#fbde2d"))))
     (font-lock-warning-face ((t (:foreground "Red" :bold t))))

     ;; show-paren
     (show-paren-match-face ((t (:foreground "#000000" :background "#F0F6FC" :weight bold))))
     (show-paren-mismatch-face ((t (:foreground "#960050" :background "#1E0010" :weight bold))))

     ;; search
     (isearch ((t (:foreground "#a33a37" :background "#f590ae"))))
     (isearch-fail ((t (:foreground "#ffffff" :background "#f590ae"))))
     (lazy-highlight ((t (:foreground "#465457" :background "#000000"))))

     ;; ido mode
     (ido-first-match ((t (:foreground "#fbde2d" :weight bold))))
     (ido-only-match ((t (:foreground "#d8fa3c" :weight bold))))
     (ido-subdir ((t (:foreground "#adc6ee"))))

     ;; org-mode
     (org-agenda-date-today
      ((t (:foreground "white" :slant italic :weight bold))) t)
     (org-agenda-structure
      ((t (:inherit font-lock-comment-face))))
     (org-archived ((t (:foreground "#eeeeec" :weight bold))))
     (org-checkbox ((t (:background "#5f5f5f" :foreground "white"
                                    :box (:line-width 1 :style released-button)))))
     (org-date ((t (:foreground "#8cd0d3" :underline t))))
     (org-deadline-announce ((t (:foreground "#8787FF"))))
     (org-done ((t (:bold t :weight bold :foreground "#bff740"))))
     (org-headline-done ((t (:foreground "#8787ff"))))
     (org-level-1 ((t (:foreground "#dfaf8f"))))
     (org-level-2 ((t (:foreground "#aeaeae"))))
     (org-level-3 ((t (:foreground "#94bff3"))))
     (org-level-4 ((t (:foreground "#e0cf9f"))))
     (org-level-5 ((t (:foreground "#93e0e3"))))
     (org-level-6 ((t (:foreground "#8fb28f"))))
     (org-level-7 ((t (:foreground "#8c5353"))))
     (org-level-8 ((t (:foreground "#4c7073"))))
     (org-table ((t (:foreground "#8787FF"))))
     (org-todo ((t (:bold t :foreground "#e21d24" :weight bold))))
     (org-upcoming-deadline ((t (:inherit font-lock-keyword-face))))
     (org-warning ((t (:bold t :foreground "#cc9393"d :weight bold))))
     )))
(provide 'color-theme-heroku)

;;; color-theme-heroku.el ends here

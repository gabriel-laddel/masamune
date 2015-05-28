;;; tronesque-theme.el --- Color Theme based on Tron universe.

;; Author: Aurélien Bottazini <aurelienbottazini.com>
;; URL: https://github.com/aurelienbottazini/tronesque
;; Version: 20130822.1536
;; X-Original-Version: 1.3
;;
;;; Commentary:

;; In your init file:
;; (load-theme 'tronesque)
;;
;; You can also add:
;; (tronesque-mode-line)
;; To get a custom mode-line with additional colors
;;
;; Supports both Emacs with a window system and Emacs within a
;; terminal. When used within a terminal you should use tronesque
;; themed ansi colors (available in github repository)


(deftheme tronesque
  "Theme based on Tron universe. Colors are inspired / taken from the movies.
More information on Tron: https://en.wikipedia.org/wiki/Tron")

(let (
      (base00 (if (window-system) "#081724" "black"))
      (base01 (if (window-system) "#033340" "brightblack"))
      (base02 (if (window-system) "#1d5483" "brightyellow"))
      (base03 (if (window-system) "#2872b2" "brightblue"))
      (base04 (if (window-system) "#d3f9ee" "white"))
      (base05 (if (window-system) "#a6f3dd" "brightgreen"))
      (base06 (if (window-system) "#effffe" "brightwhite"))
      (base07 (if (window-system) "#fffed9" "brightcyan"))
      (red (if (window-system) "#ff694d" "red"))
      (orange (if (window-system) "#f5b55f" "brightred"))
      (yellow (if (window-system) "#fffe4e" "yellow"))
      (magenta (if (window-system) "#afc0fd" "brightmagenta"))
      (violet (if (window-system) "#96a5d9" "magenta"))
      (blue (if (window-system) "#bad6e2" "blue"))
      (cyan (if (window-system) "#d2f1ff" "cyan"))
      (green (if (window-system) "#68f6cb" "green")))

  (custom-theme-set-faces
   'tronesque

   `(button ((t (:background ,base00 :foreground ,magenta :inherit nil :box (:line-width 2 :style released-button)))))
   `(cursor ((t (:background ,base06))))
   `(custom-button ((t (:background ,magenta :foreground ,base00 :inherit nil :box (:line-width 2 :style released-button)))))
   `(custom-state ((t (:foreground ,green))))
   `(default ((t (:background ,base00 :foreground ,base04))))
   `(escape-glyph ((t (:foreground ,green))))
   `(flymake-errline ((t (:background ,red :foreground ,base00 :underline nil))))
   `(flymake-warnline ((t (:background ,orange :foreground ,base00 :underline nil))))
   `(font-lock-builtin-face ((t (:slant italic :foreground ,violet))))
   `(font-lock-comment-delimiter-face ((t (:foreground ,base02))))
   `(font-lock-comment-face ((t (:foreground ,base03))))
   `(font-lock-constant-face ((t (:weight bold :foreground ,magenta))))
   `(font-lock-doc-face ((t (:slant italic :foreground ,green))))
   `(font-lock-function-name-face ((t (:foreground ,blue))))
   `(font-lock-keyword-face ((t (:weight bold :foreground ,violet))))
   `(font-lock-negation-char-face ((t (:foreground ,red))))
   `(font-lock-preprocessor-face ((t (:foreground ,violet))))
   `(font-lock-regexp-grouping-backslash ((t (:inherit (bold)))))
   `(font-lock-regexp-grouping-construct ((t (:inherit (bold)))))
   `(font-lock-string-face ((t (:foreground ,green))))
   `(font-lock-type-face ((t (:foreground ,orange))))
   `(font-lock-variable-name-face ((t (:foreground ,red))))
   `(font-lock-warning-face ((t (:foreground ,orange :underline t))))
   `(fringe ((t (:foreground ,base04 :background ,base01))))
   `(header-line ((t (:box (:line-width -1 :color nil :style released-button) :foreground ,base00 :background ,base05))))
   `(highlight ((t (:background ,yellow :foreground ,base00))))
   `(hl-line ((t (:background ,base01 :inherit nil))))
   `(idle-highlight ((t (:background ,base01 :foreground nil))))
   `(ido-indicator ((t (:background ,red :foreground ,base00 :width condensed))))
   `(ido-only-match ((t (:foreground ,green))))
   `(ido-subdir ((t (:foreground ,red))))
   `(info-menu-star ((t (:foreground ,red))))
   `(info-xref ((t (:foreground ,green))))
   `(isearch ((t (:weight bold :foreground ,base00 :background ,yellow))))
   `(isearch-fail ((t (:weight bold :foreground ,red))))
   `(lazy-highlight ((t (:weight bold :foreground ,base00 :background ,base07))))
   `(link ((t (:foreground "#d2f1ff" :underline t))))
   `(link-visited ((t (:foreground ,blue :underline t))))
   `(match ((t (:foreground ,base00 :background ,blue))))
   `(minibuffer-prompt ((t (:foreground ,yellow))))
   `(next-error ((t (:inherit (region)))))
   `(org-hide ((t (:background ,base00 :foreground ,base00))))
   `(query-replace ((t (:inherit (isearch)))))
   `(region ((t (:background ,base02))))
   `(secondary-selection ((t (:background "#002b36"))))
   `(shadow ((t (:foreground ,base02))))
   `(trailing-whitespace ((t (:background ,red))))

   `(magit-header ((t (:background ,base01 :foreground ,magenta :inherit nil ))))
   `(magit-item-highlight ((t (:background ,base01 :foreground nil))))
   `(magit-diff-hunk-header ((t (:background ,base02))))
   `(diff-file-header ((t (:background ,base00 :foreground ,magenta :inherit nil :box (:line-width 2 :style released-button)))))
   `(diff-context ((t (:inherit diff-changed :foreground ,base03))))
   `(diff-added ((t (:inherit diff-changed :foreground ,green :background ,base00))))
   `(diff-removed ((t (:inherit diff-changed :foreground ,red :background ,base00))))

   `(outline-1 ((t (:foreground ,red))))
   `(outline-2 ((t (:foreground ,orange))))
   `(outline-3 ((t (:foreground ,yellow))))
   `(outline-4 ((t (:foreground ,blue))))
   `(outline-5 ((t (:foreground ,magenta))))
   `(outline-6 ((t (:foreground ,green))))
   `(outline-7 ((t (:foreground ,cyan))))
   `(outline-8 ((t (:foreground ,violet))))

   `(success ((t (:background ,green :foreground ,base00 :weight bold))))
   `(compilation-mode-line-exit ((t (:inherit compilation-info :background ,green :weight bold))))
   `(compilation-mode-line-fail ((t (:inherit compilation-error :background ,red :weight bold))))

   `(erc-dangerous-host-face ((t (:foreground ,red))))
   `(erc-direct-msg-face ((t (:foreground ,red))))
   `(erc-error-face ((t (:foreground ,red))))
   `(erc-fool-face ((t (:foreground ,base02))))
   `(erc-header-line ((t (:background ,base07 :foreground ,base00))))
   `(erc-inverse-face ((t (:background ,base04 :foreground ,base00))))
   `(erc-keyword-face ((t (:foreground ,green :weight bold))))
   `(erc-nick-msg-face ((t (:foreground ,red :weight bold))))
   `(erc-notice-face ((t (:foreground ,base02 :weight bold))))
   `(erc-pal-face ((t (:foreground ,magenta :weight bold))))
   `(erc-prompt-face ((t (:background ,violet :foreground ,base00 :weight bold))))
   `(erc-timestamp-face ((t (:foreground ,green :weight bold))))
   `(erc-input-face ((t (:foreground ,orange))))
   `(erc-my-nick-face ((t (:foreground ,yellow))))
   `(erc-current-nick-face ((t (:foreground ,yellow))))

   `(show-paren-match ((t (:background ,green :foreground ,base00))))
   `(show-paren-mismatch ((t (:background ,red :foreground ,base00))))
   `(sp-show-pair-match-face ((t (:inherit show-paren-match))))
   `(sp-show-pair-mismatch-face ((t (:inherit show-paren-mismatch))))
   `(sp-pair-overlay-face ((t (:inherit sp-show-pair-match-face))))
   `(ac-completion-face ((t (:foreground ,base02 :underline t))))
   `(ac-candidate-face ((t (:background ,base04 :foreground ,base00))))
   `(ac-candidate-mouse-face ((t (:inherit ac-completion-face))))
   `(ac-selection-face ((t (:background ,base02)))))

  (custom-theme-set-variables
   'tronesque

   `(ansi-color-names-vector
     ;; black, red, green, yellow, blue, magenta, cyan, white
     [,base00 ,red ,green ,yellow ,blue ,magenta ,cyan ,base04])
   `(ansi-term-color-vector
     ;; black, red, green, yellow, blue, magenta, cyan, white
     [unspecified ,base00 ,red ,green ,yellow ,blue ,magenta ,cyan ,base04]))

  ;; Extra mode line faces
  (make-face 'mode-line-read-only-face)
  (make-face 'mode-line-modified-face)
  (make-face 'mode-line-clean-face)
  (make-face 'mode-line-folder-face)
  (make-face 'mode-line-filename-face)
  (make-face 'mode-line-position-face)
  (make-face 'mode-line-mode-face)
  (make-face 'mode-line-minor-mode-face)
  (make-face 'mode-line-vc-face)
  (make-face 'mode-line-80col-face)

  (set-face-attribute 'mode-line nil
                      :foreground base00 :background base04
                      :inverse-video nil
                      :box `(:line-width 6 :color ,base04  :style nil))
  (set-face-attribute 'mode-line-inactive nil
                      :foreground base00 :background base02
                      :inverse-video nil
                      :box `(:line-width 6 :color ,base02 :style nil))
  (set-face-attribute 'mode-line-read-only-face nil
                      :inherit 'mode-line-face
                      :foreground red)
  (set-face-attribute 'mode-line-modified-face nil
                      :inherit 'mode-line-face
                      :background red
                      :box `(:line-width 6 :color ,red :style nil))
  (set-face-attribute 'mode-line-clean-face nil
                      :inherit 'mode-line-face
                      :background green
                      :box `(:line-width 6 :color ,green :style nil))
  (set-face-attribute 'mode-line-folder-face nil
                      :inherit 'mode-line-face
                      :foreground base00
                      :background violet
                      :box `(:line-width 6 :color ,violet :style nil))
  (set-face-attribute 'mode-line-filename-face nil
                      :inherit 'mode-line-face
                      :foreground base02
                      :background violet
                      :box `(:line-width 6 :color ,violet :style nil)
                      :weight 'bold)
  (set-face-attribute 'mode-line-position-face nil
                      :inherit 'mode-line-face
                      :height 100)
  (set-face-attribute 'mode-line-mode-face nil
                      :inherit 'mode-line-face
                      :foreground base00
                      :background orange
                      :box `(:line-width 6 :color ,orange :style nil))
  (set-face-attribute 'mode-line-minor-mode-face nil
                      :foreground base00
                      :height 110)
  (set-face-attribute 'mode-line-vc-face nil
                      :inherit 'mode-line-face
                      :background green
                      :box `(:line-width 6 :color ,green :style nil))
  (set-face-attribute 'mode-line-80col-face nil
                      :inherit 'mode-line-position-face
                      :foreground "black" :background orange
                      :box `(:line-width 6 :color ,orange :style nil))
  )

(defun tronesque-mode-line ()
  "change default modeline"
  (interactive)
  (setq-default
   mode-line-format
   '(
     "%e"
     mode-line-front-space
     mode-line-mule-info
     mode-line-client
     mode-line-modified
     mode-line-remote
     mode-line-frame-identification
     (:propertize " "
                  face mode-line-filename-face)
     (:propertize mode-line-buffer-identification
                  face mode-line-filename-face)
     (:propertize " "
                  face mode-line-filename-face)
     " "
     mode-line-position
     (vc-mode vc-mode)
     " "
     (:propertize " "
                  face mode-line-mode-face)
     (:propertize mode-name
                  face mode-line-mode-face)
     (:propertize " "
                  face mode-line-mode-face)

     (:eval (propertize (format-mode-line minor-mode-alist)
                        'face 'mode-line-minor-mode-face))
     mode-line-misc-info
     mode-line-end-spaces)))

;; Helper function
(defun shorten-directory (dir max-length)
  "Show up to `max-length' characters of a directory name `dir'."
  (let ((path (reverse (split-string (abbreviate-file-name dir) "/")))
        (output ""))
    (when (and path (equal "" (car path)))
      (setq path (cdr path)))
    (while (and path (< (length output) (- max-length 4)))
      (setq output (concat (car path) "/" output))
      (setq path (cdr path)))
    (when path
      (setq output (concat ".../" output)))
    output))


;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'tronesque)
;;; tronesque-theme.el ends here

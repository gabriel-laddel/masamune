;;; late-night-theme.el --- Late Night theme for Emacs 24

;; Author: Alex Schroeder
;; Maintainer: Daehyub Kim <lateau at gmail.com>
;; URL: https://gist.github.com/4420862
;; Version: 0.0
;; Keywords: theme, color

;;; Commentary:

;; To install the theme, M-x `load-theme' then enter
;; `late-night'.  If you run into any issues, check the
;; `custom-theme-load-path' variable.

;;; Code:

(deftheme late-night
  "Color theme by Alex Schroeder, created 2003-08-07.
This theme is for use late at night, with only little light in the room.
The goal was to make something as dark and subtle as the text console in
its default 80x25 state -- dark grey on black.")

(custom-theme-set-faces
 'late-night
 '(cursor ((t (:background "#888"))))
 '(escape-glyph ((t (:foreground "#555"))))
 '(minibuffer-prompt ((t (:foreground "#555"))))
 '(highlight ((t (:foreground "light blue" :background "dark slate blue"))))
 '(region ((t (:background "#322A31"))))
 '(shadow ((t (:foreground "#555555"))))
 '(secondary-selection ((t (:background "paleturquoise"))))
 '(trailing-whitespace ((t (:background "#999"))))
 '(font-lock-builtin-face ((t (:foreground "#777" :weight bold))))
 '(font-lock-comment-delimiter-face ((t (:foreground "#555" :inherit font-lock-comment-face))))
 '(font-lock-comment-face ((t (:foreground "#555"))))
 '(font-lock-constant-face ((t (:foreground "#777"))))
 '(font-lock-doc-face ((t (:foreground "#777"))))
 '(font-lock-function-name-face ((t (:foreground "#777" :weight bold))))
 '(font-lock-keyword-face ((t (:foreground "#777" :weight bold))))
 '(font-lock-preprocessor-face ((t (:foreground "#777"))))
 '(font-lock-regexp-grouping-backslash ((t (:inherit bold :foreground "#666666"))))
 '(font-lock-regexp-grouping-construct ((t (:inherit bold :foreground "#666666"))))
 '(font-lock-reference-face ((t (:foreground "#777"))))
 '(font-lock-string-face ((t (:foreground "#777"))))
 '(font-lock-type-face ((t (:weight bold))))
 '(font-lock-variable-name-face ((t (:foreground "#888" :weight bold))))
 '(font-lock-warning-face ((t (:foreground "#999" :weight bold))))
 '(link ((t (:foreground "#666" :underline t :weight bold))))
 '(link-visited ((t (:inherit link :foreground "#666"))))
 '(fringe ((t :foreground "#444" :background "#111")))
 '(mode-line-highlight ((t (:box (:line-width 2 :color "grey40" :style released-button)))))
 '(isearch ((t (:background "red" :foreground "pink"))))
 '(compilation-error ((t (:inherit error))))
 '(error ((t (:foreground "#B20006" :weight bold))))
 '(warning ((t (:foreground "#FF8000" :weight bold))))
 '(success ((t (:foreground "#00DD00" :weight bold))))
 '(compilation-line-number ((t (:foreground "#FF8000"))))
 '(glyphless-char ((t (:background "#4F4D4D"))))
 '(lazy-highlight ((t (:background "red"))))
 '(match (( t (:foreground "#aaa" :background "#000" :weight bold))))
 '(default ((t (:stipple nil :background "#000" :foreground "#666" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :width normal)))))


;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'late-night)

;; Local Variables:
;; no-byte-compile: t
;; End:

;;; late-night-theme.el ends here

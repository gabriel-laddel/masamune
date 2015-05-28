;;; reverse-theme.el --- Reverse theme for Emacs

;; Copyright (C) 2012 by Syohei YOSHIDA

;; Author: Syohei YOSHIDA <syohex@gmail.com>
;; URL: https://github.com/syohex/emacs-reverse-theme
;; Version: 20130402.312
;; X-Original-Version: 0.01

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Color theme as 'emacs -r' or 'emacs --reverse-video'.
;; Frames created by `make-frame-command' or 'emacsclient --create-frame'
;; are not applied reverse color with '-r' or 'reverse-video' option
;;

;;; Code:


(deftheme reverse
  "Reverse color theme(like '-r' option)")

(custom-theme-set-faces
 'reverse
 '(button ((t (:underline t :foreground "cyan1"))))
 '(default ((t (:background "black" :foreground "white"))))
 '(cursor ((t (:foreground "white"))))
 '(region ((t (:background "blue3"))))
 '(font-lock-builtin-face ((t (:foreground "LightSteelBlue"))))
 '(font-lock-comment-delimiter-face ((t (:foreground "chocolate1"))))
 '(font-lock-comment-face ((t (:foreground "chocolate1"))))
 '(font-lock-constant-face ((t (:foreground "Aquamarine"))))
 '(font-lock-doc-face ((t (:foreground "LightSalmon"))))
 '(font-lock-function-name-face ((t (:foreground "LightSkyBlue"))))
 '(font-lock-keyword-face ((t (:foreground "Cyan1"))))
 '(font-lock-negation-char-face ((t (nil))))
 '(font-lock-preprocessor-face ((t (:foreground "LightSteelBlue"))))
 '(font-lock-regexp-grouping-backslash ((t (:weight bold))))
 '(font-lock-regexp-grouping-construct ((t (:weight bold))))
 '(font-lock-string-face ((t (:foreground "LightSalmon"))))
 '(font-lock-type-face ((t (:foreground "PaleGreen"))))
 '(font-lock-variable-name-face ((t (:foreground "LightGoldenrod"))))
 '(font-lock-warning-face ((t (:weight bold :foreground "Pink"))))
 '(minibuffer-prompt ((t (:foreground "cyan"))))

 '(completions-annotations ((t (:underline t))))
 '(completions-common-part ((t (:foreground "white" :background "black"))))
 '(completions-first-difference ((t (:weight bold))))
 '(dired-directory ((t (:foreground "LightSkyBlue"))))
 '(dired-flagged ((t (:weight bold :foreground "Pink"))))
 '(dired-header ((t (:foreground "PaleGreen"))))
 '(dired-ignored ((t (:foreground "grey70"))))
 '(dired-mark ((t (:foreground "Aquamarine"))))
 '(dired-marked ((t (:weight bold :foreground "DarkOrange"))))
 '(dired-perm-write ((t (:foreground "chocolate1"))))
 '(dired-symlink ((t (:foreground "Cyan1"))))
 '(dired-warning ((t (:foreground "Pink" :weight bold))))
 '(error ((t (:foreground "Pink" :weight bold))))
 '(escape-glyph ((t (:foreground "cyan"))))
 '(file-name-shadow ((t (:foreground "grey70"))))
 '(fringe ((t (:background "grey10"))))
 '(glyphless-char ((t (:height 0.6))))
 '(header-line ((t (:box (:line-width -1 :style released-button)
                    :background "grey20" :foreground "grey90" :box nil))))
 '(help-argument-name ((t (nil))))
 '(highlight ((t (:background "darkolivegreen"))))
 '(isearch ((t (:background "palevioletred2" :foreground "brown4"))))
 '(isearch-fail ((t (:background "red4"))))
 '(italic ((t (:underline t))))
 '(lazy-highlight ((t (:background "paleturquoise4"))))
 '(link ((t (:foreground "cyan1" :underline t))))
 '(link-visited ((t (:underline t :foreground "violet"))))
 '(match ((t (:background "RoyalBlue3"))))
 '(menu ((t (nil))))

 '(mode-line ((t (:background "grey75" :foreground "black"
                  :box (:line-width -1 :style released-button)))))
 '(mode-line-buffer-id ((t (:weight bold))))
 '(mode-line-emphasis ((t (:weight bold))))
 '(mode-line-highlight ((t (:box (:line-width 2 :color "grey40"
                                  :style released-button)))))
 '(mode-line-inactive ((t (:background "grey30" :foreground "grey80"
                           :box (:line-width -1 :color "grey40" :style nil)
                           :weight light))))
 '(mouse ((t (nil))))
 '(next-error ((t (:background "blue3"))))
 '(nobreak-space ((t (:foreground "cyan" :underline t))))
 '(query-replace ((t (:foreground "brown4" :background "palevioletred2"))))
 '(scroll-bar ((t (nil))))
 '(secondary-selection ((t (:background "SkyBlue4"))))
 '(shadow ((t (:foreground "grey70"))))
 '(success ((t (:foreground "Green1" :weight bold))))
 '(tool-bar ((t (:background "grey75" :foreground "black"
                 :box (:line-width 1 :style released-button)))))
 '(tooltip ((t (:background "lightyellow" :foreground "black"))))
 '(trailing-whitespace ((t (:background "red1"))))
 '(underline ((t (:underline t))))
 '(vertical-border ((t (nil))))
 '(warning ((t (:foreground "DarkOrange" :weight bold))))
 '(widget-button ((t (:weight bold))))
 '(widget-button-pressed ((t (:foreground "red1"))))
 '(widget-documentation ((t (:foreground "lime green"))))
 '(widget-field ((t (:background "dim gray"))))
 '(widget-inactive ((t (:foreground "grey70")))))

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'reverse)

;;; reverse-theme.el ends here

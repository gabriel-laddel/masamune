;;; color-theme-wombat.el --- The wombat color theme for Emacs.
;; Copyright (C) 2009 Jesus Alvarez

;; Author: Jesus Alvarez <demizer.one@gmail.com>
;; URL: https://github.com/demizer/color-theme-wombat
;; Version: 0.0.1
;; Package-Requires: ((color-theme "6.6.1"))


;; This file is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of
;; the License, or (at your option) any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty
;; of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
;; See the GNU General Public License for more details.

;; You should have received a copy of the GNU General Public
;; License along with GNU Emacs; if not, write to the Free
;; Software Foundation, 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; This is my second color theme. Apologies for the formatting; this
;; file is automatically extracted from a LaTeX master file.

;;; Code:
(require 'color-theme)

(defvar wombat-fg "#f6f3e8")
(defvar wombat-bg "#242424")
(defvar wombat-green "#95e454")
(defvar wombat-green+1 "#cae682")
(defvar wombat-green+2 "#4BC98A")
(defvar wombat-red-1 "#e5786d")
(defvar wombat-red "#95e454")
(defvar wombat-blue-2 "#2e3436")
(defvar wombat-blue-1 "#64a8d8")
(defvar wombat-blue "#8ac6f2")
(defvar wombat-magenta "#cc99cc")
(defvar wombat-orange-1 "#f57900")
(defvar wombat-orange "#e65c00")
(defvar wombat-orange+1 "#e9b96e")
(defvar wombat-orange+2 "#ffc125")
(defvar wombat-purple-1 "#ad7fa8")
(defvar wombat-purple "#cc99cc")
(defvar wombat-pink-1 "#f283b6")
(defvar wombat-pink "#F6B3DF")
(defvar wombat-gray-1 "#444444")
(defvar wombat-gray "#424242")
(defvar wombat-gray+1 "#99968b")

(defun color-theme-wombat ()
  "The wombat color theme for Emacs."
  (interactive)
  (color-theme-install
   `(color-theme-wombat
     ((background-color . ,wombat-bg)
      (background-mode . dark)
      (border-color . ,wombat-bg)
      (cursor-color . ,wombat-blue-1)
      (foreground-color . ,wombat-fg)
      (mouse-color . "black"))

     ;; Font Lock
     (font-lock-builtin-face ((t (:foreground ,wombat-blue))))
     (font-lock-comment-delimiter-face ((t (:italic t :slant italic :foreground ,wombat-gray+1))))
     (font-lock-comment-face ((t (:italic t :slant italic :foreground ,wombat-gray+1))))
     (font-lock-constant-face ((t (:foreground ,wombat-red-1))))
     (font-lock-doc-face ((t (:foreground ,wombat-gray+1))))
     (font-lock-function-name-face ((t (:foreground ,wombat-purple-1 :bold t :italic t))))
     (font-lock-keyword-face ((t (:foreground ,wombat-blue))))
     (font-lock-negation-char-face ((t (:foreground ,wombat-red))))
     (font-lock-preprocessor-face ((t (:foreground ,wombat-red-1))))
     (font-lock-regexp-grouping-backslash ((t (:bold t :weight bold))))
     (font-lock-regexp-grouping-construct ((t (:bold t ,wombat-green))))
     (font-lock-string-face ((t (:italic t :foreground ,wombat-green))))
     (font-lock-type-face ((t (:foreground ,wombat-green+1))))
     (font-lock-variable-name-face ((t (:foreground ,wombat-blue))))
     (font-lock-warning-face ((t (:bold t :foreground ,wombat-red))))


     ;; UI Items
     ;(border ((t (:background "#888a85"))))
     ;(fringe ((t (:background "grey10"))))
     (minibuffer-prompt ((t (:foreground ,wombat-red :bold t))))
     (mode-line ((t (:background ,wombat-gray-1 :foreground ,wombat-fg))))
     (mode-line-emphasis ((t (:bold t))))
     (mode-line-highlight ((t (:background ,wombat-orange :box nil))))
     (mode-line-inactive ((t (:background ,wombat-bg :box (:line-width 1 :color ,wombat-gray :style nil)))))
     (region ((t (:foreground ,wombat-fg :background ,wombat-gray-1))))


     ;; Highlighting
     (lazy-highlight ((t (:italic t :background "yellow" :foreground "black"))))
     (highlight ((t (:background ,wombat-gray-1))))
     (highlight-changes-delete-face ((t (:foreground "red" :underline t))))
     (highlight-changes-face ((t (:foreground "red"))))
     (secondary-selection ((t (:background ,wombat-blue-1 :foreground "black" :bold t))))
     (hl-line ((t (:background ,wombat-gray-1))))


     ;; Org-mode
     (org-date ((t (:foreground "Cyan" :underline t))))
     (org-agenda-date ((t (:foreground ,wombat-blue))))
     (org-agenda-date-weekend ((t (:bold t :foreground ,wombat-orange :weight bold))))
     (org-hide ((t (:foreground ,wombat-bg))))
     (org-todo ((t (:foreground ,wombat-pink :bold t))))
     (org-hide ((t (:foreground ,wombat-bg))))
     (org-done ((t (:foreground ,wombat-green+2 :bold t))))
     (org-level-1 ((t (:foreground ,wombat-blue :bold t))))
     (org-level-2 ((t (:foreground "#ee9a49")))) ;"#ee9a49"))))
     (org-level-3 ((t (:foreground "#ff83fa"))))
     (org-level-4 ((t (:foreground "#ffa500"))))
     (org-level-5 ((t (:foreground "#ff4040"))))

     ;(comint-highlight-input ((t (:italic t :bold t))))
     ;(comint-highlight-prompt ((t (:foreground "#8ae234"))))
     (isearch ((t (:background ,wombat-orange-1 :foreground ,wombat-blue-2))))
     (isearch-lazy-highlight-face ((t (:foreground ,wombat-blue-2 :background ,wombat-orange+1))))

     ;; Parenthesis Matching
     (paren-face-match ((t (:inherit show-paren-match-face))))
     (paren-face-match-light ((t (:inherit show-paren-match-face))))
     (paren-face-mismatch ((t (:inherit show-paren-mismatch-face))))
     (show-paren-match-face ((t (:background ,wombat-orange :foreground "white" :bold t))))
     (show-paren-mismatch-face ((t (:background ,wombat-purple-1 :foreground ,wombat-blue-2))))

     (persp-selected-face ((t (:foreground ,wombat-blue-2))))

     (info-xref ((t (:foreground ,wombat-blue))))
     (info-xref-visited ((t (:foreground ,wombat-purple-1))))

     )))

(provide 'color-theme-wombat)
     ;(default ((t (:background wombat-bg :foreground wombat-fg))))
                                        ;(border ((t (:background wombat-bg))))
          ;(button ((t (:underline t))))
          ;(cursor ((t (:background wombat-bg))))
          ;(mouse ((t (:background "black"))))

          ;(menu ((t (nil))))
          ;(buffer-menu-buffer ((t (:bold t :weight bold))))


          ;(minibuffer-prompt ((t (:bold t :background "#242424" :foreground "#8ac6f2"))))
          ;(tooltip ((t (:family "helv" :background "lightyellow" :foreground "black"))))


          ;(trailing-whitespace ((t (:background "Red"))))


;;           (tool-bar ((t (:background "grey75" :foreground "black" :box (:line-width 1 :style released-button)))))

;;           (region ((t (:background "#444444" :foreground "white"))))
;;           (secondary-selection ((t (:background "#e65c00" :foreground "#f6f3e8"))))
;;           (zmacs-region ((t (:background "darkslateblue" :foreground "white"))))


;;           (widget-button ((t (:bold t :weight bold))))
;;           (widget-button-pressed ((t (:background "black" :foreground "red"))))
;;           (widget-documentation ((t (:background "white" :foreground "dark green"))))
;;           (widget-field ((t (:background "gray85" :foreground "black"))))
;;           (widget-inactive ((t (:background "red" :foreground "dim gray"))))
;;           (widget-single-line-field ((t (:background "gray85" :foreground "black"))))


          ;(variable-pitch ((t (nil))))
          ;(vertical-border ((t (nil))))


;;           (bold ((t (:bold t :weight bold))))
;;           (bold-italic ((t (:italic t :bold t :slant italic :weight bold))))
;;           (italic ((t (:italic t :slant italic))))
;;           (underline ((t (:underline t))))


;;           (calendar-today ((t (:underline t))))
;;           (holiday ((t (:background "grey"))))
;;           (diary ((t (:foreground "#f6f3e8"))))
;;           (diary-anniversary ((t (:bold t :weight bold :foreground "Cyan"))))
;;           (diary-button ((t (nil))))
;;           (diary-time ((t (:foreground "LightGoldenrod"))))
          ;;(holiday-face ((t (:background "grey"))))
          ;;(calendar-today-face ((t (:underline t))))
          ;;(diary-face ((t (:bakground "#242424" :foreground "#f6f3e8"))))


;;           (comint-highlight-input ((t (:bold t :weight bold))))
;;           (comint-highlight-prompt ((t (:foreground "#f6f3e8"))))


;;           (compilation-column-number ((t (:foreground "#f6f3e8"))))
;;           (compilation-error ((t (:bold t :weight bold :foreground "white"))))
;;           (compilation-info ((t (:bold t :foreground "Green1" :weight bold))))
;;           (compilation-line-number ((t (:bold t :weight bold :foreground "white"))))
;;           (compilation-warning ((t (:bold t :foreground "red" :weight bold))))


;;           (completions-common-part ((t (:stipple nil :background "#242424" :foreground "#f6f3e8" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 138 :width normal :foundry "Bitstream" :family "Bitstream Vera Sans Mono"))))
;;           (completions-first-difference ((t (:bold t :weight bold))))

;;           (cparen-around-andor-face ((t (:bold t :foreground "maroon" :weight bold))))
;;           (cparen-around-begin-face ((t (:foreground "maroon"))))
;;           (cparen-around-conditional-face ((t (:bold t :foreground "RoyalBlue" :weight bold))))
;;           (cparen-around-define-face ((t (:bold t :foreground "Blue" :weight bold))))
;;           (cparen-around-lambda-face ((t (:foreground "LightSeaGreen"))))
;;           (cparen-around-letdo-face ((t (:bold t :foreground "LightSeaGreen" :weight bold))))
;;           (cparen-around-quote-face ((t (:foreground "SaddleBrown"))))
;;           (cparen-around-set!-face ((t (:foreground "OrangeRed"))))
;;           (cparen-around-syntax-rules-face ((t (:foreground "Magenta"))))
;;           (cparen-around-vector-face ((t (:foreground "chocolate"))))
;;           (cparen-binding-face ((t (:foreground "ForestGreen"))))
;;           (cparen-binding-list-face ((t (:bold t :foreground "ForestGreen" :weight bold))))
;;           (cparen-conditional-clause-face ((t (:foreground "RoyalBlue"))))
;;           (cparen-normal-paren-face ((t (:foreground "grey50"))))


;;           (cua-global-mark ((t (:background "yellow1" :foreground "black"))))
;;           (cua-rectangle ((t (:background "maroon" :foreground "white"))))
;;           (cua-rectangle-noselect ((t (:background "dimgray" :foreground "white"))))


;;           (cperl-array-face ((t (:bold t :background "lightyellow2" :foreground "Blue" :weight bold))))
;;           (cperl-hash-face ((t (:italic t :bold t :background "lightyellow2" :foreground "Red" :slant italic :weight bold))))
;;           (cperl-nonoverridable-face ((t (:foreground "chartreuse3"))))


;;           (custom-button ((t (:background "lightgrey" :foreground "black" :box (:line-width 2 :style released-button)))))
;;           (custom-button-mouse ((t (:background "grey90" :foreground "black" :box (:line-width 2 :style released-button)))))
;;           (custom-button-pressed ((t (:background "lightgrey" :foreground "black" :box (:line-width 2 :style pressed-button)))))
;;           (custom-button-pressed-unraised ((t (:underline t :foreground "violet"))))
;;           (custom-button-unraised ((t (:underline t))))
;;           (custom-changed ((t (:background "blue" :foreground "white"))))
;;           (custom-comment ((t (:background "dim gray"))))
;;           (custom-comment-tag ((t (:foreground "gray80"))))
;;           (custom-documentation ((t (nil))))
;;           (custom-face-tag ((t (:bold t :weight bold :height 1.2 :family "helv"))))
;;           (custom-group-tag ((t (:bold t :foreground "light blue" :weight bold :height 1.2))))
;;           (custom-group-tag-1 ((t (:bold t :foreground "pink" :weight bold :height 1.2 :family "helv"))))
;;           (custom-invalid ((t (:background "lightyellow" :foreground "red"))))
;;           (custom-link ((t (:underline t :foreground "#8ac6f2"))))
;;           (custom-modified ((t (:background "blue" :foreground "white"))))
;;           (custom-rogue ((t (:background "black" :foreground "pink"))))
;;           (custom-saved ((t (:underline t))))
;;           (custom-set ((t (:background "white" :foreground "blue"))))
;;           (custom-state ((t (:foreground "lime green"))))
;;           (custom-themed ((t (:background "blue1" :foreground "white"))))
;;           (custom-variable-button ((t (:bold t :underline t :weight bold))))
;;           (custom-variable-tag ((t (:bold t :foreground "light blue"
;;                                           :weight bold :height 1.2
;;                                           :family "helv"))))
;;           (custom-visibility ((t (:underline t :foreground "#8ac6f2" :height 0.8))))


;;           (dropdown-list-face ((t (:family "Bitstream Vera Sans Mono"
;;                                            :foundry "bitstream"
;;                                            :width normal :weight normal
;;                                            :slant normal :underline nil
;;                                            :overline nil :strike-through nil
;;                                            :box nil :inverse-video nil
;;                                            :stipple nil :background "lightyellow"
;;                                            :foreground "black" :height 121))))
;;           (dropdown-list-selection-face ((t (:foreground "black" :stipple nil
;;                                                          :inverse-video nil :box nil
;;                                                          :strike-through nil
;;                                                          :overline nil
;;                                                          :underline nil :slant normal
;;                                                          :weight normal :width normal
;;                                                          :foundry "bitstream"
;;                                                          :family "Bitstream Vera Sans Mono"
;;                                                          :background "purple"
;;                                                          :height 121))))


;;           ;;(eieio-custom-slot-tag-face ((t (:foreground "light blue"))))
;;           ;;(escape-glyph ((t (:foreground "#f6f3e8"))))
;;           ;;(excerpt ((t (:italic t :slant italic))))
;;           (extra-whitespace-face ((t (:background "pale green"))))
;;           (ffap ((t (:foreground "#f6f3e8" :background "#242424"))))
;;           (file-name-shadow ((t (:foreground "grey70"))))
;;           (shadow ((t (:foreground "grey70"))))
;;           (fringe ((t (:background "grey10"))))


;;           (fixed ((t (:bold t :weight bold))))
;;           (fixed-pitch ((t (:family "Bitstream Vera Sans Mono"))))


;;           (flymake-errline ((t (:background "#242424"))))
;;           (flymake-warnline ((t (:background "#242424"))))


;;           (flyspell-duplicate-face ((t (:bold t :foreground "Gold3" :underline t :weight bold))))
;;           (flyspell-incorrect-face ((t (:bold t :foreground "OrangeRed" :underline t :weight bold))))


;;           (gnus-cite-attribution-face ((t (:italic t :slant italic))))
;;           (gnus-cite-face-1 ((t (:foreground "Khaki"))))
;;           (gnus-cite-face-2 ((t (:foreground "Coral"))))
;;           (gnus-cite-face-3 ((t (:foreground "#4186be"))))
;;           (gnus-cite-face-4 ((t (:foreground "yellow green"))))
;;           (gnus-cite-face-5 ((t (:foreground "IndianRed"))))
;;           (gnus-cite-face-list ((t (:bold t :foreground "red" :weight bold))))
;;           (gnus-emphasis-bold ((t (:bold t :weight bold))))
;;           (gnus-emphasis-bold-italic ((t (:italic t :bold t :slant italic :weight bold))))
;;           (gnus-emphasis-italic ((t (:italic t :slant italic))))
;;           (gnus-emphasis-underline ((t (:underline t))))
;;           (gnus-emphasis-underline-bold ((t (:bold t :underline t :weight bold))))
;;           (gnus-emphasis-underline-bold-italic ((t (:italic t :bold t :underline t :slant italic :weight bold))))
;;           (gnus-emphasis-underline-italic ((t (:italic t :underline t :slant italic))))
;;           (gnus-group-mail-1-empty-face ((t (:foreground "DeepPink3"))))
;;           (gnus-group-mail-1-face ((t (:bold t :foreground "DeepPink3" :weight bold))))
;;           (gnus-group-mail-2-empty-face ((t (:foreground "HotPink3"))))
;;           (gnus-group-mail-2-face ((t (:bold t :foreground "HotPink3" :weight bold))))
;;           (gnus-group-mail-3-empty-face ((t (:foreground "magenta4"))))
;;           (gnus-group-mail-3-face ((t (:bold t :foreground "magenta4" :weight bold))))
;;           (gnus-group-mail-low-empty-face ((t (:foreground "DeepPink4"))))
;;           (gnus-group-mail-low-face ((t (:bold t :foreground "DeepPink4"))))
;;           (gnus-group-news-1-empty-face ((t (:foreground "ForestGreen"))))
;;           (gnus-group-news-1-face ((t (:bold t :foreground "ForestGreen" :weight bold))))
;;           (gnus-group-news-2-empty-face ((t (:foreground "CadetBlue4"))))
;;           (gnus-group-news-2-face ((t (:bold t :foreground "CadetBlue4" :weight bold))))
;;           (gnus-group-news-3-empty-face ((t (nil))))
;;           (gnus-group-news-3-face ((t (:bold t :weight bold))))
;;           (gnus-group-news-low-empty-face ((t (:foreground "DarkGreen"))))
;;           (gnus-group-news-low-face ((t (:bold t :foreground "DarkGreen" :weight bold))))
;;           (gnus-header-content-face ((t (:italic t :foreground "#4186be" :slant italic))))
;;           (gnus-header-from-face ((t (:foreground "Coral"))))
;;           (gnus-header-name-face ((t (:foreground "white"))))
;;           (gnus-header-newsgroups-face ((t (:italic t :foreground "#4186be" :slant italic))))
;;           (gnus-header-subject-face ((t (:foreground "#4186be"))))
;;           (gnus-signature-face ((t (:foreground "white"))))
;;           (gnus-splash-face ((t (:foreground "ForestGreen"))))
;;           (gnus-summary-cancelled-face ((t (:background "black" :foreground "yellow"))))
;;           (gnus-summary-high-ancient-face ((t (:bold t :foreground "RoyalBlue" :weight bold))))
;;           (gnus-summary-high-read-face ((t (:bold t :foreground "DarkGreen" :weight bold))))
;;           (gnus-summary-high-ticked-face ((t (:bold t :foreground "firebrick" :weight bold))))
;;           (gnus-summary-high-unread-face ((t (:bold t :weight bold))))
;;           (gnus-summary-low-ancient-face ((t (:italic t :foreground "RoyalBlue" :slant italic))))
;;           (gnus-summary-low-read-face ((t (:italic t :foreground "DarkGreen" :slant italic))))
;;           (gnus-summary-low-ticked-face ((t (:italic t :foreground "firebrick" :slant italic))))
;;           (gnus-summary-low-unread-face ((t (:italic t :slant italic))))
;;           (gnus-summary-normal-ancient-face ((t (:foreground "RoyalBlue"))))
;;           (gnus-summary-normal-read-face ((t (:foreground "DarkGreen"))))
;;           (gnus-summary-normal-ticked-face ((t (:foreground "firebrick"))))
;;           (gnus-summary-normal-unread-face ((t (nil))))
;;           (gnus-summary-selected-face ((t (:underline t))))


;;           (header-line ((t (:box (:line-width -1 :style released-button) :background "grey20" :foreground "grey90" :box nil))))
;;           (help-argument-name ((t (nil))))





;;           ;;(Info-title-1-face ((t (:bold t :weight bold :height 1.728 :family "helv"))))
;;           ;;(Info-title-2-face ((t (:bold t :weight bold :height 1.44 :family "helv"))))
;;           ;;(Info-title-3-face ((t (:bold t :weight bold :height 1.2 :family "helv"))))
;;           ;;(Info-title-4-face ((t (:bold t :weight bold :family "helv"))))


;;           (ido-first-match ((t (:inherit zenburn-primary-1))))
;;           (ido-only-match ((t (:inherit zenburn-primary-2))))
;;           (ido-subdir ((t (:foreground ,zenburn-yellow))))


;;           (info-header-node ((t (:italic t :bold t :foreground "white" :slant italic :weight bold))))
;;           (info-header-xref ((t (:bold t :foreground "cyan" :weight bold))))
;;           (info-menu-5 ((t (:foreground "red1"))))
;;           (info-menu-header ((t (:bold t :weight bold :family "helv"))))
;;           (info-node ((t (:italic t :bold t :foreground "white" :slant italic :weight bold))))
;;           (info-xref ((t (:bold t :foreground "cyan" :weight bold))))
;;           (info-xref-visited ((t (:foreground "#ad7fa8"))))


;;           (isearch ((t (:bold t :background "#e65c00" :foreground "white"))))
;;           (isearch-fail ((t (:background "red"))))
;;           (query-replace ((t (:foreground "#e65c00" :background "palevioletred2"))))


;;           (link ((t (:foreground "#8ac6f2" :underline t))))
;;           (link-visited ((t (:underline t :foreground "violet"))))


;;           (makefile-space-face ((t (:background "hotpink"))))
;;           (match ((t (:background "#444444"))))


;;           (message-cited-text-face ((t (:foreground "white"))))
;;           (message-header-cc-face ((t (:foreground "#4186be"))))
;;           (message-header-name-face ((t (:foreground "white"))))
;;           (message-header-newsgroups-face ((t (:bold t :foreground "Coral" :weight bold))))
;;           (message-header-other-face ((t (:foreground "steel blue"))))
;;           (message-header-subject-face ((t (:bold t :foreground "#4186be" :weight bold))))
;;           (message-header-to-face ((t (:bold t :foreground "#4186be" :weight bold))))
;;           (message-header-xheader-face ((t (:foreground "blue"))))
;;           (message-separator-face ((t (:foreground "brown"))))


;;           (mode-line ((t (:background "#444444" :foreground "#f6f3e8"))))
;;           (mode-line-buffer-id ((t (:bold t :background "#424242" :weight bold))))
;;           (mode-line-emphasis ((t (:bold t :weight bold))))
;;           (mode-line-highlight ((t (:background "#E65C00"))))
;;           (mode-line-inactive ((t (:background "#424242" :weight light))))
;;           (modeline-mousable ((t (nil))))
;;           (modeline-mousable-minor-mode ((t (:background "#424242"))))


;;           (next-error ((t (:foreground "white" :background "darkslateblue"))))
;;           (nobreak-space ((t (:foreground "cyan" :underline t))))


;;           (org-agenda-column-dateline ((t (:family "Bitstream Vera Sans Mono" :weight normal :slant normal :underline nil :strike-through nil :background "grey30" :height 121))))
;;
;;           (org-agenda-dimmed-todo-face ((t (:foreground "grey50"))))
;;           (org-agenda-restriction-lock ((t (:background "skyblue4"))))
;;           (org-agenda-structure ((t (:foreground "LightSkyBlue"))))
;;           (org-archived ((t (:foreground "grey70"))))
;;           (org-clock-overlay ((t (:background "SkyBlue4"))))
;;           (org-code ((t (:foreground "grey70"))))
;;           (org-column ((t (:background "grey30" :strike-through nil :underline nil :slant normal :weight normal :height 121 :family "Bitstream Vera Sans Mono"))))
;;           (org-column-title ((t (:bold t :background "grey30" :underline t :weight bold))))
;;           (org-date ((t (:foreground "Cyan" :underline t))))
;;           (org-done ((t (:bold t :foreground "PaleGreen" :weight bold))))
;;           (org-drawer ((t (:foreground "#e5786d"))))
;;           (org-ellipsis ((t (:foreground "LightGoldenrod" :underline t))))
;;           (org-footnote ((t (:foreground "Cyan" :underline t))))
;;           (org-formula ((t (:foreground "chocolate1"))))
;;           (org-headline-done ((t (:foreground "LightSalmon"))))
;;           (org-hide ((t (:foreground "#242424"))))
;;           (org-latex-and-export-specials ((t (:foreground "burlywood"))))
;;           (org-level-1 ((t (:bold t :foreground "#8ac6f2" :weight bold))))
;;           (org-level-2 ((t (:bold t :foreground "#00ee76"))))
;;           (org-level-3 ((t (:bold t :foreground "#00eeee"))))
;;           (org-level-4 ((t (:bold t :foreground "#ffdead"))))
;;           (org-level-5 ((t (:bold t :foreground "#cdaa7d"))))
;;           (org-level-6 ((t (:bold t :foreground "#eeb422"))))
;;           (org-level-7 ((t (:bold t :foreground "#ffa500"))))
;;           (org-level-8 ((t (:bold t :foreground "#cae682"))))
;;           (org-link ((t (:foreground "Cyan" :underline t))))
;;           (org-property-value ((t (:foreground "lightpink"))))
;;           (org-scheduled ((t (:foreground "PaleGreen"))))
;;           (org-scheduled-previously ((t (:foreground "chocolate1"))))
;;           (org-scheduled-today ((t (:foreground "PaleGreen"))))
;;           (org-sexp-date ((t (:foreground "Cyan"))))
;;           (org-special-keyword ((t (:foreground "LightSalmon"))))
;;           (org-table ((t (:foreground "LightSkyBlue"))))
;;           (org-tag ((t (:foreground "#8ac6f2"))))
;;           (org-target ((t (:underline t))))
;;           (org-time-grid ((t (:foreground "LightGoldenrod"))))
;;           (org-todo ((t (:bold t :foreground "#ffb6c1" :weight bold))))
;;           (org-upcoming-deadline ((t (:foreground "chocolate1"))))
;;           (org-verbatim ((t (:foreground "grey70" :underline t))))
;;           (org-warning ((t (:bold t :weight bold :foreground "red"))))


;;           (outline-1 ((t (:foreground "LightSkyBlue"))))
;;           (outline-2 ((t (:foreground "LightGoldenrod"))))
;;           (outline-3 ((t (:bold t :weight bold :foreground "Cyan"))))
;;           (outline-4 ((t (:foreground "chocolate1"))))
;;           (outline-5 ((t (:foreground "PaleGreen"))))
;;           (outline-6 ((t (:bold t :weight bold :foreground "Aquamarine"))))
;;           (outline-7 ((t (:foreground "LightSteelBlue"))))
;;           (outline-8 ((t (:foreground "LightSalmon"))))


;;           (semantic-dirty-token-face ((t (:background "gray10"))))
;;           (semantic-unmatched-syntax-face ((t (:underline "red"))))


;;           (senator-intangible-face ((t (:foreground "gray75"))))
;;           (senator-momentary-highlight-face ((t (:background "gray30"))))
;;           (senator-read-only-face ((t (:background "#664444"))))


;;           (sgml-doctype-face ((t (:foreground "orange"))))
;;           (sgml-end-tag-face ((t (:foreground "greenyellow"))))
;;           (sgml-entity-face ((t (:foreground "gold"))))
;;           (sgml-ignored-face ((t (:background "gray60" :foreground "gray20"))))
;;           (sgml-sgml-face ((t (:foreground "yellow"))))
;;           (sgml-start-tag-face ((t (:foreground "mediumspringgreen"))))


;;           (show-paren-match-face ((t (:bold t :background "#857b6f" :foreground "#f6f3e8"))))
;;           (show-paren-mismatch-face ((t (:background "red" :foreground "white"))))
;;           (show-tabs-space ((t (:background "#e65c00"))))
;;           (show-tabs-tab ((t (:background "#e65c00"))))


;;           (speedbar-button-face ((t (:foreground "green3"))))
;;           (speedbar-directory-face ((t (:foreground "light blue"))))
;;           (speedbar-file-face ((t (:foreground "cyan"))))
;;           (speedbar-highlight-face ((t (:background "sea green"))))
;;           (speedbar-selected-face ((t (:foreground "red" :underline t))))
;;           (speedbar-separator-face ((t (:background "blue" :foreground "white" :overline "gray"))))
;;           (speedbar-tag-face ((t (:foreground "yellow"))))


;;           (tabbar-button ((t (:box '(:line-width 1 :color "gray72" :style released-button)))))
;;           (tabbar-button-highlight ((t (nil))))
;;           (tabbar-default ((t (:background "grey60"))))
;;           (tabbar-highlight ((t (:underline t))))
;;           (tabbar-selected ((t (:background "#f2f2f6" :foreground "black" :box nil))))
;;           (tabbar-separator ((t (:height 0.7))))
;;           (tabbar-unselected ((t (:foreground "grey30" :background "grey85" :box nil))))


;;           (term-black ((t (:foreground "black"))))
;;           (term-blackbg ((t (:background "black"))))
;;           (term-blue ((t (:foreground "#4186be"))))
;;           (term-bluebg ((t (:background "#4186be"))))
;;           (term-bold ((t (:bold t :weight bold))))
;;           (term-cyan ((t (:foreground "#71bebe"))))
;;           (term-cyanbg ((t (:background "#71bebe"))))
;;           (term-default-bg ((t (nil))))
;;           (term-default-bg-inv ((t (nil))))
;;           (term-default-fg ((t (nil))))
;;           (term-default-fg-inv ((t (nil))))
;;           (term-green ((t (:foreground "#e5f779"))))
;;           (term-greenbg ((t (:background "#e5f779"))))
;;           (term-invisible ((t (nil))))
;;           (term-invisible-inv ((t (nil))))
;;           (term-magenta ((t (:foreground "#ef9ebe"))))
;;           (term-magentabg ((t (:background "#ef9ebe"))))
;;           (term-red ((t (:foreground "#ef8171"))))
;;           (term-redbg ((t (:background "#ef8171"))))
;;           (term-underline ((t (:underline t))))
;;           (term-white ((t (:foreground "#c0c0c0"))))
;;           (term-whitebg ((t (:background "#c0c0c0"))))
;;           (term-yellow ((t (:foreground "#fff796"))))
;;           (term-yellowbg ((t (:background "#fff796"))))

;;; color-theme-wombat.el ends here

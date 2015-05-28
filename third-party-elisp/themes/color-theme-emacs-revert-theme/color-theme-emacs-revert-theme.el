;;; color-theme-emacs-revert-theme.el --- Color-theme revert to emacs colors
;; 
;; Filename: color-theme-emacs-revert-theme.el
;; Description: Revert to Emacs Colors
;; Author: Matthew L. Fidler
;; Maintainer: Matthew L. Fidler
;; Created: Tue Feb  8 09:41:49 2011 (-0600)
;; Version: 0.1
;; Last-Updated: Tue Feb  8 09:42:27 2011 (-0600)
;;           By: Matthew L. Fidler
;;     Update #: 2
;; URL: 
;; Keywords: Color theme
;; Compatibility: Color-theme
;; 
;; Features that might be required by this library:
;;
;;   None
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Commentary: 
;; 
;; 
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Change Log:
;; 08-Feb-2011    Matthew L. Fidler  
;;    Last-Updated: Tue Feb  8 09:42:09 2011 (-0600) #1 (Matthew L. Fidler)
;;    Added comments to upload to ELPA
;; 
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;; 
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Code:




(defun color-theme-emacs-revert-theme ()
  "Color subtheme, created 2008-07-17."
  (interactive)
  (color-theme-install
   '(color-theme-emacs-revert-theme nil nil
     (widget-single-line-field ((t (:background "gray85"))))
     (widget-inactive ((t (:foreground "grey50"))))
     (widget-field ((t (:background "gray85"))))
     (widget-documentation ((t (:foreground "dark green"))))
     (widget-button-pressed ((t (:foreground "red1"))))
     (widget-button ((t (:bold t :weight bold))))
     (vertical-border ((t (nil))))
     (variable-pitch ((t (:family "helv"))))
     (underline ((t (:underline t))))
     (trailing-whitespace ((t (:background "red1"))))
     (tooltip ((t (:family "helv" :background "lightyellow" :foreground "black"))))
     (tool-bar ((t (:background "grey75" :foreground "black" :box (:line-width 1 :style released-button)))))
     (texinfo-heading ((t (:foreground "Blue1"))))
     (show-paren-mismatch ((t (:background "purple" :foreground "white"))))
     (show-paren-match ((t (:background "turquoise"))))
     (shadow ((t (:foreground "grey50"))))
     (secondary-selection ((t (:background "yellow1"))))
     (scroll-bar ((t (nil))))
     (region ((t (:background "lightgoldenrod2"))))
     (query-replace ((t (:foreground "lightskyblue1" :background "magenta3"))))
     (nobreak-space ((t (:foreground "brown" :underline t))))
     (next-error ((t (:background "lightgoldenrod2"))))
     (muse-verbatim ((t (:foreground "slate gray"))))
     (muse-link ((t (:bold t :foreground "blue" :underline "blue" :weight bold))))
     (muse-header-5 ((t (:bold t :weight bold :height 1.0 :family "helv"))))
     (muse-header-4 ((t (:bold t :weight bold :height 1.1 :family "helv"))))
     (muse-header-3 ((t (:bold t :weight bold :height 1.2 :family "helv"))))
     (muse-header-2 ((t (:bold t :weight bold :height 1.3 :family "helv"))))
     (muse-header-1 ((t (:bold t :weight bold :height 1.4 :family "helv"))))
     (muse-emphasis-3 ((t (:italic t :bold t :slant italic :weight bold))))
     (muse-emphasis-2 ((t (:bold t :weight bold))))
     (muse-emphasis-1 ((t (:italic t :slant italic))))
     (muse-bad-link ((t (:bold t :foreground "red" :underline "red" :weight bold))))
     (mouse ((t (:background "black"))))
     (mode-line-inactive ((t (:background "grey90" :foreground "grey20" :box (:line-width -1 :color "grey75" :style nil) :weight light))))
     (mode-line-highlight ((t (:box (:line-width 2 :color "grey40" :style released-button)))))
     (mode-line-buffer-id ((t (:bold t :weight bold))))
     (mode-line ((t (:background "grey75" :foreground "black" :box (:line-width -1 :style released-button)))))
     (minibuffer-prompt ((t (:foreground "medium blue"))))
     (menu ((t (nil))))
     (match ((t (:background "yellow1"))))
     (mac-ts-selected-text ((t (:underline t))))
     (mac-ts-selected-raw-text ((t (:underline t))))
     (mac-ts-selected-converted-text ((t (:underline t))))
     (mac-ts-raw-text ((t (:underline t))))
     (mac-ts-outline-text ((t (:underline t))))
     (mac-ts-no-hilite ((t (:family "apple-lucida sans typewriter" :width normal :weight normal :slant normal :underline nil :overline nil :strike-through nil :box nil :inverse-video nil :foreground "black" :background "white" :stipple nil :height 181))))
     (mac-ts-converted-text ((t (:underline "gray80"))))
     (mac-ts-caret-position ((t (nil))))
     (mac-ts-block-fill-text ((t (:underline t))))
     (link-visited ((t (:underline t :foreground "magenta4"))))
     (link ((t (:foreground "blue1" :underline t))))
     (lazy-highlight ((t (:background "paleturquoise"))))
     (italic ((t (:italic t :slant italic))))
     (isearch ((t (:background "magenta3" :foreground "lightskyblue1"))))
     (info-xref-visited ((t (:foreground "magenta4" :underline t))))
     (info-xref ((t (:underline t :foreground "blue1"))))
     (info-title-4 ((t (:bold t :family "helv" :weight bold))))
     (info-title-3 ((t (:bold t :weight bold :family "helv" :height 1.2))))
     (info-title-2 ((t (:bold t :family "helv" :weight bold :height 1.44))))
     (info-title-1 ((t (:bold t :weight bold :family "helv" :height 1.728))))
     (info-node ((t (:italic t :bold t :foreground "brown" :slant italic :weight bold))))
     (info-menu-star ((t (:foreground "red1"))))
     (info-menu-header ((t (:bold t :family "helv" :weight bold))))
     (info-header-xref ((t (:foreground "blue1" :underline t))))
     (info-header-node ((t (:italic t :bold t :weight bold :slant italic :foreground "brown"))))
     (highlight ((t (:background "darkseagreen2"))))
     (help-argument-name ((t (:italic t :slant italic))))
     (header-line ((t (:box (:line-width -1 :style released-button) :background "grey90" :foreground "grey20" :box nil))))
     (fringe ((t (:background "grey95"))))
     (font-lock-warning-face ((t (:bold t :foreground "Red1" :weight bold))))
     (font-lock-variable-name-face ((t (:foreground "DarkGoldenrod"))))
     (font-lock-type-face ((t (:foreground "ForestGreen"))))
     (font-lock-string-face ((t (:foreground "RosyBrown"))))
     (font-lock-regexp-grouping-construct ((t (:bold t :weight bold))))
     (font-lock-regexp-grouping-backslash ((t (:bold t :weight bold))))
     (font-lock-preprocessor-face ((t (:foreground "Orchid"))))
     (font-lock-negation-char-face ((t (nil))))
     (font-lock-keyword-face ((t (:foreground "Purple"))))
     (font-lock-function-name-face ((t (:foreground "Blue1"))))
     (font-lock-doc-face ((t (:foreground "RosyBrown"))))
     (font-lock-constant-face ((t (:foreground "CadetBlue"))))
     (font-lock-comment-face ((t (:foreground "Firebrick"))))
     (font-lock-comment-delimiter-face ((t (:foreground "Firebrick"))))
     (font-lock-builtin-face ((t (:foreground "Orchid"))))
     (fixed-pitch ((t (:family "courier"))))
     (file-name-shadow ((t (:foreground "grey50"))))
     (esn-font-lock-over ((t (:bold t :underline t :weight bold))))
     (escape-glyph ((t (:foreground "brown"))))
     (custom-variable-tag ((t (:bold t :family "helv" :foreground "blue1" :weight bold :height 1.2))))
     (custom-variable-button ((t (:bold t :underline t :weight bold))))
     (custom-themed ((t (:background "blue1" :foreground "white"))))
     (custom-state ((t (:foreground "dark green"))))
     (custom-set ((t (:background "white" :foreground "blue1"))))
     (custom-saved ((t (:underline t))))
     (custom-rogue ((t (:background "black" :foreground "pink"))))
     (custom-modified ((t (:background "blue1" :foreground "white"))))
     (custom-link ((t (:underline t :foreground "blue1"))))
     (custom-invalid ((t (:background "red1" :foreground "yellow1"))))
     (custom-group-tag-1 ((t (:bold t :family "helv" :foreground "red1" :weight bold :height 1.2))))
     (custom-group-tag ((t (:bold t :foreground "blue1" :weight bold :height 1.2))))
     (custom-face-tag ((t (:bold t :family "helv" :weight bold :height 1.2))))
     (custom-documentation ((t (nil))))
     (custom-comment-tag ((t (:foreground "blue4"))))
     (custom-comment ((t (:background "gray85"))))
     (custom-changed ((t (:background "blue1" :foreground "white"))))
     (custom-button-unraised ((t (:underline t))))
     (custom-button-pressed-unraised ((t (:underline t :foreground "magenta4"))))
     (custom-button-pressed ((t (:background "lightgrey" :foreground "black" :box (:line-width 2 :style pressed-button)))))
     (custom-button-mouse ((t (:background "grey90" :foreground "black" :box (:line-width 2 :style released-button)))))
     (custom-button ((t (:background "lightgrey" :foreground "black" :box (:line-width 2 :style released-button)))))
     (cursor ((t (:background "black"))))
     (completions-first-difference ((t (:bold t :weight bold))))
     (completions-common-part ((t ( :width normal :weight normal :slant normal :underline nil :overline nil :strike-through nil :box nil :inverse-video nil :foreground "black" :background "white" :stipple nil :height 181))))
     (comint-highlight-prompt ((t (:foreground "dark blue"))))
     (comint-highlight-input ((t (:bold t :weight bold))))
     (button ((t (:underline t))))
     (buffer-menu-buffer ((t (:bold t :weight bold))))
     (border ((t (:background "black"))))
     (bold-italic ((t (:italic t :bold t :slant italic :weight bold))))
     (bold ((t (:bold t :weight bold))))
     (default ((t (:stipple nil :background "white" :foreground "black" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 181 :width normal)))))))
(provide 'color-theme-emacs-revert-theme)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; color-theme-emacs-revert-theme.el ends here

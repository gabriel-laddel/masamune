;;; color-theme-actress.el --- A dark color theme for GNU Emacs.

;; Copyright (C) 2011 Inderjit Gill <email@indy.io>

;; Author: Inderjit Gill
;; Keywords: dark color theme
;; URL: http://github.com/indy/color-theme-actress
;; Version: 0.2.2
;; Package-Requires: ((color-theme "6.6.1"))

;; This file is not a part of GNU Emacs.

;;; License:

;; This is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free
;; Software Foundation; either version 2, or (at your option) any later
;; version.
;;
;; This is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
;; for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
;; MA 02111-1307, USA.

;;; Usage:

;; 1. Install the color-theme package
;;   (http://www.emacswiki.org/cgi-bin/wiki/ColorTheme)
;; 2. Load this file
;; 3. M-x color-theme-actress

;;; Code:

;; color theme (requires http://www.emacswiki.org/cgi-bin/wiki?ColorTheme )
(require 'color-theme)

(defun color-theme-actress ()
  "Actress color theme for GNU Emacs."
  (interactive)
  (let ((bg "#181916")        ; default background
        (bg-fringe "#121310")
        (bg-hi "#000000")     ; default background-highlighted
        (bg-hi2 "#383830")    ; default background-extra-highlighted
        (fg "#999999")        ; default foreground
        (fg-hi "#ffffff")     ; default foreground-highlighted

        (alpha "#2f4f4f")
        (alpha-comp1 "#4D9B9B")
        (alpha-comp2 "#6C9B9B")
        (alpha-comp-1 "#9B603D")
        (alpha-comp-2 "#4F3B2F")
        (alpha-triad1 "#4D9B9B")
        (alpha-triad2 "#5C4054")
        (alpha-triad-1 "#827840")
        (alpha-triad-2 "#9B9055")
        (alpha-pound1 "#406082")
        (alpha-pound2 "#2367B5")
        (alpha-pound-1 "#5C514E")
        (alpha-pound-2 "#824540")

        (beta "#2e8b57")
        (beta-comp1 "#0E3E24")
        (beta-comp2 "#5DD795")
        (beta-comp-1 "#3E0D08")
        (beta-comp-2 "#8B372E")
        (beta-triad1 "#0E3E25")
        (beta-triad2 "#794198")
        (beta-triad-1 "#584114")
        (beta-triad-2 "#D7A23D")
        (beta-pound1 "#2CB7BE")
        (beta-pound2 "#B0EEF1")
        (beta-pound-1 "#985861")
        (beta-pound-2 "#BE2C8A"))
    (color-theme-install
     `(color-theme-actress
       ((background-color . ,bg)
        (background-mode . dark)
        (border-color . ,bg)
        (cursor-color . ,alpha-triad-1)
        (foreground-color . ,fg))

       (default ((t (:stipple nil
                              :background ,bg
                              :foreground ,fg
                              :inverse-video nil
                              :box nil
                              :strike-through nil
                              :overline nil
                              :underline nil
                              :slant normal
                              :weight normal))))

       (bold ((t (:bold t))))
       (bold-italic ((t (:italic t :bold t))))
       (underline ((t (:underline t))))

       (css-property ((t (:foreground ,alpha-comp2))))
       (css-selector ((t (:foreground ,beta-comp-2))))

       ;; font lock
       ;;
       (font-lock-builtin-face ((t (:foreground ,alpha-comp2))))
       (font-lock-comment-delimiter-face ((t (:italic t
                                                      :slant italic
                                                      :foreground ,alpha))))
       (font-lock-comment-face ((t (:italic t
                                            :foreground ,alpha
                                            :slant italic))))
       (font-lock-constant-face ((t (:foreground ,alpha-comp2 :bold t))))
       (font-lock-function-name-face ((t (:foreground ,beta-comp-2))))
       (font-lock-keyword-face ((t (:foreground ,alpha-comp2))))
       (font-lock-string-face ((t (:foreground ,beta))))
       (font-lock-type-face ((t (:foreground ,beta-comp2))))
       (font-lock-variable-name-face ((t (:foreground ,alpha-triad-1))))
       (font-lock-warning-face ((t (:bold t :foreground ,beta-triad2))))

       
       (fringe ((t (:foreground ,alpha :background ,bg-fringe))))
       
       (trailing-whitespace ((t (:background "grey9"))))
       
       (highlight ((t (:background ,bg-hi))))
       (modeline ((t (:background ,bg-hi2 :foreground ,alpha-triad-1))))
       (minibuffer-prompt ((t (:foreground ,fg))))

       (link ((t (:foreground "SteelBlue" :underline t))))
       (link-visited ((t (:underline t :foreground "magenta4"))))

       (secondary-selection ((t (:background "dodger blue"))))
       (region ((t (:background "MidnightBlue"))))
       
       (show-paren-match ((t (:background ,bg-hi :foreground ,fg-hi))))
       (show-paren-mismatch ((t (:background ,beta-pound-2))))))))

(eval-after-load 'diff-mode
  '(progn
     (set-face-foreground 'diff-added "#0E3E24")
     (set-face-foreground 'diff-removed "#8B372E")))

(eval-after-load 'magit
  '(progn
     (set-face-foreground 'magit-diff-add "#0E3E24")
     (set-face-foreground 'magit-diff-del "#8B372E")
     (set-face-background 'magit-item-highlight "#111111")))

(eval-after-load 'yasnippet
  '(progn
     (set-face-foreground 'yas/field-highlight-face "#ffffff")
     (set-face-background 'yas/field-highlight-face "#333333")))

;;(add-to-list 'color-themes '(color-theme-actress  "Actress" "Inderjit Gill"))

(color-theme-actress)

(provide 'color-theme-actress)

;;; color-theme-actress.el ends here

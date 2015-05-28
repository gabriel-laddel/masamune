;;; color-theme-dg.el --- A black and green color theme for Emacs.

;; Copyright (c) 2011 Tim Sally

;; Author: Tim Sally
;; URL: https://github.com/poet/color-theme-dg
;; Version: 0.1.0
;; Package-Requires: ((color-theme "6.6.0"))

;;; Commentary:

;; color-theme-dg.el provides a black and green color theme for Emacs.
;; It requires the color-theme package be installed.

;;; License:

;; Copyright (c) 2011 Tim Sally

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:

(defun color-theme-dg ()
  (interactive)
  (color-theme-install
   '(color-theme-dg
      ((background-color . "#000000")
       (foreground-color . "#C0C0C0")
       (background-mode . light)
       (border-color . "#262626")
       (cursor-color . "#464646")
       (mouse-color . "black"))
      (fringe ((t (:background "#262626"))))
      (mode-line ((t (:foreground "#ffffff" :background "#00aa00"))))
      (region ((t (:background "#262626"))))
      (font-lock-builtin-face ((t (:foreground "#00cc00"))))
      (font-lock-comment-face ((t (:foreground "#707070"))))
      (font-lock-function-name-face ((t (:foreground "#00ff00"))))
      (font-lock-keyword-face ((t (:foreground "#00ff00"))))
      (font-lock-string-face ((t (:foreground "#00bb00"))))
      (font-lock-type-face ((t (:foreground"#ffffff"))))
      (font-lock-variable-name-face ((t (:foreground "#ffffff"))))
      (font-lock-constant-face ((t (:foreground "#fffff"))))
      (minibuffer-prompt ((t (:foreground "#00ff00" :bold t))))
      (font-lock-warning-face ((t (:foreground "#ff0000" :bold t))))
     )))

(provide 'color-theme-dg)

;;; color-theme-dg.el ends here

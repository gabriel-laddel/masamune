;;; jujube-theme.el --- Pastel theme loosely based on jellybeans

;; Copyright (C) 2011-2013 Jonas Arnold Clasen

;; Author: Jonas Arnold Clasen <jonasac@student.matnat.uio.no>
;; URL: http://github.com/jonasac/jujube-emacs
;; Version: 0.1

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.(deftheme jujube "Jujube for Emacs")

;; Credit:
;; Loosely based on jellybeans theme for vim, if anyone knows who created that theme,
;; please let me know.

(deftheme jujube "Jujube emacs24 theme")
(let ((jujube-orange "#ffb964")
      (jujube-background "#151515")
      (jujube-cursor "#b0d0f0")
      (jujube-dark-blue "#8197bf")
      (jujube-light-blue "#8fbfdc")
      (jujube-green "#99ad6a")
      (jujube-red "#902020")
      (jujube-white "#e8e8d3")
      (jujube-comment "#888888")
      (jujube-pink "#c6b6ee")
      (jujube-selection "#404040")
      (jujube-fringe "#1a1a1a")
      (jujube-yellow "#fad07a"))
"#203040"
  (custom-theme-set-faces
   'jujube
   `(default ((t (:foreground ,jujube-white :background ,jujube-background))))
   `(cursor ((t (:background ,jujube-cursor))))
   `(fringe ((t (:background ,jujube-fringe))))
   `(modeline ((t (:background ,jujube-selection :foreground ,jujube-white))))
   `(region ((t (:background ,jujube-selection))))
   `(font-lock-builtin-face ((t (:foreground ,jujube-orange))))
   `(font-lock-comment-face ((t (:foreground ,jujube-comment))))
   `(font-lock-function-name-face ((t (:foreground ,jujube-yellow))))
   `(font-lock-keyword-face ((t (:foreground ,jujube-dark-blue))))
   `(font-lock-string-face ((t (:foreground ,jujube-green))))
   `(font-lock-type-face ((t (:foreground ,jujube-orange))))
   `(font-lock-constant-face ((t (:foreground ,jujube-light-blue))))
   `(font-lock-variable-name-face ((t (:foreground ,jujube-pink))))
   `(minibuffer-prompt ((t (:foreground ,jujube-dark-blue :bold t))))
   `(font-lock-warning-face ((t (:foreground ,jujube-red :bold t))))
   `(hl-line ((t ((:background ,jujube-fringe)))))))

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'jujube)

;;; jujube-theme.el ends here

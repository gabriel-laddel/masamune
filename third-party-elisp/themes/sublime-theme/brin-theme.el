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

;; *****************************************************************************************
;;
;; Brin :- An Emacs port of the Space Grey ST2 theme 
;;
;; *****************************************************************************************

(unless (>= 24 emacs-major-version)
  (error "requires Emacs 24 or later."))

(deftheme brin "Space Grey theme for Emacs")

  (custom-theme-set-variables
    'brin
    '(linum-format " %5i "))

  (let ((background "#11141c")
        (gutters    "#343d46")
        (gutter-fg  "#65737e")
        (gutters-active "#4f5b66")
        (builtin      "#d08770")
        (foreground "#c0c5ce")
        (invisibles "#65737e")
        (lineHighlight "#343d46")
        (selection  "#4f5b66")
        (text       "#c0c5ce")
        (comments   "#65737e")
        (punctuation "#c0c5ce")
        (delimiters "#c0c5ce")
        (operators "#c0c5ce")
        (keywords "#b48ead")
        (variables "#bf616a")
        (functions "#8fa1b3")
        (methods    "#8fa1b3")
        (strings    "#a3be8c")
        (constants "#d08770")
        (white     "#ffffff"))

  (custom-theme-set-faces
   'brin

;; Default colors
;; *****************************************************************************************

   `(default                          ((t (:foreground ,text :background ,background))))
   `(region                           ((t (:background ,selection                       ))))
   `(cursor                           ((t (:background ,white                        ))))
   `(fringe                           ((t (:background ,background   :foreground ,white))))
   `(linum                            ((t (:background ,background :foreground ,gutter-fg))))
   `(mode-line                        ((t (:foreground ,white :background ,gutters-active  ))))
   `(mode-line-inactive               ((t (:foreground ,gutter-fg :background ,gutters  ))))

;; Font lock faces
;; *****************************************************************************************
 
   `(font-lock-keyword-face           ((t (:foreground ,keywords))))
   `(font-lock-type-face              ((t (:foreground ,punctuation))))
   `(font-lock-constant-face          ((t (:foreground ,constants))))
   `(font-lock-variable-name-face     ((t (:foreground ,variables))))
   `(font-lock-builtin-face           ((t (:foreground ,builtin))))
   `(font-lock-string-face            ((t (:foreground ,strings))))
   `(font-lock-comment-face           ((t (:foreground ,comments))))
   `(font-lock-comment-delimiter-face ((t (:foreground ,delimiters))))
   `(font-lock-function-name-face     ((t (:foreground ,functions))))
   `(font-lock-doc-string-face        ((t (:foreground ,strings)))))

;; *****************************************************************************************

   )

;;;###autoload
(when (and (boundp 'custom-theme-load-path) load-file-name)
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

;; *****************************************************************************************

(provide-theme 'brin)

;; Local Variables:
;; no-byte-compile: t
;; End:

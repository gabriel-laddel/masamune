;;; dired-rainbow.el --- Extended file highlighting according to its type

;; Copyright (C) 2014 Matus Goljer

;; Author: Matus Goljer <matus.goljer@gmail.com>
;; Maintainer: Matus Goljer <matus.goljer@gmail.com>
;; Keywords: files
;; Version: 20140222.1042
;; X-Original-Version: 0.0.1
;; Created: 16th February 2014
;; Package-requires: ((dash "2.5.0") (dired-hacks-utils "0.0.1"))

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

;; This package adds more customizable highlighting for files in dired
;; listings.  The group `dired-faces' provides only nine faces and
;; isn't very fine-grained.
;;
;; The definitions are added using a macro `dired-rainbow-define'.
;; You can display its documentation by calling
;;
;; M-x describe-function RET dired-rainbow-define RET
;;
;; Here are some example uses:
;;
;; (defconst my-dired-media-files-extensions
;;   '("mp3" "mp4" "MP3" "MP4" "avi" "mpg" "flv" "ogg")
;;   "Media files.")
;;
;; (dired-rainbow-define html "#4e9a06" ("htm" "html" "xhtml"))
;; (dired-rainbow-define media "#ce5c00" my-dired-media-files-extensions)
;;
;; ; boring regexp due to lack of imagination
;; (dired-rainbow-define log (:inherit default
;;                            :italic t) ".*\\.log")
;;
;; See https://github.com/Fuco1/dired-hacks for the entire collection.

;;; Code:

(require 'dired-hacks-utils)
(require 'dash)

(defgroup dired-rainbow ()
  "Extended file highlighting according to its type."
  :group 'dired-hacks
  :prefix "dired-rainbow-")

(defcustom dired-rainbow-date-regexp "\\sw\\sw\\sw....\\(?:[0-9][0-9]:[0-9][0-9]\\|.[0-9]\\{4\\}\\)"
  "A regexp matching the date/time in the dired listing.

It is used to determine where the filename starts.  It should
*not* match any characters after the last character of the
timestamp.  It is assumed that the timestamp is preceded and
followed by at least one space character.  You should only use
shy groups (prefixed with ?:) because the first group is used by
the font-lock to determine what portion of the name should be
colored."
  :type 'string
  :group 'dired-rainbow)

(defmacro dired-rainbow-define (symbol face-props extensions)
  "Define a custom dired face highlighting files by extension.

SYMBOL is the identifier of the face.  The macro will define a face named

  dired-rainbow-SYMBOL-face.

FACE-PROPS is either a string or a list.  If a string, it is
assumed to be either a color name or a hexadecimal code (#......)
describing a color.  If a list, it is assumed to be a property
list describing the face.  See `defface' for list of possible
attributes.

EXTENSIONS is either a list or a symbol evaluating to a list of
extensions that should be highlighted with this face.  Note that
if you specify a symbol, its value *must* be known during
compilation and must be defined before this macro is processed.

Additionally, EXTENSIONS can be a single string or a symbol
evaluating to a string that is interpreted as a regexp matching
the entire file name."
  (let* ((matcher (if (or (listp extensions)
                          (stringp extensions))
                      extensions
                    (symbol-value extensions)))
         (regexp (concat
                  "^[^!].[^d].*[ ]"
                  dired-rainbow-date-regexp
                  "[ ]\\("
                  (if (listp matcher)
                      (concat ".*\\." (regexp-opt matcher))
                    matcher)
                  "\\)$"))
         (face-name (intern (concat "dired-rainbow-" (symbol-name symbol) "-face"))))
    `(progn
       (defface ,face-name
         '((t ,(if (stringp face-props)
                   `(:foreground ,face-props)
                 face-props)))
         ,(concat "dired-rainbow face matching " (symbol-name symbol) " files.")
         :group 'dired-rainbow)
       (font-lock-add-keywords 'dired-mode '((,regexp 1 ',face-name))))))

(provide 'dired-rainbow)

;;; dired-rainbow.el ends here

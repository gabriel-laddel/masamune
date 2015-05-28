;;; color-theme-approximate.el --- Makes Emacs theme works on terminal transparently
;;;
;; Author: Tung Dao <me@tungdao.com>
;; Version: 20130328.1933
;; X-Original-Version: 0.1
;;
;; This file is NOT part of GNU Emacs
;;
;;; License: BSD http://opensource.org/licenses/BSD-3-Clause
;;
;; Copyright (c) 2013, Tung Dao
;; All rights reserved.
;;
;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions are met:
;;
;; * Redistributions of source code must retain the above copyright notice, this
;; list of conditions and the following disclaimer.
;; * Redistributions in binary form must reproduce the above copyright notice,
;; this list of conditions and the following disclaimer in the documentation
;; and/or other materials provided with the distribution.
;; * Neither the name of the <ORGANIZATION> nor the names of its contributors may
;; be used to endorse or promote products derived from this software without
;; specific prior written permission.
;;
;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
;; AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
;; IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;; ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
;; LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
;; CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
;; SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
;; INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
;; CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
;; ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
;; POSSIBILITY OF SUCH DAMAGE.
;;
;;
;;; Commentary:
;;
;; This package advises the `load-theme' function and convert non terminal colors
;; to their closest approximation. Inspired by VIM's CSApprox plugin
;; http://www.vim.org/scripts/script.php?script_id=2390
;;
;;; Installation:
;; Add the to your .emacs or similar:
;;
;; (autoload 'color-theme-approximate-on "color-theme-approximate")
;; (color-theme-approximate-on)
;;
;;; Changelog
;;
;; v0.2, Mar 29 2013
;; - Fix error that degrades colors on graphical frame running the same Emacs server
;; - Fix error that `ca-defined-rgb-map' is wrong when start Emacs with graphical frame
;;
;; v0.1, Jan 14 2013
;; - Initial version
;;

(require 'color)

(defvar ca-defined-rgb-map nil
  "Map of defined colors and it's RGB value. To speed things up.")

(defvar ca-closest-map
  (make-hash-table :test 'equal :size 256)
  "Approximation cache.")

(defun ca-make-defined-rgb-map ()
  (let ((rgb-map (make-hash-table :test 'equal :size 256)))
    (dolist (name (defined-colors) rgb-map)
      (puthash name (color-name-to-rgb name) rgb-map))))

(defun ca-color-to-rgb (color)
  "Convert color to RGB without implied approximation.
Fallback to `color-name-to-rgb' for named colors."
  (if (not (string-match "#[a-fA-F0-9]\\{6\\}" color))
      (color-name-to-rgb color)
    (mapcar (lambda (component)
              (/ (string-to-number component 16) 255.0))
            (list (substring color 1 3)
                  (substring color 3 5)
                  (substring color 5 7)))))

(defun ca-distance (red green blue)
  (sqrt (+ (* red red) (* green green) (* blue blue))))

(defun ca-rgb-diff (rgb1 rgb2)
  "Distance in RGB colorspace."
  (ca-distance
   (- (nth 0 rgb1) (nth 0 rgb2))
   (- (nth 1 rgb1) (nth 1 rgb2))
   (- (nth 2 rgb1) (nth 2 rgb2))))

(defun ca-rgb-diff-real (rgb1 rgb2)
  "Like `ca-rgb-diff' but scale the components according to eye sensitivity."
  (ca-distance
   (* 0.3 (- (nth 0 rgb1) (nth 0 rgb2)))
   (* 0.59 (- (nth 1 rgb1) (nth 1 rgb2)))
   (* 0.11 (- (nth 2 rgb1) (nth 2 rgb2)))))

(defvar ca-approximator #'ca-rgb-diff-real
  "Function used to calculate the different between colors.
The approximator is called with two lists of RGB values, for
the pre-defined color and the current processed respectly.")

(defun ca--approximate (color)
  "Find the closest defined color. Use our custom `ca-color-to-rgb'
because `color-name-to-rgb' is already return the wrong approximation."
  (let ((diff nil)
        (min nil)
        (min-diff 3)
        (rgb (ca-color-to-rgb color)))
    (dolist (defined (defined-colors) min)
      (setq diff (funcall ca-approximator rgb (gethash defined ca-defined-rgb-map)))
      (when (< diff min-diff)
        (setq min-diff diff
              min defined)))))

(defun ca-approximate (color)
  "See `ca--approximate'."
  (or (gethash color ca-closest-map)
      (puthash color (ca--approximate color) ca-closest-map)))

(defun ca-process-face (face)
  (let ((background (face-background face))
        (foreground (face-foreground face))
        (frame (selected-frame)))
    (when background
      (set-face-attribute face frame :background (ca-approximate background)))
    (when foreground
      (set-face-attribute face frame :foreground (ca-approximate foreground)))))

(defadvice load-theme (after ca-apply-approximation)
  (unless (display-graphic-p (selected-frame))
    (setq ca-defined-rgb-map (ca-make-defined-rgb-map))
    (mapc #'ca-process-face (face-list))))

;;;###autoload
(defun color-theme-approximate-on ()
  (interactive)
  (ad-enable-advice 'load-theme 'after 'ca-apply-approximation)
  (ad-activate 'load-theme))

;;;###autoload
(defun color-theme-approximate-off ()
  (interactive)
  (ad-disable-advice 'load-theme 'after 'ca-apply-approximation)
  (ad-activate 'load-theme))

(provide 'color-theme-approximate)

;;; color-theme-approximate.el ends here

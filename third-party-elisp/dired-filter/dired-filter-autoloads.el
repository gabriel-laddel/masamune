;;; dired-filter-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (dired-filter-mode dired-filter-add-saved-filters
;;;;;;  dired-filter-load-saved-filters dired-filter-delete-saved-filters
;;;;;;  dired-filter-save-filters dired-filter-pop-all dired-filter-pop
;;;;;;  dired-filter-decompose dired-filter-negate dired-filter-or
;;;;;;  dired-filter-transpose dired-filter-define) "dired-filter"
;;;;;;  "dired-filter.el" (21289 7446 802861 84000))
;;; Generated autoloads from dired-filter.el

(defvar dired-filter-map (let ((map (make-sparse-keymap))) (define-key map "n" 'dired-filter-by-name) (define-key map "r" 'dired-filter-by-regexp) (define-key map "." 'dired-filter-by-extension) (define-key map "h" 'dired-filter-by-dot-files) (define-key map "o" 'dired-filter-by-omit) (define-key map "e" 'dired-filter-by-predicate) (define-key map "f" 'dired-filter-by-file) (define-key map "d" 'dired-filter-by-directory) (define-key map "m" 'dired-filter-by-mode) (define-key map "|" 'dired-filter-or) (define-key map "!" 'dired-filter-negate) (define-key map "*" 'dired-filter-decompose) (define-key map (kbd "TAB") 'dired-filter-transpose) (define-key map "p" 'dired-filter-pop) (define-key map "/" 'dired-filter-pop-all) (define-key map "S" 'dired-filter-save-filters) (define-key map "D" 'dired-filter-delete-saved-filters) (define-key map "A" 'dired-filter-add-saved-filters) (define-key map "L" 'dired-filter-load-saved-filters) map) "\
Keymap used for `dired-filter-mode'.")

(defvar dired-filter-mark-map (let ((map (make-sparse-keymap))) (define-key map "n" 'dired-filter-mark-by-name) (define-key map "r" 'dired-filter-mark-by-regexp) (define-key map "." 'dired-filter-mark-by-extension) (define-key map "h" 'dired-filter-mark-by-dot-files) (define-key map "o" 'dired-filter-mark-by-omit) (define-key map "e" 'dired-filter-mark-by-predicate) (define-key map "f" 'dired-filter-mark-by-file) (define-key map "d" 'dired-filter-mark-by-directory) (define-key map "m" 'dired-filter-mark-by-mode) map) "\
Keymap used for marking files.")

(autoload 'dired-filter-define "dired-filter" "\
Create a filter NAME.

Files matched by the predicate are kept in the listing.

For filters where the reverse behaviour makes more sense as
default, you can set the `:remove' argument to `t' to flip the
truth value by default.  Do not flip the value in the predicate
itself!

DOCUMENTATION is the documentation of the created filter.

BODY should contain forms which will be evaluated to test whether
or not a particular file should be displayed or not.  The forms
in BODY will be evaluated with FILE-NAME bound to the file name,
and QUALIFIER bound to the current argument of the filter.
During the evaluation point is at the beginning of line.

:description is a short description of this filter (usually one
or two words).

:reader is a form that is used by `interactive' to read optional
argument.  If not specified or nil, the filter does not accept
argument from user.

:qualifier-description is a form to format qualifier for display.

:remove reverses the default matching strategy of the filter.

\(fn NAME DOCUMENTATION (&key description (qualifier-description (quote (identity qualifier))) reader remove) &rest BODY)" nil (quote macro))
 (autoload 'dired-filter-by-dot-files "dired-filter")
 (autoload 'dired-filter-by-name "dired-filter")
 (autoload 'dired-filter-by-regexp "dired-filter")
 (autoload 'dired-filter-by-extension "dired-filter")
 (autoload 'dired-filter-by-omit "dired-filter")
 (autoload 'dired-filter-by-predicate "dired-filter")
 (autoload 'dired-filter-by-directory "dired-filter")
 (autoload 'dired-filter-by-file "dired-filter")
 (autoload 'dired-filter-by-mode "dired-filter")

(autoload 'dired-filter-transpose "dired-filter" "\
Transpose the two top filters.

\(fn)" t nil)

(autoload 'dired-filter-or "dired-filter" "\
Or the top two filters.

\(fn)" t nil)

(autoload 'dired-filter-negate "dired-filter" "\
Logically negate the top filter.

\(fn)" t nil)

(autoload 'dired-filter-decompose "dired-filter" "\
Decompose the composite filter on top of the stack.

This means, if the filter is an `or' or `not' filter, pop it and
push all its constituents back on the stack.

\(fn)" t nil)

(autoload 'dired-filter-pop "dired-filter" "\
Remove the top filter in this buffer.

\(fn &optional ARG)" t nil)

(autoload 'dired-filter-pop-all "dired-filter" "\
Remove all the filters in this buffer.

\(fn)" t nil)

(autoload 'dired-filter-save-filters "dired-filter" "\
Save the the FILTERS in this dired buffer under a NAME for later use.

\(fn NAME FILTERS)" t nil)

(autoload 'dired-filter-delete-saved-filters "dired-filter" "\
Delete saved filters with NAME from `dired-filter-saved-filters'.

\(fn NAME)" t nil)

(autoload 'dired-filter-load-saved-filters "dired-filter" "\
Set this buffer's filters to filters with NAME from `dired-filter-saved-filters'.

\(fn NAME)" t nil)

(autoload 'dired-filter-add-saved-filters "dired-filter" "\
Add to this buffer's filters filters with NAME from `dired-filter-saved-filters'.

\(fn NAME)" t nil)

(autoload 'dired-filter-mode "dired-filter" "\
Toggle filtering of files in Dired.

When you toggle the filter mode, the filter stack and all other
state is preserved, except the display is not altered.  This
allows you to quickly toggle the active filter without need of
popping the stack and then re-inserting the filters again.

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads nil nil ("dired-filter-pkg.el") (21289 7446 894670
;;;;;;  254000))

;;;***

(provide 'dired-filter-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; dired-filter-autoloads.el ends here

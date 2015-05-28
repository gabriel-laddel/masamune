;;; find-file-in-repository-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (find-file-in-repository) "find-file-in-repository"
;;;;;;  "find-file-in-repository.el" (21157 25259 520994 777000))
;;; Generated autoloads from find-file-in-repository.el

(autoload 'find-file-in-repository "find-file-in-repository" "\
find-file-in-repository will autocomplete all files in the
   current git, mercurial or other type of repository, using
   ido-find-file when available. When the current file is not
   located inside of any repository, falls back on a regular
   find-file operation.

\(fn)" t nil)

(defalias 'ffir 'find-file-in-repository)

(put 'ffir-repository-types 'safe-local-variable 'listp)

(put 'ffir-avoid-HOME-repository 'safe-local-variable 'booleanp)

;;;***

;;;### (autoloads nil nil ("find-file-in-repository-pkg.el") (21157
;;;;;;  25259 656365 721000))

;;;***

(provide 'find-file-in-repository-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; find-file-in-repository-autoloads.el ends here

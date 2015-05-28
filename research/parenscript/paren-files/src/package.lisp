(defpackage parenscript.reader
  (:nicknames parenscript-reader)
  (:use :common-lisp :parenscript)
  (:documentation "The Parenscript reader.  Used for reading Parenscript forms.

The only significant difference between this and the standard lisp reader is that package names are
script package names rather than Lisp package names.")
  (:shadow #:readtablep
           #:readtable-case
           #:copy-readtable
           #:get-macro-character
           #:get-dispatch-macro-character
           #:set-macro-character
           #:set-dispatch-macro-character
           #:make-dispatch-macro-character
           #:set-syntax-from-char
           #:read-preserving-whitespace
           #:read
           #:read-from-string
           #:read-delimited-list
           #:backquote-comma-dot
           #:backquote
           #:backquote-comma
           #:backquote-comma-at
           
           #:*read-eval*
           #:*read-base*
           #:*read-default-float-format*
           #:*read-suppress*
           #:*readtable*
           #:*read-suppress*
           #:*reader-error*
           #:*read-suppress*
           
           #:readtable
           #:backquote
           #:reader-error)
  (:export #:read
	   #:read-from-string
	   #:read-delimited-list))

(defpackage #:parenscript.asdf
  (:use :parenscript :asdf :common-lisp)
  (:documentation "ASDF extensions that help compile and use Parenscript systems.")
  (:export #:compile-script-system))

(defpackage #:parenscript.files
  (:nicknames :paren-files)
  (:use :common-lisp :parenscript.asdf)
  (:documentation "Library that compiles Parenscript files and systems.")
;  (:import-from :parenscript
;		#:*compilation-environment*)
  (:export #:compile-script-file
	   #:compile-script-file-to-string
	   #:compile-script-file-to-js-file
	   #:compile-script-system))

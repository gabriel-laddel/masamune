(in-package #:asdf)

(defsystem #:masamune
  :serial t
  :license "I. You do not have, nor can you ever acquire the right to use, copy or distribute this software ;

    II. Should you use this software for any purpose, or copy and distribute it, to anyone or in any manner, you are breaking the laws of whatever soi-disant \"sovereign jurisdiction\" you may be deemed to be located within, and you promise to continue doing so in the indefinite future."
  :depends-on (#:alexandria
	       #:archive
	       #:arnesi
	       #:anaphora
	       #:asdf
	       #:bordeaux-threads
	       #:cl-date-time-parser
	       #:trivial-timers
	       #:cl-arrows
	       #:cl-css
	       #:cl-csv
	       #:cl-html-parse
	       #:cl-json
	       #:cl-ppcre
	       #:cl-who
	       #:spellcheck
	       #:enchant
	       #:enchant-autoload
	       #:chipz
	       #:closer-mop
	       #:manardb
	       #:clx
	       #:clx-cursor
	       #:do-urlencode
	       #:drakma
	       #:fset
	       #:fiasco
	       #:generic-sequences
	       #:iterate
	       #:let-plus
	       #:local-time
	       #:mcclim
	       #:mcclim-png-bitmaps
	       #:mcclim-gif-bitmaps
	       #:mcclim-jpeg-bitmaps
	       #:mcclim-tiff-bitmaps
	       #:climacs
	       #:lparallel
	       #:opticl
	       #:optima
	       #:parenscript
	       ;; #:serapeum
	       #:stumpwm
	       #:uuid
	       #:parse-js
	       #:vecto
	       #:zpng
	       #:fare-quasiquote-optima
	       #:fare-quasiquote
	       #:cl-syntax-fare-quasiquote 
	       #:fare-quasiquote-extras
	       #:fare-quasiquote-readtable
	       #:fare-quasiquote-test
	       #:xmls)
  :components ((:file "packages")
	       (:file "init")
	       (:file "util")
	       (:file "monkey-patches")
	       (:file "classes")	       
	       (:file "masamune-gui")
	       (:file "dashboard")
	       (:file "kmap")
	       (:file "default-data")
	       (:file "systems/captains-log")
	       (:file "systems/mathematics-practice")
	       (:file "systems/programming-practice")
	       (:file "systems/summarize-logs")
	       (:file "browser/conkeror")
	       ;; (:file "browser/js-to-ps")
	       (:file "finalize")
	       (:file "save-state")))

(defpackage #:masamune
  (:nicknames :mm)
  (:export #:*alphabet*
	   #:cat
	   #:port-in-use-p
	   #:-pi
	   #:open-ports
	   #:rp
	   #:rp-in-dir
	   #:export-project-rpcs
	   #:ppath
	   #:slurp-file
	   #:node
	   #:message
	   #:node-focusedp
	   #:name
	   #:parents
	   #:description
	   #:program
	   #:object-slots-plist
	   #:run-program
	   #:convert-image-file
	   #:class-slot-names
	   #:defalias
	   #:download-url
	   #:QLPP 
	   #:RECURSIVE-CONTENTS
	   #:EMACS-BACKUP?
	   #:awhen
	   #:aif
	   #:it
	   #:drop
	   #:encoded-query
	   #:even?
	   #:filter
	   #:frotate
	   #:http
	   #:keys
	   #:keys
	   #:lambdab
	   #:llast
	   #:ls
	   #:ls-clean
	   #:parse-html
	   #:parse-json
	   #:read-from-file
	   #:save-masamune-state
	   #:save-screenshot-as-png
	   #:screenshot
	   #:screenshot-window-to-png-file
	   #:start!
	   #:take
	   #:thread-alive?
	   #:thread-by-name
	   #:threads
	   #:write-to-file
	   #:regex-replace-in-file)
  (:use #:alexandria
	#:anaphora
	#:cl-fad
	#:cl
	#:cl-arrows
	#:cl-ppcre
	#:generic-seq
	#:iterate
	#:let-plus
	#:local-time
	;; #:manardb
	#:xlib)
  (:import-from #:uiop #:run-program)
  (:import-from #:html-parse #:parse-html)
  (:import-from #:arnesi #:awhile)
  (:shadow #:timestamp)
  (:shadowing-import-from #:alexandria #:array-index #:copy-stream #:copy-file)
  (:shadowing-import-from #:cl-ppcre #:regex-replace-all))

(defpackage #:masamune-gui
  (:nicknames :mmg)
  (:use #:alexandria
	#:anaphora
	#:cl-ppcre
	#:clim
	#:clim-internals
	#:clim-lisp
	#:masamune
	#:zpng
	#:local-time)
  (:shadow #:frame #:frame-p #:frame-name)
  (:shadowing-import-from #:clim #:simple-parse-error)
  (:export #:run-or-focus-dashboard #:run-or-focus-kmap #:run-or-focus-repository))

(defpackage #:masamune-browser
  (:nicknames #:mmb)
  (:use #:cl
	#:cl-ppcre
	#:optima
	#:parenscript
	#:mm
	#:usocket
	#:parse-js))

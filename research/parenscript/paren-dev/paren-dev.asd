(in-package :cl-user)

(defpackage paren-dev-system
  (:use :cl :asdf))

(in-package :paren-dev-system)

(asdf:operate 'asdf:load-op :paren-files)

(defsystem :paren-dev
  :name "paren-dev"
  :author "Red Daly <reddaly@gmail.com.>"
  :version "0.0.1"
  :license "MIT"
  :description "Library for developing with Parenscript.  Hooks into
  slime-proxy and slime-parenscript.  Includes means of including
  javascript files for WebSocket imitation necessary to work on IE,
  Firefox, Chrome."
  :components ((:static-file "paren-dev.asd")
	       (:module "src"
			:components
			((:file "package")
			 (:file "paren-dev" :depends-on ("package"))
			 (:module "paren"
				  :components
				  ((:javascript-file "js-glue")
                                   (:parenscript-file "paren-dev" :depends-on ("js-glue")))
                                  :depends-on ("vendor"))
                         (:module "vendor"
                                  :pathname #P"../vendor/"
				  :components
                                  ((:module "web-socket-js"
                                            :serial t
                                            :components
                                            ((:javascript-file "swfobject")
                                             (:javascript-file "FABridge")
                                             (:javascript-file "web_socket")
                                             (:static-file "WebSocketMain.swf"))))))))
  :depends-on (:parenscript :paren-psos))

; to emit the necessary javascripts:
; (with-output-to-string (s)
;           (paren-files:compile-script-system :paren-dev :output-stream t))

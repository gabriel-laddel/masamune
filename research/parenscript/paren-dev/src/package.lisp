(defpackage org.iodb.paren-dev
  (:nicknames #:paren-dev)
  (:use :common-lisp :parenscript)
  (:export #:dev-scripts
           #:ws-emulation-swf-truename
           #:*web-sockets-swf-uri*

           ;; parenscript exports
           #:*swank-web-socket*
           #:make-swank-connection
           #:close-swank-connection
           #:make-web-socket))





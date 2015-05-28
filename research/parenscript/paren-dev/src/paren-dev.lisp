(in-package :paren-dev)

(defparameter *web-sockets-swf-uri* nil
  "Location of SWF file used to emulate WebSockets.")

(defun ws-emulation-swf-truename ()
  (asdf:system-relative-pathname (asdf:find-system :paren-dev)
                                 #P"vendor/web-socket-js/WebSocketMain.swf"))

(defun dev-scripts (swf-location)
  "Returns a string of all the Javascript needed to emulate a web socket."
  (let ((*web-sockets-swf-uri* swf-location))
    (with-output-to-string (s)
      (paren-files:compile-script-system :paren-dev :output-stream t))))

(defpsmacro swanklog (fmt &rest args)
  `(paren-psos:pslog (+ "swank: " ,fmt) ,@args))
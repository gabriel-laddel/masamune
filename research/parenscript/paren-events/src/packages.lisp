(defpackage :paren-events
    (:use :cl :parenscript :paren-util)
  (:export
   ;; parenscript symbols
   #:event
   #:event-origin
   #:event-description
   #:event-handlers
   #:event-fired
   #:register-event-handler
   #:register-dom-event-handler
   #:event-handler-fits?
   #:stop-dom-event))


(in-package :paren-events)
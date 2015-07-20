(in-package #:mm)

(defun comms ()
  (mmb::open-uri "http://mail.google.com" t)
  (mmb::open-uri "http://facebook.com" t))

(defun start-communications (habit)
  (record-event habit (event :started))
  (comms)
  (record-event habit (event :finished)))

(defun communications-install ()
  (push (i 'habit
	   :name "Communications"
	   :description "~check mail"
	   :initialization-function 'start-communications
	   :occurrence :daily)
	*habits*))

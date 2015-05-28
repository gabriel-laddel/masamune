(in-package #:mm)

(defun start-communications (habit)
  (record-event habit (event :started))
  (mmb::open-uri "http://mail.google.com" t)
  (mmb::open-uri "http://facebook.com" t)
  (record-event habit (event :finished)))

(defun communications-install ()
  (push (i 'habit
	   :name "Communications"
	   :description "~check mail"
	   :initialization-function 'start-communications
	   :occurrence :daily)
	*habits*))

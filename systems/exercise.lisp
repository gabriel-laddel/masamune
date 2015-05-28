(in-package #:mm)

(defun start-exercise (habit)
  (record-event habit (event :started))
  (record-event habit (event :finished)))

(defun exercise-install ()
  (push (i 'habit
	   :name "Exercise"
	   :initialization-function 'start-exercise
	   :occurrence :daily)
	*habits*))

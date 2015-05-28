(in-package #:mm)

(defmacro c (name (&rest superclasses) (&rest slots) &rest args)
  `(defclass ,name ,superclasses 
     ,(loop for slot-name in slots
	    collect (list slot-name :accessor slot-name 
				    :initarg (intern (format nil "~a" slot-name) 'keyword) 
				    :initform nil))
     ,@args))

;;; Knowledge map
;;; ============================================================================

(c node () (name parents description program))

(defun make-node (name &optional (parents) program)
   (push (i 'node :name name
		  :parents (mapcar #'node-by-name parents)
		  :program program)
	 *nodes*))

(defun node-by-name (name)
   (aif (some (lambda (n) (when (string= (string-downcase name) (string-downcase (name n))) n)) *nodes*)
	it (error "no nodes with that name exist")))

(defmethod node-focusedp ((node node)) (eq node *focused-node*))

(defmethod children ((node node))
  (filter (lambda (n) (member node (parents n) :test #'eq)) *nodes*))

(mmg::define-presentation-type node ())

;;; habits
;;; ============================================================================

(c habit () (name description occurrence initialization-function visualization-function))

(defmethod print-object ((habit habit) stream)
  (with-slots (name description) habit
    (format stream "#< habit ~a , ~a >" name description)))

(defmethod occurs-now? ((habit habit))
  "habits should specialize this method as they see fit"
  ;; TODO 2014-11-21T16:26:04-08:00 Gabriel Laddel
  ;; tie into the sleep cycle, currently approximating it.
  (or (null (events habit))
      (let* ((last-finished-event (llast (filter (lambda (l) (eq :finished (getf l :event))) (events habit))))
	     (day (* 60 60 24))
	     (week (* day 7))
	     (time (getf last-finished-event :time))
	     (now (get-universal-time)))
	(if last-finished-event 
	    (ecase (occurrence habit)
	      (:daily (<= time (- now (* .7 day))))
	      (:weekly (<= time (- now week))))
	    t))))

(defun event (event-name &rest misc-information)
  (list :event event-name :time (get-universal-time) :misc-information misc-information))

(defun note (note-string)
  (list :note note-string :time (get-universal-time)))

(defun habit-events-pathname (habit) 
  (format nil "~~/.masamune/data/~a" (regex-replace-all " "  (string-downcase (name habit)) "-")))

(defun habit-notes-pathname (habit) 
  (format nil "~~/.masamune/data/~a-notes" (regex-replace-all " "  (string-downcase (name habit)) "-")))

(defmethod events ((habit habit))
  (when (probe-file (habit-events-pathname habit))
    (read-file (habit-events-pathname habit))))

(defmethod notes ((habit habit))
  (when (probe-file (habit-notes-pathname habit))
    (read-file (habit-notes-pathname habit))))

(defmethod latest-note ((habit habit))
  (awhen (notes habit) (llast it)))

(defmethod record-event ((habit habit) event)
  (with-open-file (stream (habit-events-pathname habit)
			  :if-exists :append
			  :if-does-not-exist :create
			  :direction :output)
    (write event :stream stream)))

(defmethod record-note ((habit habit) note)
  (with-open-file (stream (habit-notes-pathname habit)
			  :if-exists :append
			  :if-does-not-exist :create
			  :direction :output)
    (write note :stream stream)))

;;; Problems  
;;; ============================================================================

(defun submission (state problem-start &optional submission)
  "STATE is one of #{ :abort, :correct, :incorrect, :hint, :surrender}
PROBLEM-START, is the slot by the same name on the problem that has been
submitted, specifically, it's unix time."
  (list :state state :time (- (get-universal-time) problem-start) :submission submission))

(c problem (event-store) (prompt answers hint correctness-test start-time))

(defmethod start (problem)
  (format t "~%~s~%" (prompt problem))
  (setf (start-time problem) (get-universal-time))
  (let+ ((submission (read-line)))
    (cond ((equal (string-downcase submission) "surrender")
           (push (submission :surrender (start-time problem) submission) 
                 (submissions problem))
           (format t "~%Lame. The answer is ~S~%"  (answers problem))
	   (start problem))

          ((or (funcall (correctness-test problem) submission (answers problem))
	       (member submission (answers problem) :test #'equal))
           (push (submission :correct (start-time problem) submission) 
                 (submissions problem))
           (format t "~%~a~%" "Correct!")
           t)

          ((equal submission "hint")
           (push (submission :hint (start-time problem)) 
                 (submissions problem))
           (format t "~%~a~%" (hint problem)))

          ((member submission '("quit" "abort" "exit") :test #'equal)
           (push (submission :abort (start-time problem)) (submissions problem))
           (format t "~%See you again soon.~%"))

          (t (format t "~%~a is incorrect. Try again.~%" submission)
             (push (submission :incorrect (start-time problem) submission) 
                   (submissions problem))
             (start problem)))))

(defun problem (prompt answers)
  (unless (listp answers) (setf answers (list answers)))
  (make-instance 'problem
		 :prompt prompt
		 :answers answers))

(defclass pgraphic nil
  ((x :accessor x :initarg :x :initform nil)
   (y :accessor y :initarg :y :initform nil))
  (:documentation 
   "persistent graphic. Manardb does not save slots inherited from classes w/o
mm-metaclass")
  (:metaclass manardb::mm-metaclass))

;;; Agenda
;;; ============================================================================

(c agenda-item () (due-date title description start-function))

(defun make-agenda-item (title &optional description due-date start-function)
  (make-instance 'agenda-item
		 :title title
		 :due-date due-date
		 :description description
		 :start-function start-function))

(defun populate-agenda-items ()
  (setf *agenda* (l (make-agenda-item "Test agenda item 1")
		    (make-agenda-item "Test agenda item 2")
		    (make-agenda-item "Test agenda item 3")
		    (make-agenda-item "Test agenda item 4"))))

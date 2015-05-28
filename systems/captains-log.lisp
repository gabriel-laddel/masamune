(in-package #:mm)

(defvar *captains-log-start-time* nil)
(defparameter *captains-log-length* 25 "# of minutes")
(defparameter topics '("post fiat IP" 
		       "post fiat security"
		       "robotics"
		       "the law throughout history"
		       "PURSUING THE LIMITS OF FAILED SYMMETRY") 
  "list of strings naming topics to expound on")
(defparameter wips nil
  "list of pathnames naming wip documents")

(c captains-log () (start-time end-time title body word-count vocabulary-words))

(defmethod start-timestamp ((captains-log captains-log))
  (universal-to-timestamp (start-time captains-log)))

(defmethod vocabulary-words-used ((log captains-log))
  (loop for word in (vocabulary-words log)
	when (search word (body log) :test #'string=) collect word))

(defmethod occurs-on-p ((log captains-log) (timestamp local-time:timestamp))
  (= (timestamp-day timestamp) (timestamp-day (zero-timestamp (start-timestamp log)))))

(defun zero-timestamp (timestamp)
  (adjust-timestamp timestamp (set :hour 0) (set :minute 0) (set :sec 0) (set :nsec 0)))

(defmethod occurence-percentage ((log captains-log) (timestamp local-time:timestamp))
  "given LOG occurs on TIMESTAMP, what time of the day does it occur?"
  (float (* 100 (/ (timestamp-hour (universal-to-timestamp (start-time log))) 24))))

(defun make-empty-file (pathname)
  (with-open-file (stream pathname :direction :output)))

(defun new-captains-log (habit)
  (declare (ignore habit))
  (funcall 'mmg::com-init-captains-log))

(defun init-captains-log% (title) 
  (let* ((safe-title (mm::regex-replace-all "/" (mm::regex-replace-all " " (string-downcase title) "-") ""))
	 (log-temporary-pathname (format nil "/tmp/~A.txt" safe-title))
	 (log-pathname (format nil "~~/.masamune/captains-logs/~A.lisp" safe-title)))
    (if (probe-file log-pathname)
	(loop for i from 0 to 3
	      do (progn (sleep 1) (stumpwm::message-no-timeout "file already exists, rename please")) 
	      finally (stumpwm::eval-command "init-captains-log"))
	(progn (mm::make-empty-file log-temporary-pathname)
	       (setf mm::*captains-log-start-time* (get-universal-time))
	       (let* ((climacs-gui:*with-scrollbars* nil)) 
		 (climacs::edit-file log-temporary-pathname))
	       (stumpwm::run-with-timer (* mm::*captains-log-length* 60) nil
					(lambda () (captains-log-cleanup log-pathname log-temporary-pathname title)))))
    (stumpwm::pop-top-map)))

(defun captains-log-cleanup (log-pathname log-temporary-pathname title &optional vocabulary-words)
  (stumpwm::message-no-timeout "save the buffer you're working in, time is almost up")
  (record-event (mmg::habit-by-name "captains log") (event :finished))
  (loop for i from 15 downto 0
	do (when (< i 10) (stumpwm::message "~d seconds remaining to save" i) (sleep 1))
	finally (progn (stumpwm::message-no-timeout "finished")
		       (awhen (mm:thread-by-name "Climacs") (bt:destroy-thread it))
		       (let* ((body (with-open-file (stream log-temporary-pathname :direction :input)
				      (slurp-stream stream))))
			 (with-open-file (stream log-pathname :direction :output 
							      :if-does-not-exist :create)
			   (write (list :start-time *captains-log-start-time* 
					:end-time (get-universal-time)
					:title title
					:body body
					:word-count (length (split " " body))
					:vocabulary-words vocabulary-words) 
				  :stream stream))))))

(in-package #:mmg)

(defun captains-log-file-obj (pathname)
  (apply #'make-instance (cons 'mm::captains-log (car (mm::read-file pathname)))))

(defun logs-by-time (l l1)
  (< (mm::start-time l) (mm::start-time l1)))

(defparameter *time-sorted-captains-logs*
  (sort (filter #'mm::start-time
		(mapcar #'captains-log-file-obj
			(ls-clean "~/.masamune/captains-logs/")))
	#'logs-by-time))

(defparameter summary-focused-day
  (awhen (llast (take 10 *time-sorted-captains-logs*))
    (mm::start-timestamp it)))

(defparameter *focused-log*
  (llast (take 10 *time-sorted-captains-logs*)))

(defparameter plot-ink +blue+)

(define-presentation-type captains-log ())

(defun days-and-logs (start-timestamp n-days)
  "returns tuples for N-DAYS from START-TIMESTAMP, inclusively as a tuple with
the correspondingly logs"
  (loop for i from 0 to n-days
	for tstamp = (adjust-timestamp start-timestamp (offset :day i))
	for o = (filter (lambda (log) (when (= (day-of (mm::start-timestamp log)) (day-of tstamp))
				   log))
			*time-sorted-captains-logs*)
	collect (list tstamp o)))

(defun total-days-and-logs ()
  (let* ((start-time (mm::start-timestamp (car *time-sorted-captains-logs*)))
	 (end-time (mm::start-timestamp (llast *time-sorted-captains-logs*))))
    (days-and-logs start-time (- (day-of end-time) (day-of start-time)))))

(defun visualize-captains-log (habit sheet)
  ;; XXX 2014-11-09T05:34:38-08:00 Gabriel Laddel
  ;; 
  ;; the dashboard doesn't render correctly on startup. for whatever reason it
  ;; thinks that it has about half the rendering space alotted and will draw the
  ;; graphics as such. `(step (run-dashboard))' to debug
  ;;
  ;; additionally, there appears to be some sort of threading issue preventing a
  ;; hack to just send the dashboard a key event to fix the resolution.  see:
  ;; https://github.com/stumpwm/stumpwm/issues/166
  (declare (ignore habit))
  "when two logs were written on the same day, the summary displays only the one
with the highest word count, and the detail view displays both"
  (when summary-focused-day
    (let* ((y-detail-start (- (/ vph 4) 50))
	   (y-detail-end (* 2 (/ vph 3)))
	   (detail-height (- y-detail-end y-detail-start))
	   (y-summary-start (+ 40 y-detail-end))
	   (y-summary-end vph)
	   (summary-height (- y-summary-end y-summary-start))
	   (max-word-count (apply #'max (mapcar #'mm::word-count *time-sorted-captains-logs*)))
	   ;; detail view
	   (number-of-detail-days 20)
	   (detail-day-width (floor (- (/ vpw number-of-detail-days) 2)))
	   (datapoint-width  (/ (floor vpw) (length (total-days-and-logs))))
	   (focused-timestamps (loop for i from 0 to (- number-of-detail-days 1)
				     for d = summary-focused-day then (timestamp+ d 1 :day)
				     collect d))
	   (detail-start-pos (position (day-of summary-focused-day)
				       (mapcar (compose #'day-of #'car) (total-days-and-logs)) :test #'=))
	   (x-offset 3))

      ;; Focused Log Metrics
      (let* ((*print-pretty* nil))
      	(with-slots (mm::word-count mm::end-time mm::start-time) *focused-log*
	  (format sheet
		  "~%    word count: ~d~%    vocabulary words used: ~a, missed ~a~%    session-length: ~d minutes ... (TODO: avgs.)"
		  mm::word-count nil nil (/ (- mm::end-time mm::start-time) 60))))

      ;; Detail
      (loop for (day log-list) in (drop detail-start-pos (total-days-and-logs))
      	    for i = 0 then (1+ i)
      	    when (member (mm::zero-timestamp day) 
			 (mapcar 'mm::zero-timestamp focused-timestamps) :test #'timestamp=)
	      do (let* ((timestring (format-timestring nil day :format '(:month "/" :day)))
			(log (car (sort log-list (lambda (log1 log2) (> (mm::word-count log1) (mm::word-count log2))))))
			(r (when log (/ (mm::word-count log) max-word-count)))
			(x (+ 17 x-offset (* detail-day-width i))) 
			(x-1 (+ detail-day-width x)))
		   (when log
		     (with-output-as-presentation (sheet log 'captains-log) 
		       (draw-rectangle* sheet x y-detail-end x-1 (+ y-detail-start (- detail-height (* detail-height r)))
					:ink plot-ink :filled nil)))
		   (draw-text* sheet timestring (+ x (/ detail-day-width 2)) (+ 15 y-detail-end)
			       :align-x :center :align-y :center)))

      ;; Summary
      (loop for (day log-list) in (total-days-and-logs)
      	    for i = 0 then (1+ i) 
      	    do (progn (when (member (mm::zero-timestamp day)
      				    (mapcar #'mm::zero-timestamp focused-timestamps) :test #'timestamp=)
      			(draw-rectangle* sheet
					 (+ 3 (* i datapoint-width))
					 (+ 2 y-summary-end) 
      					 (+ 3 datapoint-width (* i datapoint-width)) 
      					 y-summary-start :ink +dark-grey+))
      		      (when log-list 
      			(let* ((log (car (sort log-list (lambda (log1 log2) (> (mm::word-count log1) (mm::word-count log2))))))
      			       (x   (+ x-offset (* i datapoint-width)))
      			       (r   (/ (mm::word-count log) max-word-count))
      			       (h   (* summary-height r))
      			       (y   (- y-summary-end h)))
      			  (draw-line* sheet x y-summary-end x y))))))))

(defun end-viz-switch-timestamp ()
  "20 is derived from `num-detail-days' in visualize-captains-log"
  (car (nth (- (length (total-days-and-logs)) 20)
	    (total-days-and-logs))))

(defun start-viz-switch-timestamp ()
  (caar (total-days-and-logs)))

(define-dashboard-command (com-move-selection-forwards
			   :name t :keystroke (#\f :control)) ()
  (if (timestamp< (end-viz-switch-timestamp)
		  (timestamp+ summary-focused-day 1 :day))
      (setf summary-focused-day (start-viz-switch-timestamp))
      (setf summary-focused-day (timestamp+ summary-focused-day 1 :day))))

(define-dashboard-command (com-move-selection-back 
			   :name t :keystroke (#\b :control)) ()
  (if (timestamp> (start-viz-switch-timestamp)
		  (timestamp- summary-focused-day 1 :day))
      (setf summary-focused-day (end-viz-switch-timestamp))
      (setf summary-focused-day (timestamp- summary-focused-day 1 :day))))

(define-dashboard-command (com-set-selection :name t) ()
  "This function exists because there are bugs in the naive stumpwm input
functionality that can cause a deadlock"
  (setf summary-focused-day 
	(nth (accept 'number :prompt (format nil "Select nth day (min 0, max ~d):"
					     (length *time-sorted-captains-logs*)))
	     *time-sorted-captains-logs*)))

(defun emacs-find-file (pathname)
  (stumpwm::emacs)
  (ignore-errors 
   (mm::with-live-swank-connection
       (swank::eval-in-emacs
	`(progn (delete-other-windows)
		(find-file ,(etypecase pathname
			      (string pathname)
			      (pathname (namestring pathname))))
		nil)))))

(define-dashboard-command (com-init-captains-log :name t) ()
  ""
  (let* ((input (accept 'string :prompt "'t' to view topics, 'w' to select a WIP document, else name title")))
    (cond
      ;; WIP documents
      ((mm::cistring= "w" input) (let* ((documents (loop for i in mm::wips
							 for c = 0 then (1+ c)
							 appending (list c i))))
				   (format *query-io* "WIP documents ~{, ~S~}~%~%" documents)
				   (let* ((input-key (accept 'number :prompt "select by numeric key")))
				     (if (member input-key (keys documents) :test #'=)
					 (progn (mm::record-event (mmg::habit-by-name "captains log") (mm::event :started))
						(stumpwm::run-with-timer
						 (* mm::*captains-log-length* 60) nil
						 (lambda () (progn (mm::record-event (mmg::habit-by-name "captains log") (mm::event :finished))
							      (mm::record-state :captains-log-wip)
							      (mmg::run-or-focus-dashboard))))
						(if (mm::state-record-exists? :captains-log-wip)
						    (mm::restore-state :captains-log-wip)
						    (emacs-find-file (getf documents input-key))))
					 (com-init-captains-log)))))
      ;; View topics for consideration
      ((mm::cistring= "t" input) (progn (format *query-io* "Topics for consideration are~{, ~a~}~%~%" mm::topics)
					(com-init-captains-log)))
      ;; Regular captains log
      (t (mm::init-captains-log% input)))))

(define-dashboard-command com-focus-captains-log
    ((captains-log 'captains-log :gesture :select))
  (setf *focused-log* captains-log))

(in-package #:mm)

(defun captains-log-install ()
  (push (i 'habit
	   :name "Captains Log"
	   :initialization-function 'new-captains-log
	   :visualization-function 'mmg::visualize-captains-log
	   :occurrence :daily)
	*habits*))

;;; dashboard
;;; ============================================================================
;;; TODO
;;; - take and review notes at the end of day/week/month/years
;;; - visual bugs are due to the sutmpwm mode line being present (or not). this
;;;   can be fixed by toggling the modeline on startup

(in-package #:mm)

(defvar systems-information nil)
;; (defvar test-system-info (car (read-file #P"~/.masamune/trash-files/test-system-info")))
;; (defvar test-algol-system-stats (car (read-file #P"~/.masamune/trash-files/test-algol-system-stats")))

(define-condition choice ()
  ((query :accessor query :initarg :query))
  (:report (lambda (condition stream) (format stream (query condition))))
  (:documentation "the query slot is passed to `format' directly which currently receives no arguments"))

(defun parse-sloccount-output (sloccount-output)
  (let* ((s1 (subseq sloccount-output 
		     (search "(dominant language first)" sloccount-output) 
		     (search "Total Physical" sloccount-output))))
    (loop for line in (rest (split "\\n" s1))
	  collect (mapcar (lambda (s) (regex-replace ":" s ""))
			  (take 2 (remove "" (split " " line) :test #'string=))))))

;; (defun system-information (pathname &key override-name override-pathname)
;;   "Handles both ALGOL and lisp systems"
;;   (list :name (or override-name (name pathname))
;; 	:system-information (handler-case (parse-sloccount-output (rp (format nil "sloccount ~a" (or override-pathname pathname))))
;; 			      (error nil nil))
;; 	:pathname (or override-pathname pathname)))

;; (defun algol-system-stats ()
;;   ;; TODO 2015-02-06T13:05:27+00:00 Gabriel Laddel
;;   ;; incomplete, ignores several compression types
;;   "there are .xz compressed files, but so far they they've only compressed
;; single files with the xz program, and we ignore them. We also ignore a few
;; strings such as \"diff\" which are (probably) files"
;;   (let* ((pdists-pathname #P"/usr/portage/distfiles/")
;; 	 (pname-types '(("tbz2" "bunzip2 -f -c * | tar xvf -") 
;; 			;; ("bz2"  "bunzip2 -f -c * | tar xvf -")
;; 			("gz"   "tar zxvf")		   
;; 			("tgz"  "tar zxvf")		   
;; 			("lzma" "tar --lzma -xvf")	   
;; 			;; ("tar.xz"   "unxz -c * | tar xvf -")
;; 			;; ("tar"  "tar zxvf")
;; 			("zip"  nil)))
;; 	 (archives) (old-contents))
;;     (labels ((algol-archive? (pname) (some (lambda (type) (and (not (ssearch "patch" (namestring pname)))
;; 							  (not (ssearch "diff" (namestring pname)))
;; 							  (not (ssearch "pcf" (namestring pname)))
;; 							  (pathname-type= pname type)))
;; 					   (mapcar 'car pname-types))))
;;       (dolist (p (remove-if #'algol-archive? (ls pdists-pathname)))
;;       	(when (directory-pathname-p p)
;;       	  ;; archives are not directories, thus we don't have to worry about
;;       	  ;; deleting them.
;;       	  (sb-ext:delete-directory p :recursive t)))
;;       ;; setup state machine used to detect new directories
;;       (setf old-contents (ls pdists-pathname) archives (filter #'algol-archive? old-contents))
;;       (loop for pname in archives
;;       	    collect (loop for (type sh) in pname-types
;;       			  when (pathname-type= pname type)
;;       			    do (return (if (pathname-type= pname "zip")
;;       					   (let* ((new-dir (alter-pathname-type pname "")))
;;       					     ;; XXX 2015-02-05T04:55:01+00:00 Gabriel Laddel
;;       					     ;; unzip doesn't come with an option to
;;       					     ;; unzip into a folder with the same name
;;       					     ;; as the archive so we special case it
;;       					     (unless (probe-file new-dir) (mkdir new-dir))
;;       					     (rp (format nil "cd ~a && unzip ~a -d ~a" pdists-pathname pname new-dir))
;;       					     (system-information pname :override-pathname new-dir :override-name pname))
;;       					   (progn (rp (format nil "cd ~a && ~a ~a" pdists-pathname sh pname))
;;       						  (let* ((new-dir (car (set-difference (ls pdists-pathname) old-contents :test 'equal))))
;;       						    (progn (setf old-contents (ls pdists-pathname))
;;       							   (system-information pname :override-pathname new-dir :override-name pname)))))))))))

;; (defun calculate-operating-system-stats ()
;;   (list :local-systems (loop for pathname in (ls (qlpp)) collect (system-information pathname))
;; 	:external-systems (loop for pathname in (ls (ls "/root/quicklisp/dists/quicklisp/software")) 
;; 				collect (system-information pathname))
;; 	:algol-systems (algol-system-stats)))

(defun systems-stats-string ()
  "system stats is currently broken!"
  ;; (labels ((f (s) (apply #'+ (mapcar (lambda (o) (parse-integer o :radix 10))
  ;; 				     (flatten (mapcar (lambda (l) (mapcar #'second (second l))) 
  ;; 						      s)))) ))
  ;;   (let* ((total-local-lisp-system-loc (f (getf test-system-info :local-systems)))
  ;; 	   (total-external-lisp-system-loc (f (getf test-system-info :external-systems)))
  ;; 	   (external-lisp-system-count (length (mapcar #'car (getf test-system-info :external-systems))))
  ;; 	   (local-lisp-system-count (length (mapcar #'car (getf test-system-info :local-systems))))
  ;; 	   (s (make-string-output-stream))
  ;; 	   (algol-loc (->> test-algol-system-stats
  ;; 			   (mapcar (lambda (pl) (car (cdaadr (vals pl)))))
  ;; 			   (remove-if #'null)
  ;; 			   (mapcar #'read-from-string)
  ;; 			   (apply #'+)))
  ;; 	   (lisp-loc (+ total-external-lisp-system-loc total-local-lisp-system-loc))
  ;; 	   (total-loc (+ lisp-loc algol-loc)))
  ;;     (format s "    in total ~:d known lines of code running on the operating system, across ~:d lisp systems~%"
  ;; 	      (+ total-external-lisp-system-loc total-local-lisp-system-loc)
  ;; 	      (+ external-lisp-system-count local-lisp-system-count))
  ;;     (format s "    ~:d local lisp systems with a total of ~:d lines of code~%"
  ;; 	      local-lisp-system-count total-local-lisp-system-loc)
  ;;     (format s "    ~:d external lisp systems with a total of ~:d lines of code~%" 
  ;; 	      external-lisp-system-count total-external-lisp-system-loc)
  ;;     (format s "    in total ~:d lines of ALGOL running across ~:d systems~%"
  ;; 	      algol-loc
  ;; 	      (length test-algol-system-stats))
  ;;     (terpri s)
  ;;     (format s "    TODO: this count is incomplete and doesn't take into account local algol systems or various types of compression the kernel, gcc etc.")
  ;;     (terpri s)
  ;;     (format s "    TODO: name the number of documented and undocumented systems, record when the systems were installed, give ways to uninstall them")
  ;;     (get-output-stream-string s)))
  )

(in-package #:mmg)

(defvar *focused-habit* nil)
(defvar *dashboard* nil)

(define-application-frame dashboard ()
  ;; ((habit-pane) (calendar-pane) (visualization-pane) (interaction-pane))
  ()
  (:pointer-documentation t)
  (:menu-bar t)
  (:panes (interaction-pane :interactor)
	  ;; XXX 2014-11-08T00:26:47-08:00 Gabriel Laddel
	  ;; if scrollbars are not used on these panes they'll get resized if anything
	  ;; grows larger than it's current boundries jto make room for its graphics.
	  (habit-pane :application 
		      :scroll-bars :vertical
		      :display-function 'render-habits)
	  (system-pane :application
		       :scroll-bars :vertical
		       :display-function 'render-systems)
	  (calendar-pane :application 
			 :scroll-bars :vertical
			 :display-function 'render-calendar)
	  (visualization-pane :application
			      :scroll-bars nil
			      :display-function 'render-visualization))
  (:layouts (default (vertically () 
		       (6/7 (horizontally () 
			      (1/4 (vertically () habit-pane system-pane calendar-pane))
			      (3/4 visualization-pane)))
		       (1/7 interaction-pane)))
	    (fullscreen (horizontally () 
			  (1/4 (vertically () habit-pane system-pane calendar-pane))
			  (3/4 visualization-pane)))))

(define-presentation-type habit ())

;;; commands

(define-dashboard-command (com-init-habit :name t)
  ((habit 'habit))
  (aif (mm::initialization-function habit) (funcall it habit)
    (format (interaction-pane) "it doth not exist!")))

(define-dashboard-command (com-also-focus-habit)
    ((habit 'habit :gesture :select))
  (setf *focused-habit* habit)
  (render-visualization *dashboard* (find-pane-named *dashboard* 'visualization-pane)))

(define-dashboard-command (com-focus :name "Focus Habit") 
    ((habit 'habit :gesture :select))
  (setf *focused-habit* habit)
  (render-visualization *dashboard* (find-pane-named *dashboard* 'visualization-pane)))

(define-dashboard-command (com-run-habit :name "Run habit") 
    ((habit 'habit :gesture :select))
  (funcall (mm::initialization-function habit) habit))

(define-dashboard-command (com-next-habit :name t :keystroke (#\n :control)) ()
  (labels ((p () (position *focused-habit* mm::*habits* :test #'eq)))
    (setf *focused-habit*
	  (if (or (null *focused-habit*) (= (- (length mm::*habits*) 1) (p)))
	      (car mm::*habits*)
	      (nth (1+ (p)) mm::*habits*)))))

;;; TODO 2015-04-16T04:27:25+00:00 Gabriel Laddel
;;; wtf how does one assign multiple keystrokes to a single command? 
;;;
;;; http://bauhh.dyndns.org:8000/clim-spec/27-4.html#_1424
;;; 
;;; ^ would seem to imply that you can't - that
;;; `add-keystroke-to-command-table?' removes the previous keystroke?
;;; 
;; (add-command-to-command-table 'com-next-habit (find-command-table 'dashboard) :keystroke :down)
;; (add-keystroke-to-command-table (find-command-table 'dashboard) (:down) :command )
;;
;; The other way to go about this is to build a modified command reader, as in
;; the captain-mcclim.lisp file, but that seems wrong.

(define-dashboard-command (com-display-overview :name t :keystroke (#\o :control)) ()
  (setf *focused-habit* nil)
  (render-overview *dashboard* (find-pane-named *dashboard* 'visualization-pane)))

(define-dashboard-command (com-quit :name "Quit" :menu t) ()
  (frame-exit *application-frame*))

(define-dashboard-command (com-init-focused-habit :name t :keystroke (#\i :control) :menu t) ()
  (if *focused-habit* (funcall (mm::initialization-function *focused-habit*) *focused-habit*)
    (format (interaction-pane) "no habit currently focused!")))

(define-dashboard-command (com-previous-habit :name t :keystroke (#\p :control)) ()
  (labels ((p () (position *focused-habit* mm::*habits* :test #'eq)))
    (setf *focused-habit*
	  (if (or (null *focused-habit*) (= 0 (p)))
	      (llast mm::*habits*)
	      (nth (- (p) 1) mm::*habits*)))))

(define-dashboard-command (com-view-last-note :name t :keystroke (#\m :control) :menu t) ()
  (cond ((null *focused-habit*) (format *query-io* "no habit currently focused"))
	((null (mm::latest-note *focused-habit*)) (format *query-io* "this habit doesn't have any notes"))
	((mm::latest-note *focused-habit*) (message (mm::latest-note *focused-habit*)))))

(define-dashboard-command (com-record-note :name t :keystroke (#\m :meta) :menu t) ()
  (if (null *focused-habit*)
      (format *query-io* "no habit focused, cannot record a note")
      (progn (mm::record-note *focused-habit* (accept 'string :prompt "Record a note"))
	     (format *query-io* "note recorded sucessfully"))))

(define-dashboard-command (com-kill-dashboard :name t :keystroke (#\k :control) :menu t) ()
  (when (mm::state-record-exists? :pre-dashboard)
    (mm::restore-state :pre-dashboard))
  (stumpwm:kill-window (stumpwm::window-by-name "dashboard")))

(defun render-overview (frame pane)
  ;; TODO 2015-02-08T05:41:59+00:00 Gabriel Laddel
  ;; - time spent in programs for day, month, year, sleep
  ;; - loc added, removed
  ;; - todo, fixme etc.
  ;; - systems monitored
  ;; - concepts learned
  ;; - exceptions per hour programming
  (declare (ignore frame))
  (let* ((*print-pretty* nil)
	 (tau (* 2 pi)))
    (terpri pane)
    (format pane "    Agenda:~% ~{~%    TODO: ~a~}~%~%" (mapcar #'mm::title mm::*agenda*))
    (format pane "    Current System Information:~%")
    (format pane "~{~%~a~}" (split "\\n" (mm::systems-stats-string)))
    (format pane "~%~%~A" (rp "df -h"))
    (clim:draw-circle* pane 1200 200 173 :filled t :ink clim:+black+)
    (clim:draw-circle* pane 1200 200 170 :filled t :ink clim:+black+ :start-angle 0 :end-angle (* 0.7 tau))
    (clim:draw-circle* pane 1200 200 170 :filled t :ink clim:+white+ :start-angle (* 0.7 tau) :end-angle tau)
    (draw-rectangle* pane 0 (+ 2 vph) vpw (- vph 140) :ink +PeachPuff4+)))

(defun render-habits (frame pane)
  (declare (ignore frame))
  (destructuring-bind (x1 y1 x2 y2) 
      (coerce (slot-value (sheet-region pane) 'clim-internals::coordinates) 'list)
    (let* ((center-x (/ (- x2 x1) 2))
	   (center-y (/ (- y2 y1) 2))
	   (y-spacing 20))
      (labels ((draw-habit-text (h y) (draw-text* pane (mm::name h) center-x (* y-spacing y) 
						  :ink (if (eq *focused-habit* h) +blue+ +black+)
						  :align-x :center
						  :align-y :center
						  :text-size 20)))
	(if mm::*habits*
	    (loop with font-spacing-multiplier = 5
		  for habit in mm::*habits*
		  for y = 1 then (1+ y)
		  for habit-finished = (not (mm::occurs-now? habit))
		  for strikethrough-y = (* y-spacing y)
		  for x  = (- center-x (* font-spacing-multiplier (length (mm:name habit))))
		  for x1 = (+ center-x (* font-spacing-multiplier (length (mm:name habit))))
		  do (with-output-as-presentation (pane habit 'habit)
		       (if habit-finished
			   (progn (draw-habit-text habit y)
				  (draw-line* pane x strikethrough-y x1 strikethrough-y :line-thickness 3)) 
			   (draw-habit-text habit y))))
	    (draw-text* pane "No Habits Installed" center-x center-y
			:align-x :center :align-y :center))))))

(defun render-systems (frame pane)
  ;; TODO 2014-11-13T19:43:18-08:00 Gabriel Laddel
  ;; - issue tracking system that integrates with github, org mode style thing.
  ;; - # of docstrings, functions, marcos and variables
  (declare (ignore frame))
  (destructuring-bind (x1 y1 x2 y2) 
      (coerce (slot-value (sheet-region pane) 'clim-internals::coordinates) 'list)
    (let* ((center-x (/ (- x2 x1) 2)) (center-y (/ (- y2 y1) 2)))
      (draw-text* pane "Masamune doesn't know about any systems. Add one?" center-x center-y
		  :ink +black+ :align-x :center :align-y :center :text-size 20)))
  ;; (if mm::*system-information*
  ;;     (format pane "~%~{~:@{   System: ~@(~a~), ~@:{~%    ~@(~a~) LoC:  ~d ~}~%   #TODOs: #NOTEs: #XXXs: #FIXMEs:~%~%~}~}" mm::*system-information*)
  ;;     (destructuring-bind (x1 y1 x2 y2) 
  ;; 	  (coerce (slot-value (sheet-region pane) 'clim-internals::coordinates) 'list)
  ;; 	(let* ((center-x (/ (- x2 x1) 2)) (center-y (/ (- y2 y1) 2)))
  ;; 	  (draw-text* pane "Masamune doesn't know about any systems. Add one?" center-x center-y
  ;; 		      :ink +black+ :align-x :center :align-y :center :text-size 20))))
  )

;;; calendar

(defun month-length (month year)
  (let* ((month-lengths #(31 28 31 30 31 30 31 31 30 31 30 31)))
    (cond ((/= month 2) (aref month-lengths (- month 1)))
	  ((null (zerop (mod year 4))) 28)
	  ((null (zerop (mod year 400))) 29) (t 28))))

(defun calendar-month (month year &key (stream *standard-output*))
  (let* ((day-of-week-string (make-array 7 :initial-contents (list "Sun" "Mon" "Tue" "Wed" "Thu" "Fri" "Sat"))))
    (labels ((today? (date) t))
      (let* ((days-in-month (month-length month year)))
	(multiple-value-bind (n1 n2 n3 n4 n5 n6 start-day)
	    (decode-universal-time (encode-universal-time 0 0 0 1 month year))
	  (setq start-day (mod (+ start-day 1) 7))
	  (formatting-table (stream :move-cursor t)
	    (formatting-row (stream)
	      (dotimes (d 7)
		(formatting-cell (stream :align-x :center)
		  (write-string (aref day-of-week-string (mod d 7)) stream))))
	    (do ((date 1)
		 (first-week t nil))
		((> date days-in-month))
	      (formatting-row (stream)
		(dotimes (d 7)
		  (formatting-cell (stream :align-x :right)
		    (when (and (<= date days-in-month)
			       (or (not first-week) (>= d start-day)))
		      (format stream "~d" date)
		      (incf date))))))))))))
  
(defun render-calendar (frame pane)
  (declare (ignore frame))  
  (calendar-month 6 12 :stream pane))

(defun render-visualization (frame pane)
  (declare (ignore frame))
  (if (and *focused-habit* (mm::visualization-function *focused-habit*))
      (funcall (mm::visualization-function *focused-habit*) *focused-habit* pane)
      (render-overview *dashboard* (find-pane-named *dashboard* 'visualization-pane))))

(defun run-dashboard ()  
  (when mm::*habits* (setq *focused-habit* (or *focused-habit* (car mm::*habits*))))
  (setf *dashboard* (make-application-frame 'dashboard))
  (run-frame-top-level *dashboard* :name "Dashboard"))

(defun run-or-focus-dashboard ()
  (mm::record-state :pre-dashboard)
  (stumpwm::select-emacs)
  (stumpwm::fullscreen-emacs)
  (aif (stumpwm::window-by-name "dashboard")
       (stumpwm::select-window (stumpwm::window-name it))
       (bt:make-thread (lambda () (run-dashboard)) :name "dashboard")))
 
(defun habit-by-name (name)
  "case insensitive"
  (some (lambda (h) (when (string= (string-downcase name) (string-downcase  (mm::name h))) h))
	mm::*habits*))

(in-package #:mm)

;;; Programming Practice 
;;; ============================================================================
;;;
;;; books
;;; =====
;;; introduction to algorithms
;;; algorithm design
;;; art of computer programming
;;; hackers delight
;;; Algorithms, Robert Sedgewick
;;; The Algorithm Design Manual, Steven S Skiena
;;; 
;;; websites 
;;; ========
;;; http://www.gibiansky.com/
;;; pvk.ca
;;; http://blog.sigfpe.com/
;;; http://www.coranac.com/
;;;
;;; data structures
;;; ===============
;;; - finger trees
;;; - red black trees
;;; - AVL trees
;;; 
;;; Project Euler
;;; ============================================================================
;;; 
;;; TODO 2015-01-07T01:32:44+00:00 Gabriel Laddel
;;; * check for new problems periodically.
;;; * I was hacking out a project euler problem system ontop of CLIM and ran
;;;   into some issues with `create-project-euler-scaffolding' below. Most of
;;;   the code works, I just can't be bothered the fix it atm

;; (defvar euler-dir #P"~/.masamune/habits/project-euler/")
;; (defvar solutions-file (merge-pathnames euler-dir "solutions.lisp"))
;; (defvar focused-euler-problem nil)
(defparameter programming-practice-minutes 60)

;; (c euler-problem (pgraphic) (prompt links solution problem-number)
;;    (:metaclass manardb::mm-metaclass))

;; (defun sorted-euler-problems ()
;;   (awhen (manardb::retrieve-all-instances 'euler-problem)
;;     (sort (filter (lambda (o) (with-slots (prompt links solution problem-number) o
;; 			   (not (every 'null (list prompt links solution problem-number)))))
;; 		  it)
;; 	  (lambda (p1 p2) (< (problem-number p1) (problem-number p2))))))

;; (defun euler-problem (problem-number)
;;   (find-if (lambda (k) (= problem-number (problem-number k))) (sorted-euler-problems)))

;; (defun next-logical-unsolved-problem ()
;;   (loop for p in (sorted-euler-problems) unless (solved? p) return p))

;; (defmethod solution? ((euler-problem euler-problem) submission)
;;   (string= (solution euler-problem)
;; 	   (car (split " " (rp (format nil "echo -n '~s' | md5sum" submission))))))

;; (defmethod solved? ((euler-problem euler-problem))
;;   (let* ((fiasco:*PRINT-TEST-RUN-PROGRESS* nil)
;; 	 (test-name (format-symbol 'euler "problem-~d" (problem-number euler-problem))))
;;     (fiasco:without-debugging (funcall test-name))))

;; (defmethod render ((euler-problem euler-problem) stream)
;;   (labels ((link-type (link-string)
;; 	     (cond ((ssearch "problem=" link-string) :euler-problem-link)
;; 		   ((ssearch "about=" link-string)   :about)
;; 		   ((ssearch "http://" link-string)  :url)
;; 		   ((equal "txt" (pathname-type link-string)) :txt)
;; 		   ((string= "CabriJava.class" link-string) nil)
;; 		   (t :image)))

;; 	   (render-link (link-string stream) 
;; 	     (case (link-type link-string)
;; 	       (:about (format stream "~A" link-string))
;; 	       (:euler-problem-link (format stream "~A" link-string))
;; 	       (:image (merge-pathnames euler-dir link-string))
;; 	       (:url (multiple-value-bind (x y)
;; 			 (clim::stream-cursor-position stream)
;; 		       (mmg::draw-url link-string x y stream)))
;; 	       (:txt (format stream "text file: ~A" link-string))
;; 	       (t (format stream "included link: ~A" link-string)))))

;;     (clim:window-clear stream)
;;     (with-slots (prompt links problem-number) euler-problem
;;       (format stream "Problem Number~A~%~%~A" problem-number prompt)
;;       (loop for link-string in links
;; 	    do (clim::with-room-for-graphics (stream)
;; 		 (render-link link-string stream))))))

;; (defmethod problem-test-skeleton ((euler-problem euler-problem))
;;   `(deftest ,(%format-symbol t "problem-~d" (problem-number euler-problem))
;;        (is (solution? ,(problem-number euler-problem) nil))))

;; (defun create-project-euler-scaffolding ()
;;   "creates all files and problems for project euler"
;;   (let* ((tmp-dir "/tmp/project-euler/") 
;; 	 (zipfile (merge-pathnames tmp-dir "project-euler-extra-files.zip"))
;; 	 (textfile (merge-pathnames tmp-dir "problems.txt"))	 
;; 	 (*print-case* :downcase)
;; 	 (*print-level* nil)
;; 	 (*print-length* nil))
;;     (mkdir tmp-dir)
;;     (download-url "http://kmkeen.com/local-euler/project_euler.zip" zipfile)
;;     (download-url "http://kmkeen.com/local-euler/project_euler.txt" textfile)
;;     ;; XXX 2015-01-12T13:56:03+00:00 Gabriel Laddel
;;     ;; this exits with an error code. Why? nfi.
;;     (ignore-errors (rp (format nil "unzip ~a -d ~a" zipfile (merge-pathnames euler-dir "/resources"))))
;;     (loop with problem-strings = (->> textfile
;; 				      (slurp-file)
;; 				      (split "Problem \\d*\\n=========[=]*")
;; 				      (rest))
;; 	  for problem-string in problem-strings
;; 	  for i = 1 then (1+ i)
;; 	  do (let* ((lpos (ssearch "Visible links" problem-string t))
;; 		    (apos (ssearch "Answer: " problem-string t))
;; 		    (links (when lpos 
;; 			     (->> (subseq problem-string lpos apos)
;; 				  (split "\\n")
;; 				  (rest)
;; 				  (mapcar (lambda (k) (drop 3 (trim-dwim k))))
;; 				  (remove-if #'null)))))
;; 	       (destructuring-bind (prompt answer)
;; 		   (mapcar 'trim-dwim (split "Answer: " problem-string))
;; 		 (make-instance 'euler-problem
;; 				:problem-number i
;; 				:prompt prompt
;; 				:solution answer 
;; 				:links links
;; 				:x 0 :y 0))))
;;     ;; all problems are in manardb at this point
;;     (with-open-file (s solutions-file
;; 		       :direction :output
;; 		       :if-does-not-exist :create
;; 		       :if-exists :error)
;;       (labels ((f (sexp) (write sexp :stream s) (terpri s) (terpri s)))
;; 	(f '(fiasco:define-test-package #:euler (:use #:mm #:fiasco)))
;; 	(f '(in-package #:euler))
;; 	(loop for problem in (sorted-euler-problems)
;; 	      do (with-slots (problem-number) problem
;; 		   (f (problem-test-skeleton problem))))))))

;;; Quiz GUI modifications 
;;; ============================================================================

(in-package #:mmg)

;; (define-command-table project-euler-commands)

;; (define-quiz-command (com-next-euler-problem
;; 		      :name "Next Problem"
;; 		      :command-table project-euler-commands
;; 		      :keystroke (#\f :control)) ()
;;   (aif (mm::euler-problem (1+ (mm::problem-number mm::focused-euler-problem)))
;;        (mm::render (setf mm::focused-euler-problem it) *standard-output*)
;;        (format *query-io* "No more problems")))

;; (define-quiz-command (com-previous-euler-problem
;; 		      :command-table project-euler-commands
;; 		      :name "Previous Problem" :keystroke (#\f :control)) ()
;;   (aif (mm::euler-problem (1- (mm::problem-number mm::focused-euler-problem)))
;;        (mm::render (setf mm::focused-euler-problem it) *standard-output*)
;;        (format *query-io* "No prior problems")))

;; (define-quiz-command (com-nth-euler-problem
;; 		      :command-table project-euler-commands
;; 		      :name "Nth problem" :keystroke (#\n :control)) ()
;;   (aif (mm::euler-problem (accept 'integer :prompt "problem number"))
;;        (mm::render (setf mm::focused-euler-problem it) *standard-output*)
;;        (format *query-io* "failed")))

;;; Habit Interface
;;; ============================================================================

(in-package #:mm) 

;; (defun start-project-euler ()
;;   (if (stumpwm::something-fullscreen?)
;;       (labels ((no-solutions () 
;; 		 (with-getfs (:number-of-tests-run :number-of-failures)
;; 		   (fiasco:extract-test-run-statistics (fiasco:run-suite-tests 'euler))
;; 		   (= number-of-failures number-of-tests-run))))
;; 	(mmg::run-or-focus-quiz)
;; 	(render (euler-problem focused-euler-problem)
;; 		(mmg::find-pane-named mmg::*quiz* 'mmg::display-pane))
;; 	(stumpwm::hsplit)
;; 	(stumpwm::fnext)
;; 	(stumpwm::emacs)
;; 	(ignore-errors 
;; 	 (swank::eval-in-emacs
;; 	  '(progn 
;; 	    (window-configuration-to-register :programming-practice)
;; 	    (find-file "~/.masamune/habits/project-euler/solutions.lisp")
;; 	    (delete-other-windows)
;; 	    nil)))
;; 	(when (no-solutions) (message "")))
;;       (message "The dashboard should be fullscreen before running this program")))

(defun start-programming-practice (habit)
  (record-event habit (event :started))
  (if (mm::state-record-exists? :programming-practice)
      (mm::restore-state :programming-practice)
      (progn (stumpwm::emacs)
	     (ignore-errors
	      ;; XXX 2014-12-23T14:16:06+00:00 Gabriel Laddel
	      ;; always throws
	      (with-live-swank-connection 
		  (swank::eval-in-emacs
		   '(progn
		     (find-file "~/quicklisp/local-projects/masamune/systems/programming-practice.lisp")
		     (find-file "~/documents/writing/programming/programming-notes.org")
		     (delete-other-windows)))))))
  (stumpwm::run-with-timer
   (* programming-practice-minutes 60) nil 
   (lambda ()
     (mm::record-state :programming-practice)
     (with-live-swank-connection
	 (stumpwm::message-no-timeout "Time is almost up")
       (loop for i from 10 downto 0
	     finally (progn (record-event habit (event :finished))
			    (stumpwm::message-no-timeout "finished")
			    (mmg::run-or-focus-dashboard)))))))

(defun visualize-programming-practice (habit sheet)
  (declare (ignore habit))
  (mmg::draw-image (mmg::make-image (qlpp "masamune/images/JohnvonNeumann-LosAlamos.gif")) 400 100 sheet)
  (setf (clim::stream-cursor-position sheet) (values 460 800))
  (format sheet "\"You don't have to be responsible for the world that you're in\""))

(defun programming-practice-install ()
  (push (i 'habit
	   :name "Programming Practice"
	   :initialization-function 'start-programming-practice
	   :visualization-function 'visualize-programming-practice
	   :occurrence :daily)
	*habits*))

;; (handler-bind
;;     ((error #'(lambda (c) (invoke-restart 'ASDF/ACTION:ACCEPT))))
;;   (ql:quickload 'masamune))
;; 
;; (unless (probe-file euler-dir) (mkdir euler-dir))
;; (unless (probe-file euler-dir) (mkdir (merge-pathnames euler-dir "/resources")))
;; (unless (probe-file (merge-pathnames euler-dir "solutions.lisp"))
;;   (create-project-euler-scaffolding))
;; (compile-file solutions-file)
;; (setf focused-euler-problem (next-logical-unsolved-problem))
;; ;;; XXX 2015-01-12T13:58:17+00:00 Gabriel Laddel
;; ;;; this forces manardb to put required pclasses in memory
;; (make-instance 'mm::euler-problem)

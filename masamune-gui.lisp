(in-package #:mmg)

(defparameter *min-x* -7)
(defparameter *max-x* 7)
(defparameter *min-y* -7)
(defparameter *max-y* 7)
(defparameter *graph-size* 600)
(defparameter *graph-width* nil)
(defparameter *graph-height* nil)
(defparameter *graph-ink* +black+)
(defparameter lpw (float (* mm::screen-width .25)) "[l]eft [p]ane [w]idth")
(defparameter lph (float (/ (* mm::screen-height 6/7) 3)) "[l]eft [p]ane [h]eight")
(defparameter vpw (float(* mm::screen-width .75)) "[v]isualization [p]ane [w]idth")
(defparameter vph (float (* mm::screen-height 6/7)) "[v]isualization [p]ane [h]eight")

(defun draw-thin-bar-graph-1 (medium function scale min max dx)
  (loop for i from 0 below (floor (- max min) dx)
        for x = min then (+ x dx)
        do (draw-line* medium i 0 i (* scale (funcall function x)))))

(defun draw-vector-bar-graph
    (vector &key (stream *standard-output*) (scale-y 1) (ink +black+)
	      (key 'identity) (start 0) (end nil))
  (let ((range (- (reduce 'max vector :start start :end end :key key)
                  0 #+NIL (reduce 'min vector :start start :end end :key key)))) ; totally wrong

    (with-room-for-graphics (stream :first-quadrant t)
      (with-new-output-record (stream)
        (with-drawing-options (stream :ink ink)
          (unless (zerop range)
            (when (eql t scale-y)
              (setf scale-y (/ 250 range)))
            (draw-thin-bar-graph-1 
             stream 
             (lambda (i) (funcall key (aref vector i)))
             scale-y start (or end (length vector)) 1)))))))

(defun draw-function-filled-graph 
    (function &key (stream *standard-output*)
     (min-x *min-x*) (max-x *max-x*)
     (min-y *min-y*) (max-y *max-y*)
     size
     (width  (or size *graph-width* *graph-size*))
     (height (or size *graph-height* *graph-size*))
     (ink *graph-ink*))
  (with-room-for-graphics (stream :first-quadrant t)
    (with-new-output-record (stream)
      (with-drawing-options (stream :ink ink)
        (draw-thin-bar-graph-1 stream function
                               (float (/ height (- max-y min-y)) 0.0f0)
                               min-x max-x
                               (/ (- max-x min-x) width))))))

(defun interaction-pane ()
  ;; TODO 2015-01-06T02:31:59+00:00 Gabriel Laddel
  ;; remove me
  (find-pane-named *application-frame* 'interaction-pane))

(defun output-record-of (object)
  ;; (find-if (lambda (o) (and (presentationp o) (eq object (presentation-object o))))
  ;; 	   (output-record-children (stream-current-output-record *standard-output*)))
  (mapcar #'presentation-object 
	  (filter (lambda (o) (and (presentationp o) (presentation-object o)))
		  (output-record-children (stream-current-output-record *standard-output*)))))

;;; output history

;;; quiz application 
;;; ============================================================================

(defvar *quiz* nil)

(define-command-table application-commands)
(define-command-table quiz)

(define-quiz-command (com-clear-output :name "Clear Output History"
				       :command-table application-commands
				       :menu t
				       :provide-output-destination-keyword nil) ()
  (window-clear *standard-output*))

(define-quiz-command (com-exit :name "Quit"
			       :command-table application-commands
			       :menu t
			       :provide-output-destination-keyword nil) ()
  (frame-exit *application-frame*))

(define-application-frame quiz ()
  ((display-pane) (interaction-panne))
  (:pointer-documentation t)
  (:menu-bar t)
  (:command-table (quiz :inherit-from (application-commands)
			:menu (("Quiz"          :menu application-commands)
			       ("Project Euler" :menu project-euler-commands))))
  (:panes (interaction-pane :interactor)
	  (display-pane :application :scroll-bars t))
  (:layouts (default (vertically () (7/8 display-pane) (1/8 interaction-pane)))))

(defun run-or-focus-quiz ()  
  (if (some (lambda (w) (string= "Quiz" (stumpwm::window-name w))) 
	    (stumpwm::all-windows))
      (stumpwm::select-window "Quiz")
      (bt:make-thread (lambda () 
			(setf *quiz* (make-application-frame 'quiz))
			(run-frame-top-level *quiz* :name "Quiz"))
		      :name "quiz")))
;;; URLs

(define-presentation-type url ())

(define-quiz-command (com-browse-url) 
    ((url 'url :gesture :select))
  (mmb::open-uri url t))

(defun draw-url (url x y &optional (stream mm::*hack*))
  (multiple-value-bind (old-x old-y) (stream-cursor-position stream)
    (setf (stream-cursor-position stream) (values x y))
    (with-output-as-presentation (stream url 'url)
      (format stream url))
    (setf (stream-cursor-position stream) (values old-x old-y))))

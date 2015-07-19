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

;;; Textbook
;;; ============================================================================

(defvar *quiz* nil)

;; (mm::c question () (name description prompt concept))
;; (mm::c section () (name description prompt concept))
;; (make-instance 'question :concept "determinant" :prompt ')
;; (find-if (lambda (pane) (eq 'application-PANE (type-of pane))) (slot-value *quiz* 'clim-internals::named-panes))

(define-command-table application-commands)
(define-command-table quiz)

(define-application-frame quiz ()
  ((current-section))
  (:pointer-documentation nil)
  (:menu-bar t)
  (:command-table (quiz :inherit-from (application-commands)
			:menu (("Problems"          :menu application-commands)
			       ;; ("Project Euler" :menu project-euler-commands)
			       )))
  (:panes (interaction-pane :interactor)
	  (editor (make-pane 'text-editor :value "test text editor"))
	  (submit (make-pane 'push-button :label "Submit"))
	  (solution (make-pane 'push-button :label "Show Solution"))
	  (back (make-pane 'push-button :label "Back"))
	  (forwards (make-pane 'push-button :label "Forwards"))
	  (display-pane :application
			:scroll-bars t
			:display-function 'render-question))
  (:layouts (default (vertically () 
		       (5/8 display-pane)
		       (2/8 (horizontally () 
			      editor
			      (labelling (:label "")
				(vertically ()
				  submit solution back forwards))))
		       ;; (1/8 interaction-pane)
		       ))))

(defmacro with-centers (sheet &rest body)
  "introduces the bindings `center-x', `center-y'"
  `(destructuring-bind (x0 y0 x1 y1)
       (LOOP FOR I ACROSS (SLOT-VALUE (SLOT-VALUE ,sheet 'REGION) 'CLIM-INTERNALS::COORDINATES)
	     COLLECT I)
     (let* ((center-x (/ (- x1 x0) 2))
	    (center-y (/ (- y1 y0) 2)))
       ,@body)))

(defun render-question (frame pane)
  (declare (ignore frame))
  (mmg::with-centers pane
    (draw-text* pane "Currently nothing to render" center-x center-y :align-x :center)))

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

;; (define-quiz-command (com-show-solution))
;; (define-quiz-command (com-next-section))
;; (define-quiz-command (com-previous-section))

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

(defun draw-url (display-string url x y &optional (stream *standard-output*))
  (with-output-as-presentation (stream url 'url)
    (draw-text* stream display-string x y)))

(defmethod deactivate-gadget ((gadget RADIO-BOX-PANE))
  (dolist (c (slot-value gadget 'climi::children))
    (climi::deactivate-gadget c))
  (climi::repaint-sheet gadget +everywhere+))

(defmethod activate-gadget ((gadget RADIO-BOX-PANE))
  (dolist (c (slot-value gadget 'climi::children))
    (climi::activate-gadget c))
  (climi::repaint-sheet gadget +everywhere+))

(defmethod activate-gadget :after ((gadget toggle-button-pane))
  (climi::repaint-sheet gadget +everywhere+))

(defmethod deactivate-gadget :after ((gadget toggle-button-pane))
  (climi::repaint-sheet gadget +everywhere+))


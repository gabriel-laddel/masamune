;;; This 3D plotting code originally created by Michael Filonenko
;;; http://filonenko-mikhail.blogspot.com
;;;
;;; Eventually lisp will have first class 3D graphics (again). Until that happens
;;; we're stuck fighting OpenGL.
;;;
;;; If you'd like to build a better 3D lisp environment I recommend you talk to
;;; Cbaggers and |3b| (look them up on github and see their blogs). Also, check
;;; out the reverse-engineered OpenGL drivers.
;;;
;;; A neat visualization to copy
;;; https://www.youtube.com/watch?v=2yRFmHEYptA
;;; 
;;; Demo
;;; ============================================================================
;;; 
;;; (3d:draw-plot)

(defpackage :3d
  (:use #:cl)
  (:export #:draw-plot #:while))

(in-package #:3d)

(defvar *min-zoom* 0.1)
(defvar *zoom-step* 0.1)
(defvar *rotate-multiplicator* 0.3)
(defvar *viewport-multiplicator* 10)

(defclass plotwindow (glop:window)
  ((1st-pressed :initform nil :accessor 1st-pressed)
   (zoom :initform 1 :accessor zoom)
   (xangle :initform 0 :accessor xangle)
   (yangle :initform 0 :accessor yangle)))

(defmethod glop:on-event ((window plotwindow) (event glop:key-event))
  (when (eq (glop:keysym event) :escape)
    (glop:push-close-event window))
  (when (and (glop:pressed event) (eq (glop:keysym event) :f))
    (glop:toggle-fullscreen window))
  (when (and (glop:pressed event) (eq (glop:keysym event) :g))
    (glop:set-fullscreen window)))

(defmethod glop:on-event ((window plotwindow) (event glop:button-event))
  (case (glop:button event)
    (1 ;; main button
     (setf (1st-pressed window) (glop:pressed event)))
    (4 ;; scroll up
     (when (> (zoom window) *min-zoom*)
       (decf (zoom window) *zoom-step*)
       (glop:push-event window (make-instance 'glop:resize-event :height (glop:window-height window)
                                                                 :width (glop:window-width window))))) 
    (5 ;; scroll down
     (incf (zoom window) *zoom-step*)
     (glop:push-event window (make-instance 'glop:resize-event :height (glop:window-height window)
                                                               :width (glop:window-width window))))))

(defmethod glop:on-event ((window plotwindow) (event glop:mouse-motion-event))
  (when (1st-pressed window)
    (incf (xangle window) (* *rotate-multiplicator* (glop:dx event)))
    (incf (yangle window) (* *rotate-multiplicator* (glop:dy event)))))

(defmethod glop:on-event ((window plotwindow) (event glop:resize-event))
  (let* ((width (glop:width event))
         (height (glop:height event))
         (aspect-ratio (/ height width))
         (zoom (zoom window)))
    (opengl:viewport 0 0 width height)
    (opengl:matrix-mode :projection)
    (opengl:load-identity)
    (opengl:ortho
     (* zoom (- *viewport-multiplicator*))
     (* zoom *viewport-multiplicator*)
     (* zoom (- (* *viewport-multiplicator* aspect-ratio)))
     (* zoom (* *viewport-multiplicator* aspect-ratio))
     (* zoom (- *viewport-multiplicator*))
     (* zoom *viewport-multiplicator*))))

(defmacro while (condition &body body)
  (let ((var (gensym)))
    `(do ((,var nil (progn ,@body)))
         ((not ,condition) ,var))))

(defun draw-3d-axes ()
  "Draw opengl 3d axes"
  (opengl::color 0 1 0)
  (opengl:with-primitives :lines
    (opengl:vertex -10.0 0.0 0.0)
    (opengl:vertex 10.0 0.0 0.0)
    ;; arrow
    (opengl:vertex 10.0 0.0 0.0)
    (opengl:vertex 9.5 0.5 0.0)
    (opengl:vertex 10.0 0.0 0.0)
    (opengl:vertex 9.5 -0.5 0.0)

    (opengl:vertex 1.0 -0.2 0.0)
    (opengl:vertex 1.0 0.2 0.0))
  (opengl::color 1 1 0)
  (opengl:with-primitives :lines
    (opengl:vertex 0.0 -10.0 0.0)
    (opengl:vertex 0.0 10.0 0.0)
    ;; arrow
    (opengl:vertex 0.0 10.0 0.0)
    (opengl:vertex -0.5 9.5 0.0)
    (opengl:vertex 0.0 10.0 0.0)
    (opengl:vertex 0.5 9.5 0.0)
    ;;unit
    (opengl:vertex -0.2 1.0 0.0)
    (opengl:vertex 0.2 1.0 0.0))
  (opengl::color 0.4 0.5 1)
  (opengl::with-primitives :lines
    (opengl:vertex 0.0 0.0 -10.0)
    (opengl:vertex 0.0 0.0 10.0)
    ;; arrow
    (opengl:vertex 0.0 0.0 10.0)
    (opengl:vertex -0.5 0.0 9.5)
    (opengl:vertex 0.0 0.0 10.0)
    (opengl:vertex 0.5 0.0 9.5)
    ;; unit
    (opengl:vertex -0.2 0 1.0)
    (opengl:vertex 0.2 0 1.0)))

(defparameter *plot-color* '(1 0 1))
(defparameter animated-plot 0.2)
(defparameter direction :down)
(defparameter mstep 0.03)

(defparameter max-balls 30000)
(defparameter x-dat (make-array max-balls))
(defparameter y-dat (make-array max-balls))
(defparameter z-dat (make-array max-balls))

;;; curtosy of Mitch Ritchling 

(loop with delta = 0.003
      with x0    = 0.1
      with y0    = 0.0
      with z0    = 0.0
      with a     = 10.0
      with b     = 28.0
      with c     = (/ 8 3.0)
      for cur-bal from 0 upto (1- max-balls)
      for x = x0 then (+ X (* A (- Y X) DELTA))
      for y = y0 then (+ Y (* (- (* X (- B Z)) Y) DELTA))
      for z = z0 then (+ Z (* (- (* X Y) (* C Z)) DELTA))
      do (setf (aref x-dat cur-bal) x
	       (aref y-dat cur-bal) y
	       (aref z-dat cur-bal) z))

(defun draw-plot-points (fn x-start x-end x-step y-start y-end y-step &optional (color '(.5 0 1)))
  (do ((x x-start (+ x x-step)))
      ((< x-end x) nil)
    (opengl:with-primitives :line-strip
      (do ((y y-start (+ y y-step)))
	  ((< y-end y) nil)
	(opengl:vertex x y (funcall fn x y)))))
  (do ((y y-start (+ y y-step)))
      ((< y-end y) nil)
    (opengl:with-primitives :line-strip
      (do ((x x-start (+ x x-step)))
	  ((< x-end x) nil)
	(opengl:vertex x y (funcall fn x y))))))

(defparameter plot-type :animated)
(defparameter axes t)
(defparameter *equations*
  (list (list (lambda (x y) (+ (* 5 (* (- (sin (/ x 2))) (cos y))) (* (cos (/ x 2)) (sin (* 2 y)))))
	      (* 3 (- pi)) (* 3 pi) 0.4  (* 2 (- pi)) (* 2 pi) 0.4)
	(list (lambda (x y) (* (cos x) (+ 3 (* y (cos (/ x 2)))))) (- pi) pi 0.2 -6  1 0.2)
	(list (lambda (x y) (* (sin x) (+ 3 (* y (cos (/ x 2)))))) (- pi) pi 0.2 -6  1 0.2)
	(list (lambda (x y) (* (cos x) (+ 3 (* y (cos (/ x 3)) (sin x))))) -3 3 0.1 -2 2 0.1)
	(list (lambda (x y) (+ (sin x) (cos y))) -5 5 0.1 -5 5 0.2)))

(defun draw-plot ()
  (assert (not (some (lambda (w) (string= "Interactive 3d plot" (stumpwm::window-name w))) (stumpwm::all-windows))) () "Nope. Running two plots windows crashes stumpwm, opengl etc.")
  (glop:with-window (win "Interactive 3d plot" 300 300 :win-class 'plotwindow)
    (opengl:clear-color 1 1 1 0)
    (while (glop:dispatch-events win :blocking nil :on-foo nil)
      ;; rendering
      (opengl:matrix-mode :modelview)
      (opengl:load-identity)
      (opengl:scale 1 1 -1)
      (opengl:clear :color-buffer)
      (opengl:with-pushed-matrix 
	(opengl:rotate (xangle win) 0.0 1.0 0.0)
	(opengl:rotate (yangle win) 1.0 0.0 0.0)
	(opengl:color 1 1 1)
	(opengl:color 1 0 0)
	(when axes (draw-3d-axes))
	(case plot-type
	  (:lorenz (opengl:with-primitives :line-strip
		     (loop for i from 0 to (1- max-balls)
			   for j = 0 then (1+ j)
			   do (progn (opengl:vertex (aref x-dat i)  (aref y-dat i) (aref z-dat i))
				     (cond ((= j 7500) (opengl:color 0 0 1))
					   ((= j 15000) (opengl:color 0 1 1))
					   ((= j 22500) (opengl:color 1 0 1)))))))
	  (:animated (draw-plot-points (lambda (x y) (+ (* animated-plot (cos x) (cos (/ x 2)) (cos y)) 
						   (* (sin (/ x 2)) (sin (* 2 y))))) 
				       (* 2 (- pi)) pi 0.1 (- pi) pi 0.1 *plot-color*)
	   (cond ((<= animated-plot (- 2)) (setf animated-plot -1.9 direction :up))
		 ((>= animated-plot 2) (setf animated-plot 1.9 direction :down))
		 ((eq :down direction) (decf animated-plot mstep))
		 ((eq :up direction) (incf animated-plot mstep))
		 (t (error "should not have occured"))))
	  (t (loop for l in *equations*
		   do (destructuring-bind (fn x-start x-end x-step y-start y-end y-step) l
			(draw-plot-points fn x-start x-end x-step y-start y-end y-step *plot-color*))))))
      (opengl:flush)
      (glop:swap-buffers win))))

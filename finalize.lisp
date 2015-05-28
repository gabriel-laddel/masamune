(in-package #:stumpwm)

(defcommand dashboard () ()
  "dashboard"
  (mmg::run-or-focus-dashboard))

(defcommand kmap () ()
  "knowledge graph"
  (mmg::run-or-focus-kmap))

(define-key *root-map* (kbd "d") "dashboard")
(define-key *root-map* (kbd "m") "kmap")

(setq *input-window-gravity* :center
      *message-window-gravity* :center
      *normal-border-width* 0
      *window-border-style* :none
      *transient-border-width* 0
      *top-level-error-action* :break)

(in-package #:mm)

(defun listener ()
  (bt:make-thread (lambda () (CLIM-LISTENER:RUN-LISTENER)) :name "listener"))

(populate-agenda-items)
(mm::start-conkeror)

(in-package #:drei-commands)

(set-key `(com-backward-kill-word ,*numeric-argument-marker*)
	 'deletion-table
	 '((#\Backspace :control)))

(in-package #:mm)

(setq mm::*system-information* nil)

(load (qlpp "/masamune/systems/communications.lisp"))
(load (qlpp "/masamune/systems/observations.lisp"))

(progn (setf mm::*habits* nil mmg::*focused-habit* nil)
       (captains-log-install)
       (programming-practice-install)
       (mathematics-practice-install))

(in-package mmg)

;;; TODO 2015-04-30T13:28:41+00:00 Gabriel Laddel
;;; integrate these:
;;; http://www.xach.com/lisp/jpeg-dimensions.lisp

(mm::c graphic () (x y)) 
(mm::c image (graphic) (pattern))

(defmethod width ((pattern climi::pattern))
  (climi::image-width (climi::image pattern)))

(defmethod height ((pattern climi::pattern))
  (climi::image-height (climi::image pattern)))

(defun make-image (pathname &optional x y)
  (handler-case
      (let* ((ptype (pathname-type pathname))	     
	     (format (cond ((string= "png" ptype)  :png)
			   ((string= "jpg" ptype)  :jpeg)
			   ((string= "jpeg" ptype) :jpeg)
			   ((string= "tiff" ptype) :tiff)
			   ((string= "gif" ptype)  :gif)))
	     (image (make-instance 'image :x x :y y :pattern
				   (climi::make-pattern-from-bitmap-file pathname :format format))))
	image)
    (unsupported-bitmap-format () 
      (format *standard-output* "image format ~a not recognized" (pathname-type pathname)))))

(defun output-record-of (object)
  (find-if (lambda (o) (and (presentationp o) (eq object (presentation-object o))))
	   (output-record-children (stream-current-output-record *standard-output*))))

(defun image-draw (stream image)
  (with-output-as-presentation (stream image 'image :single-box t)
    (draw-pattern* stream (pattern image) (x image) (y image))))

(defmethod draw-image ((image image) &optional x y (stream *standard-output*))
  (setf (x image) (or x (x image)) (y image) (or y (y image)))
  (image-draw stream image))

(defmethod draw-image
    ((image pathname) &optional x y (stream *standard-output*))
  (let* ((image (make-image image x y)))
    (multiple-value-bind (cx cy) (stream-cursor-position stream)
      (setf (x image) (or x (x image) (+ 10 cx)) 
	    (y image) (or y (y image) (+ 10 cy)))
      (image-draw stream image)
      (setf (stream-cursor-position stream)
	    (values cx (+ cy 10 (height (pattern image))))))))

(clim-listener::define-listener-command (com-move-image) 
    ((image 'image) (start-x 'real) (start-y 'real))
  (let* ((x-offset (- (x image) start-x))
	 (y-offset (- (y image) start-y)))
    (multiple-value-bind (final-x final-y)
	(dragging-output (t :finish-on-release t)
	  (multiple-value-bind (px py)
	      (stream-pointer-position *standard-output*)
	    (let* ((top-x (+ x-offset px))
		   (top-y (+ y-offset py)))
	      (draw-rectangle* *standard-output*
			       top-x
			       top-y
			       (+ top-x (width (pattern image)))
			       (+ top-y (height (pattern image))) 
			       :ink +blue+ :filled nil))))
      (multiple-value-bind (px py)
	  (stream-pointer-position *standard-output*)
	(erase-output-record (output-record-of image) *standard-output*)
	(draw-image image (+ px x-offset) (+ py y-offset))))))

(clim-listener::define-listener-command (com-delete-image)
    ((image 'image :gesture :delete))
  (erase-output-record (output-record-of image) *standard-output*))

(define-presentation-to-command-translator translator-move-image
    (image com-move-image clim-listener::listener :echo nil :gesture :select)
    (object x y)
  `(,object ,x ,y))

(in-package clim-listener)

(defun display-image-at-repl (image-pathname)
  (if (probe-file image-pathname)
      (let* ((type (funcall (case (readtable-case *readtable*)
                              (:upcase #'string-upcase)
                              (:downcase #'string-downcase)
                              (t #'identity))
                            (pathname-type image-pathname)))
             (format (find-symbol type (find-package :keyword))))
        (handler-case (mmg::draw-image image-pathname)
          (unsupported-bitmap-format ()
            (format mm::*hack* "Image format ~A not recognized" type))))
      (format mm::*hack* "No such file: ~A" image-pathname)))

(define-listener-command com-display-image-at-repl
    ((path 'mime-media-type :gesture :delete))
  (display-image-at-repl path))

(define-presentation-to-command-translator translator-display-image-at-repl
    (clim:pathname com-display-image-at-repl filesystem-commands 
     :gesture :select
     :pointer-documentation ((object stream) (format stream "Display image ~A" object))
     :documentation ((stream) (format stream "displays the image in the REPL"))
     :tester ((object)
	      (and (not (wild-pathname-p object))
		   (probe-file object)
		   (pathname-name object)
		   (let ((mime-type (pathname-mime-type object)))
		     (and mime-type (subtypep mime-type 'image))))))
    (object)
  (list object))

(in-package #:mm)

(awhen (probe-file (ppath "lisp-customizations.lisp")) (load it))

(sleep 4)

(unless (mm::port-in-use-p 4258)
  (mmb::start-ps-repl))

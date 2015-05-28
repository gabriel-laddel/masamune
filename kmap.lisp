(in-package #:masamune-gui)

(defvar *kmap* nil)
(defvar *root-node* nil)

(define-application-frame kmap ()
  ((visualization-pane) (interaction-pane))
  (:pointer-documentation t)
  (:menu-bar nil)
  (:panes (interaction-pane :interactor)
	  (visualization-pane :application
			      :scroll-bars t
			      :display-function 'render-kmap))
  (:layouts (default (vertically ()
		       (7/8 visualization-pane)
		       (1/8 interaction-pane)))
	    (fullscreen visualization-pane)))

(defun run-or-focus-kmap ()
  (stumpwm::select-emacs)
  (stumpwm::fullscreen-emacs)
  (if (some (lambda (w) (string= "Kmap" (stumpwm::window-name w))) (stumpwm::all-windows))
      (STUMPWM::select-window "Kmap")
      (bt:make-thread (lambda () (setf *kmap* (make-application-frame 'kmap))
			(run-frame-top-level *kmap* :name "Kmap"))
		      :name "kmap")))

(define-kmap-command (com-quit :name "Quit") ()
  (frame-exit *application-frame*))

(define-kmap-command (com-fullscreen-layout :name "Toggle Fullscreen"
					    :keystroke (#\f :control)) ()
  (if (eq 'fullscreen (frame-current-layout *application-frame*))
      (setf (frame-current-layout *application-frame*) 'default)
      (setf (frame-current-layout *application-frame*) 'fullscreen)))

(define-kmap-command com-node-run-program
    ((node 'node :gesture :select :name "run program"))
  (if (and (slot-boundp node 'mm::program) (mm::program node))
      (funcall (mm::program node))
      (message "the program for this node is currently unimplemented")))

(define-kmap-command com-describe-node
    ((node 'node :gesture :select))
  (labels ((f (description) 
	     (typecase description
	       (cons (dolist (e description) (f e)))
	       (string (if (string= "http" (take 4 description))
			   (progn (mmb::open-uri description t) (stumpwm::fullscreen))
			   (format (interaction-pane) "~a~%" description)))
	       (t (format (interaction-pane) "~a~%" description)))))
    (f (mm::description node))))

(define-kmap-command com-node-visit-channel
    ((node 'node :gesture :select))
  ;; TODO 2014-11-01T02:43:43-07:00 Gabriel Laddel
  ;; this casues an lisp exception to appear and disappear within Emacs many
  ;; times, killing the knowledge map
  ;; (mmb::open-uri "https://github.com/jorams/birch" t)
  (declare (ignore node))
  (intern "start-or-focus-masamune-irc")
  (bt:make-thread (lambda () (mm::with-live-swank-connection 
			    (ignore-errors
			     (intern "start-or-focus-masamune-irc")
			     (swank:eval-in-emacs '(start-or-focus-masamune-irc) t)))))
  (stumpwm::select-emacs)) 

(define-kmap-command com-node-create-child
    ((node 'node :gesture :select))
  (let* ((name (accept 'string :prompt "Name"))
	 (description (accept 'string :prompt "Description"))
	 (parents (progn (remove-duplicates
			  (cons node (loop for maybe-node = (accept 'node :prompt "Select parent node(s)" :default nil)
					   if maybe-node
					     collect maybe-node into out
					   else return out)) :test #'eq)))
	 (program-file (with-open-file (s "/tmp/testfile.lisp" :direction :output 
							       :if-exists :supersede
							       :if-does-not-exist :create)
			 (format s "if the user has been through the tutorial on adding to masamune, prompt."))))
    (push (make-instance 'mm::node
			 :name name
			 :description description
			 :parents parents
			 :program program-file)
	  mm::*nodes*)
    (climacs:edit-file program-file)))

(defun draw-node (node sheet)
  (multiple-value-bind (x y) (clim:stream-cursor-position sheet)
    (destructuring-bind (x1 y1 x2 y2)
	(coerce (slot-value (sheet-region sheet) 'clim-internals::coordinates) 'list)
      (let* ((x (if (node-focusedp node) (/ (- x2 x1) 2) x)) 
	     (y (if (node-focusedp node) (/ (- y2 y1) 2) y))
	     (presentation (with-output-as-presentation (sheet node 'node :single-box t) 
			     (draw-circle* sheet x y (if (node-focusedp node)  60 40) :ink (cond ((node-focusedp node) +white+)
												 ((and (slot-boundp node 'mm::program) (mm::program node)) +light-blue+)
												 (t +grey48+)))
			     (draw-text* sheet (mm::name node) x y :ink +dark-slate-gray+ :align-x :center :align-y :center)
			     (draw-circle* sheet x y (if (node-focusedp node) 60 40) 
					   :ink (if (node-focusedp node) +light-blue+ +blue+)
					   :filled nil
					   :line-thickness (if (node-focusedp node) 5 2)))))
	presentation))))

(defun render-kmap (frame pane)
  ;; TODO 2014-10-29T00:47:06-07:00 Gabriel Laddel
  ;; - seperation arguments are not doing anything
  (declare (ignore frame))
  ;; (format t "~%~%~a" (stream-cursor-position pane))
  (destructuring-bind (x1 y1 x2 y2)
      (coerce (slot-value (sheet-region pane) 'clim-internals::coordinates) 'list)
    (let* ((x (/ (- x2 x1) 2)) (y (/ (- y2 y1) 2)))
      ;; (format (interaction-pane) "~%~%~a" (clim:stream-cursor-position pane))
      ;; (format t "x ~d ,y ~d"
      ;; (slot-value  'clim-internals::x)
      ;; (slot-value (clim:stream-cursor-position pane) 'clim-internals::y))
      ;; (setf  x)
      ;; (setf  y)
      (format-graph-from-root *root-node*
			      #'draw-node
			      #'mm::children
			      :stream pane
			      :center-nodes t
			      :graph-type :dag
			      :merge-duplicates t
			      :move-cursor t
			      :orientation :vertical))))

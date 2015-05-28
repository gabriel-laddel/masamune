;;; Typeout is the word for "output whatever". It's a facility for
;;; drawing stuff that doesn't go in a buffer. There are two kinds of
;;; typeout: typeout views, that act like uneditable views, and
;;; typeout overlays, that are highly ephemeral and temporary
;;; creations to be used for short-lived interaction.

(in-package :climacs-gui)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Typeout views.

(defclass typeout-view (drei-view textual-view)
  ((%output-history :accessor output-history
                    :initform (make-instance 'standard-tree-output-record)
                    :initarg :output-history
                    :documentation "The output record history
that will be replayed whenever the views contents are shown.")
   (%dirty :accessor dirty
           :initform t
           :documentation "This value indicates whether the
output has changed since it was last replayed.")
   (%cursor-position :accessor last-cursor-position
                     :initform nil
                     :documentation "A list (X Y) specifying
where drawing ended the last time, and where it should start the
next time. If NIL, no previous position has been recorded."))
  (:metaclass modual-class)
  (:documentation "A noneditable Drei view displaying an output
record history."))

(defun typeout-view-p (view)
  "Return true if `view' is a typeout view, false otherwise."
  (typep view 'typeout-view))

(defmethod clear-redisplay-information ((view typeout-view))
  (setf (dirty view) t))

(defun blank-typeout-view (view)
  "Blank out the contents of the typeout view `view'."
  (setf (output-history view) (make-instance 'standard-tree-output-record)
        (last-cursor-position view) nil)
  (clear-redisplay-information view)
  ;; If it's on display, clear the window too.
  (let ((window (find view (windows *application-frame*)
                 :key #'view)))
    (when window (window-clear window))))

(defmethod handle-redisplay ((pane drei-pane) (view typeout-view) (region region))
  (if (and (not (dirty view))
           (eq (output-record-parent (output-history view))
               (stream-output-history pane)))
      (unless (region-equal region +nowhere+)
        (let ((region (if (region-equal region +everywhere+)
                          (sheet-region pane)
                          (bounding-rectangle region))))
          (with-bounding-rectangle* (x1 y1 x2 y2) region
            (with-output-recording-options (pane :record nil)
              (draw-rectangle* pane x1 y1 x2 y2 :filled t :ink +background-ink+)))
          (replay (stream-output-history pane) pane region)))
      (call-next-method)))

(defmethod display-drei-view-contents ((pane pane) (view typeout-view))
  (when (or (dirty view)
            (not (eq (output-record-parent (output-history view))
                     (stream-output-history pane))))
    (with-output-recording-options (pane :record nil :draw t)
      (with-bounding-rectangle* (x1 y1 x2 y2) (or (pane-viewport-region pane)
                                                  (sheet-region pane))
        (draw-rectangle* pane x1 y1 x2 y2 :ink +background-ink+))
      (replay-output-record (output-history view) pane
                            (or (pane-viewport-region pane)
                                (sheet-region pane))))
    (unless (eq (output-record-parent (output-history view))
                (stream-output-history pane))
      (setf (output-record-parent (output-history view)) nil)
      (add-output-record (output-history view) (stream-output-history pane))))
  (setf (dirty view) nil))

(defmethod bounding-rectangle* ((view typeout-view))
  (if (output-history view)
      (bounding-rectangle* (output-history view))
      (values 0 0 0 0)))

(defun scroll-typeout-window (window y)
  "Scroll `window' down by `y' device units, but taking care not
to scroll past the size of `window'. If `window' does not have a
viewport, do nothing."
  (let ((viewport (pane-viewport window)))
    (unless (null viewport)            ; Can't scroll without viewport
      (multiple-value-bind (x-displacement y-displacement)
          (transform-position (sheet-transformation window) 0 0)
        (scroll-extent window
                       (- x-displacement)
                       (max 0 (min (+ (- y-displacement) y)
                                   (- (bounding-rectangle-height window)
                                      (bounding-rectangle-height viewport)))))))))

(defmethod page-down ((pane sheet) (view typeout-view))
  (scroll-typeout-window pane (bounding-rectangle-height (sheet-region pane))))

(defmethod page-up ((pane sheet) (view typeout-view))
  (scroll-typeout-window
   pane (- (bounding-rectangle-height (pane-viewport pane)))))

(defun ensure-typeout-view (climacs label erase)
  "Ensure that `climacs' has a typeout view with the name
`label', and return that view. If `erase' is true, clear any
already existing typeout view by that name first."
  (check-type label string)
  (or (let ((view (find-if #'(lambda (view)
                               (and (typeout-view-p view)
                                    (string= (name view) label)))
                           (views climacs))))
        (when (and view erase) (blank-typeout-view view))
        view)
      (make-new-view-for-climacs climacs 'typeout-view
       :name label)))

;; Because specialising on the type of `climacs' is so useful...
(defun invoke-with-typeout-view (climacs label erase continuation)
  "Call `continuation' with a single argument, a
stream meant for typeout. `Climacs' is the Climacs instance in
which the typeout pane should be shown, and `label' is the name
of the created typeout view. Returns NIL."
  (let* ((typeout-view (ensure-typeout-view climacs label erase))
         (pane-with-typeout-view (or (find typeout-view (windows climacs)
                                      :key #'view)
                                     (let ((pane (split-window t)))
                                       (setf (view pane) typeout-view)
                                       pane))))
    (let ((new-record (with-output-to-output-record (pane-with-typeout-view)
                        (with-output-recording-options (pane-with-typeout-view :record t :draw nil)
                          (when (last-cursor-position typeout-view)
                            (setf (stream-cursor-position pane-with-typeout-view)
                                  (values-list (last-cursor-position typeout-view))))
                          (funcall continuation pane-with-typeout-view)
                          (setf (last-cursor-position typeout-view)
                                (multiple-value-list (stream-cursor-position pane-with-typeout-view)))))))
      (add-output-record new-record (output-history typeout-view))
      (setf (dirty typeout-view) t)
      nil)))

(defmacro with-typeout-view ((stream &optional (label "Typeout") erase)
                             &body body)
  "Evaluate `body' with `stream' bound to a stream that can be
used for typeout. `Label' is the name of the created typeout
view. If `erase' is true, clear the contents of any existing
typeout view with that name."
  `(invoke-with-typeout-view *esa-instance* ,label ,erase
                             #'(lambda (,stream)
                                 ,@body)))

;;; An implementation of the Gray streams protocol that uses a Climacs
;;; typeout view to draw the output.

(defclass typeout-stream (fundamental-character-output-stream)
  ((%climacs :reader climacs-instance
             :initform (error "Must provide a Climacs instance for typeout streams")
             :initarg :climacs)
   (%label :reader label
           :initform (error "A typeout stream must have a label")
           :initarg :label))
  (:documentation "An output stream that performs output on
a (single) Climacs typeout pane. If the typeout pane is deleted
manually by the user, the stream will recreate it the next time
output is performed."))

(defmethod stream-write-char ((stream typeout-stream) char)
  (with-typeout-view (typeout (label stream))
    (stream-write-char typeout char)))

(defmethod stream-line-column ((stream typeout-stream))
  (with-typeout-view (typeout (label stream))
    (stream-line-column typeout)))

(defmethod stream-start-line-p ((stream typeout-stream))
  (with-typeout-view (typeout (label stream))
    (stream-start-line-p typeout)))

(defmethod stream-write-string ((stream typeout-stream) string &optional (start 0) end)
  (with-typeout-view (typeout (label stream))
    (stream-write-string typeout string start end)))

(defmethod stream-terpri ((stream typeout-stream))
  (with-typeout-view (typeout (label stream))
    (stream-terpri typeout)))

(defmethod stream-fresh-line ((stream typeout-stream))
  (with-typeout-view (typeout (label stream))
    (stream-fresh-line typeout)))

(defmethod stream-finish-output ((stream typeout-stream))
  (with-typeout-view (typeout (label stream))
    (stream-finish-output typeout)))

(defmethod stream-force-output ((stream typeout-stream))
  (with-typeout-view (typeout (label stream))
    (stream-force-output typeout)))

(defmethod stream-clear-output ((stream typeout-stream))
  (with-typeout-view (typeout (label stream))
    (stream-clear-output typeout)))

(defmethod stream-advance-to-column ((stream typeout-stream) (column integer))
  (with-typeout-view (typeout (label stream))
    (stream-advance-to-column typeout column)))

(defmethod interactive-stream-p ((stream typeout-stream))
  (with-typeout-view (typeout (label stream))
    (interactive-stream-p typeout)))

(defun make-typeout-stream (climacs label)
  (make-instance 'typeout-stream :climacs climacs :label label))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Typeout overlays.

(defclass overlaying-pane (bboard-pane)
  ((%overlay-pane :accessor overlay-pane
                  :initform nil
                  :type (or null pane)
                  :documentation "The overlay pane. When this is
set, the overlay tree will be updated.")
   (%overlay-tree :accessor overlay-tree
                  :initform nil
                  :type (or null pane)
                  :documentation "The pane hierarchy containing
the overlay pane. Should not be changed manually, will be updated
when the overlay-pane is set.")
   (%content-pane :reader content-pane
                  :initform (error "A content-pane must be provided")
                  :type pane
                  :initarg :contents
                  :documentation "The pane containing the usually
displayed contents."))
  (:documentation "This layout pane facilitates the addition and
removal of an overlay pane positioned at the top of the
`overlaying-pane' that will obscure the contents. For ease of
use, the overlay-pane and the pane hierarchy containing this pane
are handled seperately."))

(defun find-topmost-parent (sheet)
  "Find the topmost parent of `sheet', that is the parent of
`sheet' (or `sheet' itself) that does not have a sheet parent or
has a graft parent."
  (if (or (not (sheetp (sheet-parent sheet)))
          (typep (sheet-parent sheet) 'graft))
      sheet
      (find-topmost-parent (sheet-parent sheet))))

(defmethod (setf overlay-pane) :before (new-overlay (pane overlaying-pane))
  (when (overlay-pane pane)
    (sheet-disown-child pane (overlay-tree pane))
    (setf (overlay-tree pane) nil)))

(defmethod (setf overlay-pane) :after (new-overlay (pane overlaying-pane))
  (when new-overlay
    (let ((topmost-parent (find-topmost-parent new-overlay)))
      (sheet-adopt-child pane topmost-parent)
      (setf (overlay-tree pane) topmost-parent))))

(defmethod initialize-instance :after ((object overlaying-pane) &rest args
                                       &key overlay)
  (declare (ignore args))
  (when overlay
    (setf (overlay-pane object) overlay))
  (sheet-adopt-child object (content-pane object)))

(defmethod allocate-space ((pane overlaying-pane) width height)
  (allocate-space (content-pane pane) width height)
  (with-accessors ((overlay overlay-tree)) pane
    (when overlay
      (move-sheet overlay 0 0)
      (allocate-space
       overlay width (space-requirement-height (compose-space overlay))))))

(defmethod compose-space ((pane overlaying-pane) &rest args)
  (apply #'compose-space (content-pane pane) args))

(defmacro overlaying ((&rest options) &body contents)
  "Create an overlaying pane with `contents' arranged vertically
as the contents of the overlaying pane. There will be no initial
overlay."
  `(make-pane 'overlaying-pane ,@options :contents (vertically () ,@contents)))

(defun pane-overlayer (pane)
  "Return the `overlaying-pane' that contains `pane'"
  (if (typep pane 'overlaying-pane)
      pane
      (unless (null (sheet-parent pane))
        (pane-overlayer (sheet-parent pane)))))

(defun add-typeout (&optional (pane (current-window)))
  "Return the typeout overlay of `pane', creating one if it
doesn't exist."
  (with-look-and-feel-realization
      ((frame-manager (pane-frame pane)) (pane-frame pane))
    (let ((overlayer (pane-overlayer pane)))
      (or (overlay-pane overlayer)
          (let ((overlay (make-pane 'typeout-overlay
                          :width (bounding-rectangle-width
                                  (sheet-region overlayer)))))
            (outlining () overlay) ; This adds an outlining-pane as
                                   ; the parent of `overlay'.
            (setf (overlay-pane overlayer) overlay))))))

(defun remove-typeout (&optional (pane (current-window)))
  "Remove the typeout overlay of `pane', defaulting to the
current window."
  (setf (overlay-pane (pane-overlayer pane)) nil))

(defclass typeout-overlay (clim-stream-pane)
  ()
  (:default-initargs :background +cornsilk1+
                     :scroll-bars nil))

(defun invoke-with-typeout (pane continuation &key erase)
  "Invoke `continuation' with a single argument - a typeout
overlay for `pane'. If `erase' is true, the typeout overlay will
be newly created, and any old overlay will have been deleted."
  (with-look-and-feel-realization ((frame-manager (pane-frame pane)) (pane-frame pane))
    (when erase (remove-typeout pane))
    (let* ((typeout (add-typeout pane)))
      ;; Expand the typeout to the proper width...
      (change-space-requirements typeout)
      (let ((values (multiple-value-list
                     (funcall continuation typeout))))
        (remove-typeout pane)
        (values-list values)))))

(defmacro with-typeout ((stream &rest args &key erase (window (current-window)))
                        &body body)
  "Evaluate `body' with `stream' bound to a typeout overlay for
`window'. If `erase' is true, the typeout overlay will be newly
created, and any old overlay will have been deleted."
  (declare (ignore erase))
  (with-keywords-removed (args (:window))
    `(invoke-with-typeout ,window
                          #'(lambda (,stream)
                              ,@body)
                          ,@args)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; A frame manager for using typeout when appropriate.

(defclass climacs-frame-manager (frame-manager)
  ((%standard-frame-manager
    :reader standard-frame-manager
    :initform (find-frame-manager)
    :type frame-manager
    :documentation "The frame manager that this
`climacs-frame-manager' dispatches functions to."
    :initarg :standard-frame-manager))
  (:documentation "This class thinly wraps another frame manager
instance and delegates most frame managing to this other
manager. It is used to implement Climacs \"look and feel\" where
appropriate."))

;;; A simple dispatching implementation of the frame manager protocol.

(defmacro define-dispatching-fun (name (frame-manager-arg &rest args))
  "Defines a dispatching function for the frame manager protocol
for `climacs-frame-manager'. Will assume that `frame-manager-arg'
is the frame manager."
  `(defmethod ,name ((,frame-manager-arg climacs-frame-manager) ,@args)
     (when (standard-frame-manager ,frame-manager-arg)
       (,name (standard-frame-manager ,frame-manager-arg) ,@args))))

(define-dispatching-fun frame-manager-frames (frame-manager))
(define-dispatching-fun adopt-frame (frame-manager frame))
(define-dispatching-fun disown-frame (frame-manager frame))
(define-dispatching-fun port (frame-manager))
(define-dispatching-fun note-frame-enabled (frame-manager frame))
(define-dispatching-fun note-frame-disabled (frame-manager frame))
(define-dispatching-fun note-frame-iconified (frame-manager frame))
(define-dispatching-fun note-frame-deiconified (frame-manager frame))
(define-dispatching-fun note-command-enabled (frame-manager frame command-name))
(define-dispatching-fun note-command-disabled (frame-manager frame command-name))

(defmethod frame-manager-notify-user ((frame-manager climacs-frame-manager) message &rest args)
  (apply #'frame-manager-notify-user frame-manager message args))

(define-dispatching-fun generate-panes (frame-manager frame))
(define-dispatching-fun find-pane-for-frame (frame-manager frame))

;;; Now for the look & feel.

(defun menu-item-option (menu-item option &optional default)
  (if (listp menu-item)
      (getf (menu-item-options menu-item) option default)
      default))

(define-presentation-type typeout-menu-item ())

(defmethod menu-choose-from-drawer
    ((menu typeout-overlay) presentation-type drawer
     &key x-position y-position cache unique-id id-test cache-value cache-test
     default-presentation pointer-documentation)
  (declare (ignore cache unique-id
                   id-test cache-value cache-test default-presentation
                   x-position y-position))
  (with-room-for-graphics (menu :first-quadrant nil)
    (funcall drawer menu presentation-type))
  (let ((*pointer-documentation-output* pointer-documentation))
    (handler-case
        (with-input-context (`(or ,presentation-type blank-area) :override t)
            (object type event) 
            (prog1 nil (loop for gesture = (read-gesture :stream menu :peek-p t)
                             until (or (and (typep gesture 'keyboard-event)
                                            (keyboard-event-character gesture))
                                       (characterp gesture))
                             do (read-gesture :stream menu)))
          (blank-area nil)
          (t (values object event)))
      (abort-gesture () nil))))

(defmethod frame-manager-menu-choose
    ((frame-manager climacs-frame-manager) items
     &rest options
     &key (associated-window (current-window)) printer presentation-type
     (default-item nil default-item-p)
     text-style label cache unique-id id-test cache-value cache-test
     max-width max-height n-rows n-columns x-spacing y-spacing row-wise
     cell-align-x cell-align-y (scroll-bars :vertical)
     ;; We provide pointer documentation by default.
     (pointer-documentation *pointer-documentation-output*))
  (flet ((drawer (overlay type)
           (let* ((height (bounding-rectangle-height
                           (with-new-output-record (overlay)
                             (when label
                               (with-text-style (overlay (make-text-style :serif :italic :large))
                                 (write-string label overlay)
                                 (terpri overlay)))
                             (draw-standard-menu overlay type items
                                                 (if default-item-p
                                                     default-item
                                                     (first items))
                                                 :item-printer (or printer
                                                                   #'print-menu-item)
                                                 :max-width max-width
                                                 :max-height max-height
                                                 :n-rows n-rows
                                                 :n-columns n-columns
                                                 :x-spacing x-spacing
                                                 :y-spacing y-spacing
                                                 :row-wise row-wise
                                                 :cell-align-x cell-align-x
                                                 :cell-align-y cell-align-y))))
                  (overlayer (pane-overlayer overlay))
                  (overlay-tree (overlay-tree overlayer)))
             ;; Tell it how big it is.
             (change-space-requirements overlay :height height)
             ;; Bigger than the available space? User OK with ugly?
             ;; Then add scrolling.
             (when (and (> height (bounding-rectangle-height overlayer))
                        scroll-bars
                        (not (typep overlay-tree 'scroller-pane)))
               (setf (overlay-pane overlayer) nil ; To clear the parent/child relationship
                     (overlay-pane overlayer)
                     (prog1 overlay
                       (scrolling (:scroll-bars scroll-bars) ; Now re-add with scroll-bars.
                         overlay-tree)))
               ;; The overlayer has default space requirements now,
               ;; make it reevaluate its life.
               (change-space-requirements overlayer)))))
    (multiple-value-bind (object event)
        (with-typeout (menu :erase t :window associated-window)
          (when text-style
            (setf (medium-text-style menu) text-style))
          (letf (((stream-default-view menu) +textual-menu-view+))
            (menu-choose-from-drawer menu (or presentation-type 'typeout-menu-item)
                                     #'drawer
                                     :cache cache
                                     :unique-id unique-id
                                     :id-test id-test
                                     :cache-value cache-value
                                     :cache-test cache-test
                                     :pointer-documentation pointer-documentation)))
      (unless (null event)             ; Event is NIL if user aborted.
        (let ((subitems (menu-item-option object :items 'menu-item-no-items)))
          (if (eq subitems 'menu-item-no-items)
              (values (menu-item-value object) object event)
              (apply #'frame-manager-menu-choose
                     frame-manager subitems
                     options)))))))

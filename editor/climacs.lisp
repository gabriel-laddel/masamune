(in-package :climacs)

(defun find-climacs-frame ()
  (let ((frame-manager (find-frame-manager)))
    (when frame-manager
      (find-if (lambda (x) (and (typep x 'climacs) 
                                (eq (clim:frame-state x) :enabled)))
               (frame-manager-frames frame-manager)))))

(defun climacs (&rest args &key new-process (process-name "Climacs")
		(text-style *climacs-text-style*)
                (width 900) (height 400))
  "Starts up a climacs session"
  (declare (ignore new-process process-name width height text-style))
  (apply #'climacs-common nil args))

(defun climacs-rv (&rest args &key new-process (process-name "Climacs")
		   (text-style *climacs-text-style*)
                   (width 900) (height 400))
  "Starts up a climacs session with alternative colors."
  ;; SBCL doesn't inherit dynamic bindings when starting new
  ;; processes, so start a new processes and THEN setup the colors.
  (declare (ignore text-style width height))
  (flet ((run ()
           (let ((*background-color* +black+)
                 (*foreground-color* +gray+)
                 (*info-bg-color* +darkslategray+)
                 (*info-fg-color* +gray+)
                 (*mini-bg-color* +black+)
                 (*mini-fg-color* +white+))
             (apply #'climacs-common nil :new-process nil args))))
    (if new-process
        (clim-sys:make-process #'run :name process-name)
        (run))))

(defun edit-file (thing &rest args
                  &key (process-name "Climacs") (width 900) (height 400)
		  (text-style *climacs-text-style*))
  "Edit THING in an existing climacs process or start a new one. THING
can be a filename (edit the file) or symbol (edit its function definition)."
  (declare (ignore process-name width height text-style))
  (let ((climacs-frame (find-climacs-frame))
        (command 
         (typecase thing
           (null nil)
           (symbol (list 'drei-lisp-syntax::com-edit-definition thing))
           ((or string pathname)
            (truename thing)  ; raise file-error if file doesn't exist
            (list 'esa-io::com-find-file thing))
           (t (error 'type-error :datum thing
                     :expected-type '(or null string pathname symbol))))))
    (if climacs-frame
        (when command
          (execute-frame-command climacs-frame command))
        (apply #'climacs-common command :new-process t args)))
  t)

(defun climacs-common (command &key new-process (process-name "Climacs")
		       (text-style *climacs-text-style*)
                       (width 900) (height 400))
  (let* ((frame (make-application-frame 'climacs :width width :height height))
	 (*climacs-text-style* text-style)
         (*application-frame* frame)
         (esa:*esa-instance* frame))
    (adopt-frame (find-frame-manager) *application-frame*)
    (when command (execute-frame-command *application-frame* command))
    (flet ((run () (run-frame-top-level frame)))
      (if new-process
          (clim-sys:make-process #'run :name process-name)
          (run)))))

;;; Append to end of *ed-functions* so we don't overwrite the user's
;;; preferred editor
#+sbcl
(unless (member 'edit-file sb-ext:*ed-functions*)
  (setf sb-ext:*ed-functions* (append sb-ext:*ed-functions* (list 'edit-file))))

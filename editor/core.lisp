(in-package :climacs-core)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Buffer handling

(defmethod frame-make-new-buffer ((application-frame climacs)
                                  &key (name "*scratch*"))
  (make-instance 'climacs-buffer :name name))

(define-presentation-method present ((object drei-view) (type view)
                                     stream (view textual-view)
                                     &key acceptably for-context-type)
  (declare (ignore acceptably for-context-type))
  (princ (subscripted-name object) stream))

(define-presentation-method accept ((type view) stream (view textual-view)
                                    &key (default nil defaultp)
                                    (default-type type))
  (multiple-value-bind (object success string)
      (complete-input stream
		      (lambda (so-far action)
			(complete-from-possibilities
			 so-far (views *esa-instance*) '()
                         :action action
			 :name-key #'subscripted-name
			 :value-key #'identity))
		      :partial-completers '(#\Space)
		      :allow-any-input t)
    (cond ((and success (plusp (length string)))
           (if object
               (values object type)
               (values string 'string)))
	  ((and (zerop (length string)) defaultp)
           (values default default-type))
	  (t
           (values string 'string)))))

(defgeneric switch-to-view (drei view)
  (:documentation "High-level function for changing the view
displayed by a Drei instance."))

(defmethod switch-to-view ((drei climacs-pane) (view drei-view))
  (setf (view drei) view))

(defmethod switch-to-view (pane (name string))
  (let ((view (find name (views (pane-frame pane))
               :key #'subscripted-name :test #'string=)))
    (switch-to-view
     pane (or view (make-new-view-for-climacs
                    (pane-frame pane) 'textual-drei-syntax-view
                    :name name)))))

(defun switch-or-move-to-view (pane view)
  "Switch `pane' to show `view'. If `view' is already on display
in some other pane, switch that pane to be the active one."
  (handler-bind ((view-already-displayed
                  #'(lambda (c)
                      (declare (ignore c))
                      (invoke-restart 'switch-to-pane))))
    (switch-to-view pane view)))

(defun views-having-buffer (climacs buffer)
  "Return a list of the buffer-views of `climacs' showing
`buffer'."
  (loop for view in (views climacs)
     when (and (typep view 'drei-buffer-view)
               (eq (buffer view) buffer))
     collect view))

(defun buffer-of-view-needs-saving (view)
  "Return true if `view' is a `drei-buffer-view' and it needs to
be saved (that is, it is related to a file and it has changed
since it was last saved)."
  (and (typep view 'drei-buffer-view)
       (filepath (buffer view))
       (needs-saving (buffer view))))

(defun dummy-buffer ()
  "Create a dummy buffer object for use when killing views, to
prevent increasing memory usage."
  (make-instance 'drei-buffer))

(defgeneric kill-view (view)
  (:documentation "Remove `view' from the Climacs specified in
`*esa-instance*'. If `view' is currently displayed in a window,
it will be replaced by some other view."))

(defmethod kill-view ((view view))
  (with-accessors ((views views)) *esa-instance*
    ;; It might be the case that this view is the only view remaining
    ;; of some particular buffer, in that case, the user might want to
    ;; save it.
    (when (and (buffer-of-view-needs-saving view)
               (= (length (views-having-buffer *esa-instance* (buffer view)))
                  1)
               (handler-case (accept 'boolean :prompt "Save buffer first?")
                 (error () (progn (beep)
                                  (display-message "Invalid answer")
                                  (return-from kill-view nil)))))
      (save-buffer (buffer view)))
    (setf views (remove view views))
    ;; If we don't change the buffer of the view, a reference to the
    ;; view will be kept in the buffer, and the view will thus not be
    ;; garbage-collected. So create a circular reference structure
    ;; that can be garbage-collected instead.
    (when (buffer-view-p view)
      (setf (buffer view) (dummy-buffer)))
    (full-redisplay (current-window))
    (current-view)))

(defmethod kill-view ((name string))
  (let ((view (find name (views *application-frame*)
                 :key #'subscripted-name :test #'string=)))
    (when view (kill-view view))))

(defmethod kill-view ((symbol null))
  (kill-view (current-view)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; File handling

(defun filepath-filename (pathname)
  (if (null (pathname-type pathname))
      (pathname-name pathname)
      (concatenate 'string (pathname-name pathname)
		   "." (pathname-type pathname))))

(defun syntax-class-name-for-filepath (filepath)
  (let ((syntax-description
         (find (or (pathname-type filepath)
                   (pathname-name filepath))
               drei-syntax::*syntaxes*
               :test (lambda (x y)
                       (member x y :test #'string-equal))
               :key #'drei-syntax::syntax-description-pathname-types)))
    (if syntax-description
        (drei-syntax::syntax-description-class-name
         syntax-description)
        *default-syntax*)))

(defun evaluate-attributes (view options)
  "Evaluate the attributes `options' and modify `view' as
appropriate. `Options' should be an alist mapping option names to
their values."
  ;; First, check whether we need to change the syntax (via the SYNTAX
  ;; option). MODE is an alias for SYNTAX for compatibility with
  ;; Emacs. If there is more than one option with one of these names,
  ;; only the first will be acted upon.
  (let ((specified-syntax
         (syntax-from-name
          (second (find-if #'(lambda (name)
                               (or (string-equal name "SYNTAX")
                                   (string-equal name "MODE")))
                           options
                           :key #'first)))))
    (when (and specified-syntax
               (not (eq (class-of (syntax view))
                        specified-syntax)))
      (setf (syntax view)
            (make-syntax-for-view view specified-syntax))))
  ;; Now we iterate through the options (discarding SYNTAX and MODE
  ;; options).
  (loop for (name value) in options
     unless (or (string-equal name "SYNTAX")
                (string-equal name "MODE"))
     do (eval-option (syntax view) name value)))

(defun split-attribute (string char)
  (let (pairs)
    (loop with start = 0
	  for ch across string
	  for i from 0
	  when (eql ch char)
	    do (push (string-trim '(#\Space #\Tab) (subseq string start i))
		     pairs)
	       (setf start (1+ i))
	  finally (unless (>= start i)
		    (push (string-trim '(#\Space #\Tab) (subseq string start))
			  pairs)))
    (nreverse pairs)))

(defun split-attribute-line (line)
  (when line
    (mapcar (lambda (pair) (split-attribute pair #\:))
	    (split-attribute line #\;))))

(defun find-attribute-line-position (buffer)
  (let ((scan (make-buffer-mark buffer 0)))
    ;; skip the leading whitespace
    (loop until (end-of-buffer-p scan)
       until (not (buffer-whitespacep (object-after scan)))
       do (forward-object scan))
    ;; stop looking if we're already 1,000 objects into the buffer
    (unless (> (offset scan) 1000)
      (let ((start-found
	     (loop with newlines = 0
                when (end-of-buffer-p scan)
                do (return nil)
                when (eql (object-after scan) #\Newline)
                do (incf newlines)
                when (> newlines 1)
                do (return nil)
                until (looking-at scan "-*-")
                do (forward-object scan)
                finally (return t))))
	(when start-found
          (let* ((end-scan (clone-mark scan))
                 (end-found
                  (loop when (end-of-buffer-p end-scan)
                     do (return nil)
                     when (eql (object-after end-scan) #\Newline)
                     do (return nil)
                     do (forward-object end-scan)
                     until (looking-at end-scan "-*-")
                     finally (return t))))
            (when end-found
              (values scan
                      (progn (forward-object end-scan 3)
                             end-scan)))))))))

(defun get-attribute-line (buffer)
  (multiple-value-bind (start-mark end-mark)
      (find-attribute-line-position buffer)
   (when (and start-mark end-mark)
     (let ((line (buffer-substring buffer
				   (offset start-mark)
				   (offset end-mark))))
       (when (>= (length line) 6)
	 (let ((end (search "-*-" line :from-end t :start2 3)))
	   (when end
	     (string-trim '(#\Space #\Tab) (subseq line 3 end)))))))))

(defun replace-attribute-line (view new-attribute-line)
  (let ((full-attribute-line (concatenate 'string
                                          "-*- "
                                          new-attribute-line
                                          "-*-")))
   (multiple-value-bind (start-mark end-mark)
       (find-attribute-line-position (buffer view))
     (cond ((not (null end-mark))
            ;; We have an existing attribute line.
            (delete-region start-mark end-mark)
            (let ((new-line-start (clone-mark start-mark :left)))
              (insert-sequence start-mark full-attribute-line)
              (comment-region (syntax view)
                              new-line-start
                              start-mark)))
           (t
            ;; Create a new attribute line at beginning of buffer.
            (let* ((mark1 (make-buffer-mark (buffer view) 0 :left))
                   (mark2 (clone-mark mark1 :right)))
              (insert-sequence mark2 full-attribute-line)
              (insert-object mark2 #\Newline)
              (comment-region (syntax view)
                              mark1
                              mark2)))))))

(defun update-attribute-line (view)
  (replace-attribute-line
   view (make-attribute-line (syntax view))))

(defun evaluate-attribute-line (view)
  (evaluate-attributes
   view (split-attribute-line (get-attribute-line (buffer view)))))

;; Adapted from cl-fad/PCL
(defun directory-pathname-p (pathspec)
  "Returns NIL if PATHSPEC does not designate a directory."
  (let ((name (pathname-name pathspec))
	(type (pathname-type pathspec)))
    (and (or (null name) (eql name :unspecific))
	 (or (null type) (eql type :unspecific)))))

(defun findablep (pathname)
  "Return non-NIL if `pathname' can be opened by Climacs. That
  is, check whether the file exists and is not a directory."
  (and (probe-file pathname)
       (not (directory-pathname-p pathname))))

(defun find-view-with-pathname (pathname)
  "Return the (first) with associated with the file designated by
`pathname'. Returns NIL if no buffer can be found."
  (flet ((usable-pathname (pathname)
           (if (probe-file pathname)
               (truename pathname)
               pathname)))
    (find pathname (remove-if-not #'(lambda (view)
                                      (typep view 'drei-buffer-view))
                                  (views *application-frame*))
     :key #'(lambda (view) (filepath (buffer view)))
     :test #'(lambda (fp1 fp2)
               (and fp1 fp2
                    (equal (usable-pathname fp1)
                           (usable-pathname fp2)))))))

(defun ensure-open-file (pathname)
  "Make sure a buffer opened on `pathname' exists, finding the
file if necessary."
  (when (and (findablep pathname)
             (not (find-buffer-with-pathname pathname)))
    (find-file pathname)))

(defun find-file-impl (filepath &optional readonlyp)
  (cond ((null filepath)
	 (display-message "No file name given.")
	 (beep))
	((directory-pathname-p filepath)
	 (display-message "~A is a directory name." filepath)
	 (beep))
        (t
         (let ((existing-view (find-view-with-pathname filepath)))
           (if (and existing-view (if readonlyp (read-only-p (buffer existing-view)) t))
               (switch-to-view (current-window) existing-view)
               (let* ((newp (not (probe-file filepath)))
                      (buffer (if (and newp (not readonlyp))
                                  (make-new-buffer)
                                  (with-open-file (stream filepath :direction :input)
                                    (make-buffer-from-stream stream))))
                      (view (make-new-view-for-climacs
                             *esa-instance* 'textual-drei-syntax-view
                             :name (filepath-filename filepath)
                             :buffer buffer)))
                 (unless (buffer-pane-p (current-window))
                   (other-window (or (find-if #'(lambda (window)
                                                  (typep window 'climacs-pane))
                                              (windows *esa-instance*))
                                     (split-window t))))
                 (setf (offset (point buffer)) (offset (point view))
                       (syntax view) (make-syntax-for-view view (syntax-class-name-for-filepath filepath))
                       (file-write-time buffer) (if newp (get-universal-time) (file-write-date filepath))
                       (needs-saving buffer) nil
                       (name buffer) (filepath-filename filepath))
                 (setf (current-view (current-window)) view)
                 (evaluate-attribute-line view)
                 (setf (filepath buffer) (pathname filepath)
                       (read-only-p buffer) readonlyp)
                 (beginning-of-buffer (point view))
                 buffer))))))

(defmethod frame-find-file ((application-frame climacs) filepath)
  (find-file-impl filepath nil))

(defmethod frame-find-file-read-only ((application-frame climacs) filepath)
  (find-file-impl filepath t))

(defun directory-of-buffer (buffer)
  "Extract the directory part of the filepath to the file in BUFFER.
If BUFFER does not have a filepath, the path to the user's home
directory will be returned."
  (make-pathname
   :directory
   (pathname-directory
    (or (filepath buffer)
	(user-homedir-pathname)))))

(defmethod frame-set-visited-filename ((application-frame climacs) filepath buffer)
  (setf (filepath buffer) (pathname filepath)
	(file-saved-p buffer) nil
	(file-write-time buffer) nil
	(name buffer) (filepath-filename filepath)
	(needs-saving buffer) t))

(defun check-file-times (buffer filepath question answer)
  "Return NIL if filepath newer than buffer and user doesn't want
to overwrite."
  (let ((f-w-d (file-write-date filepath))
	(f-w-t (file-write-time buffer)))
    (if (and f-w-d f-w-t (> f-w-d f-w-t))
	(if (accept 'boolean
		    :prompt (format nil "File has changed on disk. ~a anyway?"
				    question))
	    t
	    (progn (display-message "~a not ~a" filepath answer)
		   nil))
	t)))

(defmethod frame-exit :around ((frame climacs) #-mcclim &key)
  (dolist (view (views frame))
    (handler-case
        (when (and (buffer-of-view-needs-saving view)
                   (handler-case (accept 'boolean
                                  :prompt (format nil "Save buffer of view: ~a ?" (name view)))
                     (error () (progn (beep)
                                      (display-message "Invalid answer")
                                      (return-from frame-exit nil)))))
          (save-buffer (buffer view)))
      (file-error (e)
        (display-message "~A (hit a key to continue)" e)
        (read-gesture))))
  (when (or (notany #'buffer-of-view-needs-saving (views frame))
	    (handler-case (accept 'boolean :prompt "Modified buffers of views exist.  Quit anyway?")
	      (error () (progn (beep)
			       (display-message "Invalid answer")
			       (return-from frame-exit nil)))))
    (call-next-method)))

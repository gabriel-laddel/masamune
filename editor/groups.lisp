;;; Implementation of a groups concept.

(in-package :climacs-core)

(defvar *persistent-groups* (make-hash-table :test #'equal)
  "A hash table of groups that are persistent across invocations
of the Climacs editor. Typically, these do not designate concrete
pathnames, but contain more abstract designations such as \"all
files in the current directory\".")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; File/View group classes.

(defclass group (name-mixin)
  ())

(defclass group-element (group)
  ((%element :initarg :element :initform nil :reader element))
  (:documentation "Group class denoting a single element"))

(defclass standard-group (group)
  ((%elements :initarg :elements :initform nil :reader elements))
  (:documentation "Group class denoting a sequence of elements."))

(defclass current-view-group (group)
  ()
  (:documentation "Group class denoting the currently active
view."))

(defclass synonym-group (group)
  ((%other-name :initarg :other-name
                         :initform (error "The name of another group must be provided")
                         :reader other-name))
  (:documentation "Group class that forwards all methods to a
group with a specific name."))

(defclass custom-group (group)
  ((%list-pathnames-lambda
    :initarg :pathname-lister
    :initform (error "A custom group must have code for retrieving a list of pathnames")
    :reader pathname-lister)
   (%select-group-lambda
    :initarg :select-response
    :initform #'(lambda (&rest a)
                   (declare (ignore a)))
    :reader select-response)
   (%value-plist
    :initform nil
    :accessor value-plist))
  (:documentation "A group that will call a provided function
when it is selected or asked for pathnames."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; The group protocol.

(defgeneric group-views (group)
  (:documentation "Get a list of views in `group'. Only already
existing views will be returned, use `ensure-group-views' if
you want all views defined by the group."))

(defgeneric ensure-group-views (group)
  (:documentation "For each pathname in `group' that does not
have a corresponding view, open a view for that pathname."))

(defgeneric select-group (group)
  (:documentation "Tell the group object `group' that the user
has selected it. This method is responsible for setting the
active group. If `group' needs additional information, it should
query the user when this method is invoked. The standard method
should be sufficient for most group classes.")
  (:method ((group group))
    ;; Use a synonym group so that changes to the group of this name
    ;; will be reflected in the active group.
    (setf (active-group *application-frame*)
          (make-synonym-group group))))

(defgeneric display-group-contents (group stream)
  (:documentation "Display the contents of `group' to
`stream'. Basically, this should describe which views or files
would be affected by group-aware commands if `group' was the
active group. There is no standard format for the output, but it
is intended for displaying to the user."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Protocol implementation.

;; Display helper functions.
(defun normalise-group-element (element)
  "Turn `element' into either a pathname, an existing view or
NIL. If a pathname is returned, it is assumed to be safe to find
the file with that name."
  (typecase element
    (drei-view
     (find element (views *application-frame*)))
    ((or pathname string)
     (or (find-view-with-pathname (pathname element))
         (when (findablep element)
           element)))
    (group-element
     (normalise-group-element (element element)))))

(defun display-group-element (element stream)
  (let ((norm-element (normalise-group-element element)))
   (typecase norm-element
     (drei-view
      (present norm-element 'view stream))
     ((or pathname string)
      (present norm-element 'pathname stream)))))

;; Singular group elements.
(defmethod group-views ((group group-element))
  (let ((element (element group)))
    (cond ((and (typep element 'drei-view)
                (find element (views *application-frame*)))
           (list element))
          ((or (pathnamep element)
               (stringp element))
           (let ((view (find-view-with-pathname (pathname element))))
             (when view (list view))))
          (t '()))))

(defmethod ensure-group-views ((group group-element))
  (typecase (element group)
    (drei-view
     (unless (find (element group) (views *application-frame*))
       (ensure-open-file (pathname (filepath (element group))))))
    (pathname
     (ensure-open-file (element group)))
    (string
     (ensure-open-file (pathname (element group))))))

(defmethod display-group-contents ((group group-element) (stream extended-output-stream))
  (display-group-element (element group) stream))

;; Standard sequence groups.
(defmethod group-views ((group standard-group))
  (apply #'append (mapcar #'group-views (elements group))))

(defmethod ensure-group-views ((group standard-group))
  (mapcar #'ensure-group-views (elements group)))

(defmethod display-group-contents ((group standard-group) (stream extended-output-stream))
  (present (remove-if #'null (mapcar #'normalise-group-element (elements group)))
           '(sequence (or pathname view)) :stream stream))

;; The current view group (default).
(defmethod group-views ((group current-view-group))
  (list (current-view)))

(defmethod ensure-group-views ((group current-view-group))
  nil)

(defmethod display-group-contents ((group current-view-group) (stream extended-output-stream))
  (display-group-element (current-view) stream))

;; Custom groups.
(defmethod group-views ((group custom-group))
  (remove-if #'null (mapcar #'find-view-with-pathname (funcall (pathname-lister group) group))))

(defmethod ensure-group-views ((group custom-group))
  (mapcar #'ensure-open-file (funcall (pathname-lister group) group)))

(defmethod select-group ((group custom-group))
  (funcall (select-response group) group)
  (setf (active-group *application-frame*) group))

(defmethod display-group-contents ((group custom-group) (stream extended-output-stream))
  (present (remove-if #'null (mapcar #'normalise-group-element (funcall (pathname-lister group) group)))
           '(sequence (or pathname view)) :stream stream))

;; Synonym groups.

(define-condition group-not-found (simple-error)
  ((%group-name :reader group-name
                :initarg :group-name
                :initform (error "A name for the group must be provided")))
  (:report (lambda (condition stream)
	     (format stream "Group named ~a not found" (group-name condition))))
  (:documentation "This condition is signaled whenever a synonym
  group is unable to find the group that it is supposed to
  forward method invocations to."))

(defmethod group-views ((group synonym-group))
  (if (get-group (other-name group))
      (group-views (get-group (other-name group)))
      (error 'group-not-found :group-name (other-name group))))

(defmethod ensure-group-views ((group synonym-group))
  (if (get-group (other-name group))
      (ensure-group-views (get-group (other-name group)))
      (error 'group-not-found :group-name (other-name group))))

(defmethod select-group ((group synonym-group))
  (if (get-group (other-name group))
      (select-group (get-group (other-name group)))
      (error 'group-not-found :group-name (other-name group))))

(defmethod display-group-contents ((group synonym-group) stream)
  (if (get-group (other-name group))
      (display-group-contents (get-group (other-name group)) stream)
      (error 'group-not-found :group-name (other-name group))))

;; Util stuff.
(defun make-synonym-group (group)
  "Create and return a synonym group that refers to `group'. All
group protocol-specified methods called on the synonym group will
be forwarded to a group with the same name as `group'."
  (make-instance 'synonym-group
                 :other-name (name group)
                 :name (name group)))

(defun make-group-element (object)
  "Make a `group-element' object containg `object' as element."
  (make-instance 'group-element :element object))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Interface

(defun add-group (name elements)
  "Define a group called `name' (a string) containing the elements `elements',
which must be a list of pathnames and/or views, and add it to
the list of defined groups."
  (setf (gethash name (groups *application-frame*))
        (make-instance
         'standard-group
         :name name
         :elements (mapcar #'make-group-element elements))))

(defun get-group (name)
  "Return the group with the name `name'."
  (or (gethash name (groups *application-frame*))
      (gethash name *persistent-groups*)))

(defun get-active-group ()
  "Return the currently active group."
  (or (active-group *application-frame*)
      (deselect-group)))

(defun deselect-group ()
  "Deselect the currently active group."
  (setf (active-group *application-frame*)
        (make-instance 'current-view-group
                       :name "none")))

(defmacro with-group-views ((views group &key keep) &body body)
  "Make sure that all files designated by `group' are open in
views during the evaluation of `body'. If `keep' is NIL, all
views created by this macro will be saved and killed after
`body' has run. Also, `views' will be bound to a list of the
views containing the files designated by `group' while `body'
is run."
  (with-gensyms (views-before views-after view-diff)
    (once-only (group keep)
      `(let ((,views-before (views *application-frame*))
             (,group ,group))
         (ensure-group-views ,group)
         (let* ((,views-after (views *application-frame*))
                (,view-diff (set-difference ,views-after
                                                  ,views-before))
                (,views (group-views ,group)))
           (unwind-protect (progn ,@body)
             (unless ,keep
               (loop for view in ,view-diff
                  do (save-view view)
                  do (kill-view view)))))))))

(defmacro define-group (name (group-arg &rest args) &body body)
  "Define a persistent group named `name'. `Body' should return a
list of pathnames and will be used to calculate which files are
designated by the group. `Args' should be two-element lists, with
the first element bound to the result of evaluating the second
element. The second element will be evaluated when the group is
selected to be the active group by the user."
  (with-gensyms (group)
   (once-only (name)
     `(let ((,name ,name))
        (assert (stringp ,name))
        (setf (gethash ,name *persistent-groups*)
              (make-instance 'custom-group
                             :name ,name
                             :pathname-lister #'(lambda (,group)
                                                  (destructuring-bind
                                                        (&key ,@(mapcar #'(lambda (arg)
                                                                            `((,arg ,arg)))
                                                                        (mapcar #'first args)))
                                                      (value-plist ,group)
                                                    (let ((,group-arg ,group))
                                                      ,@body)))
                             :select-response #'(lambda (group)
                                                  (declare (ignorable group))
                                                  ,@(loop for (name form) in args
                                                       collect `(setf (getf (value-plist group) ',name) ,form)))))))))

(define-group "Current Directory Files" (group)
  (declare (ignore group))
  (directory (make-pathname :directory (pathname-directory (filepath (current-view)))
                            :name :wild
                            :type :wild)))

(define-group "Directory Files" (group (directory (accept 'pathname
                                                          :prompt "Directory"
                                                          :default (directory-of-buffer (buffer (current-view)))
                                                          :insert-default t)))
  (declare (ignore group))
  (directory (make-pathname :directory (pathname-directory directory)
                            :name :wild
                            :type :wild)))

(define-group "Directory Lisp Files" (group (directory (accept 'pathname
                                                               :prompt "Directory"
                                                               :default (directory-of-buffer (buffer (current-view)))
                                                               :insert-default t)))
  (declare (ignore group))
  (directory (make-pathname :directory (pathname-directory directory)
                            :name :wild
                            :type "lisp")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; CLIM interface stuff.

(define-presentation-method accept
    ((type group) stream view &key (default nil defaultp)
     (default-type type))
  (multiple-value-bind (object success string)
      (complete-input stream
		      (lambda (so-far action)
			(complete-from-possibilities
			 so-far
                         (append (loop for key being the hash-keys of (groups *application-frame*)
                                    collecting key)
                                 (loop for key being the hash-keys of *persistent-groups*
                                    collecting key))
                         '(#\Space)
                         :action action
			 :name-key #'identity
			 :value-key #'identity))
		      :partial-completers '(#\Space)
		      :allow-any-input nil)
    (cond (success
	   (values (get-group object) type))
	  ((and (zerop (length string)) defaultp)
           (values default default-type))
	  (t (values string 'string)))))

(define-presentation-method present (object (type group) stream view &key)
  (let ((name (name object)))
    (princ name stream)))

(define-presentation-method present ((object synonym-group) (type group) stream view &key)
  (if (get-group (other-name object))
      (present (get-group (other-name object)) type :stream stream :view view)
      (error 'group-not-found :group-name (other-name object))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Now hook it all up.

(defclass group-target-specification (view-list-target-specification)
  ((%group :initarg :group
           :reader group
           :initform (error "A group must be provided for a group target specification")))
  (:documentation "The target-specification class used for groups
in Climacs."))

(defmethod activate-target-specification ((spec group-target-specification))
  (ensure-group-views (group spec))
  (setf (views spec) (group-views (group spec)))
  (call-next-method))

(defmethod next-target :around ((spec group-target-specification))
  (handler-bind ((view-already-displayed
                  #'(lambda (c)
                      (declare (ignore c))
                      (invoke-restart 'remove-other-use))))
    (call-next-method)))

(defmethod previous-target :around ((spec group-target-specification))
  (handler-bind ((view-already-displayed
                  #'(lambda (c)
                      (declare (ignore c))
                      (invoke-restart 'remove-other-use))))
    (call-next-method)))

(setf *climacs-target-creator*
      #'(lambda (drei)
          (make-instance 'group-target-specification
           :group (get-active-group)
           :drei-instance drei)))

;;; Climacs-dependent functionality on stock lisp syntax

(in-package :drei-lisp-syntax)

(defmethod frame-clear-completions ((frame climacs-gui:climacs))
  (let ((completions-pane
         (when (typep *application-frame* 'esa-frame-mixin)
           (find "Completions" (windows *application-frame*)
                 :key #'pane-name
                 :test #'string=))))
    (unless (null completions-pane)
      (climacs-gui:delete-window completions-pane)
      (setf completions-pane nil))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Compiler note hyperlinking

(defclass location () ()
  (:documentation "The base for all locations."))

(defclass error-location (location)
  ((error-message :initarg :error-message :accessor error-message)))

(defclass actual-location (location)
  ((source-position :initarg :position :accessor source-position)
   (snippet :initarg :snippet :accessor snippet :initform nil))
  (:documentation "The base for all non-error locations."))

(defclass view-location (actual-location)
  ((view-name :initarg :view :accessor view-name)))

(defclass file-location (actual-location)
  ((file-name :initarg :file :accessor file-name)))

(defclass source-location (actual-location)
  ((source-form :initarg :source-form :accessor source-form)))

(defclass basic-position () ()
  (:documentation "The base for all positions."))

(defclass char-position (basic-position)
  ((char-position :initarg :position :accessor char-position)
   (align-p :initarg :align-p :initform nil :accessor align-p)))

(defun make-char-position (position-list)
  (make-instance 'char-position :position (second position-list)
                 :align-p (third position-list)))

(defclass line-position (basic-position)
  ((start-line :initarg :line :accessor start-line)
   (end-line :initarg :end-line :initform nil :accessor end-line)))

(defun make-line-position (position-list)
  (make-instance 'line-position :line (second position-list)
                 :end-line (third position-list)))

(defclass function-name-position (basic-position)
  ((function-name :initarg :function-name)))

(defun make-function-name-position (position-list)
  (make-instance 'function-name-position :function-name (second position-list)))

(defclass source-path-position (basic-position)
  ((path :initarg :source-path :accessor path)
   (start-position :initarg :start-position :accessor start-position)))

(defun make-source-path-position (position-list)
  (make-instance 'source-path-position :source-path (second position-list)
                 :start-position (third position-list)))

(defclass text-anchored-position (basic-position)
  ((start :initarg :text-anchored :accessor start)
   (text :initarg :text :accessor text)
   (delta :initarg :delta :accessor delta)))

(defun make-text-anchored-position (position-list)
  (make-instance 'text-anchored-position :text-anchored (second position-list)
                 :text (third position-list)
                 :delta (fourth position-list)))

(defclass method-position (basic-position)
  ((name :initarg :method :accessor name)
   (specializers :initarg :specializers :accessor specializers)
   (qualifiers :initarg :qualifiers :accessor qualifiers)))

(defun make-method-position (position-list)
  (make-instance 'method-position :method (second position-list)
                 :specializers (third position-list)
                 :qualifiers (last position-list)))

(defun make-location (location-list)
  (ecase (first location-list)
    (:error (make-instance 'error-location :error-message (second location-list)))
    (:location
     (destructuring-bind (l buf pos hints) location-list
       (declare (ignore l))
       (let ((location
              (apply #'make-instance
                     (ecase (first buf)
                       (:file 'file-location)
                       (:buffer 'view-location)
                       (:source-form 'source-location))
                     (case (first buf)
                       (:buffer (cons :view (rest buf)))
                       (t buf))))
             (position
              (funcall
               (ecase (first pos)
                 (:position #'make-char-position)
                 (:line #'make-line-position)
                 (:function-name #'make-function-name-position)
                 (:source-path #'make-source-path-position)
                 (:text-anchored #'make-text-anchored-position)
                 (:method #'make-method-position))
               pos)))
         (setf (source-position location) position)
         (when hints
           (setf (snippet location) (rest hints)))
         location)))))

(defmethod initialize-instance :after ((note compiler-note) &rest args)
  (declare (ignore args))
  (setf (location note) (make-location (location note))))

(defun show-note-counts (notes &optional seconds)
  (loop with nerrors = 0
     with nwarnings = 0
     with nstyle-warnings = 0
     with nnotes = 0
     for note in notes
     do (etypecase note
          (error-compiler-note (incf nerrors))
          (read-error-compiler-note (incf nerrors))
          (warning-compiler-note (incf nwarnings))
          (style-warning-compiler-note (incf nstyle-warnings))
          (note-compiler-note (incf nnotes)))
     finally
     (esa:display-message "Compilation finished: ~D error~:P ~
                            ~D warning~:P ~D style-warning~:P ~D note~:P ~
                            ~@[[~D secs]~]"
                          nerrors nwarnings nstyle-warnings nnotes seconds)))

(defun one-line-ify (string)
  "Return a single-line version of STRING.
Each newline and following whitespace is replaced by a single space."
  (loop with count = 0
     while (< count (length string))
     with new-string = (make-array 0 :element-type 'character :adjustable t
                                   :fill-pointer 0)
     when (char= (char string count) #\Newline)
     do (loop while (and (< count (length string))
                         (whitespacep (current-syntax) (char string count)))
           do (incf count)
           ;; Just ignore whitespace if it is last in the
           ;; string.
           finally (when (< count (length string))
                     (vector-push-extend #\Space new-string)))
     else
     do (vector-push-extend (char string count) new-string)
     (incf count)
     finally (return new-string)))

(defgeneric print-for-menu (object stream))

(defun print-note-for-menu (note stream severity ink)
  (with-accessors ((message message) (short-message short-message)) note
    (with-drawing-options (stream :ink ink
                                  :text-style (make-text-style :sans-serif :italic nil))
      (princ severity stream)
      (princ " " stream))
    (princ (if short-message
               (one-line-ify short-message)
               (one-line-ify message))
           stream)))

(defmacro def-print-for-menu (class name colour)
  `(defmethod print-for-menu ((object ,class) stream)
     (print-note-for-menu object stream ,name ,colour)))

(def-print-for-menu error-compiler-note "Error" +red+)
(def-print-for-menu read-error-compiler-note "Read Error" +red+)
(def-print-for-menu warning-compiler-note "Warning" +dark-red+)
(def-print-for-menu style-warning-compiler-note "Style Warning" +brown+)
(def-print-for-menu note-compiler-note "Note" +brown+)

(defun show-notes (notes view-name definition)
  (climacs-gui:with-typeout-view (stream (format nil "Compiler Notes: ~A ~A" view-name definition))
    (loop for note in notes
       do (with-output-as-presentation (stream note 'compiler-note)
            (print-for-menu note stream))
       (terpri stream)
       count note into length
       finally (change-space-requirements stream
                :height (* length (stream-line-height stream)))
       (scroll-extent stream 0 0))))

(defgeneric goto-location (location))

(defmethod goto-location ((location error-location))
  (esa:display-message (error-message location)))

(defmethod goto-location ((location view-location))
  (let ((view (find (view-name location)
                    (climacs-gui:views *esa-instance*)
                    :test #'string= :key #'name)))
    (unless view
      (esa:display-message "No view ~A" (view-name location))
      (beep)
      (return-from goto-location))
    (climacs-core:switch-to-view (current-window) view)
    (goto-position (point)
                   (char-position (source-position location)))))

(defmethod goto-location ((location file-location))
  (let ((view (find (file-name location)
                      (views *application-frame*)
                      :test #'string= :key #'(lambda (view)
                                               (when (typep view 'drei-buffer-view)
                                                 (let ((path (filepath (buffer view))))
                                                   (when path
                                                     (namestring path))))))))
    (if view
        (climacs-core:switch-to-view (current-window) view)
        (find-file (file-name location)))
    (goto-position (point (current-view))
                   (char-position (source-position location)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Macroexpansion and evaluation

(defun macroexpand-token (syntax token &optional (all nil))
  (with-syntax-package (syntax (start-offset token))
    (let* ((string (form-string syntax token))
           (expression (read-from-string string))
           (expansion (macroexpand-for-drei (get-usable-image syntax)
                                               expression
                                               all))
           (expansion-string (with-output-to-string (s)
                               (pprint expansion s))))
      (let ((view (climacs-core:switch-or-move-to-view (current-window) "*Macroexpansion*")))
        (set-syntax view "Lisp"))
      (let ((header-string (one-line-ify (subseq string 0
                                                 (min 40 (length string))))))
        (end-of-buffer (point))
        (unless (beginning-of-buffer-p (point))
          (insert-object (point) #\Newline))
        (insert-sequence (point)
                         (format nil ";;; Macroexpand-~:[1~;all~] ~A...~%"
                                 all header-string))
        (insert-sequence (point) expansion-string)
        (insert-object (point) #\Newline)))))

(defun compile-definition-interactively (view mark)
  (let* ((syntax (syntax view))
         (token (definition-at-mark syntax mark))
         (string (form-string syntax token))
         (m (clone-mark mark))
         (*read-base* (base syntax)))
    (with-syntax-package (syntax mark)
      (forward-definition m syntax 1 nil)
      (if (backward-definition m syntax 1 nil)
          (multiple-value-bind (result notes)
              (compile-form-for-drei (get-usable-image syntax)
                                     (form-to-object syntax token
                                      :read t
                                      :package (package-at-mark syntax mark))
                                     view m)
            (show-note-counts notes (second result))
            (when (not (null notes))
              (show-notes notes (name view)
                          (one-line-ify (subseq string 0 (min (length string) 20))))))
          (display-message "No definition at point")))))

(defun compile-file-interactively (view &optional load-p)
  (let ((buffer (buffer view)))
    (cond ((null (filepath buffer))
           (esa:display-message "View ~A is not associated with a file" (name view)))
          (t
           (when (and (needs-saving buffer)
                      (accept 'boolean :prompt (format nil "Save buffer ~A ?" (name view))))
             (climacs-core:save-buffer buffer))
           (let ((*read-base* (base (syntax view))))
             (multiple-value-bind (result notes)
                 (compile-file-for-drei (get-usable-image (syntax view))
                                        (filepath buffer)
                                        (package-at-mark (syntax view) 0) load-p)
               (show-note-counts notes (second result))
               (when notes (show-notes notes (name view) ""))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Definition editing

(defparameter *find-definition-stack* '())

(defun pop-find-definition-stack ()
  (unless (null *find-definition-stack*)
    (let* ((offset+view (pop *find-definition-stack*))
           (offset (first offset+view))
           (view (second offset+view)))
      (if (find view (views *esa-instance*))
          (progn (climacs-core:switch-to-view (current-window) view)
                 (goto-position (point) offset))
          (pop-find-definition-stack)))))

;; KLUDGE: We need to put more info in the definition objects to begin
;; with.
(defun definition-type (definition)
  (let ((data (read-from-string (first definition))))
    (case (first data)
      ((cl:defclass)
       'cl:class)
      ((cl:defgeneric
           cl:defmethod
           cl:defun
         cl:defmacro)
       'cl:function)
      (t t))))

(defvar *local-function-definers* '(flet labels macrolet)
  "A list of macros that define local functions, as per
`find-local-definition.")

(defun find-local-definition (syntax symbol-form)
  "Return a form locally defining `symbol-form' as a
function (explicitly via `flet' or `labels', does not expand
macros or similar). If no such form can be found, return NIL."
  (labels ((locally-binding-p (form)
             (when (form-operator form)
               (find-if #'(lambda (symbol)
                            (form-equal syntax (form-operator form) (string symbol)))
                        *local-function-definers*)))
           (match (form-operator)
             (when form-operator
               (form-equal syntax form-operator symbol-form)))
           (find-local-binding (form)
             (when form
               (or (when (locally-binding-p form)
                     (loop for binding in (form-children (first (form-operands form)))
                        when (and (form-list-p binding)
                                  (match (form-operator binding)))
                        return binding))
                   (unless (form-at-top-level-p form)
                     (find-local-binding (parent form)))))))
    (find-local-binding (list-at-mark syntax (start-offset symbol-form)))))

(defun edit-definition (symbol &optional type)
  (let ((all-definitions (find-definitions-for-drei
                          (get-usable-image (when (syntax-view-p (current-view))
                                              (current-syntax)))
                          symbol)))
    (let ((definitions (if (not type)
                           all-definitions
                           (remove-if-not #'(lambda (definition)
                                              (eq (definition-type definition) type))
                                          all-definitions))))
      (cond ((null definitions)
             (esa:display-message "No known definitions for: ~A" symbol)
             (beep))
            (t
             (goto-definition symbol definitions))))))

(defun goto-definition (name definitions)
  (when (point-mark-view-p (current-view))
    (push (list (offset (point)) (current-view)) *find-definition-stack*))
  (cond ((null (cdr definitions))
         (let* ((def (car definitions))
                (xref (make-xref def)))
           (goto-location xref)))
        (t
         (let ((xref (show-definitions name definitions)))
           (when xref (goto-location xref))))))

(defclass xref ()
  ((dspec :initarg :dspec :accessor dspec)
   (location :initarg :location :accessor location)))

(defun make-xref (xref-list)
  (destructuring-bind (dspec location) xref-list
    (make-instance 'xref
                   :dspec dspec
                   :location (make-location location))))

(defmethod goto-location ((xref xref))
  (goto-location (location xref)))

(defun show-definitions (name definitions)
  (show-xrefs (loop for xref-list in definitions
                 collect (make-xref xref-list))
              'definition name))

(defun show-xrefs (xrefs type symbol)
  (cond ((null xrefs)
         (esa:display-message "No references found for ~A." symbol)
         (beep))
        (t
         (flet ((printer (item stream)
                  (with-drawing-options (stream :ink +dark-blue+
                                                :text-style (make-text-style :fixed nil nil))
                    (princ (dspec item) stream))))
           (climacs-gui:with-typeout-view (stream (format-sym "~A ~A" type symbol))
             (loop for xref in xrefs
                do (with-output-as-presentation (stream xref 'xref)
                     (printer xref stream))
                (terpri stream)
                count xref into length
                finally (change-space-requirements stream
                         :height (* length (stream-line-height stream)))
                (scroll-extent stream 0 0)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Some group support

;; WARNING, using this group can be dangerous, as Climacs is not
;; really suited to opening up a large amount of buffers that each
;; require a full syntax reparse.
(climacs-core:define-group "ASDF System Files" (group (system (asdf:find-system (accept 'symbol :prompt "System"))))
  (declare (ignore group))
  (when system
    (mapcar #'asdf:component-pathname
            (remove-if-not (lambda (c)
                             (typep c 'asdf:cl-source-file))
                           (asdf:module-components system)))))

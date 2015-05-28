(in-package :parenscript.asdf)

(defvar *parenscript-file-extension* "paren")
(defvar *javascript-file-extension* "js")

;;; ASDF manual: http://constantly.at/lisp/asdf/index.html

(defun slurp-file-3000 (pathname)
  "A SLURP-FILE function inspired Mr. Insane 3000's
     SLURP-STREAM4."
  (with-open-file (strm pathname)
    (let ((string (make-string (file-length strm))))
      (read-sequence string strm)
      string)))


;;; a parenscript file is a source file:
;;; A source file is any file that the system does not know how to generate
;;; from other components of the system.
(defclass asdf::parenscript-file (asdf:source-file)
  ())

(defclass asdf::javascript-file (asdf:source-file)
  ())

(defclass asdf::parenscript-compile-op (asdf:operation)
  ((output-spec
    :initarg :output-spec   :initform :javascript :accessor output-spec)
   (comp-env
    :initarg :comp-env      :initform nil         :accessor comp-env
    :documentation "Compilaiton environment to use to compile.")
   (pretty-print-p
    :initarg :pretty-print  :initform nil         :accessor pretty-print-p
    :documentation "T if Javascript should be printed readably.")
   (output-stream
    :initarg :output-stream :initform *standard-output* :accessor output-stream
    :documentation "The output stream in which to print the Javascript output.")
   (force-p 
    :initarg :force-p :initform nil :accessor force-p
    :documentation "T to force compilation."))
  (:documentation "The operation used in conjunction with parenscript:compile-script-system."))

;;;; OUR CUSTOM PARENSCRIPT COMPILATION
(defmethod output-files ((op asdf::parenscript-compile-op) general-component)
  "Parenscript compilation does not, in general, produce output files."
  nil)

(defmethod perform ((op asdf::parenscript-compile-op) general-component)
  "Do the usual on some non-specific component."
  (call-next-method))

(defmethod perform ((op asdf::parenscript-compile-op) (file asdf:source-file))
  "Do nothing on a source (non-compound) component."
  nil)

(defmethod perform ((op asdf::parenscript-compile-op) (file asdf::javascript-file))
  "Include the javascript in the output stream."
  (write-string (slurp-file-3000 (component-pathname file)) (output-stream op)))


(defmethod perform ((op asdf::parenscript-compile-op) (file asdf::parenscript-file))
  (paren-files:compile-script-file 
   (component-pathname file)
   :comp-env (comp-env op)
   :output-spec (output-spec op)
   :pretty-print (pretty-print-p op)
   :output-stream (output-stream op))
  (write-char #\Newline (output-stream op)))

(defmethod operation-done-p ((op asdf::parenscript-compile-op) general-component)
  (call-next-method)
  nil)

(defmethod operation-done-p ((op asdf::parenscript-compile-op) (file asdf::parenscript-file))
  (and (not (force-p op))
       (call-next-method)))

(defmethod operation-done-p ((op asdf::parenscript-compile-op) (file asdf::javascript-file))
  (and (not (force-p op))
       (call-next-method)))



;;; FIXME: we simply copy load-op's dependencies.  this is Just Not Right.
;; compiling a script system requires compiling the lisp system as well
(defmethod asdf:component-depends-on ((op asdf::parenscript-compile-op) (c component))
  (let ((what-would-load-op-do (cdr (assoc 'load-op
                                           (slot-value c 'asdf::in-order-to)))))
    (mapcar (lambda (dep)
              (if (eq (car dep) 'load-op)
                  (cons 'asdf::parenscript-compile-op (cdr dep))
                  dep))
            what-would-load-op-do)))

(defmethod asdf:component-depends-on ((op asdf::parenscript-compile-op) (system system))
  "Returns the list of operations the system depends on to ps-compile.  We requrie that the
lisp system be loaded first."
  (cons (list 'asdf:load-op system) (call-next-method)))

;;;; STANDARD LISP COMPILATION -- currently does nothing
;;; file extension for parenscript files is ".paren"
;;; e.g. (defmethod source-file-type ((c cl-source-file) (s module)) "lisp")
(defmethod asdf:source-file-type ((c asdf::parenscript-file) (s asdf:module))
  (declare (ignore c) (ignore s))
  *parenscript-file-extension*)

(defmethod asdf:source-file-type ((c asdf::javascript-file) (s asdf:module))
  (declare (ignore c) (ignore s))
  *javascript-file-extension*)

;;; when you compile the system, compile the Parenscript files in it.
(defmethod asdf:perform ((op compile-op) (paren-file asdf::parenscript-file))
;  (parenscript:compile-parenscript-file (component-pathname paren-file)))
  )

(defmethod asdf:perform ((op compile-op) (file asdf::javascript-file))
;  (parenscript:compile-parenscript-file (component-pathname paren-file)))
  )

;;; when you load the system, do nothing with the parenscript files.  This could
;;; be enhanced so that files are automatically installed into the appropriate web
;;; framework, etc.  for now we do nothing.
(defmethod asdf:perform ((op load-op) (paren-file asdf::parenscript-file))
  nil)

(defmethod asdf:perform ((op load-op) (file asdf::javascript-file))
  nil)

(defun compile-script-system (system 
			      &key
			      (pretty-print t)
			      (output-spec :javascript)
			      (output-to-stream t)
			      (output-stream *standard-output*)
			      output-to-files ;; currently ignored
			      comp-env)
  "Compiles a collection of parenscripts as described by an ASDF system into files or
a specified output stream."
  (declare (ignore output-to-files output-to-stream))
  (asdf:operate 'asdf::parenscript-compile-op system
		:output-spec output-spec
		:pretty-print pretty-print
;		:output-to-stream t
		:output-stream output-stream
		:comp-env comp-env
		:force-p t
		))
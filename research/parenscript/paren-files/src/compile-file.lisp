(in-package :paren-files)


(defmacro with-ps-compilation-environment ((env) &body body)
  (let ((%env (gensym "env")))
  `(let ((,%env ,env))
     (declare (ignore ,%env))
     ,@body)))
     

(defmacro with-toplevel-compilation ((env) &body body)
  (let ((%env (gensym "env")))
  `(let ((,%env ,env))
     (declare (ignore ,%env))
     ;; (setf (ps::comp-env-compiling-toplevel-p comp-env) t)
     ,@body)))

(defun read-parenscript-form (&optional (stream *standard-input*) eof-error-op eof-value recursivep)
    "Reads a parenscript form from a stream.  Use instead of READ so we can replace the reader
with a parenscript reader, if necessary."
    #+parenscript-reader
    (parenscript.reader:read stream eof-error-op eof-value recursivep)
    #-parenscript-reader
    (read stream eof-error-op eof-value recursivep))

(defun compile-script-file (source-file
			    &key
			    (output-spec :javascript)
			    comp-env
			    (pretty-print t)
			    (output-stream *standard-output*))
  "Compiles the given Parenscript source file and outputs the results
to the given output stream."
  (declare (ignore output-spec pretty-print))
  (with-ps-compilation-environment (comp-env)
    (with-toplevel-compilation (comp-env)
      (let ((ps::*read-function* #'read-parenscript-form))
	(write-string (ps:ps-compile-file source-file) output-stream)))))


(defun compile-script-file-to-string (source-file &rest rest)
  "Compile SOURCE-FILE (a parenscript file) to a javascript string. (in-package ...) forms
behave as expected and all other forms are evaluated according to the value of
EVAL-FORMS-P. If the result of the evaluation is not nil then it's compiled with
js:js* and written to the output."
  (with-output-to-string (stream)
    (apply 'compile-script-file source-file :output-stream stream rest)))
  
(defun compile-script-file-to-js-file (source-file &rest args &key destination-file &allow-other-keys)
  "Compile SOURCE-FILE (a parenscript file) to a javascript file with
compile-parenscript-file-to-string. When DESTINATION-FILE is omitted,
then it will be named the same as SOURCE-FILE but with js extension."
  (setf args (copy-list args))
  (remf args :destination-file)
  (unless destination-file
    (setf destination-file (merge-pathnames (make-pathname :type "js")
                                            source-file)))
  (with-open-file (output destination-file :if-exists :supersede :direction :output)
    (write-string (apply #'compile-script-file-to-string source-file args) output)))

(defun compile-script-system (system &rest rest)
  (apply #'parenscript.asdf:compile-script-system system rest))
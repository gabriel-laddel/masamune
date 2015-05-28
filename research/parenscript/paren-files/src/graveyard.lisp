
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


(in-package :paren-util)

(defpsmacro in-package (package-designator)
    `(eval-when (:compile-toplevel)
       (setf cl:*package* (cl:find-package ,package-designator))))

(defpsmacro use-package (package-designator &optional into-package)
    (let ((all-args (cons package-designator (if into-package (cons into-package nil) nil))))
      `(eval-when (:compile-toplevel)
	 (use-package ,@all-args))))

;(defpsmacro funcall (fun &rest args)
;    `(,fun ,@args))

(defpsmacro methcall (method this-obj &rest args)
    `((slot-value ,this-obj ,method) ,@args))

(defpsmacro methcall* (methods this-obj &rest args)
    `((slot-value ,this-obj ,@methods) ,@args))

(defpsmacro with-arguments-array ((args-var) &body body)
    `(let ((,args-var (to-array js-global::arguments)))
       ,@body))

(defpsmacro defaultf (place default-val)
    `(setf ,place (if (ps:!== ,place ps:undefined)
		      ,place
		      ,default-val)))



;;;; main.lisp
;;;
;;; Defines the MAIN function which is the entry-point for the delivered binary.
;;;
;;; Copyright (c) 2006 James Wright
;;; See LICENSE for full licensing details.
;;;
(in-package :js-parser)

(defun command-line-arguments ()
  "return a list of the command-line arguments"
  #+sbcl sb-ext:*posix-argv*
  #+lispworks system:*line-arguments-list*
  #-(or sbcl lispworks) (error "Lispworks and SBCL are the only currently-supported compilers for binary creation"))
  
(defun main ()
  "This is the main entry-point for the js-on-cl binary."
  (show-banner)
  (handler-case 
      (multiple-value-bind (template output prefix-lookup bundle-mode compress-mode runtime target)
          (decode-arguments)
        (let ((build-args (list target)))
          (unless target
            (format *error-output* "~&js-on-cl: No target specified~%")
            (show-usage)
            (return-from main 255))
          (format t "~&Main source file:  ~A" target)
          (when template
            (format t "~&~%Template URI path: ~A" template)
            (format t "~&Template file:     ~A" (truename (resolve-import-uripath target template prefix-lookup)))
            (setf build-args (append build-args (list :template-uripath template))))
          (when runtime
            (format t "~&~%Runtime URI path:  ~A" runtime)
            (format t "~&Runtime file:      ~A" (truename (resolve-import-uripath target runtime prefix-lookup)))
            (setf build-args (append build-args (list :runtime-uripath runtime))))
          (when output
            (format t "~&~%Output URI path:   ~A" output)
            (format t "~&Output file:       ~A" (resolve-import-uripath target output prefix-lookup))
            (setf build-args (append build-args (list :output-uripath output))))
          (unless (eq :default bundle-mode)
            (format t "~&Bundle mode:       ~A" (if bundle-mode "on" "off"))
            (setf build-args (append build-args (list :combine-mode bundle-mode))))
          (unless (eq :default compress-mode)
            (format t "~&Compress mode:     ~A" (if compress-mode "on" "off"))
            (setf build-args (append build-args (list :compress-mode compress-mode))))

          (apply 'build-app build-args)
          (format t "~&~%Done.~%~%")
          0))
    (condition (c)
       (let ((*print-escape* nil))
         (format *error-output* "~&js-on-cl: ")
         (print-object c *error-output*)
         (terpri *error-output*)
         254))))

(defun decode-arguments ()
  "Decode the command-line arguments and return the resulting option values"
  (let (template output prefix-lookup (bundle-mode :default) (compress-mode :default) runtime target)
    (do* ((arg-cell (cdr (command-line-arguments)) (cddr arg-cell))
          (arg-name (car arg-cell) (car arg-cell))
          (arg-value (cadr arg-cell) (cadr arg-cell)))
         ((null arg-cell))
      (cond
        ((string= "-t" arg-name)
         (setf template arg-value))
        ((string= "-o" arg-name)
         (setf output arg-value))
        ((string= "-r" arg-name)
         (setf runtime arg-value))
        ((string= "-p" arg-name)
         (setf prefix-lookup (parse-prefix-lookup arg-value)))
        ((string= "-b" arg-name)
         (setf bundle-mode (boolean-arg arg-name arg-value)))
        ((string= "-c" arg-name)
         (setf compress-mode (boolean-arg arg-name arg-value)))
        ((null arg-value)
         (setf target (truename arg-name)))
        (t
         (show-usage)
         (error "Unrecognized option '~A'" arg-name))))
      (values template output prefix-lookup bundle-mode compress-mode runtime target)))

(defun boolean-arg (name val)
  "Converts an argument to a boolean option to a boolean value"
  (let ((true-args '("t" "true" "on" "yes"))
        (false-args '("nil" "false" "off" "no")))
    (cond
      ((find val true-args :test 'string-equal)
       t)
      ((find val false-args :test 'string-equal)
       nil)
      (t
       (error "The argument to the ~A option must be one of: ~A" name (append true-args false-args))))))

(defun parse-prefix-lookup (raw-str)
  "Takes the argument to the prefix lookup command line option and parses it into a
   prefix-lookup table."
  (flet ((parse-cell (cell-str)
           (let* ((components (split "=+" cell-str))
                  (uri (first components))
                  (path (second components)))

             ;; Validate input
             (assert (= 2 (length components)) nil
                     "Ill-formed lookup entry ~S" cell-str)
             (assert (char= #\/ (aref uri 0)) (uri)
                     "'~A' is not an absolute URI path" uri)

             ;; We'll silently add trailing slashes if required
             (when (char/= #\/ (aref uri (1- (length uri))))
               (setf uri (format nil "~A/" uri)))
             (when (and (char/= #\\ (aref path (1- (length path))))
                        (char/= #\/ (aref path (1- (length path)))))
               (setf path (format nil "~A/" path)))
             
             (cons uri (parse-namestring path)))))
    (mapcar #'parse-cell (split ";" raw-str))))
      
(defun show-banner ()
  (format t "~%~%~
             ===============================================================================~%~
             js-on-cl - Javascript With Advanced Continuation Support~%~
             version: ~A~%~
             -------------------------------------------------------------------------------"
          jw-system:*version*)
  (write-line "" *standard-output*))

(defun show-usage ()
  (let ((foo (namestring #P"/static-web/foo/"))
        (bar (namestring #P"/contrib/bar/")))
    (format t "~&Usage: ~A [options] <main_source_file>~%~
               ~%Options:~
               ~%   -t <uri-path>  URI-path of the template file to use.  Default: the name of~
               ~%                  the main source file, with new extension \".template\".~
               ~%   -r <uri-path>  URI-path of the runtime script to use.  Default: \"jw-rt.js\".~
               ~%   -o <uri-path>  URI-path of the output file to create.  Default: the name~
               ~%                  of the main source file with new extension \".html\".~
               ~%   -p <uri-path>=<directory>[;<uri-path>=<directory> ...]~
               ~%                  Specifies the mapping between absolute URI paths and the~
               ~%                  filesystem.~
               ~%                  eg: -i /foo/=~A;/foo/bar/=~A~
               ~%                  maps all absolute URI paths under /foo/ to ~A,~
               ~%                  except for those under /foo/bar/ which are searched for~
               ~%                  in ~A.~
               ~%   -c on|off      Turn compress mode on or off.  When on, unnecessary whitespace~
               ~%                  will be omitted in output Javascript.  When off, indentation~
               ~%                  and comments will be preserved.  Defaults to on.~
               ~%   -b on|off      Turn bundle mode on or off.  When on, all output Javascript~
               ~%                  files will be combined into a single file.  When off, each~
               ~%                  js-on-cl file will be compiled into a seperate Javascript file,~
               ~%                  and each imported Javascript file will be linked to separately.~
               ~%                  Defaults to on.
               ~%~%"
            jw-system:*executable-name* foo bar foo bar)))

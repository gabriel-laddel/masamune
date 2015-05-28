;;; -*- lexical-binding: t -*-
(defalias 'dec '1-)
(defalias 'drop '-drop)
(defalias 'drop-while '-drop-while)
(defalias 'ffirst 'caar)
(defalias 'filter 'remove-if-not)
(defalias 'flatten '-flatten)
(defalias 'inc '1+)
(defalias 'keys 'mm:keys)
(defalias 'llet 'lexical-let*)
(defalias 'ls 'directory-files)
(defalias 'mapcat '-mapcat)
(defalias 'now 'current-time-string)
(defalias 'p1 'prin1-to-string)
(defalias 'partial '-partial)
(defalias 'rotate '-rotate)
(defalias 'take '-take)
(defalias 'take-while '-take-while)
(defalias 'uuid 'uuid-string)
(defalias 'vals 'mm:vals)

(defmacro* comment (&rest body) nil)

(defmacro clambda (&rest body)
  `(cl-function (lambda ,@body)))

(defun uuid-symbol ()
  (make-symbol (uuid-string)))

(defun llast (l) 
  (etypecase l
    (cons (car (last l)))
    (string (subseq l (dec (length l)) (length l)))))

(defun empty? (x)
  (= 0 (length x)))

(defun mm:keys (pl)
  (assert (evenp (length pl)))
  (loop for e in pl
	for k = 1 then (if (= 0 k) 1 0)
	when (= 1 k)
	collect e))

(defun mm:vals (pl)
  (loop for e in pl
	for k = 0 then (if (= 0 k) 1 0)
	when (= 1 k)
	collect e))

(defun* cat (&rest args) 
  (apply #'concatenate 'string (mapcar (lambda (x) (if (stringp x) x (p1 x))) args)))

(defun even? (integer)
  (when (eq (logand integer 1) 0) integer))

(defun odd? (integer)
  (when (not (eq (logand integer 1) 0)) integer))

(defun every? (pred coll)
  (not (clj:some pred coll)))

(defun identity (x) x)

(defun current-line (&optional properties?)
  (thing-at-point 'line t))

(defun line-number () (count-lines 1 (point)))

(defun slurp (file)
  "FILE contents as string"
  (with-temp-buffer
    (insert-file-contents (expand-file-name file))
    (buffer-string)))

(defun buffer-string-no-properties ()
  (buffer-substring-no-properties (point-min) (point-max)))

(defalias 'bsnp 'buffer-string-no-properties)

(defun buffer-name-list ()
  (mapcar #'buffer-name (buffer-list)))

(defun kill-buffer* (name)
  "When buffer exists, kill it."
  (awhen (some (lambda (b) (when (equal (buffer-name b) name) b)) (buffer-list))
    (kill-buffer it)))

(defun region ()
  (when mark-active
    (buffer-substring (region-beginning) (region-end))))

(defun region-no-properties ()
  (when mark-active
    (buffer-substring-no-properties (region-beginning) (region-end))))

(defun* line-number-and-sexp ()
  (list (read (current-buffer))
	(save-excursion (slime-beginning-of-defun) (1+ (line-number)))))

(defun* file-sexps (file)
  (llet ((out))  
    (with-temp-file file
      (save-excursion (beginning-of-buffer)
		      (condition-case nil
			  (while t
			    (push (read (current-buffer)) out))
			(error nil))))
    (nreverse out)))

(defun query-string-encode (s)
  (replace-regexp-in-string "[ ]+" "+" s))

(defun mm:dissoc (pl &rest ks)
  (assert (even? (length pl)))
  (loop for k in ks
	for i = (position k pl)
	do (setf pl (append (take i pl) (drop (+ i 2) pl)))
	finally (return pl)))

(defun mm:update (pl k f)
  (llet ((new-val (funcall f (mm:getf pl k))))
    (mm:assoc (mm:dissoc pl k) new-val)))

(defun mm:assoc (pl &rest kvs)
  (assert (and (even? (length pl)) (even? (length kvs))))
  (append kvs pl))

(defun mm:getf (plist sym)
  (loop for elm in plist
	for i = 0 then (1+ i)
	when (equal (p1 elm) (p1 sym))
	return (nth (1+ i) plist)))

(defun getf-in (nested-plist &rest keys)
  (iter (for k in keys)
	(for lv initially nested-plist then (getf l k))
	(finally return lv)))

(defun select-keys (keys pl)
  (loop for k in keys append (list k (mm:getf pl k))))

(defun* range (n)
  (loop for i from 0 below n collect i))

(defun read-sexps-from (file)
     "Sexps are returned in a list. No comments."
     (llet (out)
       (with-temp-buffer
	 (insert-file-contents (expand-file-name file))
	 (emacs-lisp-mode)
	 (beginning-of-buffer)
	 (condition-case nil
	     (while t (push (read (current-buffer)) out))
	   (error nil))
	 (kill-buffer (current-buffer)))
       (reverse out)))

(defun line-count (&optional buffer)
  "Without buffer, returns line count for `current-buffer'"
  (with-current-buffer (or buffer (current-buffer))
    (save-excursion (end-of-buffer) (line-number-at-pos))))

(defun buffer-around? (buffer-or-name)
  (awhen (if (stringp buffer-or-name)
	     (member buffer-or-name (buffer-name-list))
	   (member buffer-or-name (buffer-list)))
    (car it)))

(defsubst curry (function &rest arguments)
  (lexical-let ((function function)
                (arguments arguments))
    (lambda (&rest more) (apply function (append arguments more)))))

(defsubst rcurry (function &rest arguments)
  (lexical-let ((function function)
                (arguments arguments))
    (lambda (&rest more) (apply function (append more arguments)))))

(defsubst compose (function &rest more-functions)
  (cl-reduce (lambda (f g)
               (lexical-let ((f f) (g g))
                 (lambda (&rest arguments)
                   (funcall f (apply g arguments)))))
             more-functions
             :initial-value function))

(defun iso-now ()
  "Insert current date-time string in full ISO 8601 format. Example:
2010-11-29T23:23:35-08:00
see http://en.wikipedia.org/wiki/ISO_8601 for more info"
  (interactive)
  (when (use-region-p)
    (delete-region (region-beginning) (region-end)))
  (concat
   (format-time-string "%Y-%m-%dT%T")
   ((lambda (x) (concat (substring x 0 3) ":" (substring x 3 5)))
    (format-time-string "%z"))))

(defun set-volume (n)
  (interactive "nVolume, 70 min, 0 max:")
  (shell-command-to-string (cl-format nil "amixer -c 0 -- sset Master playback -~ddB" n)))

(defun launch-maxima ()
  "XXX the maxima init file (for whatever reason) wasn't loading correctly and I
wasn't getting any debug info. yes this is a hack, andh yes, it needs to be
fixed, but the *truely correct* way to go about it would be to modify maxima so
it is quickload-able. I'm willing to wait for that."
  (interactive)
  (alert "please wait ~~5 seconds while SLIME connects to Maxima")
  (let* ((swank-port 4007))
    (when (buffer-around? "*Async Shell Command*")
      (with-current-buffer "*Async Shell Command*" (rename-uniquely)))
    (async-shell-command "maxima --enable-lisp-debugger")
    (sleep-for 1)
    (with-current-buffer "*Async Shell Command*"
      (end-of-buffer)
      (insert "to_lisp();")
      (comint-send-input)
      (sleep-for 1)
      (end-of-buffer)
      (insert "(load \"~/quicklisp/setup.lisp\")")
      (comint-send-input)
      (sleep-for 1)
      (insert "(ql:quickload 'swank)")
      (comint-send-input)
      (sleep-for 1)
      (insert (cat "(swank:create-server :port " swank-port " :style swank:*communication-style* :dont-close t)"))
      (comint-send-input)
      (sleep-for 2))
    (call-interactively 'slime-connect)))

(defun string/reverse (str)
  "Reverse the str where str is a string"
  (apply #'string 
	 (reverse 
	  (string-to-list str))))

(defun buffer-with-filename (filename)
  (find-if (lambda (buffer) (equal (expand-file-name filename) (with-current-buffer buffer buffer-file-name)))
	   (buffer-list)))

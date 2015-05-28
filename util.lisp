(in-package #:mm)

(defun cistring= (s1 s2 &key (start1 0) end1 (start2 0) end2)
  "[c]ase [i]nsentive string="
  (string= (string-downcase (subseq s1 start1 end1))
	   (string-downcase (subseq s2 start2 end2))))

(defun g!-symbol-p (s)
  "case insensitive"
  (and (symbolp s)
       (> (length (symbol-name s)) 2)
       (cistring= "!g" (symbol-name s) :end2 2)))

(defmacro m (name args &rest body)
  (let* ((syms (remove-duplicates (remove-if-not 'g!-symbol-p (flatten body)))))
    `(defmacro ,name ,args 
       (let* ,(mapcar (lambda (s) `(,s (gensym ,(subseq (symbol-name s) 2)))) syms)			      
	 ,@body))))

(defmacro defalias (new old)
  `(defun ,new (&rest args) (apply #',old args)))

(defalias i make-instance)
(defalias l list)
(defalias ^ expt)

(defvar *alphabet* "abcdefghijklmnopqrstuvwxyz")

(defun scans (regex target-string)
  (let*  ((matches) (current-start-pos 0))
    (awhile (multiple-value-bind (match-start match-end)
		(scan regex target-string :start current-start-pos)
	      (when match-start
		(push (list match-start match-end) matches)
		(setq current-start-pos match-end))))
    (nreverse matches)))

(defun drop-nth (n l)
  (assert (< n (length l)))
  (if (= n 0) (rest l)
      (append (take n l) (drop (1+ n) l))))

(defun convert-image-file (file-pathname new-format)
  "TODO convert from any image format to another. currently uses imagemagicks's
convert script. new-format can be specifed as a string or keyword due to the
semantics of `format'"
  (uiop:run-program (format nil "convert ~a ~a~a.~a"
			    (namestring file-pathname)
			    (cat "/" (reduce (lambda (s1 s2) (cat s1  "/" s2)) (rest (pathname-directory file-pathname))) "/")
			    (pathname-name file-pathname) 
			    new-format) :output :string))

(defun squash (operator &rest ls)
  (assert (every (lambda (l) (= (length l) (length (car ls)))) ls))
  (iter (for i from 0 to (1- (length (car ls))))
    (collect (apply operator (mapcar (lambda (l) (nth i l)) ls)))))

(defun slurp-stream (stream)
  ;; TODO 2015-01-20T20:08:01+00:00 Gabriel Laddel
  ;; combine this and `slurp-file' into `%string' or some such
  (the string
       (let ((seq (make-array (etypecase stream
				(string-stream (SB-IMPL::STRING-INPUT-STREAM-END stream))
				(file-stream (file-length stream))) :element-type 'character :fill-pointer t)))
	 (setf (fill-pointer seq) (read-sequence seq stream))
	 seq)))

;; (slot-value (socket-stream socket) 'sb-impl::char-pos)

(defun slurp-file (pathname)
  (with-open-file (stream pathname :direction :input)
    (slurp-stream stream)))

(defun read-file (pathname)
  "File contents as a list of sexps"
  ;; TODO 2014-12-23T15:52:24+00:00 Gabriel Laddel
  ;; ->fread
  (with-open-file (stream pathname :direction :input)
    (let* ((out))
      (awhile (read stream nil nil)
	(push arnesi::it out))
      (nreverse out))))

(defun write-to-file (filename object &optional (if-exists :append))
  ;; TODO 2014-12-23T15:52:24+00:00 Gabriel Laddel
  ;; ->fwrite
  (with-open-file (s filename :direction :output :if-does-not-exist :create :if-exists if-exists)
    ;; binding *package* thusly forces fully qualified symbols
    (let* ((*print-level* nil) (*print-length* nil) (*package* (find-package "KEYWORD"))) 
      (write object :stream s))))

(defun cat (&rest objs)
  (apply #'concatenate 'string
	 (mapcar (lambda (o) (if (stringp o) o (write-to-string o))) objs)))

(defun llast (l)
  (etypecase l
    (string (subseq l (1- (length l))))
    (list (car (last l)))))

(defun thread-alive? (thread)
  (when (bt:thread-alive-p thread) thread))

(defun thread-by-name (name)
  "case insensitive"
  (car (remove-if-not (lambda (thread) (equal (string-downcase name)
					 (string-downcase (bt:thread-name thread))))
		      (bt:all-threads))))

(defun threads () (bt:all-threads))

(defun take (n l)
  (subseq l 0 (if (< (length l) n) (if (> n (1- (length l))) (length l) (1- (length l))) n)))

(defun drop (n l)
  (unless (> n (length l)) (subseq l n (length l))))

(defun interleave (l1 l2)
  (loop for i in l1 for j in l2 append (list i j)))

(defun interpose (o l)
  (loop for i in l append (list i o)))

(defun frotate (sequence &optional (n 1))
  "Functional rotate, returns a new sequence. Alexandira sucks and does destructive updates."
  (loop repeat (1+ n) for l = sequence then (append (cdr l) (list(car l))) finally (return l)))

(defun vals (plist)
  (assert (evenp (length plist)))
  (iter (for e in plist)
    (for k initially 1 then (if (= 0 k) 1 0))
    (when (= 0 k) (collect e))))

(defun keys (plist)
  (assert (evenp (length plist)))
  (iter (for e in plist)
    (for k initially 0 then (if (= 0 k) 1 0))
    (when (= 0 k) (collect e))))

(defun walk-tree (fun tree)
  (subst-if t (constantly nil) tree :key fun))

(defun ls-clean (pathname)
  "ls, with emacs file recovery files, buffers removed"
  (remove-if (lambda (file) 
	       (let* ((name (pathname-name file))
		      (file-type (pathname-type file)))
		 (or (when name
		       (and (< 1 (length name)) 
			    (or (string= "~" (subseq name (1- (length name))))
				(string= "#"  (subseq name 0 1))
				(string= ".#" (subseq name 0 2)))))
		     (when file-type
		       (string= "~" (subseq file-type  (1- (length file-type))))))))
	     (ls pathname)))

(defun download-url (http-resource pathname)
  (with-open-file (file pathname
			:direction :output
			:if-does-not-exist :create
			:if-exists :supersede
			:element-type '(unsigned-byte 8))
    (let ((input (drakma:http-request http-resource
				      :want-stream t)))
      (awhile (read-byte input nil nil)
	(write-byte arnesi:it file))
      (close input))))

(defun extract-tarball (pathname)
  "Extract a tarball (.tar.gz) file to a directory (*default-pathname-defaults*). from http://8arrow.org/"
  (with-open-file (tarball-stream pathname
				  :direction :input
				  :element-type '(unsigned-byte 8))
    (archive::extract-files-from-archive
     (archive:open-archive 'archive:tar-archive
			   (chipz:make-decompressing-stream 'chipz:gzip tarball-stream)
			   :direction :input))))

(defmacro with-getfs (getfs plist &rest body)
  (assert (every 'keywordp getfs))
  `(let* ,(loop for getf in getfs 
 		collect (list (intern (symbol-name getf))
			      (list 'getf plist getf)))
     ,@body))

(defun object-slots-plist (object)
  (loop with slot-names = (mapcar #'closer-mop::slot-definition-name
				  (closer-mop:class-slots
				   (find-class (type-of object))))
	for slot-name in slot-names
	appending (list (make-keyword slot-name) (slot-value object slot-name))))

;;; parsers, generators for common formats 
;;; ============================================================================

;;; TODO 2014-09-09T13:34:28-07:00 Gabriel Laddel
;;; - find or make a json parser that returns alists instead of plists.
;;;   see `alist-plist' and `plist-alist'.
(defalias parse-json json:decode-json-from-string)
;;; `parse-xml'
(defalias http drakma:http-request)
(defalias filter remove-if-not)
(defalias ls cl-fad:list-directory)
(defalias distinct remove-duplicates)
(defmacro html-string (sexp)
  (let* ((out-var (gensym)))
    `(cl-who:with-html-output-to-string (,out-var)
       ,sexp
       ,out-var)))

(defun regex-matches (regex input)
  (cl-ppcre:all-matches-as-strings
   regex
   (etypecase input
     (pathname (slurp-file input))
     (string input))))

;;; useful stumpwm utilities
;;; ============================================================================

(defun monitors ()
  (let* ((shell-string (run-program "xrandr -q" :output :string)))
    (mapcar (lambda (x) (car (split " " x)))
	    (filter (curry #'scan "[^dis]connected")
		    (split "\\n" shell-string)))))

(defun activate-monitor (monitor-name position)
  "position is one of :up :down :left :right"
  (assert (member monitor-name (monitors) :test 'string=))
  (rp (if (eq :mirror position)
	  "xrandr --auto --output LVDS-1  --mode 1920x1080 --same-as VGA-1"
	  (format nil "xrandr --auto --output ~a --mode 1920x1080 ~a LVDS-1"
		  monitor-name
		  (case position
		    (:up "--above")
		    (:down "--below")
		    (:left "--left-of")
		    (:right "--right-of")
		    (:mirror "--same-as"))))))

(defun adjust-monitor (&optional (monitor-name (llast (monitors))))
  (rp (format nil "xrandr --output ~a --auto" monitor-name)))

(defun deactivate-monitor (monitor-name)
  (assert (member monitor-name (monitors) :test 'string=))
  (run-program (format nil "xrandr --output ~a --off" monitor-name))
  (sleep 1)
  (rp "xrandr --output LVDS-1 --auto")
  (compute-master-screen-dimensions))

(defmacro with-display (host (display screen root-window) &body body)
  `(let* ((,display (xlib:open-display ,host))
	  (,screen (first (xlib:display-roots ,display)))
	  (,root-window (xlib:screen-root ,screen)))
     (unwind-protect (progn ,@body)
       (xlib:close-display ,display))))

(defparameter screen-width nil)
(defparameter screen-height nil)
(defparameter lpw nil "[l]eft [p]ane [w]idth")
(defparameter lph nil "[l]eft [p]ane [h]eight")
(defparameter vpw nil "[v]isualization [p]ane [w]idth")
(defparameter vph nil "[v]isualization [p]ane [h]eight")

(defun compute-master-screen-dimensions ()
  (setf screen-width (with-display "" (display xlib::screen _)
		       (xlib:screen-width xlib::screen))
	screen-height (with-display "" (display xlib::screen _)
			(xlib:screen-height xlib::screen))
	lpw (float (* screen-width .25))
	lph (float (/ (* screen-height 6/7) 3)) 
	vpw (float(* screen-width .75)) 
	vph (float (* screen-height 6/7))))

(compute-master-screen-dimensions)

(defun take-screenshot (&key (width screen-width) (height screen-height) (host ""))
  (assert (and (<= width screen-width) (<= height screen-height)))
  (with-display host (display screen root-window)
    (xlib:get-image root-window :x 0 :y 0 
				:width width
				:height height
				:result-type 'xlib::image-x)))

(defun save-screenshot-as-png (screenshot file)
  (let+ ((height   (xlib::image-x-height screenshot))
	 (width    (xlib::image-x-width screenshot))
	 (png      (make-instance 'zpng:pixel-streamed-png
				  :color-type :truecolor-alpha
				  :width width
				  :height height))
	 (pixarray (xlib::image-x-data screenshot)))
    (with-open-file (stream file
			    :direction :output
			    :if-exists :supersede
			    :if-does-not-exist :create
			    :element-type '(unsigned-byte 8))
      (zpng:start-png png stream)
      (if (xlib::image-x-bit-lsb-first-p screenshot)
	  ;; lsb first 
	  (do ((i 0 (+ 4 i)))
	      ((>= i (length pixarray)))
	    (zpng:write-pixel (list (aref pixarray (+ 2 i))
				    (aref pixarray (+ 1 i))
				    (aref pixarray i)
				    #xFF)
			      png))
	  ;; msb first
	  (do ((i 0 (+ 4 i)))
	      ((>= i (* height width 4)))
	    (zpng:write-pixel (list (aref pixarray (1+ i))
				    (aref pixarray (+ 2 i))
				    (aref pixarray (+ 3 i))
				    #xFF)
			      png)))
      (zpng:finish-png png))))

(defun screenshot-window-to-png-file
    (drawable file &key (height (xlib:drawable-height drawable))
		     (width (xlib:drawable-width drawable)))
  "usage (screenshot-window (stumpwm::window-x-win (stumpwm::current-window)))"
  (let+ ((png (make-instance 'zpng:pixel-streamed-png
			     :color-type :truecolor-alpha
			     :width width
			     :height height))
	 (pixarray (xlib:get-raw-image drawable :x 0 :y 0 :width width :height height
						:format :Z-PIXMAP)))
    
    (with-open-file (stream file
			    :direction :output
			    :if-exists :supersede
			    :if-does-not-exist :create
			    :element-type '(unsigned-byte 8))
      (zpng:start-png png stream)
      (case (xlib:display-byte-order (xlib:drawable-display drawable))
	(:lsbfirst
	 (do ((i 0 (+ 4 i)))
	     ((>= i (length pixarray)))
	   (zpng:write-pixel (list (aref pixarray (+ 2 i))
				   (aref pixarray (+ 1 i))
				   (aref pixarray i)
				   #xFF)
			     png)))
	(:msbfirst 
	 (do ((i 0 (+ 4 i)))
	     ((>= i (* height width 4)))
	   (zpng:write-pixel (list (aref pixarray (1+ i))
				   (aref pixarray (+ 2 i))
				   (aref pixarray (+ 3 i))
				   #xFF)
			     png))))
      (zpng:finish-png png))))

(defun cursor-coordinates ()
  "(x y) in X windows coordinates"
  ;; TODO, Sat Mar 08 2014, Francis Wolke
  ;; This can be done using `xlib'! 
  (let* ((a (uiop:run-program "xdotool getmouselocation" :output :string))
         (b (ppcre:split " " a))
         (number? (lambda (x) (car (member x '(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9) :test #'equal))))
         (c (mapcar (lambda (s) (loop for c across s collect (funcall number? c))) b))
         (d (mapcar (lambda (l) (mapcar #'string (remove-if-not #'characterp l))) c))
         (e (subseq d 0 2))
         (x (apply 'concatenate 'string (first e)))
         (y (apply 'concatenate 'string (second e))))
    (list (parse-integer x) (parse-integer y))))

;;; search 
;;; ============================================================================
;;; TODO 2015-04-30T08:54:03+00:00 Gabriel Laddel
;;; yandex, duck duck go search functionality

(defvar wikipedia-search-base-url
  "http://en.wikipedia.org/w/api.php?action=opensearch&format=json&search=")
(defvar wikipedia-article-base-url
  "http://en.wikipedia.org/wiki/")

(defun search-wikipedia-article-titles (search-string)
  "Tree of wikipedia pages associated with string"
  ;; TODO 2014-08-25T00:22:47-07:00 Gabriel Laddel
  ;; - some values returned are anchors within another page
  ;; - spell check prior to searching
  (destructuring-bind (_ titles __ urls)
      (->> (regex-replace-all " " search-string "_")
	   (cat wikipedia-search-base-url)
	   (drakma:http-request)
	   (flexi-streams:octets-to-string)
	   (json:decode-json-from-string))
    ;; (loop for title in titles
    ;; 	  for url in urls
    ;; 	  collect (title url))
    ))

(defun google (query ;; &optional (number-of-pages 1)
	       )
  (declare (string query) (optimize (speed 3) (safety 0)))
  (let* ((out))
    (walk-tree (lambda (l)
  		 (declare (cons l))
  		 (when (and (equal 'cons (type-of l))
  			    (equal '(:a :href) (mm:take 2 l))
  			    (equal "/u" (mm:take 2 (nth 2 l))))
  		   (let ((k (nth 2 l)))
  		     (declare (string k) (cons l))
  		     (push (subseq k 7 (position #\& k)) out))))
  	       (html-parse:parse-html
  		(drakma:http-request
  		 (format nil "http://www.google.com/search?q=~A&start=1" 
  			 (drakma:url-encode query :latin1)))))
    (nreverse (mapcar (lambda (l)
  			(declare (string l))
  			(subseq l 0 (position #\& l))) out)))
  ;; (iter (with encoded-query = (drakma:url-encode query :latin1))
  ;;   (with out = ())
  ;;   (for i from 1 to (* 10 number-of-pages) by 10)
  ;;   (for page-number = (if (= 1 i) "" (cat "&start="  (write-to-string i))))
  ;;   (for query = (cat "http://www.google.com/search?q=" encoded-query page-number))
  ;;   (for page = (html-parse:parse-html (drakma:http-request query)))
  ;;   (walk-tree (lambda (l) (when (and (equal 'cons (type-of l))
  ;; 				 (equal '(:a :href) (mm:take 2 l))
  ;; 				 (equal "/u" (mm:take 2 (nth 2 l))))
  ;; 			(push (subseq (nth 2 l) 7 (position #\& l)) out)))		     
  ;; 	       page)
  ;;   (finally (return (nreverse (mapcar (lambda (l) (subseq l 0 (position #\& l))) out)))))
  )

;;; misc 
;;; ============================================================================

(defun class-slot-names (class-or-instance)
  (let* ((class (if (equal 'standard-class (type-of class-or-instance))
		    class-or-instance
		    (find-class (type-of class-or-instance)))))
    (mapcar #'closer-mop:slot-definition-name (closer-mop:class-slots class))))

(defun url-encode (s) (drakma:url-encode s :latin1))

(defun definitions (english-word)
  ;; TODO 2015-04-30T08:55:30+00:00 Gabriel Laddel
  ;; the parsing is unfinished
  (let+ ((req-url (cat "http://dictionary.reference.com/browse/" (url-encode english-word) "?s=t"))
	 (page (drakma:http-request req-url))
	 defns skipped definitions)
    (handler-case
	(if (scan "def-content" page)
	    (progn (walk-tree (lambda (l) (when (and (listp l) (equal '(:div :class "def-content") (car l)))
				       (push (cadr l) defns))) (html-parse:parse-html page))
		   (push english-word definitions)
		   (push defns definitions))
	    (push english-word skipped))
      ('error () (push english-word skipped)))
    (values skipped definitions)))

(defun ppath (string)
  "[p]roject [path]name"
  (format nil "~~/quicklisp/local-projects/masamune/~a"
	  (if (string= "/" (subseq string 0 1)) (subseq string 1) string)))

(defun qlpp (&optional string)
  "[q]uicklisp [l]ocal-[p]rojects [p]athname"
  (pathname (if string (format nil "~~/quicklisp/local-projects/~a"
			       (if (string= "/" (subseq string 0 1)) (subseq string 1) string))
		"~/quicklisp/local-projects")))

(defun format-escape (control-string) (regex-replace-all "~" control-string "~~"))

(defun rp (program-string &optional (output-stream :string) (ignore-error-status nil))
  "shorthand, returns shell program output as string"
  (run-program program-string :output output-stream :ignore-error-status ignore-error-status))

(defun rp-in-dir (commands dir &optional (output-stream :string) (ignore-error-status nil))
  (if (listp  commands)
      (dolist (shell-command commands)
	(rp (format nil "cd ~A && ~A" dir shell-command) output-stream ignore-error-status))
      (rp (format nil "cd ~A && ~A" dir commands) output-stream ignore-error-status)))

(defalias cm compose)

(defun emacs-backup? (pathname)
  (let* ((name (pathname-name pathname))
	 (file-type (pathname-type pathname)))
    (or (string= "#"  (subseq name 0 1))
	(string= ".#" (subseq name 0 2))			       
	(when file-type (string= "~" (subseq file-type  (1- (length file-type)))))
	(string= "~" (subseq name (1- (length name))))))  )

(defun recursive-contents (pathname)
  (assert (probe-file pathname))
  (loop for p in (ls pathname)
	collect (if (cl-fad:directory-pathname-p p)  
		    (recursive-contents p)
		    (list p)) into out
	finally (return (remove-if #'null (apply #'append out)))))

(defun start-conkeror ()  
  "We use the GTK2_RC_FILES env variable to ensure that conkeror doesn't have
scrollbars if desired"
  ;; this belongs in ~/.gtk2-conkeror - remove it if you want scrollbars
  ;; 
  ;;   style "noscrollbars" {
  ;;   GtkScrollbar::slider-width=0
  ;;   GtkScrollbar::trough-border=0
  ;;   GtkScrollbar::has-backward-stepper=0
  ;;   GtkScrollbar::has-forward-stepper=0
  ;;   GtkScrollbar::has-secondary-backward-stepper=0
  ;;   GtkScrollbar::has-secondary-forward-stepper=0
  ;; }
  ;; widget "MozillaGtkWidget.*" style "noscrollbars"
  ;; GTK2_RC_FILES=~/.gtkrc-2.0.conkeror << add this to the front of the shell command
  (stumpwm::run-shell-command
   "~/algol/xulrunner/xulrunner ~/algol/conkeror/application.ini > ~/.masamune/browser-output"))

(defun open-ports ()
  "I don't know if this implementation is correct, I'm following the 3rd answer
here:
http://stackoverflow.com/questions/9609130/quick-way-to-find-if-a-port-is-open-on-linux

I'm currently guessing that /proc/net/tpc6 corresponds to ipv6 and parsing that too.

NOTE: it sometimes happens that a port # occurs twice in the output. why?"
  ;; NOTE 2014-12-17T01:30:39+00:00 Gabriel Laddel
  ;; `slurp-file' doesn't work for whatever reason
  (labels ((parse-ports-from-procfile (procfile)
	     (mapcar (lambda (line) (car (split " " (nth 2 (split ":" line))))) 
		     (rest (split "\\n" (rp (format nil "cat ~a" procfile)))))))
    (let* ((ipv6-hex-ports (parse-ports-from-procfile "/proc/net/tcp6"))
	   (ipv4-hex-ports (parse-ports-from-procfile "/proc/net/tcp")))
      (mapcar (lambda (hex) (parse-integer hex :radix 16)) (append ipv6-hex-ports ipv4-hex-ports)))))

(defun port-in-use-p (port-number)
  (member port-number (open-ports) :test #'=))

(defun valid-open-port ()
  "http://unix.stackexchange.com/questions/16564/why-are-the-first-1024-ports-restricted-to-the-root-user-only"
  (loop with p = 1024 while (mm::port-in-use-p p) 
	do (incf p) finally (return p)))

(defun trim-dwim (string) (string-trim '(#\space #\newline) string))

(defmacro with-live-swank-connection (&rest body)
  ;; XXX 2014-12-19T09:12:42+00:00 Gabriel Laddel
  ;; this shouldn't have to exist at all
  `(let* ((swank::*emacs-connection* (car swank::*connections*)))
     ,@body))

(defmacro eval-in-emacs (sexp)
  `(mm::with-live-swank-connection
       (ignore-errors
	(swank::eval-in-emacs ,sexp t))))

(defun format-message-for-stumpwm (string)
  (loop with message = (coerce string 'list)
	while message
	collect (if (or (> 80 (length message)) (equal #\space (nth 80 message)))
		    (let* ((o (take 80 message))) 
		      (setf message (drop 80 message))
		      o)
		    (let* ((to-drop (position #\space (reverse (take 80 message))))
			   (to-collect (subseq message 0 (- 80 to-drop))))
		      (setf message (drop (length to-collect) message))
		      to-collect)) into out
	finally (return (coerce (flatten (let* ((clean-out (remove-if #'null (mapcar (lambda (l) (if (equal #\space (car l)) (rest l) l)) out))))
					   (interpose (list #\~ #\%) clean-out))) 'string))))

(defun scontains (search-string test-string)
  (and (<= (length search-string) (length test-string))
       (search search-string test-string :test 'string= )))

;;; Debuggering of UNIX
;;; ============================================================================

(defun machine-information ()
  "programmers claim machine independence, but generally fail to delivier. got a
script to derive some information about the machine? contribute it and aid in
the debuggering of linux."
  ;; TODO 2014-11-10T11:55:59-08:00 Gabriel Laddel
  ;; 
  ;; - review man pages for each one of these and give flags to get the
  ;;   information possiblbe - or reverse engineer and unify into something
  ;;   sane.
  ;; 
  ;; - 'tree' /proc & /etc, review the combined ~37832 files and mark all
  ;;   interesting information for export.
  ;;
  ;; - use inxi, sources are available in ~/algol/. apparently it'll give a
  ;;   bunch of interesting information back
  ;;
  ;; - lspci has a bunch of interesting information, see the manpage
  ;;
  ;; check it out: (parse-html (rp "lshw -xml")) << returns all hardware
  (loop for program in (append '("lsb_release -a" "uname --all" "lshw" "lshw-businfo"
				 "lspci" "lspci -v" "lscpu" "lsusb" "lsblk" "df" 
				 "fdisk -l" "mount" "free" "free -m")
			       (mapcar (lambda (a) (format nil "cat ~a" a))
				       '("/proc/buddyinfo"
					 "/proc/cgroups"
					 "/proc/consoles"
					 "/proc/cpuinfo"
					 "/proc/devices"
					 "/proc/diskstats"
					 "/proc/execdomains"
					 "/proc/fbfilesystems"
					 "/proc/interrupts"
					 "/proc/iomem"
					 "/proc/ioports"
					 "/proc/kallsyms"
					 "/proc/loadavg"
					 "/proc/lockso"
					 "/proc/meminfo"
					 "/proc/misc"
					 "/proc/modules"
					 "/proc/mounts"
					 "/proc/mtrr"
					 "/proc/pagetypeinfo"
					 "/proc/partitions"
					 "/proc/slabinfo"
					 "/proc/softirqs"
					 "/proc/stat"
					 "/proc/swaps"
					 "/proc/sysrq-trigger"
					 "/proc/timer_list"
					 "/proc/timer_stats"
					 "/proc/uptime"
					 "/proc/version"
					 "/proc/vmallocinfo"
					 "/proc/vmstat"
					 "/proc/zoneinfo")))
	nconcing (list program (handler-case (rp program) (error nil)))))

(defun environment ()
  (list :machine-information (machine-information)
	:emacs (uiop:run-program "emacs --version" :output :string)
	:machine-type (machine-type)
	:machine-instance (machine-instance)
	:machine-version (machine-version)
	:software-type (software-type)
	:software-version (software-version)
	:lisp-implementation-type (lisp-implementation-type)
	:lisp-implementation-version (lisp-implementation-version)))

(defun levenshtein (a b)
  (let* ((la  (length a))
	 (lb  (length b))
	 (rec (make-array (list (1+ la) (1+ lb)) :initial-element nil)))
    (labels ((leven (x y)
	       (cond
		 ((zerop x) y)
		 ((zerop y) x)
		 ((aref rec x y) (aref rec x y))
		 (t (setf (aref rec x y)
			  (+ (if (char= (char a (- la x)) (char b (- lb y))) 0 1)
			     (min (leven (1- x) y)
				  (leven x (1- y))
				  (leven (1- x) (1- y)))))))))
      (leven la lb))))

(defun read-from-whole-string (string)
  (loop with start = 0 
	with out = nil
	with string  = (trim-dwim string)
	while (/= start (length string))
	do (multiple-value-bind (j k) 
	       (read-from-string string nil nil :start start)
	     (setf start k)
	     (push j out))
	finally (return (nreverse out))))

(defun mkdir (pathname)
  (rp (format nil "mkdir -p ~a" pathname)))

(defun lisp-page (url) (parse-html (http url)))
    
(defun group-by (l group-function)
  (loop with out = ()
	for e in l
	for v = (funcall group-function e)
	do (push e (getf out v))
	finally (return (loop with j = (mapcar (lambda (j) (if (stringp j) j (nreverse j))) out)
			      for i from 0 to (- (length j) 1) by 2
			      collect (subseq j i (+ 2 i))))))

(defun kill-thread (thread-or-name)
  (etypecase thread-or-name
    (SB-THREAD:THREAD (bt:destroy-thread thread-or-name))
    (string (bt:destroy-thread (thread-by-name thread-or-name)))))

(defun ssearch (query-string search-string &optional from-end)
  (search query-string search-string :test 'string= :from-end from-end))

(defun %format-symbol (package format-string &rest arguments)
  (apply (curry 'format-symbol package (string-upcase format-string)) 
	 (mapcar (lambda (ms) (if (stringp ms) (string-upcase ms) ms)) arguments)))

(defun range (n)
  (assert (numberp n))
  (loop for nn from 0 to n collect nn))

(defun repeat (e n)
  (assert (numberp n))
  (loop repeat n collect e))

(defun +> (&rest l) 
  "the sum of the first element, the sum of the first two elements, etc.

APL style sum scan, http://www.jsoftware.com/papers/tot.htm this is traditionally
notated as +\, but CL won't accept it as a name. The complementary operator is +/,
which is known as + in CL"
  (loop for n in l for i = 1 then (1+ i) collect (apply '+ (take i l))))

(defun *> (&rest l)
  "product scan. The product of the first element, the product of the first two
elements etc., see `+>'"
  (loop for n in l for i = 1 then (1+ i) collect (apply '* (take i l))))

(defun regex-replace-in-file (regex replacement pathname)
  "replaces all occurrences REGEX in PATHNAME with REPLACEMENT"
  (assert (probe-file pathname))
  (let* ((filestring (regex-replace-all regex (slurp-file pathname) replacement))
	 (*print-level* nil)
	 (*print-length* nil))
    (with-open-file (s pathname :direction :output
				:if-exists :supersede
				:if-does-not-exist :error)
      (princ filestring s)
      nil)))

(defun frequencies (l &key (test 'equal))
  (loop for o in (distinct l :test test)
	collect (list o (count o l :test test))))

(defun package-symbol-occurrences (package-identifier lisp-file)
  (assert 
   (and (probe-file lisp-file) 
	(member (pathname-type lisp-file) '("l" "lisp" "ps" "paren") :test 'string=)))
  (let* ((package (find-package package-identifier)) (out))
    (walk-tree (lambda (l) (when (and (listp l) (symbolp (car l))
				 (equal (symbol-package (car l)) package))
			(push (car l) out))) 
	       (read-file lisp-file))
    (frequencies out :test 'eq)))

(defun dir? (pathname)
  (and (probe-file pathname) (directory-pathname-p pathname)))

(defun recursive-contents-of-type (dir pathname-type &optional (clean t))
  (assert (dir? dir))
  (if clean
      (filter (lambda (p) (and (not (emacs-backup? p)) 
			  (string= pathname-type (pathname-type p)))) 
	      (recursive-contents dir))
      (filter (lambda (p) (string= pathname-type (pathname-type p))) 
	      (recursive-contents dir))))

(defmethod alter-pathname-type ((pathname pathname) (new-type string))
  "XXX, special casing on known double . filetypes is wrong, but linux sucks
and doesn't offer a right answer as far as I know. An empty string NEW-TYPE
will correctly strip the trailing . from a pathname"
  (labels ((f (type) (let* ((type-length (length type)))
		       (and (> (length (namestring pathname)) type-length)
			    (string= (reverse type) (take type-length  (reverse (namestring pathname))))))))
    (let* ((type (cond ((f "tar.gz")  "tar.gz")
		       ((f "tar.xz")  "tar.xz")
		       ((f "tar.bz2") "tar.bz2")
		       (t (pathname-type pathname))))
	   (namestring (namestring pathname)))
      (pathname (mm::cat (subseq namestring 0 (- (length namestring) 
						 (if (emptyp new-type) (1+ (length type)) (length type))))
			 (if (string= "." (subseq namestring
						  (- (length namestring) (+ (length type) 1))
						  (- (length namestring) (length type))))
			     "" ".")
			 new-type)))))

(defmethod filename ((pathname pathname))
  (assert (not (directory-pathname-p pathname)))
  (llast (split "/" (namestring pathname))))

(defmethod alter-pathname-filename ((pathname pathname) (new-filename string) 
				    &optional new-type)
  (alter-pathname-type (pathname (apply 'cat 
					(append (interpose "/" (butlast (split "/" (namestring pathname))))
						(list new-filename))))
		       (or new-type (pathname-type pathname))))

(defmethod pathname-type= ((pathname pathname) (type string))
  (and (< (length type) (length (namestring pathname)))
       (string= type (subseq (namestring pathname)
			     (- (length (namestring pathname)) (length type))
			     (length (namestring pathname))))))


(defmethod alter-file-type ((pathname pathname) (new-type string))
  (assert (probe-file pathname) () "file does not exist")
  (rename-file pathname (alter-pathname-type pathname new-type)))

(defun pathname-dir-path (pathname)
  (pathname (apply 'cat (cons "/" (interpose "/" (rest (pathname-directory pathname)))))))

(defun last-modified (dir)
  (car (mapcar (lambda (s) (llast (split "[0-9]*:[0-9]* " s)))
	       (rest (split "\\n" (rp (format nil "ls -alt ~a" dir)))))))

(defmethod name ((pathname pathname))
  (llast (split "/" (namestring pathname))))

(defun package-symbols (&optional (package *package*))
  (loop for s being the symbols of (find-package package)
	collect s))

;;; stacktrace printing, copy/pasted from the ql-test by Fare:
;;; ssh://common-lisp.net/home/frideau/git/ql-test.git

(defun print-backtrace (stream)
  "Print a backtrace (implementation-defined)"
  (declare (ignorable stream))
  #+clozure (let ((*debug-io* stream))
	      (ccl:print-call-history :count 100 :start-frame-number 1)
	      (finish-output stream))
  #+sbcl
  (sb-debug:backtrace most-positive-fixnum stream))

;;; Many thanks to pjb, who wrote this when I requested an implementation
;;; ============================================================================

(defun string-prepare-token (kind name)
  (declare (ignore kind))
  (string name))

(defun uninterned-prepare-token (kind name)
  (declare (ignore kind))
  (make-symbol (string name)))

(defun keyword-prepare-token (kind name)
  (declare (ignore kind))
  (intern (string name) (load-time-value (find-package "KEYWORD"))))

(defun smem (e l) (member e l :test 'string=))

;; (defun sexp-for-package (package-designator &optional (prepare-token (function string-prepare-token)))
;;   (let ((package (find-package package-designator)))
;;     (assert package)
;;     (let* ((used-packages     (package-use-list package))
;;            (used-symbols      (mapcan (function com.informatimago.common-lisp.cesarum.package:package-exports)
;;                                       used-packages))
;;            (shadows           '())
;;            (shadowing-imports (make-hash-table))
;;            (exports           (com.informatimago.common-lisp.cesarum.package:package-exports package))
;;            (shadowed-symbols  (package-shadowing-symbols package))
;;            (imports           (make-hash-table)))
;;       (do-symbols (sym package)
;;         (unless (member sym exports)
;;           (let ((home (symbol-package sym)))
;;             (unless (or (eq home package)
;;                         (member sym shadowed-symbols)
;;                         (member sym used-symbols)
;;                         (member home used-packages))
;;               (push sym (gethash home imports '()))))))
;;       (dolist (sym shadowed-symbols)
;;         (let ((home (symbol-package sym)))
;;           (if (eq home package)
;;               (push sym shadows)
;;               (push sym (gethash home shadowing-imports '())))))
;;       (flet ((pname (x) (funcall prepare-token :package x))
;;              (sname (x) (funcall prepare-token :symbol  x)))
;; 	`(defpackage ,(pname (package-name package))
;; 	   ,@(when (package-nicknames package)
;; 	       `((:nicknames ,@(mapcar (function pname) (package-nicknames package))))) 
;; 	   (:use ,@(mapcar (lambda (p) (pname (package-name p))) used-packages))
;; 	   ,@(when shadows
;; 	       `((:shadow ,@(mapcar (function sname) shadows))))
;; 	   ,@(when exports
;; 	       `((:export  ,@(mapcar (function sname) exports))))
;; 	   ,@(when (plusp (hash-table-count shadowing-imports))
;; 	       (let ((forms '()))
;; 		 (maphash (lambda (pack syms)
;; 			    (push `(:shadowing-import-from 
;; 				    ,(pname (package-name pack))
;; 				    ,@(mapcar (function sname) syms))
;; 				  forms))
;; 			  shadowing-imports)
;; 		 forms))
;; 	   ,@(when (plusp (hash-table-count imports))
;; 	       (let ((forms '()))
;; 		 (maphash (lambda (pack syms)
;; 			    (push `(:import-from 
;; 				    ,(pname (package-name pack))
;; 				    ,@(mapcar (function sname) syms))
;; 				  forms))
;; 			  imports)
;; 		 forms)))))))

;; (dolist (pack (list-all-packages))
;;   (pprint (sexp-for-package pack (function uninterned-prepare-token))))

;;; TODO 2015-04-30T09:08:00+00:00 Gabriel Laddel
;;; conversion fails on ffmpeg? 

;; (defun youtube->mp3 (video-url output-mp3-name
;; 		     &key (output-dir #P"/tmp/youtube-mp3s/") (output-stream *standard-output*))
;;   (unless (probe-file output-dir) (mkdir output-dir))
;;   (let* ((old-contents (ls output-dir)) (new-vid))
;;     (rp-in-dir (format nil "youtube-dl ~a" video-url) output-dir output-stream)
;;     (setf new-vid (set-difference (ls output-dir) old-contents))
;;     (rp-in-dir (format nil "ffmpeg -i ~a ~a.wav" new-vid output-mp3-name) output-dir output-stream t)
;;     (rp-in-dir (format nil "lame ~a.wav ~a.mp3" output-mp3-name output-mp3-name) output-dir output-stream)))

(defun browser ()
  (mm::start-conkeror)
  (sleep 3)
  (mmb::start-ps-repl))

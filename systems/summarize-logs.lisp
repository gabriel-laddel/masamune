(in-package #:mm)

;;; NOTE 2015-03-17T10:41:22+00:00 Gabriel Laddel
;;; 
;;; The original intent was that I'd highlight interesting parts of the logs and
;;; discuss them. This failed becuase I didn't care to spend all that much time
;;; figuring out the X rendering loop and how it relates to McCLIM which is
;;; required for a custom application for this purpose.
;;;
;;; I doubt that I'll ever return to this, but if you'd like to use it, feel
;;; free

;; http://log.bitcoin-assets.com/?date=26-04-2013

;;; 04:23:11 mircea_popescu: all the way to making the girls do the cucumbers.
;;; http://log.bitcoin-assets.com/?date=05-02-2015

(c log-summary () 
    (end-date start-date dialogues))
(c dialogue () 
    (date title comments messages))
(defclass log-line ()
  ((nick :accessor nick :initarg :nick :initform nil)
   (entry-time :accessor entry-time :initarg :time :initform nil)
   (entry-message :accessor entry-message :initarg :message :initform nil)
   (link :accessor link :initarg :link :initform nil)))

(export 'LOG-LINE)

(defun log-summary-cleanup ()
  "Emacs calls this function when I've pushed the logs to master"
  (record-event (mmg::habit-by-name "Summarize Logs") (event :finished))
  (mmg::run-or-focus-dashboard))

(defun visualize-summarize-logs (habit sheet)
  (let* ((y 1084) (x 1460))
    (format sheet " ~%    notes word count: ~d~%    log-length: ~d lines~%    session length: ~d minutes"
	    (random 200) (random 200) (random 200))))

(defmethod human-readable-string ((log-summary log-summary))
  "Returns a string suitable for viewing as plaintext"
  (let* ((header (apply #'cat (loop repeat 80 collect "="))))
    (with-slots (start-date end-date dialogues) log-summary
      (apply #'cat (cons (format nil "#bitcoin-assets log summary for ~a through ~a~%~%" start-date end-date)
			 (loop for dialog in dialogues
			       collect (with-slots (comments date title messages) dialog
					 (format nil "~a - ~a~%~a~%~{~%~{~a~%~}~}~%"
						 title date header messages))))))))

;; (defmethod publish-new-summary ((log-summary log-summary))
;;   (let* ((publishing-dir "~/quicklisp/local-projects/")
;; 	 (text-filename (format nil "~a~a-through-~a" 
;; 				publishing-dir (start-date log-summary) (end-date log-summary)))
;; 	 (index-file "~/quicklisp/local-projects/gabriel-laddel.github.io/index.lisp"))
;;     ;; write human readable version
;;     (write-to-file (format nil
;; 			   "~/quicklisp/local-projects/gabriel-laddel.github.io/"
;; 			   text-filename) 
;; 		   (human-readable-string log-summary))
;;     ;; update index
;;     (write-to-file index-file (push (list :a :href (cat "/" text-filename) text-filename)
;; 				    (read-file index-file))
;; 		   :supersede)
;;     ;; TODO 2014-11-30T05:22:21-08:00 Gabriel Laddel
;;     ;; generate html file
;;     ))

(defmethod print-object ((log-summary log-summary) stream)
  (with-slots (start-date end-date dialogues) log-summary
    (format stream "#<~a thru ~a, ~d dialogs>" start-date end-date (length dialogues))))

(defun log-message-plists (log-html-parse)
  (let* ((dirty-messages)) 
    (walk-tree (lambda (l) (when (and (listp l) (eq :body (car l)))
			(setq dirty-messages l)))
	       log-html-parse)
    (mapcar (lambda (l) (let* ((ugh (rest (car (cdr (car (cdr l)))))))
		     (list :nick (cadadr ugh) :message (cdaddr ugh)
			   :link (llast (caadar ugh)) :time (llast (cadar ugh)))))
	    (butlast (drop 5 dirty-messages)))))

(defun log-timestring-timestamp (timestring)
  (let* ((_ (regex-matches "\\d\\d" timestring))
	 (day (read-from-string (car _)))
	 (month (read-from-string (second _)))
	 (year (read-from-string (apply #'cat (drop 2 _)))))
    (encode-timestamp 0 0 0 0 day month year)))

(defun log-timestring (&optional (timestamp (now)))
  (format-timestring nil timestamp :format  '(:day "-" :month "-" :year)))

(defun log-url (date)
  (format nil "http://log.bitcoin-assets.com/?date=~a" date))

(defun logs (start-date &optional (end-date (log-timestring (now))))
  "if END-TIMESTAMP is not supplied, returns a plist of logs up `now'"
  (let* ((o))
    (restart-case
	(loop for date in (loop while t
				for i = 0 then (1+ i)
				for k = (-> (log-timestring-timestamp start-date)
					    (timestamp+ i :day)
					    (log-timestring )) 
				collect k into out
				do (when (string= k end-date) (return out)))
	      do (push (list date (log-message-plists (parse-html (http (log-url date))))) o))
      (return-current () :report "return the logs collected so far" (nreverse o)))
    o))

(defun b-a-log (date)
  (parse-html (http )))

(defun unprocessed-logs ()
  (take 2 (car (read-file "/tmp/test-logs"))))

(defun start-summarize-logs (habit)
  (record-event habit (event :started))
  (stumpwm::add-hook STUMPWM:*FOCUS-WINDOW-HOOK* 'maybe-finished-with-log-summary)
  (stumpwm::pause-to-read "focus the dashboard again when you'd like to be prompted to finish off this habit")
  (if (last-log-date)
      (stumpwm::pause-to-read "this shouldn't have happened until manardb takes the place of the current ad-hoc process.")
      (progn (stumpwm::pause-to-read
	      "it appears that this is your first time running log summaries, a somewhat recent date has been selected to start, with logs up to now being downloaded in the background to the file /tmp/21-11-2014-22-11-2014")
	     (write-to-file "/tmp/21-11-2014-22-11-2014" (logs "21-11-2014" "22-11-2014"))
	     (with-live-swank-connection
		 (handler-case (swank::eval-in-emacs
				'(progn (find-file "/root/quicklisp/local-projects/masamune/systems/summarize-logs.lisp") nil) t)
		   (error nil))))))

;;; GUI
;;; ============================================================================

;; (in-package #:mmg)

;; (defvar *log-processor* nil)

;; (define-application-frame log-processor ()
;;   ((conversations-pane) (input-pane) (interaction-pane))
;;   (:pointer-documentation t)
;;   (:menu-bar nil)
;;   (:panes (interaction-pane :interactor)
;; 	  (conversations-pane :application
;; 			      :scroll-bars :vertical
;; 			      :display-function 'render-conversations)
;; 	  (input-pane :application
;; 		      :scroll-bars :vertical
;; 		      :display-function 'render-input))
;;   (:layouts (defualt (vertically ()
;; 		       (7/8 (horizontally () 
;; 			      (1/2 conversations-pane)
;; 			      (1/2 input-pane)))
;; 		       (1/8 interaction-pane)))
;; 	    (read-mode (vertically () (7/8 input-pane)
;; 			 (1/8 interaction-pane)))))

;; (define-presentation-type log-line ())

;; (define-log-processor-command (com-select-message :name t)
;;     ;; - turn yellow, and everything after it yellow too
;;     ;; - how do i do incremental redraw?
;;     ((message 'log-line :gesture :select))
;;   (format *query-io* "message: ~s" message))

;; (defun run-or-raise-log-processor ()
;;   (labels ((run-log-processor ()
;; 	     (setf *log-processor* (make-application-frame 'log-processor))
;; 	     (run-frame-top-level *log-processor* :name "Log-Processor"))) 
;;     (aif (stumpwm::window-by-name "log-processor")
;; 	 (stumpwm::select-window (stumpwm::window-name it))
;; 	 (bt:make-thread (lambda () (run-log-processor)) :name "Log-Processor"))))

;; (defun render-conversations (frame pane)
;;   (declare (ignore frame))
;;   (format pane "conversation here"))

;; (defun render-input (frame pane)
;;   (declare (ignore frame))
;;   (let* ((*print-pretty* nil))
;;     (loop for (date log-messages) in (mm::unprocessed-logs) 
;; 	  do (progn (format pane "~%~A~%~%" date)
;; 		    (loop for log-line in log-messages
;; 			  for ll = (apply (curry #'make-instance 'log-line) log-line)
;; 			  do (with-slots (mm::nick mm::entry-message) ll
;; 			       (with-output-as-presentation (pane ll 'log-line)
;; 				 (format pane "~a ~a~%" mm::nick mm::entry-message))))))))

;; (defvar my-log-summary
;;   (make-instance 'log-summary
;; 		 :start-date "21-11-2014"
;; 		 :end-date "23-11-2014"
;; 		 :dialogues (list (make-instance 'dialogue
;; 						 :title "More DDoS and hacks. Watch your step."
;; 						 :date "22-11-2014"
;; 						 :messages '(("16:29:08mats_cd03: \"Linux full disk encryption does not really work, for that matter.\" << anyone know what mp is referring to here?"
;; 							      "16:31:21mats_cd03:i suspect he means keys need to be stored in RAM for decryption"
;; 							      "16:35:07asciilifeform:mats_cd03: this plus the fact of the decryptor routine itself normally stored on disk"
;; 							      "16:35:44asciilifeform:mats_cd03: where it can be overwritten by pwner with a version that saves key"
;; 							      "16:42:44mats_cd03:a"
;; 							      "17:26:42asciilifeform:'the time has come, the walrus said, to talk of many things: of shoes, and ships, and sealing-wax, of cabbages and kings; and why the sea is boiling hot, and whether pigs have wings...'"
;; 							      "17:27:11asciilifeform:specifically, there is a vandal lurking in #b-a who 'metasploits' uncloaked folks"
;; 							      "17:27:59asciilifeform:about 2 in 3 of extant consumer-grade routers have published remote anal orifices."
;; 							      "17:28:00asciilifeform:cheers."
;; 							      "17:33:40kanzure:haha metasploit"
;; 							      "17:33:44kanzure:v. professional"
;; 							      "17:33:49kanzure:such leetness"
;; 							      "17:34:03asciilifeform:not necessarily 'metasploit' but perhaps a similar autodiddler."
;; 							      "17:34:09kanzure:don't they know it only counts if you come up with a novel zero day? otherwise all the points go to charity."
;; 							      "17:34:17asciilifeform:lol"
;; 							      "17:34:19kanzure:(and who wants that?)"
;; 							      "17:35:04kanzure:asciilifeform: okay to pm?"
;; 							      "17:35:12asciilifeform:sure, though i'm about to eat"
;; 							      "17:38:22mats_cd03:msf isn't necessarily an amateur tool, though it is used by amateurs"
;; 							      "17:44:43assbot:[HAVELOCK] [AM1] 67 @ 0.1 = 6.7 BTC [-]"
;; 							      "17:45:07kanzure:.title https://bitcointalk.org/index.php?topic=437926.0"
;; 							      "17:45:08assbot:If I was chairman of the Fed ... ( http://bit.ly/1xDre0T )"
;; 							      "17:59:10punkman:so deeds server was maybe hacked"
;; 							      "17:59:45punkman:bitcents still at address though"
;; 							      "18:01:01asciilifeform:punkman: what suggested that it was hacked ?"
;; 							      "18:01:33punkman:kakobrekla got a mail that an attack was originating from the deed server"
;; 							      "18:04:28kakobrekla:all i can say (as i dont have or had access to the machine) that there is something that looks like ddos + complaint from outside of do")
;; 							     ("21:46:29asciilifeform:to briefly continue this morning's thread - don't rely on freenode's 'cloaks', if you have a consumer garbage-grade router on your premises, throw it out, replace with actual router.")
;; 							     ("20:01:41asciilifeform:so it appears that i've merited own, personal ddos."
;; 							      "20:03:58assbot:[MPEX] [S.MPOE] 70800 @ 0.00041465 = 29.3572 BTC [-] {3}"
;; 							      "20:05:11asciilifeform:anybody else?"
;; 							      "20:05:16asciilifeform:or did the gods smile on me alone."))
;; 						 :comments "If you've not been following #b-a up until this point, note that trilema.com, Loper-os.org, log.bitcoin-assets.com and qntra.net have all been getting DDoS'd for some time now. The \"Reddit police\" or some other such nonsense showed up at one point to claim responsibility.")

;; 				  (make-instance 'dialogue
;; 						 :title "The Real Bitcoin"
;; 						 :date "23-11-2014"
;; 						 :messages '(("04:37:12asciilifeform:kakobrekla: should we even be doing bitcoinwhatever on x86 << what do you think."
;; 							      "04:38:20Vexual:yes"
;; 							      "04:38:41asciilifeform:back to that thread: can't speak for others, but i don't intend to use the trimmed 0.5.3 in anger."
;; 							      "04:38:48asciilifeform:beyond verifying that it indeed functions.")
;; 							     ("04:44:06asciilifeform:well it doesn't, at present"
;; 							      "04:44:17asciilifeform:plan was, someone wanted to fix it..."
;; 							      "04:44:32kakobrekla:no that wasnt the question"
;; 							      "04:45:08asciilifeform:lol, question was re: stealth hardfork ?"
;; 							      "04:45:46kakobrekla:yea i guess"
;; 							      "04:45:59asciilifeform:somebody here, iirc, suggested exactly this.")
;; 							     ("04:53:31asciilifeform:quite possibly this is ideal scenario. a hardfork that sane folks on the whole planet would immediately flee to."))
;; 						 :comments "see therealbitcoin.org for the who what when and why of this bitcoind implementation.")

;; 				  (make-instance 'dialogue
;; 						 :title "WoTnet"
;; 						 :date "23-11-2014"
;; 						 :messages '(("05:04:09asciilifeform:mircea_popescu: this is why, in my unofficial wonderland, you can't even open a socket without transmitting an rsa-signed a 'this is me, and my wot' breath of life packet."
;; 							      "05:04:56mircea_popescu:asciilifeform in any case, my original change to github and generally the mangement of open source codebase of \"add a read by X\" field is not actually enough.")
;; 							     ("05:09:30asciilifeform:back to syn packets, signed breath-of-life neatly licks ddos."
;; 							      "05:09:41asciilifeform:all comms answered in priority of wot rank.")
;; 							     ("05:11:53asciilifeform:a 4096-bit rsa signature and key fp fit handily in a udp minimal packet."
;; 							      "05:12:46asciilifeform:i'm going to confess now that i implemented this..."
;; 							      "05:12:48asciilifeform:and am sitting on it"
;; 							      "05:13:10asciilifeform:because not ready for battlefield yet, and other work")
;; 							     ("05:17:13asciilifeform:decimation: in point of fact, you can get by without either tcp or the proverbial 'bad reimplementation' of it")		    
;; 							     ("05:17:39asciilifeform:decimation: one can use error-coding to get around lost and re-ordered packets."
;; 							      "05:17:44decimation:exactly"
;; 							      "05:17:57decimation:tcp really is pretty retarded if you think about"
;; 							      "05:18:03asciilifeform:this also neatly dovetails into 'apocalyptic shortwave radio' net."
;; 							      "05:18:17decimation:it violates the principle of \"the highest protocol layer ought to control\" (end-to-end argument)"
;; 							      "05:18:24mircea_popescu:decimation it's not retarded if you think of how fucking cheap it is."
;; 							      "05:18:25decimation:exactly"
;; 							      "05:18:37mircea_popescu:think about that. think about the early days of bbs, which is how routing fucking got invented in the first placer"
;; 							      "05:18:43mircea_popescu:deliver all the mail locally, cheaper calls"
;; 							      "05:18:47asciilifeform:mircea_popescu: the excess packets - cheap. the ddosyness - no."
;; 							      "05:18:57decimation:well, in elder days when the internet wasn't full of orcs it kinda seemed ok"
;; 							      "05:19:03decimation:now the costs are different"
;; 							      "05:19:06mircea_popescu:asciilifeform it was five cents a minute yo! at the time a minute meant one page, or a gallon of gasoline."
;; 							      "05:19:30asciilifeform:everything in future will work - or not - depending on how readily it cuts through swaths of orcs.")
;; 							     ("05:22:21asciilifeform:wotnet."
;; 							      "05:22:27asciilifeform:'you read it here phirst (tm)'"))))))

(in-package #:mm)

(defun summarize-logs-install () 
  (push (i 'habit
	   :name "Summarize Logs"
	   :description "Read the #b-a logs, take notes, summarize and publish findings online"
	   :initialization-function 'start-summarize-logs
	   :visualization-function 'visualize-summarize-logs
	   :occurrence :weekly)
	*habits*))


(defun maybe-finished-with-log-summary (current-window last-window)
  (when (and (string= "dashboard" (string-downcase (stumpwm::window-name current-window)))
	     (stumpwm::y-or-n-p "would you like to mark the log summarization as complete?"))
    (progn (stumpwm::remove-hook STUMPWM:*FOCUS-WINDOW-HOOK* 'maybe-finished-with-log-summary)
	   (record-event (mmg::habit-by-name "summarize logs") 
			 (event :finished)))))

(defun last-log-date ()
  "to be replaced by manardb"
  nil)

(defun update-index ()
  (let* ((a (qlpp "gabriel-laddel.github.io/index.org"))
	 (b (search "* Contents" (slurp-file a) :test #'string=))
	 ;; + 2 for newline
	 (c (take (+ 2 b (length "* Contents")) (slurp-file a)))) 
    (with-open-file (s a :direction :output
			 :if-exists :supersede) 
      (format s "~a" 
	      (->> (ls-clean (qlpp "gabriel-laddel.github.io"))
		   (filter (lambda (pathname) (and (string= "html" (pathname-type pathname))
					      (not (string= "index" (pathname-name pathname))))))
		   (mapcar (lambda (html-file) 
			     (let* ((post-title (pathname-name html-file))
				    (relative-pathname (cat post-title "." (pathname-type html-file))))
			       (format nil "[[file:./~a][~a]]" relative-pathname post-title))))
		   (cons c)
		   (apply #'cat))
	      :supersede))))



;;; log summaries
;;; ============================================================================
;;;
;;; botched.
;;; 
;;; '((13852 5812)
;;;   (15124 9717)
;;;   (17101 2436)
;;;   (19437 15967)
;;;   (18777 1644)
;;;   (23515 6453))

;; (in-package masamune)

;; (defvar *failed-parses* nil)

;; (defun parse-log-time (old-timestring)
;;   (let* ((months '(("Jan" 1) ("Feb" 2) ("Mar" 3) ("Apr" 4)
;; 		   ("May" 5) ("Jun" 6) ("Jul" 7) ("Aug" 8)
;; 		   ("Sep" 9) ("Oct" 10) ("Nov" 11) ("Dec" 12))))
;;     (destructuring-bind (_ month day j)
;; 	(butlast (remove-if #'emptyp (split " " old-timestring)))
;;       (destructuring-bind (hour min sec) (split ":" j)
;; 	(encode-timestamp 0 (parse-integer sec) (parse-integer min)
;; 			  (parse-integer hour) (parse-integer day) 
;; 			  (loop for (tt o) in months
;; 				when (string= tt month) return o) 2014)))))

;; (defun parse-log (pathname)
;;   (let* ((title) (body) (start-time))
;;     (handler-case
;; 	(walk-tree (lambda (l)
;; 		     (and (listp l) 
;; 			  (case (car l)
;; 			    (title (setq title (second l)))
;; 			    (time (setq start-time (timestamp-to-universal (parse-log-time (second l)))))
;; 			    (date (setq start-time (timestamp-to-universal (parse-log-time (second l)))))
;; 			    (body (setq body (second l)))
;; 			    (chronicle nil)))) 
;; 		   (read-file pathname))
;;       (error nil (progn (push pathname *failed-parses*)
;; 			(return-from parse-log))))
;;     (list :title title :body body
;;     	  :start-time start-time
;;     	  :end-time (+ (* 20 60) start-time)
;;     	  :vocabulary-words nil 
;;     	  :word-count (length (split " " body)))))

;; (:NICK "mircea_popescu:" :MESSAGE
;;        ("im not sure why you want to run 2014 hw at all. but whatever, maybe i don&#039;t do enough hi res porn conversion on a laptop to care ?")
;;        :LINK "/?date=2-12-2014#942992" :TIME "23:45:39")
;; (:NICK "kakobrekla:" :MESSAGE
;;        ("nsa check, bitcoin foundation check, ... whats next, usg?") :LINK
;;        "/?date=3-12-2014#943182" :TIME "00:45:16")

;; (defparameter canonical-logs
;;   ;; (write-to-file "~/.masamune/b-a-logs/working-logs.lisp" (logs "21-11-2014"))
;;   (car (read-file "~/.masamune/b-a-logs/working-logs.lisp")))
;; (defparameter dateless-logs (apply #'append (mapcar #'second canonical-logs)))
;; (defvar *nicks* (distinct (mapcar (lambda (l) (getf l :nick)) dateless-logs) :test #'string=))
;; (defvar *failed* nil)

;; (defparameter *finished-logs* 
;;   (with-open-file (s "~/.masamune/b-a-logs/finally-fucking-finished.lisp" :direction :input)
;;     (let* ((k) (o)) 
;;       (handler-case (loop while t for line = (read-line s)
;; 			  do (cond ((and (emptyp (trim-dwim line)) k) (progn (push k o) (setf k nil)))
;; 				   ((emptyp (trim-dwim line)))			 
;; 				   (t (push line k))))
;; 	(error nil (progn (push k o)
;; 			  (mapcar (lambda (j) (read-from-whole-string (apply #'cat (nreverse j)))) 
;; 				  (nreverse o))))))))

;; (defun locate-message (nick message-string)
;;   (find-if (lambda (l) (and (equal nick (getf l :nick))
;; 		       (equal (list message-string) (getf l :message-string))))
;; 	   dateless-logs))

;; (defun filter-by-nick (nick) 
;;   (filter (lambda (l) (string= (trim-dwim nick) (trim-dwim (getf l :nick)))) dateless-logs))

;; (defun find-selection (start-message stop-message)
;;   (subseq dateless-logs 
;; 	  (position start-message dateless-logs :test 'equal)
;; 	  (1+ (position stop-message dateless-logs :test 'equal))))

;; (defun maybe-lord (nick class)
;;   (if (member nick '("nanotube" "mircea_popescu" "kakobrekla" "joecool" "jurov"
;; 		     "fluffypony" "Apocalyptic" "davout" "thickasthieves"
;; 		     "bingoboingo" "Namworld" "mod6" "nubbins`" "asciilifeform"
;; 		     "ben_vulpes" "dignork" "mike_c" "justusranvier" "FabianB"
;; 		     "thestringpuller" "pete_dushenski" "moiety" "pankkake" "artifexd"
;; 		     "punkman" "chetty" "mthreat" "dub" "bounce" "diametric")
;; 	      :test #'string=)
;;       "lord" class))

;; (defun fuzzy-locate-message (nick message)
;;   (find-if (lambda (l) (some (lambda (k) (or (and (stringp k) (stringp message) (search message k :test 'string=))
;; 				   (and (listp k) (listp message) (equal k message))))
;; 			(getf l :message)))
;; 	   (filter-by-nick nick)))

;; (defun lpos (pl) (position pl dateless-logs :test 'equal))

;; (defun resolve-date (l)
;;   (loop for (date logs) in canonical-logs
;; 	do (when (member l logs :test 'equal) (return date))))

;; (length (remove-if (lambda (j) (if (= 1 (length j))
;; 			      (apply #'locate-message (car j))
;; 			      (and (apply #'locate-message (car j)) (apply #'locate-message (llast j))))) 
;; 		   *tasty-logs*))

;; (mapcar (lambda (i) (handler-case (if (= 1 (length i))
;; 				 (apply 'locate-message (car i))
;; 				 (list (apply 'locate-message (car i) )
;; 				       (apply 'locate-message (llast i))))
;; 		 (error nil (push i *failed*)))) *tasty-logs*)

;; (with-open-file (stream "~/.masamune/b-a-logs/finally-fucking-finished.lisp"
;; 			:direction :input)
;;   (let* ((current-tidbit) (out)) 
;;     (handler-case 
;; 	(loop while t
;; 	      do (let* ((line (read-line stream)))
;; 		   (if (emptyp (trim-dwim line))
;; 		       (progn (push current-tidbit out)
;; 			      (setf current-tidbit nil))
;; 		       (push line current-tidbit))))
;;       (error nil (mapcar #'reverse 
;; 			 (nreverse 
;; 			  (remove-if #'null 
;; 				     (progn (push current-tidbit out) out))))))))

;; (group-by (loop for (f l) in near
;; 		collect (subseq dateless-logs f l))
;; 	  (lambda (k) (resolve-date (car k))))


;; ;; (with-open-file (stream #P"~/.masamune/b-a-logs/finally-fucking-finished.lisp"
;; ;; 			:direction :output
;; ;; 			:if-exists :append
;; ;; 			:if-does-not-exist :create)
;; ;;   (loop for (i j) in (filter (lambda (l) (< (car l) (second l))) (remove-if 'numberp *testing*))
;; ;; 	do (format stream "~%~%~%~s~%~s" (nth i dateless-logs) (nth (- j 1) dateless-logs))))
 
;; ;; (length (remove-if (lambda (j) (if (= 1 (length j))
;; ;; 			       (apply #'fuzzy-locate-message (car j))
;; ;; 			       (and (apply #'fuzzy-locate-message (car j)) (apply #'fuzzy-locate-message (llast j))))) 
;; ;; 		    *tasty-logs*))filter

;; (group-by (take 3 *finished-logs*) (lambda (l) (resolve-date (car l))))

;; (loop for (date start-and-end-logs) in (group-by *finished-logs* (lambda (l) (resolve-date (car l))))
;;       do (let* ((logs (loop for l in start-and-end-logs
;; 			    ;; do (format t "~%first: ~s~%second: ~s" (car l) (second l ))
;; 			    collect (if (= 1 (length l)) l 
;; 					(subseq dateless-logs (lpos (car l)) (1+ (lpos (second l))))))))
;; 	   (with-open-file (stream (merge-pathnames "~/quicklisp/local-projects/gabriel-laddel.github.io/plists/" (cat date ".lisp"))
;; 				   :direction :output
;; 				   :if-exists :supersede
;; 				   :if-does-not-exist :create)
;; 	     (loop for l in logs
;; 		   do (format stream "~%(~{~%~S~})~%" logs)))
;; 	   ;; (with-open-file (stream (merge-pathnames "~/quicklisp/local-projects/gabriel-laddel.github.io/html-pages/" (cat date ".html"))
;; 	   ;; 			:direction :output
;; 	   ;; 			:if-exists :supersede
;; 	   ;; 			:if-does-not-exist :create)
;; 	   ;;   (eval `(cl-who:with-html-output (s ,stream)
;; 	   ;; 	   (:html (:meta :http-equiv "Content-Type" :content "text/html;charset=utf8")
;; 	   ;; 		  (:link :rel "stylesheet" :type "text/css" :href "finishing-touches.css")
;; 	   ;; 		  (:body
;; 	   ;; 		   (:div :id "content"
;; 	   ;; 			 (:h1 "01-01-2015")
;; 	   ;; 			 ,@(loop for j in (loop for lol in start-and-end-logs
;; 	   ;; 						collect (if (= 1 (length lol)) lol
;; 	   ;; 							    (subseq dateless-logs (lpos (car lol)) (1+ (lpos (second lol))))))
;; 	   ;; 				 collect (list :div :class "selection" 
;; 	   ;; 					       (loop for pl in j
;; 	   ;; 						     for class = "zero" then (if (string= "zero" class)
;; 	   ;; 										 "one" "zero")
;; 	   ;; 						     collect (with-getfs (:nick :message :link) pl
;; 	   ;; 							       `(:div :class ,class
;; 	   ;; 								      (:a :href ,(log-url link) ,(cat nick " ")) ,@message)))))))))))
;; 	   ))

;;; Captain McCLIM
;;; ============================================================================
;;; 
;;; This is a doodle of a game based around the mechanics of regular expressions
;;; and common lisp format strings. The idea is that enemies fall down the
;;; screen at you, labeled with strings. You use regexes to kill them, and they
;;; drop their strings. You can catch the strings, switch weapons to format
;;; and write a format control that will consume your new string ammunition and
;;; fire in some clever manner.
;;;
;;; I concluded that CLIM is not a suitable vehicle for such an endeavor because
;;; it is too slow.
;;;
;;; `handle-event' contains key logic and `top-level-command-loop' game logic.
;;;
;;; compiling this file won't get you anywhere btw. The most interesting thing
;;; one can learn from it is that you must use a `defmethod' rather than a
;;; `defun' if you want to incrementally compile it. Why? idk.
;;; 
;;; Look & Feel
;;; ============================================================================
;;; http://www.youtube.com/watch?v=lf5NaSPODtg
;;; http://www.youtube.com/watch?v=dMRYiJCSmGk
;;; https://www.youtube.com/watch?v=HtWBdCMrXwQ
;;; https://www.youtube.com/watch?v=JEBN8vqfaGo
;;; http://percsich.hu/tgmforum/index.php?topic=446.0
;;; https://www.youtube.com/watch?v=YZo6zpKMKZw
;;; 
;;; format
;;; ============================================================================
;;; (format t "it was the best of times ~
;;;            it was the worst of times")
;;; (format nil "~d-~d-~2,'0DT~A.000000-07:00" 12 12 1 12)
;;; https://www.cs.cmu.edu/Groups/AI/html/cltl/clm/node200.html
;;; http://www.gigamonkeys.com/book/a-few-format-recipes.html
;;; http://psg.com/~dlamkins/sl/chapter24.html
;;; 
;;; Directive   Interpretation
;;; ---------   --------------
;;;    ~&       fresh line
;;;    ~|       page break
;;;    ~T       tab stop
;;;    ~<       justification
;;;    ~>       terminate ~<
;;;    ~C       character
;;;    ~(       case conversion
;;;    ~)       terminate ~(
;;;    ~D       decimal integer
;;;    ~B       binary integer
;;;    ~O       octal integer
;;;    ~X       hexadecimal integer
;;;    ~bR      base-b integer
;;;    ~R       spell an integer
;;;    ~P       plural
;;;    ~F       floating point
;;;    ~E       scientific notation
;;;    ~G       ~F or ~E, depending upon magnitude
;;;    ~$       monetary
;;;    ~A       legibly, without escapes
;;;    ~S       READably, with escapes
;;;    ~~       ~
;;; 
;;; Ideas
;;; ============================================================================
;;;
;;; - launch 'cascading' iterations that hit multiple of the enemies, eat their
;;;   strings and use them as ammo to the iteration?
;;;
;;; - should you be able to launch regexs and formats at the same time?
;;;
;;; - there isn't any real reason you wouldn't want to have at least one weapon
;;;   enabled - give some sort of speed boost if both are off?
;;;
;;; - can tree traversals somehow be worked into the game?
;;; 
;;; TODO
;;; ============================================================================
;;; - I find the behavior of `regex-replace-all' counter-intuitive. I expect
;;; that when given :start and :end arguments they specify the parts of the
;;; string that will be replaced, returning the whole string. instead, it
;;; splices it out, as if by subseq.
;;;
;;; - methods can be reloaded on a running application but functions cannot -
;;;   how can `top-level-command-loop' be restructured so that it will allow
;;;   for live reloading?
;;; 
;;; - turn off screen recording
;;; 
;;; Storyboard
;;; ============================================================================
;;; The main idea here is to teach the underlying mechanics of regexs and format 
;;;
;;; you should be forced to catch the falling strings if you want to feed them
;;; into format, rather tham just absorbing them
;;; 
;;; notes
;;; =====
;;; - specifically leaving out parsing JSON and XML, there are standard ways to
;;;   interface with this and you don't want to teach bad habits.
;;;
;;; Regex
;;; ============================================================================
;;; http://perldoc.perl.org/5.8.8/perlre.html 
;;; http://perldoc.perl.org/5.8.8/perlrequick.html
;;; http://perldoc.perl.org/5.8.8/perlretut.html
;;; 
;;; cl-ppcre has perl 5.8 regex synax, the following features work (from the
;;; cl-ppcre doc page):
;;; 
;;; - non-greedy repetitionns
;;; 
;;; - positive and negative look-ahead and look-behind assertions
;;; 
;;; - "standalone" subexpressions
;;; 
;;; - conditional subpatterns
;;; 
;;; - \t, \n, \r, \f, \a, \e, \033 (octal character codes), \x1B (hexadecimal
;;;   character codes), \c[ (control characters), \w, \W, \s, \S, \d, \D, \b,
;;;   \B, \A, \Z, and \z are supported.
;;;   
;;; - The mode keyword arguments are equivalent to the "imsx" modifiers in
;;;   Perl. The destructive keyword will be ignored.
;;;   
;;;   NOTE: these, though I can't find where to pass them in the regex matches,
;;;   should be powerups
;;;
;;; - Perl's \Q and \E - see *ALLOW-QUOTING* below. Make sure you also read the
;;;   relevant section in "Bugs and problems."
;;; 
;;; - support for AllegroCL's (?<name>"<regex>") named registers and \k<name>
;;;   back-references syntax, have a look at *ALLOW-NAMED-REGISTERS* for
;;;   details.
;;; 
;;; - named properties (\p and \P), but only the long form with braces is
;;;   supported, i.e. \p{Letter} and \p{L} will work while \pL won't.
;;; 
;;; - The keyword arguments are just for your convenience. You can always use
;;;   embedded modifiers like "(?i-s)" instead.
;;;
;;; - simple string matching
;;;   
;;; - non-printable ASCII characters
;;;   \t tab
;;;   \n newline
;;;   \r carriage return
;;;   arbitary bytes can be represented by octal escape sequences, eg, \033 or
;;;   hexadecimal escape sequences, eg, \x1B
;;;
;;; - lines 
;;; 
;;; - detect a single quote " but not \" within some string of text.
;;; 
;;; functions 
;;; =========
;;; the following functions might map to game mechanics
;; scan
;; scan-to-strings
;; do-scans
;; do-matches
;; do-matches-as-strings
;; all-matches
;; all-matches-as-strings
;; split
;; regex-replace
;; regex-replace-all
;; quote-meta-chars
;; register-groups-bind
;; do-register-groups
;;; 
;;; variables
;;; =========
;;; the following variables influence game mechanics
;; *regex-char-code-limit*
;; *allow-quoting*
;; *allow-named-registers*
;; *property-resolver*

(defpackage #:captain-mcclim
  (:nicknames :cmc)
  (:use #:alexandria
	#:anaphora
	#:cl-ppcre
	#:clim
	#:clim-internals
	#:clim-lisp
	#:iterate
	#:let-plus
	#:masamune
	#:zpng)
  (:shadow :frame :frame-p :frame-name)
  (:shadowing-import-from :clim :simple-parse-error)
  (:export #:play-captain-mcclim))

(in-package #:climi)

(defmethod save-pattern ((pattern rgb-pattern) pattern-pathname)
  (mm:write-to-file pattern-pathname (clim-internals::image-data (clim-internals::image pattern))))

(defun load-pattern (pattern-pathname)
  (make-instance 'rgb-pattern :image
		 (let* ((read-pattern-data (mm:read-from-file pattern-pathname))
			(array-dimensions (array-dimensions read-pattern-data ))
			(h (car array-dimensions)) 
			(w (cadr array-dimensions))
			(correctly-typed-array (make-array array-dimensions :element-type '(unsigned-byte 32))))
		   (loop for i from 0 below h
			 do (loop for j from 0 below w
				  do (setf (aref correctly-typed-array i j) (aref read-pattern-data i j))))
		   (make-instance 'rgb-image :data correctly-typed-array
					     :height h :width w))))

(export '(save-pattern load-pattern))

(in-package #:cmc)

(defvar *application-frame* nil)
(defvar *latest-uuid* 0)
(defvar *event-stream* nil)
(defparameter *animation-rate* 30)
(defparameter *grunt-pattern*
  (clim-internals::make-pattern-from-bitmap-file (ppath "/images/grunts0.png") :format :png))
(defparameter *splash-pattern*
  (make-pattern-from-bitmap-file (ppath "/images/splash.png") :format :png))
(defparameter *tile*
  (make-pattern-from-bitmap-file (ppath "/images/tile.png") :format :png))
(defparameter *player-movement-step* 8)
(defparameter *game-state* :playing)
(defparameter *hero* '(40 100))
(defparameter *units* nil)
(defparameter *munitions* nil)
(defparameter *formatting* t)
(defvar *format-chars* nil)
(defvar *ammunition* "ab 2 2 1")
(defvar *regex-chars* '(#\[ #\a #\- #\z #\0 #\- #\9 #\]))
(defparameter *modifier* nil)
(defparameter *input-mode* nil)
(defparameter *debuggering* nil)
(defparameter +highlight-ink+ +purple+)
(defparameter +body-ink+ +black+)
(defparameter *effects* nil)

(define-application-frame captain-mcclim () ()
  (:pointer-documentation nil)
  (:menu-bar nil)
  (:panes (viz :application 
	       :text-cursor nil
	       :scroll-bars nil))
  (:layouts (default viz))
  (:top-level (top-level-command-loop)))

(mm::c piece () (x y))
(mm::c enemy (piece) (strings))
(mm::c effect (piece) (frames))
(mm::c hero (piece) ())
(mm::c enemy-a (enemy) ())
(mm::c enemy-b (enemy) ())
(mm::c explosion (effect) ())
(mm::c regex-bullet (piece))
(mm::c format-bullet (piece))

(defgeneric offscreen? (piece)
  (:documentation "if a piece is still (vertically) onscreen, return it. XXX, main game loop is dependent on this behavior."))

(defmethod offscreen? ((piece piece))
  (when (> (y piece) mm::screen-height) piece))

(defgeneric draw-piece (piece sheet)
  (:documentation "every peice must implement its own drawing routine and manage its own position information."))

;;; implementations

(defmethod draw-piece ((hero hero) sheet)
  (with-slots (x y) hero (draw-circle* sheet x y 10 :ink +blue+)))

(defmethod draw-piece ((a enemy-a) sheet)
  (with-slots (x y) a (draw-circle* sheet x y 7 :ink +orange+)))

(defmethod draw-piece ((b enemy-b) sheet)
  (with-slots (x y) b (draw-circle* sheet x y 10 :ink +orange+)))

(defmethod draw-piece ((explosion explosion) sheet)
  (with-slots (x y frames) effect
    (when (and x y frames)
      (let* ((radius (car frames)))
	(draw-circle* sheet x y radius :ink +red+ :filled nil :line-thickness 2)
	(draw-circle* sheet x y (if (< (- radius 4) 0) 0 (- radius 4)) :ink +orange+ :filled nil :line-thickness 5.5)
	(setf (frames effect) (rest (frames effect)))))))

(defmethod draw-piece ((b enemy-b) sheet)
  (with-slots (x y) b (draw-circle* sheet x y 10 :ink +orange+)))

(defmethod draw-piece ((b enemy-b) sheet)
  (with-slots (x y) b (draw-circle* sheet x y 3 :ink +orange+)))

(defun enemies () (filter (lambda (p) (typep p 'enemy)) *pieces*))
(defun effects () (filter (lambda (p) (typep p 'effect)) *pieces*))
(defun fire-format ())
(defun fire-regex ())

(defmethod event-handler ((application-frame captain-mcclim) (gesture t))
  "you can update a method on an object and it will be loaded into the running
CLIM system, but not a function, thus `top-level-command-loop' defers here"
  (loop for mod-key = (unless (characterp gesture) (keyboard-event-key-name gesture))
	when (and gesture (not (typep gesture 'pointer-button-press-event)))
	  do (progn (when *debuggering* 
		      (format mm::*swank-connection-hack* "~% gesture: ~s" gesture)
		      (push gesture *event-stream*))
		    (labels ((nullify-modifier () (setq *modifier* nil)))
		      (macrolet ((add-char (location) `(setf ,location (append ,location (list gesture))))
				 (backspace (location) `(when (/= 0 (length ,location)) 
							  (setf ,location (subseq ,location 0 (- (length ,location) 1))))))
			(cond ((and mod-key (eq :ESCAPE mod-key)) 
			       (setf *game-state* (if (eq :paused *game-state*) :playing :paused)))

			      ((and mod-key (member mod-key '(:CONTROL-RIGHT :CONTROL-LEFT) :test #'eq)) 
			       (setf *modifier* mod-key))

			      ((and *modifier* (equal gesture #\space))
			       (cond ((eq :format *input-mode*) (format-blast))
				     ((eq :regex *input-mode*)  (regex-blast))
				     (t (setf (cadr *hero*) (- (cadr *hero*) (* 12 *player-movement-step*)))))
			       (nullify-modifier))

			      ((and *modifier* (equal gesture #\r))
			       (if (or (null *input-mode*) (eq :format *input-mode*))
				   (setf *input-mode* :regex)
				   (setf *input-mode* nil))
			       (nullify-modifier))

			      ((and *modifier* (equal gesture #\f))
			       (if (or (null *input-mode*) (eq :regex *input-mode*)) 
				   (setf *input-mode* :format)
				   (setf *input-mode* nil))
			       (nullify-modifier))

			      ((equal #\Backspace gesture)         
			       (case *input-mode* 
				 (:regex (backspace *regex-chars*))
				 (:format (backspace *format-chars*))))
		     
			      (t (if mod-key
				     (case mod-key
				       (:up    (setf (cadr *hero*) (- (cadr *hero*) *player-movement-step*)))
				       (:down  (setf (cadr *hero*) (+ (cadr *hero*) *player-movement-step*)))
				       (:left  (setf (car *hero*) (- (car *hero*) *player-movement-step*)))
				       (:right (setf (car *hero*) (+ (car *hero*) *player-movement-step*))))
				     (case *input-mode*
				       (:regex (add-char *regex-chars*))
				       (:format (add-char *format-chars*))))
				 (nullify-modifier))))))))

(defun top-level-command-loop (frame &rest args)
  (declare (ignore args))
  (loop for gesture = (read-gesture :stream (frame-standard-input frame))
	do (event-handler frame gesture)))
    
(defclass game-state-update-event (climi::standard-event) ()
  (:default-initargs :sheet nil))

(defun draw-weapon-store (sheet identifier x y)
  (coerce *ammunition* 'string)
  (let* ((ink (if (or (and (eq :regex identifier) (eq :regex *input-mode*))
		      (and (eq :format identifier) (eq :format *input-mode*)))
		  +highlight-ink+ +black+))
	 (identifier-chars (case identifier
			     (:ammunition *ammunition*)
			     (:format *format-chars*)
			     (:regex *regex-chars*)))
	 (nil-string (case identifier
		       (:ammunition "[out of ammo]")
		       (t "nil"))))
    (draw-text* sheet (or identifier-chars nil-string) x y :ink ink :align-x :center :align-y :bottom)))

(defmethod handle-event (sheet (event game-state-update-event))
  (let* ((viz-pane (find-pane-named *application-frame* 'viz)))
    (with-bounding-rectangle* (x0 y0 x1 y1) (sheet-region viz-pane)
      (labels ((draw-centered-string (char-list) 
		 (draw-text* viz-pane (coerce char-list 'string) (/ x1 2) (/ y1 2)
			     :align-x :center :align-y :bottom :ink +black+ :text-size 40))
	       (hero-distance (unit) (with-slots (x y) unit
				       (round (sqrt (+ (expt (- (car *hero*) x) 2)
						       (expt (- (second *hero*) y) 2))))))
	       (clear-pane () (draw-rectangle* viz-pane x0 y0 x1 y1 :ink +white+)))
	(if (some (lambda (unit) (> 17 (hero-distance unit))) *units*)
	    (draw-centered-string "Game Over")
	    (ecase *game-state*
	      (:splash
	       (clear-pane) 
	       (draw-centered-string "Splash Screen"))
	      (:paused
	       (clear-pane)
	       (draw-centered-string "Paused")
	       ;; (setf *animation-rate* nil) ?
	       )
	      (:tutorial
	       (clear-pane)
	       (draw-text* viz-pane "Simple string matching" (/ x1 2) (/ y1 2) :ink +highlight-ink+ :align-x :center)
	       (draw-text* viz-pane "Type words and hit C-SPC to destroy enemies with matching strings."  (/ x1 2) (+ 20 (/ y1 2)) :ink +body-ink+ :align-x :center)
	       (draw-text* viz-pane "Metacharacters" (/ x1 2) (+ 40 (/ y1 2)) :ink +highlight-ink+ :align-x :center)
	       (draw-text* viz-pane "the metacharacters \"{}[]^$.|*+?\\\" are reserved for regex notation but can be matched by prefixing with a backslash." (/ x1 2) (+ 60 (/ y1 2)) :ink +body-ink+ :align-x :center))
	      (:playing
	       (clear-pane)
	       (setf *pieces* (loop for piece in *pieces* collect (progn (draw-piece piece) (offscreen? piece))))
	       (draw-weapon-store viz-pane :ammunition (/ x1 2) (- y1 10))
	       (draw-weapon-store viz-pane :regex (- (/ x1 2) 50 ) (- y1 30))
	       (draw-weapon-store viz-pane :format (+ 50 (/ x1 2)) (- y1 30)))))))))
 
(defmethod run-frame-top-level :around ((app captain-mcclim) &key)
  (let ((running t))
    (clim-sys:make-process
     (lambda ()
       (loop while running do
	 (sleep (/ 1.0 *animation-rate*))
	 (climi::event-queue-append
	  (climi::frame-event-queue app)
	  (make-instance 'game-state-update-event)))))
    (unwind-protect (call-next-method)
      (setf running nil))))

(defun run-captain-mcclim ()
  (setq *application-frame* (make-application-frame 'captain-mcclim))
  (run-frame-top-level *application-frame* :name "Captain McCLIM & The Land of Lisp"))

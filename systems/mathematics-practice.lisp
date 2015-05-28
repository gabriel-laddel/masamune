(in-package #:mm)

(defvar mathematics-practice-length 60
  "# of minutes ")

(defun start-mathematics-practice (habit)
  (record-event habit (event :started))
  (if (state-record-exists? :mathematics-practice)
      (restore-state :mathematics-practice)
      (progn (stumpwm::emacs)
	     (ignore-errors 
	      (with-live-swank-connection 
		  (swank::eval-in-emacs
		   '(progn (delete-other-windows)
		     (find-file "~/quicklisp/local-projects/masamune/systems/mathematics-practice.lisp")
		     (find-file "~/quicklisp/local-projects/masamune/mathematics-scratch.lisp")
		     nil))))))
  (stumpwm::run-with-timer
   (* mathematics-practice-length 60) nil 
   (lambda () 
     (stumpwm::message-no-timeout "Time is almost up")
     (mm::record-state :mathematics-practice)
     (loop for i from 10 downto 0
	   finally (progn (record-event habit (event :finished)) 
			  (stumpwm::message-no-timeout "finished")
			  (mmg::run-or-focus-dashboard))))))

(defun visualize-mathematics (habit sheet)
  (declare (ignore habit))
  (let* ((n 8))
    (labels ((gen (i)
	       (let* ((out-and-start '(f x)))
		 (loop
		   for k from 0 to i
		   do (setq out-and-start 
			    (apply #'append (mapcar 
					     (lambda (s) (case s
						      ;; (x '(y f + f f + + x)) 
						      ;; (y '(y f + f f x - - f f x))
						      (x '(+ y f + f f + y y +))
						      (y '(f - y f f x f f))
						      )) out-and-start))))
		 (remove-if (lambda (sym) (member sym '(x y) :test 'eq)) out-and-start))))

      (let* ((x 300) (y 300) (new-x x) (new-y y) (a 1) (step 15))
	(loop
	  for r in (gen n)
	  do (progn (cond ((= a 1) (setq new-y (+ step y))) ; .5 pi
			  ((= a 2) (setq new-x (- x step))) ; and onwards
			  ((= a 3) (setq new-y (- y step)))
			  ((= a 4) (setq new-x (+ step x))))
		    (case r
		      (f (clim:draw-line* sheet x y new-x new-y :ink clim:+blue+ :line-thickness 6 :line-cap-shape :capped)
		       (setq x new-x y new-y))
		      (- (setq a (if (= 1 a) 4 (1- a))))
		      (+ (setq a (if (= 4 a) 1 (1+ a))))
		      (t nil)))))

      (let* ((x 300) (y 300) (new-x x) (new-y y) (a 1) (step 15))
	(loop
	  for r in (gen n)
	  do (progn (cond ((= a 1) (setq new-y (+ step y))) ; .5 pi
			  ((= a 2) (setq new-x (- x step))) ; and onwards
			  ((= a 3) (setq new-y (- y step)))
			  ((= a 4) (setq new-x (+ step x))))
		    (case r
		      (f (clim:draw-line* sheet x y new-x new-y :ink clim:+white+ :line-thickness 2 :line-cap-shape :capped)
		       (setq x new-x y new-y))
		      (- (setq a (if (= 1 a) 4 (1- a))))
		      (+ (setq a (if (= 4 a) 1 (1+ a))))
		      (t nil)))))))
  (mmg::draw-image (mmg::make-image (qlpp "masamune/images/ghhardy.jpg")) 550 200 sheet) 
  (setf (clim:stream-cursor-position sheet) (values 500 600))
  (format sheet "Reductio ad absurdum, which Euclid loved so much, is one of a")
  (setf (clim:stream-cursor-position sheet) (values 500 615))
  (format sheet "mathematician's finest weapons. It is a far finer gambit than any chess play:")
  (setf (clim:stream-cursor-position sheet) (values 500 630))
  (format sheet "a chess player may offer the sacrifice of a pawn or even a piece, but a")
  (setf (clim:stream-cursor-position sheet) (values 500 645))
  (format sheet "mathematician offers the game. --A Mathematician's Apology (London 1941)"))

(defun mathematics-practice-install ()
  (push (i 'habit
	   :name "Mathematics Practice"
	   :visualization-function 'visualize-mathematics
	   :initialization-function 'start-mathematics-practice
	   :occurrence :daily)
	*habits*))

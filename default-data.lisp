(in-package #:mm)

(defun message (string) 
  (stumpwm::message-no-timeout (format-message-for-stumpwm string)))

(defun start-image-recognition ()
  "needs to be converted to CL"
  (mmb::open-uri "http://cs.stanford.edu/people/karpathy/deepimagesent/" t))

(defun start-maxima-introduction ()
  (message "Maxima is a system for the manipulation of symbolic and numerical expressions, including differentiation, integration, Taylor series, Laplace transforms, ordinary differential equations, systems of linear equations, polynomials, and sets, lists, vectors,matrices, and tensors. Maxima yields high precision numeric results by using exact fractions, arbitrary precision integers, and variable precision floating point numbertos. Maxima can plot functions and data in two and three dimensions. M-x imaxima from within Emacs will launch the imaxima program. Documentation is avalible via Emacs info system. Maxima is written in CL. Switching between CL and the Maxima syntax can be done via to_lisp(); and (to-maxima). Maxima currently doesn't integrate into the rest of the Common Lisp world (ie, use quicklisp) this will change in due time."))

(defun start-masamune-tutorial ()
  (with-live-swank-connection
      (ignore-errors
       (swank::eval-in-emacs '(progn 
			       (find-file "~/quicklisp/local-projects/masamune/introduction.lisp")
			       (end-of-buffer)
			       (delete-other-windows) nil) t))))

(defun start-common-lisp ()
  (stumpwm::select-emacs)
  (mm::listener) 
  (sleep 2)
  (stumpwm::hsplit)
  (stumpwm::select-emacs)
  (with-live-swank-connection
      (ignore-errors
       (swank::eval-in-emacs '(progn 
			       (find-file "~/quicklisp/local-projects/masamune/introduction.lisp")
			       (search-forward "Common Lisp")
			       (delete-other-windows) nil) t))))

(defun start-non-von-neumann-research-module ()
  (with-live-swank-connection
      (ignore-errors
       (swank::eval-in-emacs 
	'(progn
	  (find-file "/root/quicklisp/local-projects/masamune/systems/non-von-neumann-computing.txt")
	  (delete-other-windows)
	  nil)
	t)))
  (stumpwm::select-emacs))

(defun start-analytical-combinatorics () 
  (mmb::open-uri "http://algo.inria.fr/flajolet/Publications/books.html" t))

;;; Metric System
;;; ============================================================================
;;; I relish the thought of completely ignoring the imperial unit system.

(defvar metric-prefixes-table
  '((:text :symbol :factor)
    ("tera" "T" 1000000000000)
    ("giga" "G" 1000000000)
    ("mega" "M" 1000000)
    ("kilo" "k" 1000)
    ("hecto" "h" 100)
    ("deca" "da" 10)
    ("centi" "c" 0.01)
    ("milli" "m" 0.001)
    ("micro" "*u" 0.000001)
    ("nano" "n" 0.0000000001)
    ("pico" "p" 0.000000000001)))

(defun start-metric-system-lesson ()
  (message (format nil "implement metric system quizzes based on ~%~{~%~a~}" metric-prefixes-table)))

(defun start-cl-testing ()
  "see http://aperiodic.net/phil/archives/Geekery/notes-on-lisp-testing-frameworks.html for more information on the common lisp test frameworks. Fiasco, is the successor of stefil (see https://github.com/luismbo/stefil/issues/9 for more information) and is the best common lisp testing framework by a long shot. It currently isn't in quicklisp" 
  (unless (probe-file "~/quicklisp/local-projects/fiasco")
    (rp "cd ~/quicklisp/local-projects/ && git clone https://github.com/capitaomorte/fiasco.git"))
  (message "implement cl-testing lessons"))

(defun start-feynman-lectures ()
  (mmb::open-uri "http://www.feynmanlectures.caltech.edu/I_01.html"))

(defun start-chemistry ()
  ;; http://www.iza-structure.org/databases/  
  ;; (ql:quickload 'cl-glut-examples)
  ;; followed by
  ;; (cl-glut-examples::molview)
  ;;
  ;; to review,
  ;; - http://chemistry.stackexchange.com/questions/9044/learning-chemistry-with-software
  (message "As it stands there are a bunch of databases (some of them on paper) detailing properties of different compounds etc. there are a few programs which understand molecular formulas and some of the diagrams we use to depict them. In organic chemistry courses worldwide students are memorize reaction charts. This is amazingly primitive. An intelligently designed system would have all of these charts computerized - instantaneous access to any database you might desire, taking into account intellectual property in some fashion (not according to the currently laws, this much is certain) and also allow one to change between any given representation of molecular structure in question on a whim, modify it on a whim. Also, one should be able to specify \"I want to go from here to here\" in the reaction charts and either get a 'recipe' for their lab (ie, program 4 robotz) or run up against the current human understanding of the subject - or the limits of your own personal fork of the \"chemistry\" program that takes into account your lab data and bent. Mathematica wants to be this, but isn't and never will be for a number of reasons I'll address another day. The generalization of this is the ability for all programs in some context to seamlessly interoperate with each other without any loss of expressivity. The studious will note that this is a completely solved problem that no machine learning algorithm will be able to approximate anytime soon. That we don't have this right now today is simply because Silicon Valley (and the rest of the world) are terrible at computing."))

(defun start-backups ()
  "There should be a canonical way of going about backups. This doesn't really belong here but w/e

rsync instead of cp -a or dd, which can fail

also used to copy .iso files onto this

dd -i /dev/null >> -o /dev/sda -bs 1M ;; or something similar to write 0s to the hard drive

the 'real way' is via clonezilla, which will verify they both have the same bits

http://clonezilla.org/")

(defun start-usocket ()
  ;; writing to xterm, from: http://symbo1ics.com/blog/?p=1991
  (let* ((port 8003)
         (_ (stumpwm::run-commands (format nil "exec xterm -e nc -l -p ~d" port)))
         (socket (progn (sleep 2) (usocket:socket-connect "127.0.0.1" port :element-type 'character
									   :protocol :stream)))
         (stream (usocket:socket-stream socket)))
    (format stream "testing console output~%don't forget to `finish-output' or `force-output'")
    (force-output stream))
  (mmb::open-uri "http://mihai.bazon.net/blog/howto-multi-threaded-tcp-server-in-common-lisp"))
(progn (setq *nodes* nil)
       (make-node "Masamune Tutorial" () 'start-masamune-tutorial)
       (make-node "Metric System" '("Masamune Tutorial") 'start-metric-system-lesson)
       (make-node "Mathematics" '("Masamune Tutorial"))
       (make-node "Physics" '("Masamune Tutorial"))
       (make-node "Chemistry" '("Masamune Tutorial") 'start-chemistry)
       (make-node "Feynman Lectures" '("Physics") 'start-feynman-lectures)
       (make-node "Linear Algebra" '("Mathematics")
		  ;; emerge app-doc/blas-docs
		  ;; emerge sci-libs/blas-reference
		  ;; emerge app-doc/lapack-docs
		  ;; emerge sci-libs/lapack-reference
		  ;; http://cap-lore.com/MathPhys/Vector/
		  ;; 
		  ;; mgl-mat + lla are the common lisp libraries you want to
		  ;; use.
		  )
       (make-node "Partial Differential Equations" '( "Linear Algebra"))
       (make-node "Neural Networks" '("Linear Algebra")) ;; see Christopher Olah's blog for more information
       (make-node "Analytical Combinatorics" '( "Linear Algebra") 'start-analytical-combinatorics)
       (make-node "Facial Recognition" '("Neural Networks"))
       (make-node "Image Recognition" '("Neural Networks") 'start-image-recognition)
       (make-node "Hacking Masamune" '("Masamune Tutorial"))
       (make-node "Backups" '("Masamune Tutorial") 'start-backups)
       (make-node "Reporting Bugs"  '("Hacking Masamune")
		  ;; all current bug report formats suck. ideally, one would like to
		  ;; make use of `save-sbcl-and-die' instead of a bug report. this would
		  ;; allow one to quickly diagnose if the problem lies in masamune, the
		  ;; hardware, or compiler. also, any quicklisp packages installed, any
		  ;; information that can be grokked about the current hardware etc.
		  ;;
		  ;; start from showing how to debug https://github.com/stumpwm/stumpwm/wiki/Freezes
		  )
       (make-node "CLIM, Stumpwm and Emacs" '("Hacking Masamune"))
       (make-node "The browser" '("Hacking Masamune"))
       (make-node "The dashboard" '("Hacking Masamune"))
       (make-node "Maxima" '("Masamune Tutorial") 'start-maxima-introduction)
       (make-node "Garbage Collection [research]" '("Masamune Tutorial")
		  (lambda () (mmb::open-uri "https://github.com/waterhouse/emiya" t))) 
       (make-node "Non-Von Neumann Computing [research]" '("Masamune Tutorial") 'start-non-von-neumann-research-module)
       (make-node "Common Lisp" '("Masamune Tutorial") 'start-common-lisp)
       (make-node "CL format" '("Common Lisp"))
       (make-node "Pathnames" '("Common Lisp"))
       (make-node "CL regular expressions" '("Common Lisp"))
       (make-node "CL Introduction " '("Common Lisp"))       
       (make-node "Inline assembler " '("Common Lisp")
		  (lambda () 
		    (mmb::open-uri "http://www.pvk.ca/Blog/2014/03/15/sbcl-the-ultimate-assembly-code-breadboard/")
		    (mmb::open-uri "http://www.tldp.org/HOWTO/Assembly-HOWTO/" t)))       
       (make-node "CLOS" '("CL Introduction " "Common Lisp"))
       (make-node "CLIM" '("CLOS"))
       (make-node "testing CL code" '("Common Lisp") 'start-cl-testing)
       (make-node "Async CL" '("Common Lisp")
		  (lambda () (mmb::open-uri "https://glyph.twistedmatrix.com/2012/01/concurrency-spectrum-from-callbacks-to.html")))
       (make-node "CL-CONT" '("Async CL"))       
       (make-node "CL-ASYNC" '("Async CL"))
       (make-node "usocket" '("Common Lisp") 'start-usocket)
       (make-node "Interfacing with C code" '("Common Lisp"))
       (make-node "Manipulating the C AST" '("Interfacing with C code"))
       (make-node "Introduction to Maxima" '("Maxima"))
       (make-node "Hacking Maxima" '("Common Lisp" "Maxima"))
       (make-node "hcsw.org, Nuff" '("Masamune Tutorial"))
       (make-node "SICP" '("Masamune Tutorial"))
       (make-node "SICM" '("Physics"))
       (setq mmg::*root-node* (node-by-name "Masamune Tutorial"))
       (setq *focused-node* (node-by-name "Masamune Tutorial"))
       (setf (description (node-by-name "Common Lisp")) '("http://en.wikipedia.org/wiki/Common_Lisp"
							  "http://common-lisp.net/tutorials/")))

;;; Introduction + an Architectural Tour
;;; ============================================================================
;;; If you're not already familiar with Emacs keybindings, an introductory
;;; tutorial is available via C-h t.
;;; 
;;; If you're already familiar with Emacs Lisp and CL you will want to read the
;;; sections: Browser, CLIM, 3D, Maxima and Masamune Specific Abstractions.
;;;
;;;
;;;
;;; Contemporary computing is a mess of design anti-patterns. Wikipedia's List
;;; of Programming Languages names ~700.[1] Surely all these languages, their
;;; libraries, frameworks etc cannot be necessary? How is one to choose the
;;; correct set of abstractions on which to build?
;;;
;;; A common, and incorrect, answer to this particular dilemma is to accept that
;;; there is no "right way" of developing software, or that 'it' has not been
;;; discovered yet. The correct answer is that the height of programmatic
;;; expressivity was reached on the Symbolics Lisp machine during the 1980s and
;;; that in spite of endless derping to the contrary, everything since then has
;;; been braindamage of one variety or another.[2]
;;; 
;;; Masamune contains a set of abstractions that span most of computing. As of
;;; today it is incredibly underdeveloped. Even so, the foundations are solid
;;; enough to use in one's day-to-day programming and internal tooling for small
;;; organizations.
;;;
;;; Emacs
;;; ====================================
;;; Is written in a combination of Emacs Lisp and C. The tutorial covers only a
;;; fraction of its capabilities. Details are available in the User
;;; Manual, accessible via C-h i. C-h ? is the general purpose Help command.
;;;
;;; NOTE: Emacs allows you to record the current window configuration using
;;; C-x r w and restore it using C-x r j. Try this now. (Save the window
;;; configuration, split it many times and restore the previous one).
;;;
;;; This will be immensely useful for working through this lesson and beyond.
;;;
;;; Common Lisp
;;; ====================================
;;; Is the golden standard in programmatic expressive power. It is completely
;;; modifiable and eminently comprehensible. Lisp is composed of two fundamental
;;; types: atoms & s-expressions. S-expressions are sometimes referred to as
;;; "forms", "sexprs" or "sexps".
;;;
;;; Atoms evaluate to themselves. Numbers, keywords, characters and strings
;;; are all atoms.
;;; 
;;; 10
;;; :keyword
;;; "string"
;;; #\C
;;;
;;; Symbols are unique strings that can be bound to other values. The code
;;; 
;;; (defvar my-value :my-keyword)
;;; 
;;; Binds the symbol `my-value' to :my-keyword. An sexpr is a list of atoms
;;; aor forms and symbols in fully-parenthesized prefix notation. The
;;; preceding form (beginning with `defvar') is a an s-expression.
;;;
;;; The strategy for evaluating s-expressions is to treat the first argument as
;;; a function, passing the remaining elements as its arguments. The function
;;; `defvar' binds its first argument to its second. The arithmetic expression
;;; 2+3+4 is notated as
;;;
;;; (+ 2 3 4)
;;;
;;; You can evaluate arbitrary s-expressions with the key-chord C-x C-e. Try
;;; moving the cursor to the end of the previous s-expression and evaluating
;;; it. Try also C-u C-x C-e.
;;;
;;; Lisp systems cater to the curious. The general strategy for learning about
;;; that which you do not understand is to,
;;;
;;; 1. Move onto or to the end of the symbol in question and use M-x
;;;    slime-describen-symbol (C-c C-d C-d) to get a description of what the
;;;    symbol is bound to and any documentation it may have.
;;;
;;;    Try this now on the symbol `apropos'
;;;    
;;; 2. Read the Common Lisp HyperSpec. CLHS documents the entire standard
;;;    library.  Use M-o h on a `symbol' to view its hyperspec definition.
;;;                                ^
;;;                                |
;;;                       try it on this symbol!
;;;
;;;    The explanation previously offered about atoms, symbols and s-expressions
;;;    was incomplete. You can read more about `atoms' and `symbol's by opening
;;;    the corresponding pages in CLHS. "S-expression" is a bit of a
;;;    colloquialism, the corresponding Hyperspec nodes are entitled `cons' and
;;;    `list'.
;;;
;;; 3. You can jump to the source of *any* form using M-. and use M-, to return
;;;    to the jump point. Try this on `apropos'. Because you can compile
;;;    arbitrary forms without stopping the program (incremental compilation) it
;;;    is possible to jump to the definition of a form, modify it in place and
;;;    then return to whatever one was doing. There are *NO* restrictions on
;;;    what you can modify, but CL will warn you if you're going to changeP
;;;    something in the standard library.
;;;
;;;    Let's do this now. M-. `apropos' and add the form
;;;
;;;    (format t "l33t hacks!~%~%")
;;;
;;;    above the `dolist' sexpr so the definition of `apropos' resembles:
;;; 
;;;    (defun apropos ...
;;;      ;; we can cons and GC a lot faster than the typical user can read..
;;;      (format t "l33t hacks!~%~%")
;;;      (dolist ...))
;;; 
;;;    After modifying `apropos' compile the form at point using C-c C-c. CL
;;;    will notice that you're modifying a standard library function and signal
;;;    a `condition'. The CL condition (exception) system is quite interesting,
;;;    and deserves a brief tour. If you've not already, compile your modified
;;;    `apropos' now.
;;;
;;;    Under the description of the condition are, 
;;;
;;;    *Restarts:* lists possible actions. You could press N, or the restart
;;;    itself to activate it. Leave it be for the time being.
;;; 
;;;    *Backtrace:* Move your cursor over 'frames' in the backtrace and press
;;;    RET, or click on them to see local variables for that stack frame. Do
;;;    this on the 0th frame and then click / RET the
;;;    
;;;    'PACKAGE = #<PACKAGE "COMMON LISP">'
;;;    
;;;    form.  This will open up the *slime-inspector*. The slime inspector
;;;    allows you to examine and modify arbitrary objects at runtime. There are
;;;    many other circumstances in which the inspector will be useful and we
;;;    will explore them later.
;;; 
;;;    Move into the backtrace and press M-n and M-p to move through the
;;;    frames. Notice that the sources for the code in question are being
;;;    opened. You can evaluate arbitrary code in the environment of a frame
;;;    using 'e'.
;;;
;;;    OK. Activate the "0: [CONTINUE]" restart. You'll get a similar condition
;;;    from SB-IMPL. Use the same restart.
;;;
;;;    4. `apropos' (now hotpatched) is used for locating code loaded into the
;;;    current lisp process. Usually it is used at the REPL. Summon the REPL via
;;;    C-c C-z. The promt, SOMETHING-UPCASE> indicates the current package. For
;;;    example, if SOMETHING-UPCASE> were the prompt, the current *package*
;;;    would be #<PACKAGE "SOMETHING-UPCASE">.  You can view the current package
;;;    by typing "*package*" (no quotes) and pressing RET at the REPL. What can
;;;    one do with packages? `apropos' accepts two arguments, a search string
;;;    and a package to search in. Evaluating
;;;
;;;    (apropos "package" 'cl)
;;;
;;;    Will print all symbols containing "package" found in the CL *package*.
;;;    From there, one usually M-.s around. Notice that the string,
;;;
;;;    l33t hacks!
;;;
;;;    was printed prior to the `apropos' results. While we're here, try
;;;    (find-package 'cl) at the REPL. It will return #<PACKAGE "COMMON-LISP">
;;;    which you can inspect by right clicking on it and selecting the
;;;    appropriate option.
;;;    
;;;    5. Lisp systems are largely self-documenting. Searching the Internet
;;;    should be a last resort. 
;;; 
;;; Emacs and the CL process are connected by a program known as
;;; SLIME/SWANK. One can program Emacs from the CL process and vice-versa.
;;;
;;; Evaluating the next form will open an introduction to the language CL from
;;; http://learnxinyminutes.com/docs/common-lisp/ that has been modified to
;;; better accommodate Masamune users.

(ignore-errors
 ;; evaluating elisp always throws , idk, hacking around it fttb
 (swank:eval-in-emacs '(progn 
			(find-file "~/quicklisp/local-projects/masamune/learn-x-in-y-minutes-common-lisp.lisp")
			(delete-other-windows)
			(slime-switch-to-output-buffer)
			(pop-to-buffer "learn-x-in-y-minutes-common-lisp.lisp")) t))

;;; The browser is also connected to the Lisp process and can also be programmed
;;; in CL. Evaluating the next form will open introductory CL books you may find
;;; more suited to your tastes than the learn-x-in-y-minutes introduction.

(progn
  (mmb::open-uri "http://learnlispthehardway.org/book/")
  (mmb::open-uri "http://www.psg.com/~dlamkins/sl/contents.html")
  (mmb::open-uri "http://www.gigamonkeys.com/book/" t))

;;; Browser
;;; ====================================
;;; "The web" is currently a necessary evil. We control the browser via
;;; Parenscript, an extended subset of CL that compiles down to javascript.
;;; Emacs, CL and the browser are currently somewhat loosely coupled, but
;;; there are a few niceties that make life that much easier:
;;;
;;; - You can open files into the browser from emacs while in dired using 'e' 
;;;
;;; - Pdf files and the like can be opened in emacs from the browser by calling
;;;   "emacsclient" when prompted for a shell command to run on the downloaded
;;;   file. Emacs renders pdfs to images.
;;; 
;;; - output for the browser is located at ~/.masamune/browser-output and can
;;;   be written to using the dump("javascript function"); 
;;; 
;;; - For evaluating JS in the browser, use M-:
;;;
;;; - C-i to edit a browser text field in emacs, C-x # to signal completion.
;;;   (TODO: this doesn't always work - wtf why?)
;;;
;;; Parenscript
;;; -----------
;;;
;;; To play with parenscript, you must be in either the parenscript or
;;; mmb packages.
;;; 
;;; use (mps (parenscript-sexpr...)) to evaluate parenscript in the
;;; browser. Again, you /must/ be in #<PACKAGE "MASAMUNE-BROWSER">.
;;; 
;;; The Parenscript reference manual is located at:
;;; 
;;; (mmb::open-uri "file:///root/quicklisp/local-projects/Parenscript/docs/reference.html" t)
;;;
;;; There is a partially complete javascript to parenscript translator. At some
;;; point the whole of conkeror will be converted to parenscript.

(ignore-errors
 (swank:eval-in-emacs
  '(progn (find-file "~/quicklisp/local-projects/masamune/browser/js-to-ps.lisp") nil)))

;;; The parenscript repl is based on MozREPL, which uses undocumented socket
;;; functionality. I tried my best to minimize the codebase, but ended up
;;; breaking everything and had a rather difficult time determining wtf was
;;; going on at all.
;;;
;;; As such, there are many oddities - we get the browser state by dumping it to
;;; ~/.masamune/browser-output and having the REPL printer also read that file
;;; for the appropriate sexprs and updating `mm::state'.
;;;
;;; *retch*
;;; 
;;; There is some comments on working with this mess @, 

(ignore-errors
 (swank:eval-in-emacs
  '(progn (find-file "~/quicklisp/local-projects/masamune/browser/documentation.lisp"))))

;;; CLIM (~100k vs. ~15MM loc browsers)
;;; ====================================
;;; Run (mm::listener)
;;;
;;; Filesystem > Show Directory
;;; Submit: /root/quicklisp/local-projects/masamune/images/
;;;
;;; Click on some of the images to print them out the REPL, and drag+drop them.
;;;
;;; Listener > Clear Output History
;;;
;;; Show > Class Subclasses
;;; Submit: gadget
;;;
;;; Try (at the listener REPL)
;;; (loop for i from 1 to 10
;;;       do (draw-circle* *standard-output* 300 300 (expt i 3) :line-thickness (expt i 2) :filled nil :ink +pink+))
;;; 
;;; (ql:quickload 'clim-examples)
;;; (clim-demo::demodemo) << To exit, C-t k on the master window

(in-package #:clim-user)

(defparameter sleep-time 0.001
  "use to speed or slow the animation, set to `nil' to stop")

(defun cos-animation ()
  (let* ((range (loop for k from 0 to (* 2 pi) by 0.1 collect k)) ; length of 62
	 (idx 0)
	 (record (updating-output (*standard-output*)
		   (loop for x from (nth idx range) to (+ (nth idx range) (* 2 pi)) by 0.01
			 with scale-multiplier = 100
			 with y-offset = 150
			 for x-offset = (- 10 (* scale-multiplier (nth idx range)))
			 for y-value = (+ y-offset (* scale-multiplier (cos x)))
			 for x-value = (+ x-offset (* scale-multiplier x))
			 do (progn (draw-point* *standard-output* x-value y-value :ink +green+ :line-thickness 3))))))
    (loop while sleep-time
	  do (progn (sleep sleep-time)
		    (if (= 61 idx) (setq idx 0) (incf idx))
		    (redisplay record *standard-output*)))))

;;; evaluate both the above forms and then (cos-animation) at the CLIM Listener
;;; to demonstrate CLIM's animation capabilities. You *cannot* evaluate
;;; (cos-animation) in Emacs, as *standard-output* is bound to the SLIME REPL
;;; output stream which isn't a `sheet', and thus cannot be drawn on. You can
;;; modify `sleep-time' from Emacs to regulate the animation.
;;;
;;; Set `sleep-time' to `nil' and Clear Output History
;;;
;;; At the center of CLIM is the notion of a presentation. You define
;;; presentation methods for types of objects, and when asked, they'll render
;;; themselves onscreen using said presentation method.
;;;
;;; Evaluate the following forms then (dolist (i identities) (present i)) at the
;;; CLIM Listener.
;;;
;;; Try typing '(lord?' at the listener and then clicking on one of the
;;; identities. Add a closing paren and RET.
;;;
;;; Try '(describe' click on ident ')' RET.
 
(defvar lords '("mircea_popescu" "asciilifeform" "ben_vulpes"))

(mm::c wot-identity () (name avatar))

(defmethod lord? ((i wot-identity))
  (member (name i) lords  :test #'string=))

(define-presentation-type wot-identity ())

(defun make-identity (name avatar-pathname)
  (make-instance 'wot-identity
		 :name name
		 :avatar avatar-pathname))

(defparameter identities
  (mapcar (lambda (l) (apply #'make-identity l))
	  '(("mircea_popescu" #P"/root/quicklisp/local-projects/masamune/images/mp-avatar.png")
	    ("ben_vulpes" #P"/root/quicklisp/local-projects/masamune/images/vulpes-avatar.png")
	    ("asciilifeform" #P"/root/quicklisp/local-projects/masamune/images/stas-avatar.png")
	    ("Suit" #P"/root/quicklisp/local-projects/masamune/images/suit-avatar.png")
	    ("RainbowDash" #P"/root/quicklisp/local-projects/masamune/images/rainbow-dash-avatar.png")
	    ("ChaosLord" #P"/root/quicklisp/local-projects/masamune/images/chaos-lord-avatar.png")
	    ("HotSlut22" #P"/root/quicklisp/local-projects/masamune/images/hot-slut22-avatar.png"))))

(define-presentation-method present (object (type wot-identity)
					    stream
					    (view textual-view)
					    &key acceptably)
  (multiple-value-bind (x y)
      (stream-cursor-position stream)
    (with-slots (name avatar) object      
      (draw-pattern* stream (climi::make-pattern-from-bitmap-file avatar :format :png)
		     (+ 150 x) (+ 30 y))
      (draw-text* stream name (+ 153 x) (+ 167 y) :ink +black+ :text-size 20)
      (draw-text* stream name (+ 152 x) (+ 166 y) :ink (if (lord? object) +gold+ +blue+) :text-size 20))
    (setf (stream-cursor-position stream)
	  (values x (+ 200 y)))
    object))

;;; CL is a very high level language but at all times compiles directly to
;;; assembler and also allows for inline assembler. For now, evaluate this form
;;; and call it at the listener to print out the disassembly

(defun print-draw-circle-disassembly ()
  (disassemble (lambda () (draw-circle *standard-output* 50 50 30 :ink +blue+))))

;;; one could even
;;; 
;;; '(disassemble (lambda () ' click on wot identity presentation to supply it as an argument '))'
;;; 
;;; 3D
;;; ====================================
;;; zoom with the scrollpad/wheel, rotate by clicking and dragging with the mouse
;;; kill the plot with ESC or C-t k

(in-package #:3d)

(load (compile-file #P"~/quicklisp/local-projects/masamune/3d.lisp")) ;; compile this
(3d:draw-plot) ; then this.

(defparameter plot-type :animated
  "valid values are :lorenz, t, :animated. No need to stop the render loop, just
C-c C-c")
(defparameter axes t)			; valid values are t, nil
(defparameter *equations*
  ;; (un)comment these equations one at a time and recompile the `defparameter' expression
  ;; to modify the graphs being plotted. `plot-type' must be set to `t' to display them.
  (list (list (lambda (x y) (+ (* 5 (* (- (sin (/ x 2))) (cos y))) (* (cos (/ x 2)) (sin (* 2 y)))))
	      (* 3 (- pi)) (* 3 pi) 0.4  (* 2 (- pi)) (* 2 pi) 0.4)

	;; (list (lambda (x y) (* (cos x) (+ 3 (* y (cos (/ x 2)))))) (- pi) pi 0.2 -6  1 0.2)

	;; (list (lambda (x y) (* (sin x) (+ 3 (* y (cos (/ x 2)))))) (- pi) pi 0.2 -6  1 0.2)

	;; (list (lambda (x y) (* (cos x) (+ 3 (* y (cos (/ x 3)) (sin x))))) -3 3 0.1 -2 2 0.1)
	))

;;; If you would like a better 3D programming experience should read / watch the
;;; following:

(progn
  (mmb::open-uri "http://3bb.cc/blog/")
  (mmb::open-uri "file:///root/quicklisp/local-projects/masamune/arsttep.html#sec-5-2")
  (mmb::open-uri "https://github.com/cbaggers/cepl")
  (mmb::open-uri "https://www.youtube.com/watch?v=yqCQ7etGnhM&list=PL2VAYZE_4wRKKr5pJzfYD1w4tKCXARs5y&index=13" t))

;;; glop, the windowing library used to create the OpenGL context has the
;;; tightest CFFI bindings I'm aware of. Since we're not supporting anything
;;; other than Masamune, all win32/osx crud can be cut.
;;;
;;; Glut and SDL are commonly used with CL-OPENGL because they have windowing
;;; abstractions and handle sound, drawing of primitives such as cubes, spheres.
;;; They're insane messes of Cee code, so I've decided against using them.
;;; 
;;; Maxima
;;; ====================================
;;; M-x imaxima will start a Maxima process. Maxima is written in CL, but it
;;; cannot be loaded into the regular lisp process atm because it has its own
;;; build system. Someone should fix this.
;;;
;;; imaxima usually has LaTeX output, but currently it is borked. Evaluate
;;; "display2d:true;" at the prompt to get ascii output instead.
;;;
;;; Stumpwm isn't loaded via quicklisp either, but is instead built, dumped
;;; using `save-lisp-and-die' and then started as an executable, loads quicklisp
;;; and allowing for you to use it. Maxima is built in a similar fashion. The
;;; Imaxima 'REPL' talks to a lisp executable that was built and dumped. One
;;; could built a Maxima executable, use that to built a Stumpwm executable and
;;; then use it to run Masamune. This would allow those running Masamune to make
;;; use of its computer algebra facilities in their day-to-day hacking without
;;; having to ferry sexprs across processes.
;;;
;;; XXX 2015-05-24T17:29:22+00:00 Gabriel Laddel
;;; *Don't* M-x launch-maxima. There is a bug that will force you to restart.
;;; It will be fixed shortly.
;;; 
;;; M-x launch-maxima launches a new imaxima, switches to the lisp proc and
;;; connects to it via SLIME so one can interact with it via Emacs Lisp REPL.
;;; 
;;; You can switch between Maxima syntax and the raw Lisp proc using to_lisp();
;;; and (to-maxima)
;;;
;;; Maxima documentation can be found at

(mmb::open-uri "file:///usr/share/maxima/5.18.1/doc/html/maxima.html" t)

;;; It has all sorts of neat demos, poke around.
;;;
;;; If you find the latex output is too small, (setf imaxima-scale-factor 2.0)
;;; will help.
;;;
;;; You can get the macsyma language AST (in sexprs) using e.g.,
;;; (macsyma-read-string "diff(sin(x^%e)*arctan(%pi/x),x);")
;;; 
;;; GnuPlot
;;; -------
;;; Maxima does its plotting via GnuPlot, which is unfortunately a dead-end
;;; codebase. It will have to do for the time being. 
;;;
;;; Mathematica vs. Open Science Packages
;;; -------------------------------------
;;; Mathematica includes standard libraries for all sorts of scientific
;;; applications. CL actually has quite a few, but they're not yet
;;; integerated. if this is something you're interested in, read the following

(progn (mmb::open-uri "https://github.com/matlisp/matlisp")
       (mmb::open-uri "https://github.com/mathematical-systems/clml")
       (mmb::open-uri "http://melisgl.github.io/mgl-pax-world/mgl-manual.html")
       (mmb::open-uri "http://www.mitchr.me/SS/mjrcalc/")
       (mmb::open-uri "https://common-lisp.net/project/gsll/")
       (mmb::open-uri "http://www.femlisp.org/" t))

;;; Masamune Specific Abstractions
;;; ====================================
;;; The preceding abstractions were choosen because they are the Lispiest in
;;; their class. Aside from this, I've introduced a few niceties to make
;;; development easier. Eventually the mechanics of teaching, research etc will
;;; be included by default.
;;; 
;;; State Saving, Restoration
;;; -------------------------
;;; `mm::state' holds the current state of the entire desktop and is updated
;;; continually by making use of hooks. See

(ignore-errors
 (swank:eval-in-emacs '(progn
			(find-file "~/quicklisp/local-projects/masamune/save-state.lisp")
			(delete-other-windows)
			(find-file "~/quicklisp/local-projects/masamune/save-state.el")
			(pop-to-buffer "save-state.lisp")
			nil)))

;;; for usage information.
;;;
;;; Dashboard
;;; ---------
;;; C-t d launches the dashboard. It does exactly what you'd think.
;;;
;;; Knowledge Graph
;;; ---------------
;;; C-t m launches the "knowledge graph" (for lack of a better name). For now it
;;; is just a tease. When Masamune is more developed it will essentially fill in
;;; as a skill / quest tree for life.
;;;
;;; For a long term vision / direction, visit
;;; 
;;; (mmb::open-uri "file:///root/quicklisp/local-projects/masamune/arsttep.html" t)
;;;
;;; Footnotes
;;; ============================================================================
;;; [1] http://en.wikipedia.org/wiki/List_of_programming_languages
;;; [2] Read loper-os.org in its entirety
;;;     & file:///root/quicklisp/local-projects/masamune/arsttep.html


;;; TODO 2015-05-22T11:39:08+00:00 Gabriel Laddel
;;; integrate into the lesson.

;; * Setup

;; Lacking full program interoperability and practical image recognition routines, the operator must manually enable a few niceties.  

;; ** emacs/lisp customizations

;; The files =emacs-customizations.el= and =lisp-customizations.lisp= are located in [[file:///root/quicklisp/local-projects/masamune/][~/quicklisp/local-projects/masamune/]] and are loaded at the end of the init process. Modify them instead of =~/.sbclrc= and your =~/.emacs=.
   
;; ** Installing adblock

;; Jonas Kalderstam

;; Just a lone cowboy programmer...

;; Conkeror supports firefox addons to varying degrees. I found that a good indicator is if the addon has support for Firefox 3. This means you can use Adblock 2.0. But, the GUI for selecting a filter subscription will not show. Hence the need to install Adblock 1.3 first. To get Adblock up and running in Conkeror, do the following:

;;     In your rc-file, set:

;; session_pref("xpinstall.whitelist.required", false);  

;;     Go to Adblock versions.
;;     Install 1.3.10.
;;     Open extensions: M-x extensions.
;;     Go into preferences for Adblock and subscribe to a list, like
;;     Easylist. The list might complain about requiring Adblock 2 for some filters, which is fine since we will fix that next.
;;     Now go back and download/install version 2.0.1.
;;     Enjoy the web again.
 
;; I followed this guide with the modification of installing adblock 2.6.5
;; after installing versions 1.3.10 and 2.0.1 (which didn't let me subscribe to
;; filters).
 
;; Version info: binary 33.1 of XULrunner to launch Conkeror commit
;; 48d3ef4369f267faf42451a580b1ac6bcb6a5a18, master branch of the repository
;; git://repo.or.cz/conkeror.git
 
;; to run: ~/path/to/xulrunner/xulrunner ~/path/to/conkeror/application.ini
 
;; make sure to subscribe to easylist in 2.6.5 or it won't work

;; Thanks to [[http://cowboyprogrammer.org/getting-adblock-to-work-in-conkeror/][Jonas Kalderstam]] for this tip.

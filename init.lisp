(in-package #:masamune)

(DEFCONSTANT -PI (- PI))
(DEFVAR *HABITS* NIL)
(DEFPARAMETER *SWANK-CONNECTION-HACK*
  NIL
  "for some scoping reasons mcclim applications won't have access t
swank::*emacs-connection* unless referenced through this variable")
(DEFPARAMETER *SYSTEMS*
  '("masamune")
  "list of strings naming dirs in ~/quicklisp/local-projects/. Masamune finds
   and tracks lisp systems in these dirs.")
(DEFVAR *NODES* NIL)
(DEFVAR *FOCUSED-NODE* NIL)
(DEFVAR *AGENDA* NIL "~org-mode style agenda items")
(DEFVAR *SYSTEM-INFORMATION* NIL)
(DEFVAR *HACK* NIL "Occasionally I need somewhere to put an intermediate value.")
(DEFVAR *PROJECT-LOCATION* #P"~/quicklisp/local-projects/masamune/")
(DEFPARAMETER *SWANK-CONNECTION-HACK*
  NIL
  "for some scoping reasons mcclim applications won't have access to
swank::*emacs-connection* unless referenced through this variable")
(DEFVAR *HABITS* NIL)
(DEFPARAMETER *SYSTEMS*
  '("masamune")
  "list of strings naming dirs in ~/quicklisp/local-projects/. Masamune finds
   and tracks lisp systems in these dirs.")
(DEFVAR *NODES* NIL)
(DEFVAR *FOCUSED-NODE* NIL)
(DEFVAR *AGENDA* NIL "~org-mode style agenda items")
(DEFVAR *SYSTEM-INFORMATION* NIL)

(in-package #:manardb)

;;; PCLOS initalization

(use-mmap-dir #P"~/.masamune/pclos-datastore/")
(open-all-mmaps)

(DEFCLASS force-manardb-init ()
  ((test-slot :ACCESSOR test-slot :INITARG :test-slot :INITFORM nil))
  (:metaclass manardb::mm-metaclass))

;;; XXX 2015-01-12T13:58:17+00:00 Gabriel Laddel
;;; this forces manardb to put required pclasses in memory
(make-instance 'force-manardb-init :test-slot t)

(in-package #:stumpwm)

;;; NOTE 2015-01-02T00:55:28+00:00 Gabriel Laddel
;;; without these whenever `mmb::open-uri' or one of its derivatives are called
;;; conkeror will steal focus
(setf *deny-raise-request* '((:class "Conkeror"))
      *suppress-deny-messages* '((:class "Conkeror"))
      *input-window-gravity* :bottom-left
      *message-window-gravity* :bottom-left
      *normal-border-width* 0      
      *window-border-style* :none
      *transient-border-width* 0
      *top-level-error-action* :break
      STUMPWM:*MODE-LINE-BORDER-WIDTH* 0
      STUMPWM:*MODE-LINE-BACKGROUND-COLOR* "white"
      STUMPWM:*MODE-LINE-FOREGROUND-COLOR* "black"
      STUMPWM:*MODE-LINE-PAD-X* 0
      STUMPWM:*MODE-LINE-PAD-Y* 0
      STUMPWM:*MODE-LINE-POSITION* :bottom)

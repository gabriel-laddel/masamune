;;; Commands for developing the Climacs editor. 

(in-package :climacs-commands)

(define-command (com-reset-profile :name t :command-table development-table) ()
  #+sbcl (sb-profile:reset)
  #-sbcl nil)

(define-command (com-report-profile :name t :command-table development-table) ()
  #+sbcl (sb-profile:report)
  #-sbcl nil)

(define-command (com-recompile :name t :command-table development-table) ()
  (asdf:operate 'asdf:load-op :climacs))


(define-gesture-name :select-other #+mcclim :pointer-button-press #-mcclim :pointer-button (:left :meta) :unique nil)

(define-presentation-translator lisp-string-to-string
    (drei-lisp-syntax::lisp-string string development-table
                  :gesture :select-other
                  :tester-definitive t
                  :menu nil
                  :priority 10)
    (object)
  object)

(define-command (com-accept-string :name t :command-table development-table) ()
  (display-message (format nil "~s" (accept 'string))))
 
(define-command (com-accept-symbol :name t :command-table development-table) ()
  (display-message (format nil "~s" (accept 'symbol))))	 

(define-command (com-accept-lisp-string :name t :command-table development-table) ()
  (display-message (format nil "~s" (accept 'lisp-string))))

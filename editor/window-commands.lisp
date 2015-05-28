;;; Windows commands for the Climacs editor. 

(in-package :climacs-commands)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Commands for splitting windows

(defun split-window-maybe-cloning (vertically-p clone-current-view-p)
  "Split `(current-window)', vertically if `vertically-p' is true,
horizontally otherwise. If `clone-current-view-p' is true, use a
clone of `(current-view)' for the new window."
  (handler-bind ((view-already-displayed
                  #'(lambda (condition)
                      (declare (ignore condition))
                      ;; If this happens, `clone-current-view-p' is false.
                      (display-message "Can't split: no view available for new window")
                      (return-from split-window-maybe-cloning nil))))
    (split-window vertically-p clone-current-view-p)))

(define-command (com-split-window-vertically :name t
                                             :command-table window-table)
    ((clone-current-view 'boolean :default nil))
  (split-window-maybe-cloning t clone-current-view))

(set-key `(com-split-window-vertically ,*numeric-argument-marker*)
	 'window-table
	 '((#\x :control) (#\2)))

(define-command (com-split-window-horizontally :name t
                                               :command-table window-table)
    ((clone-current-view 'boolean :default nil))
  (split-window-maybe-cloning nil clone-current-view))

(set-key `(com-split-window-horizontally ,*numeric-argument-marker*)
	 'window-table
	 '((#\x :control) (#\3)))

(define-command (com-other-window :name t :command-table window-table) ()
  (other-window))

(set-key 'com-other-window
	 'window-table
	 '((#\x :control) (#\o)))

(defun click-to-offset (window x y)
  (with-accessors ((top top) (bot bot)) (view window)
    (let ((new-x (floor x (stream-character-width window #\m)))
          (new-y (floor y (stream-line-height window)))
          (buffer (buffer (view window))))
      (loop for scan from (offset top)
         with lines = 0
         until (= scan (offset bot))
         until (= lines new-y)
         when (eql (buffer-object buffer scan) #\Newline)
         do (incf lines)
         finally (loop for columns from 0
                    until (= scan (offset bot))
                    until (eql (buffer-object buffer scan) #\Newline)
                    until (= columns new-x)
                    do (incf scan))
         (return scan)))))

(define-command (com-switch-to-this-window :name nil :command-table window-table)
    ((window 'pane) (x 'integer) (y 'integer))
  (other-window window)
  (when (and (buffer-pane-p window)
             (typep (view window) 'point-mark-view))
    (setf (offset (point (view window)))
	  (click-to-offset window x y))))

(define-presentation-to-command-translator blank-area-to-switch-to-this-window
    (blank-area com-switch-to-this-window window-table
                :echo nil)
    (window x y)
  (list window x y))

(define-gesture-name :select-other :pointer-button (:right) :unique nil)

(define-command (com-mouse-save :name nil :command-table window-table)
    ((window 'pane) (x 'integer) (y 'integer))
  (when (and (buffer-pane-p window)
	     (eq window (current-window)))
    (setf (offset (mark (view window)))
	  (click-to-offset window x y))
    (drei-commands::com-exchange-point-and-mark)
    (drei-commands::com-copy-region)))

(define-presentation-to-command-translator blank-area-to-mouse-save
    (blank-area com-mouse-save window-table :echo nil :gesture :select-other)
    (window x y)
  (list window x y))

(define-gesture-name :middle-button :pointer-button (:middle) :unique nil)

(define-command (com-yank-here :name nil :command-table window-table)
    ((window 'pane) (x 'integer) (y 'integer))
  (when (buffer-pane-p window)
    (other-window window)
    (setf (offset (point (view window)))
	  (click-to-offset window x y))
    (drei-commands::com-yank)))

(define-presentation-to-command-translator blank-area-to-yank-here
    (blank-area com-yank-here window-table :echo nil :gesture :middle-button)
    (window x y)
  (list window x y))

(defun single-window ()
  (loop until (null (cdr (windows *application-frame*)))
	do (rotatef (car (windows *application-frame*))
		    (cadr (windows *application-frame*)))
	   (com-delete-window))
  (setf *standard-output* (car (windows *application-frame*))))

(define-command (com-single-window :name t :command-table window-table) ()
  (single-window))

(set-key 'com-single-window
	 'window-table
	 '((#\x :control) (#\1)))

(define-command (com-scroll-other-window :name t :command-table window-table) ()
  (let ((other-window (second (windows *application-frame*))))
    (when other-window
      (page-down other-window (view other-window)))))

(set-key 'com-scroll-other-window
	 'window-table
	 '((#\v :control :meta)))

(define-command (com-scroll-other-window-up :name t :command-table window-table) ()
  (let ((other-window (second (windows *application-frame*))))
    (when other-window
      (page-up other-window (view other-window)))))

(set-key 'com-scroll-other-window-up
	 'window-table
	 '((#\V :control :meta)))

(define-command (com-delete-window :name t :command-table window-table) ()
  (delete-window))

(set-key 'com-delete-window
	 'window-table
	 '((#\x :control) (#\0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Commands for switching/killing current view.

(define-command (com-switch-to-view :name t :command-table window-table)
    ;; Perhaps the default should be an undisplayed view?
    ((view 'view :default (or (find (current-view) (views *application-frame*)
                               :test (complement #'eq))
                              (any-view))))
  "Prompt for a view name and switch to that view.
If the a view with that name does not exist, create a buffer-view
with the name and switch to it. Uses the name of the next
view (if any) as a default."
  (handler-case (switch-to-view (current-window) view)
    (view-already-displayed (condition)
      (other-window (window condition)))))

(set-key `(com-switch-to-view ,*unsupplied-argument-marker*)
	 'window-table
	 '((#\x :control) (#\b)))

(define-command (com-kill-view :name t :command-table window-table)
    ((view 'view :prompt "Kill view"
                 :default (current-view)))
  "Prompt for a view name and kill that view.
If the view is of a buffer and the buffer needs saving, you will
be prompted to do so before killing it. Uses the current view
as a default."
  (kill-view view))

(set-key `(com-kill-view ,*unsupplied-argument-marker*)
	 'window-table
	 '((#\x :control) (#\k)))

(define-menu-table window-menu-table (window-table)
  '(com-split-window-vertically nil)
  '(com-split-window-horizontally nil)
  'com-other-window
  'com-single-window
  'com-delete-window
  :divider
  `(com-switch-to-view ,*unsupplied-argument-marker*)
  `(com-kill-view ,*unsupplied-argument-marker*))

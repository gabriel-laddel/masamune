(cond
 ((fboundp 'screen-height)
  (fset 'revive:screen-height 'screen-height)
  (fset 'revive:screen-width 'screen-width))
 ((fboundp 'frame-height)
  (fset 'revive:screen-height 'frame-height)
  (fset 'revive:screen-width 'frame-width))
 (t (error "I don't know how to run revive.el on this Emacs...")))

(defun pdf-filename? (filename)
  (string= "fdp." (substring (string/reverse filename) 0 4)))

(defun abort-if-file-too-large (size op-type filename)
  "If file SIZE larger than `large-file-warning-threshold', allow user to abort.
OP-TYPE specifies the file operation being performed (for message to user)."
  ;; NOTE 2015-05-04T10:40:59+00:00 Gabriel Laddel
  ;; We assume that you always want to open large .pdfs, because otherwise
  ;; `restore-doc-page' won't work.
  (unless (and (stringp filename) (pdf-filename? filename))
    ;; (progn (add-hook 'doc-view-mode-hook #'restore-doc-page) nil)
    (when (and large-file-warning-threshold size
  	       (> size large-file-warning-threshold)
  	       (not (y-or-n-p (format "File %s is large (%s), really %s? "
  				      (file-name-nondirectory filename)
  				      (file-size-human-readable size) op-type))))
      (error "Aborted"))))

(defun revive:window-edges (&optional window)
  (window-edges window))

(defun revive:minx () 0)
(defun revive:miny ()
  (car (cdr (revive:window-edges (frame-first-window nil)))))
(defun revive:get-buffer (l)
  (nth 1 l))
(defun revive:get-point (l)
  (nth 2 l))
(defun revive:get-window-start (l)
  (nth 3 l))

(defun revive:window-list ()
  "Return the all window list in sorted order."
  (let*((curwin (selected-window)) (win curwin) wlist)
    (if (null
	 (catch 'found
	   (while t
	     (if (and (= (revive:minx) (car (revive:window-edges win)))
		      (= (revive:miny) (car (cdr (revive:window-edges win)))))
		 (throw 'found t))
	     (if (eq (setq win (next-window win)) curwin)
		 (throw 'found nil)))))
	(error "Unexpected window configuration."))
    (setq curwin win wlist (list win))
    (while (not (eq curwin (setq win (next-window win "w/o mini"))))
      (setq wlist (append wlist (list win)))) ;use append to preserve order
    wlist))

(defun revive:all-window-edges ()
  "Return the all windows edges by list."
  (let ((wlist (revive:window-list)) edges)
    (while wlist
      (setq edges (append edges (list (revive:window-edges (car wlist))))
	    wlist (cdr wlist)))
    edges))

(defun revive:select-window-by-edge (x y)
  "Select window whose north west corner is (X, Y).
If the matching window is not found, select the nearest window."
  (let*((curwin (selected-window)) (win (next-window curwin)) edges
	s2 (min 99999) minwin)
    (or
     (catch 'found
       (while t
	 (setq edges (revive:window-edges win)
	       s2 (+ (* (- (car edges) x) (- (car edges) x))
		     (* (- (nth 1 edges) y) (- (nth 1 edges) y))))
	 (cond
	  ((= s2 0)
	   (select-window win)
	   (throw 'found t))
	  ((< s2 min)
	   (setq min s2 minwin win)))
	 (if (eq win curwin) (throw 'found nil)) ;select the nearest window
	 (setq win (next-window win))))
     (select-window minwin))))

(defun revive:split-window-safe (window size &optional hor-flag)
  "Same as split-window but avoids error."
  (split-window
   window
   (min (max (if hor-flag window-min-width window-min-height) size)
	(if hor-flag (- (revive:screen-width) window-min-width 1)
	  (- (revive:screen-height) window-min-height 1)))
   hor-flag))

(defun revive:restore-winconf (x1 y1 x2 y2 edges)
  "Restore partial window configuration.
Assume (X1, Y1), (X2, Y2) as diagonal corners of partial window.
EDGES is a list of sub-windows' edges."
  (let*((topwin (car edges)) (width (- x2 x1)) (height (- y2 y1))
	right lower)
    (cond
     ((= (length edges) 1) nil)		;nothing to do.

     ;;if the top window has the same width as whole frame.
     ;; +---------+
     ;; |top      |
     ;; +-----+---+
     ;; |2    |3  |
     ;; +-----+---+
     ((= width (- (nth 2 topwin) (car topwin)))
      (setq lower (cdr edges))
      (revive:select-window-by-edge x1 y1)
      (revive:split-window-safe nil (- (nth 3 topwin) (nth 1 topwin)))
      (revive:restore-winconf
       (car (car lower)) (nth 1 (car lower)) x2 y2 lower))

     ;;if the top window has the same height as whole frame.
     ;; +-----+---+
     ;; |top  |2  |
     ;; |     +---+
     ;; |     |3  |
     ;; +-----+---+
     ((= height (- (nth 3 topwin) (nth 1 topwin)))
      (setq right (cdr edges))
      (revive:select-window-by-edge x1 y1)
      (revive:split-window-safe nil (- (nth 2 topwin) (car topwin)) t)
      (revive:restore-winconf
       (car (car right)) (nth 1 (car right)) x2 y2 right))

     ;;These two cases above are specialized solution of below for speed.

     ;;general cases.
     ;; +------+--+  Detect whether window is mainly divided vertically or
     ;; |top   |2 |  horizontally.  And call this function recursively on
     ;; +---+--+--+  former (that is, upper half in vertical division or
     ;; |3  |4..  |  left half in horizontal) and latter configuration.
     ;; +---+-----+
     (t
      (let ((flist (list topwin))
	    (elist (cdr edges)) divwin div-x div-y former latter)
	(while elist
	  (if (or (and (= x1 (car (car elist)))
		       (not (eq (car divwin) x1)))
		  (and (= y1 (nth 1 (car elist)))
		       (not (eq (nth 1 divwin) y1))))
	      (setq divwin (car elist)
		    former flist
		    latter elist))
	  (setq flist (append flist (list (car elist))))
	  (setq elist (cdr elist)))
	(setq div-x (car divwin) div-y (nth 1 divwin))
	(cond
	 ((= x1 (car divwin))	;Mainly divided vertically
	  (revive:select-window-by-edge x1 y1)
	  (revive:split-window-safe nil (- div-y y1))
	  (revive:restore-winconf x1 y1 x2 div-y former)
	  (revive:restore-winconf x1 div-y x2 y2 latter)
	  (message "=="))
	 ((= y1 (nth 1 divwin))
	  (revive:select-window-by-edge x1 y1)
	  (revive:split-window-safe nil (- div-x x1) t)
	  (revive:restore-winconf x1 y1 div-x y2 former)
	  (revive:restore-winconf div-x y1 x2 y2 latter)
	  (message "||"))
	 (t (message "dame!"))))))))

(defun revive:normalize-edges (width height edgelist)
  "Normalize all coordinates for current screen size.
'(WIDTH, HEIGHT) is old screen size and EDGELIST is a list of
window-edges."
  (let (normalized (curw (revive:screen-width))
		   (curh (revive:screen-height)) e n)
    (if (and (equal curw width) (equal curh height))
	edgelist
      (while edgelist
	(setq e (car edgelist)
	      n (list (/ (+ (* curw (nth 0 e)) (/ width 2)) width)
		      (/ (+ (* curh (nth 1 e)) (/ height 2)) height)
		      (/ (+ (* curw (nth 2 e)) (/ width 2)) width)
		      (/ (+ (* curh (nth 3 e)) (/ height 2)) height))
	      normalized (append normalized (list n))
	      edgelist (cdr edgelist)))
      normalized)))

(defun construct-window-configuration (edgelist)
  "Restore window configuration by EDGELIST.  EDGELIST should be sorted."
  (delete-other-windows)
  (revive:restore-winconf (revive:minx) (revive:miny)
			  (revive:screen-width)
			  (1- (revive:screen-height)) edgelist))

(defun current-window-configuration-printable ()
  "Return the printable current-window-configuration.
This configuration will be stored by restore-window-configuration.
Returned configurations are list of:
'(Screen-Width Screen-Height Edge-List Buffer-List)

Edge-List is a return value of revive:all-window-edges, list of all
window-edges whose first member is always of north west window.

Buffer-List is a list of buffer property list of all windows.  This
property lists are stored in order corresponding to Edge-List.  Buffer
property list is formed as
'((buffer-file-name) (buffer-name) (point) (window-start))."
  (let ((curwin (selected-window))
	(wlist (revive:window-list)) 
	(edges (revive:all-window-edges)) 
	buflist)
    (save-excursion
      (while wlist
	(select-window (car wlist))
					;should set buffer on Emacs 19
	(set-buffer (window-buffer (car wlist)))
	(setq buflist
	      (append buflist (list (list
				     (if (and
					  (buffer-file-name)
					  (fboundp 'abbreviate-file-name))
					 (abbreviate-file-name
					  (buffer-file-name))
				       (buffer-file-name))
				     (buffer-name)
				     (if (eq 'doc-view-mode major-mode)
					 (doc-view-current-page) 
				       (point))
				     (window-start))))
	      wlist (cdr wlist)))
      (select-window curwin)
      (list (revive:screen-width) (revive:screen-height) edges buflist))))

(defun revive:find-file (file)
  "Make the best effort to find-file FILE."
  (cond
   ((or (null file) (not (stringp file))) nil)
   ((file-exists-p file) (find-file file) (current-buffer))
   ((string-match ":" file)		;maybe ange-ftp's external file
    (if (progn (load "ange-ftp" t) (featurep 'ange-ftp))
	(progn (condition-case err
		   (find-file file)
		 (ftp-error
		  (message "Can't remote file `%s'" file)
		  (condition-case err2	;give a user one more chance.
		      (find-file file)
		    (ftp-error (error "Maybe you made mistake twice.")))))
	       (current-buffer))))
   (t nil)))

(defvar page-to-jump nil "")
(defvar doc-buffer nil "")

(defun restore-doc-page ()
  (cond ((null doc-buffer))
	((buffer-around? doc-buffer) (progn (switch-to-buffer doc-buffer)
					    (doc-view-goto-page page-to-jump)
					    (remove-hook 'doc-view-mode-hook #'restore-doc-page)
					    (setq page-to-jump nil doc-buffer nil)))
	(t (error "couldn't find doc-buffer"))))

(defun restore-window-configuration (config)
  "Restore the window configuration.
Configuration CONFIG should be created by
`current-window-configuration-printable'"
  (let* ((width (car config)) 
	 (height (nth 1 config))
	 (edges (nth 2 config))
	 (buffers (nth 3 config)))
    (set-buffer (get-buffer-create "*scratch*"))
    (setq edges (revive:normalize-edges width height edges))
    (construct-window-configuration edges)
    (revive:select-window-by-edge (revive:minx) (revive:miny))
    (loop for buffer-description in buffers
	  do (let* ((maybe-buffer (nth 1 buffer-description))
		    (maybe-buffer (aif (and maybe-buffer (get-buffer maybe-buffer))
				      it maybe-buffer))
		    (maybe-file (car buffer-description)))	       
	       (cond ((and maybe-buffer (stringp maybe-file) (pdf-filename? maybe-file))
		      (aif (buffer-with-filename maybe-file)
			  (progn (switch-to-buffer it)
				 (goto-char (revive:get-window-start buffer-description)) ;to prevent high-bit missing
				 (set-window-start nil (point))
				 (doc-view-goto-page (or (revive:get-point buffer-description) 1)))
			(progn (setf page-to-jump (or (revive:get-point buffer-description) 1)
				     doc-buffer maybe-buffer)
			       (add-hook 'doc-view-mode-hook #'restore-doc-page)
			       (find-file maybe-file))))
		     
		     ((and maybe-buffer (buffer-with-filename maybe-file)) 
		      (switch-to-buffer maybe-buffer)
		      (goto-char (revive:get-window-start buffer-description)) ;to prevent high-bit missing
		      (set-window-start nil (point))
		      (goto-char (revive:get-point buffer-description)))
		     
		     ((and (stringp maybe-file)
			   (not (file-directory-p maybe-file))
			   (revive:find-file maybe-file)) 
		      (let* ((window-start (nth 3 buffer-description))
			     (buffer-description-point (nth 2 buffer-description)))
			(set-window-start nil window-start)
			(if (and (stringp maybe-file) (pdf-filename? maybe-file)) 
			    (message "handle pdf in second `cond' clause")
			  (goto-char buffer-description-point))))
		     (t (message "ERROR: `restore-window-configuration' we should not have hit this cond clause")))
	       (other-window 1)))))

(defun update-emacs-and-browser-state ()
  ;; XXX 2015-05-03T00:26:34+00:00 Gabriel Laddel
  ;; trivial timers were not working as reliably as emacs timers.  This function
  ;; assumes that mm::*swank-connection-hack* has been bound to standard outuput
  ;; from the buffer *slime-repl sbcl*
  (slime-eval-async `(cl::progn (cl::setf (cl::getf mm::state :emacs) 
					  (quote ,(current-window-configuration-printable)))
				(mmb::update-browser-state))))

(defvar save-state-timer nil)

(defun start-save-state ()
  (setf save-state-timer
	(run-with-idle-timer .1 t #'update-emacs-and-browser-state)))

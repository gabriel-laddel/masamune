;;; Search commands

(in-package :climacs-commands)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Multiple query replace

(make-command-table 'multiple-query-replace-drei-table :errorp nil)

(defun multiple-query-replace-find-next-match (mark re list)
  (multiple-value-bind (foundp start)
      (re-search-forward mark re)
    (when foundp
      (loop with buffer = (buffer mark)
	    for string in list
	    when (buffer-looking-at buffer start string)
	      do (return string)))))

(define-command (com-multiple-query-replace :name t :command-table search-table) ()
  "Prompts for pairs of strings, replacing the first with the second.
Entering an empty search string stops the prompting."
  (let ((strings
	 (loop for string1 = (accept 'string :prompt "Multiple Query Replace")
	       until (string= string1 "")
	       for string2
		 = (accept 'string
			   :prompt (format nil
					   "Replace ~A with"
					   string1))
	       collecting (cons string1 string2))))
    (multiple-query-replace strings)))

(define-command (com-multiple-query-replace-from-buffer :name t :command-table search-table)
    ((view 'view :prompt "View with Query Repace strings"))
  (unless (member view (views *esa-instance*))
    (beep)
    (display-message "~A not an existing buffer" (name view))
    (return-from com-multiple-query-replace-from-buffer nil))
  (let* ((buffer (buffer view))
         (contents (buffer-substring buffer 0 (1- (size buffer))))
	 (strings (loop with length = (length contents)
			with index = 0
			with start = 0
			while (< index length)
			do (loop until (>= index length)
				 while (whitespacep (syntax buffer)
                                                    (char contents index))
				 do (incf index))
			   (setf start index)
			   (loop until (>= index length)
				 until (whitespacep (syntax buffer)
                                                    (char contents index))
				 do (incf index))
			until (= start index)
			collecting (string-trim '(#\Space #\Tab #\Newline)
						 (subseq contents start index)))))
    (unless (evenp (length strings))
      (beep)
      (display-message "Uneven number of strings in ~A" (name buffer))
      (return-from com-multiple-query-replace-from-buffer nil))
    (multiple-query-replace (loop for (string1 string2) on strings by #'cddr
				  collect (cons string1 string2)))))

(define-command (com-query-exchange :name t :command-table search-table) ()
  "Prompts for two strings to exchange for one another."
  (let* ((string1 (accept 'string :prompt "Query Exchange"))
	 (string2 (accept 'string :prompt (format nil
						  "Exchange ~A and"
						  string1))))
    (multiple-query-replace (list (cons string1 string2) (cons string2 string1)))))

(defun multiple-query-replace (strings)
  (declare (special strings))
  (let* ((occurrences 0)
	 (search-strings (mapcar #'car strings))
	 (re (format nil "~{~A~^|~}" search-strings)))
    (declare (special occurrences re))
    (when strings
      (let* ((point (point)) 
	     (found (multiple-query-replace-find-next-match point re search-strings)))
	(when found
	  (setf (query-replace-state (current-window))
		(make-instance 'query-replace-state
                 :string1 found
                 :string2 (cdr (assoc found strings :test #'string=)))
		(query-replace-mode (current-window))
		t)
	  (display-message "Replace ~A with ~A: "
			   (string1 (query-replace-state (current-window)))
			   (string2 (query-replace-state (current-window))))
	  (simple-command-loop 'multiple-query-replace-drei-table
			       (query-replace-mode (current-window))
			       ((setf (query-replace-mode (current-window)) nil))))))
    (display-message "Replaced ~D occurrence~:P" occurrences)))

(define-command (com-multiple-query-replace-replace
		 :name t
		 :command-table multiple-query-replace-drei-table)
    ()
  (declare (special strings occurrences re))
  (let* ((point (point (current-view))) 
	 (state (query-replace-state (current-window)))
	 (string1 (string1 state))
	 (string1-length (length string1)))
    (backward-object point string1-length)
    (replace-one-string point string1-length (string2 state) (no-upper-p string1))
    (incf occurrences)
    (let ((found (multiple-query-replace-find-next-match
		  point
		  re
		  (mapcar #'car strings))))
      (cond ((null found) (setf (query-replace-mode (current-window)) nil))
	    (t (setf (query-replace-state (current-window))
		     (make-instance 'query-replace-state
			:string1 found
			:string2 (cdr (assoc found strings :test #'string=))))
	       (display-message "Replace ~A with ~A: "
				(string1 (query-replace-state (current-window)))
				(string2 (query-replace-state (current-window)))))))))


(define-command (com-multiple-query-replace-replace-and-quit
		 :name t
		 :command-table multiple-query-replace-drei-table)
    ()
  (declare (special strings occurrences))
  (let* ((point (point))
	 (state (query-replace-state (current-window)))
	 (string1 (string1 state))
	 (string1-length (length string1)))
    (backward-object point string1-length)
    (replace-one-string point string1-length (string2 state) (no-upper-p string1))
    (incf occurrences)
    (setf (query-replace-mode (current-window)) nil)))

(define-command (com-multiple-query-replace-replace-all
		 :name t
		 :command-table multiple-query-replace-drei-table)
    ()
  (declare (special strings occurrences re))
  (let* ((point (point)) 
	 (found nil))
    (loop for state = (query-replace-state (current-window))
	  for string1 = (string1 state)
	  for string1-length = (length string1)
	  do (backward-object point string1-length)
	     (replace-one-string point
		     string1-length
		     (string2 state)
		     (no-upper-p string1))
	     (incf occurrences)
	     (setf found (multiple-query-replace-find-next-match
				  point
				  re
				  (mapcar #'car strings)))
	  while found
	  do (setf (query-replace-state (current-window))
		   (make-instance 'query-replace-state
		      :string1 found
		      :string2 (cdr (assoc found strings :test #'string=))))
	  finally (setf (query-replace-state (current-window)) nil))))

(define-command (com-multiple-query-replace-skip
		 :name t
		 :command-table multiple-query-replace-drei-table)
    ()
  (declare (special strings re))
  (let* ((point (point))
	 (found (multiple-query-replace-find-next-match
		 point
		 re
		 (mapcar #'car strings))))
    (cond ((null found) (setf (query-replace-mode (current-window)) nil))
	    (t (setf (query-replace-state (current-window))
		     (make-instance 'query-replace-state
			:string1 found
			:string2 (cdr (assoc found strings :test #'string=))))
	       (display-message "Replace ~A with ~A: "
				(string1 (query-replace-state (current-window)))
				(string2 (query-replace-state (current-window))))))))

(defun multiple-query-replace-set-key (gesture command)
  (add-command-to-command-table command 'multiple-query-replace-drei-table
				:keystroke gesture
				:errorp nil))

(multiple-query-replace-set-key '(#\Newline) 'com-query-replace-exit)
(multiple-query-replace-set-key '(#\Space) 'com-multiple-query-replace-replace)
(multiple-query-replace-set-key '(#\Backspace) 'com-multiple-query-replace-skip)
(multiple-query-replace-set-key '(#\Rubout) 'com-multiple-query-replace-skip)
(multiple-query-replace-set-key '(#\q) 'com-query-replace-exit)
(multiple-query-replace-set-key '(#\y) 'com-multiple-query-replace-replace)
(multiple-query-replace-set-key '(#\n) 'com-multiple-query-replace-skip)
(multiple-query-replace-set-key '(#\.) 'com-multiple-query-replace-replace-and-quit)
(multiple-query-replace-set-key '(#\!) 'com-multiple-query-replace-replace-all)

;;; miscellaneous commands for the Climacs editor. 

(in-package :climacs-commands)

(define-command (com-not-modified :name t :command-table buffer-table) ()
  "Clear the modified flag for the current buffer.
The modified flag is automatically set when the contents 
of the buffer are changed. This flag is consulted, for instance, 
when deciding whether to prompt you to save the buffer before killing it."
  (setf (needs-saving (current-buffer)) nil))

(set-key 'com-not-modified
	 'buffer-table
	 '((#\~ :meta)))

(define-command (com-what-cursor-position :name t :command-table info-table) ()
  "Print information about point.
Gives the character after point (name and octal, decimal and hexidecimal charcode), 
the offset of point, the total objects in the buffer, 
and the percentage of the buffers objects before point.

FIXME: gives no information at end of buffer."
  (let* ((char (or (end-of-buffer-p (point)) (object-after (point))))
	 (column (column-number (point))))
    (display-message "Char: ~:[none~*~;~:*~:C (#o~O ~:*~D ~:*#x~X)~] point=~D of ~D (~D%) column ~D"
		     (and (characterp char) char)
		     (and (characterp char) (char-code char))
		     (offset (point)) (size (current-buffer))
		     (if (size (current-buffer))
                         (round (* 100 (/ (offset (point))
                                          (size (current-buffer)))))
                         100)
		     column)))

(set-key 'com-what-cursor-position
	 'info-table
	 '((#\x :control) (#\=)))

(define-command (com-browse-url :name t :command-table base-table) 
    ((url 'url :prompt "Browse URL"))
  (declare (ignorable url))
  #+ (and sbcl darwin)
     (sb-ext:run-program "/usr/bin/open" `(,url) :wait nil)
  #+ (and openmcl darwin)
     (ccl:run-program "/usr/bin/open" `(,url) :wait nil))

(define-command (com-set-syntax :name t :command-table buffer-table) 
    ((syntax 'syntax
      :prompt "Name of syntax"))
  "Prompts for a syntax to set for the current buffer.
   Setting a syntax will cause the buffer to be reparsed using the new syntax."
  (set-syntax (current-view) syntax))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Groups

(define-command (com-define-group :name t :command-table global-climacs-table)
    ((name 'string :prompt "Name")
     (views '(sequence view) :prompt "Views"))
  (when (or (not (get-group name))
            (accept 'boolean :prompt "Group already exists. Overwrite existing group?"))
    (add-group name views))
  (select-group (get-group name)))

(set-key `(com-define-group ,*unsupplied-argument-marker* ,*unsupplied-argument-marker*)
         'global-climacs-table
         '((#\x :control) (#\g) (#\d)))

(define-command (com-define-file-group :name t :command-table global-climacs-table)
    ((name 'string :prompt "Name")
     (pathnames '(sequence pathname) :prompt "Files"))
  (when (or (not (get-group name))
            (accept 'boolean :prompt "Group already exists. Overwrite existing group?"))
    (add-group name pathnames))
  (select-group (get-group name)))

(set-key `(com-define-file-group ,*unsupplied-argument-marker* ,*unsupplied-argument-marker*)
         'global-climacs-table
         '((#\x :control) (#\g) (#\f)))

(define-command (com-select-group :name t :command-table global-climacs-table)
    ((group 'group))
  (select-group group))

(set-key `(com-select-group ,*unsupplied-argument-marker*)
         'global-climacs-table
         '((#\x :control) (#\g) (#\s)))

(define-command (com-deselect-group :name t :command-table global-climacs-table)
    ()
  (deselect-group)
  (display-message "Group deselected"))

(set-key 'com-deselect-group
         'global-climacs-table
         '((#\x :control) (#\g) (#\u)))

(define-command (com-current-group :name t :command-table global-climacs-table)
    ()
  (with-minibuffer-stream (s)
    (format s "Active group is: ")
    (present (get-active-group) 'group :stream s)))

(set-key 'com-current-group
         'global-climacs-table
         '((#\x :control) (#\g) (#\c)))

(define-command (com-list-group-contents :name t :command-table global-climacs-table)
    ()
  (with-minibuffer-stream (s)
    (format s "Active group designates: ")
    (display-group-contents (get-active-group) s)))

(set-key 'com-list-group-contents
         'global-climacs-table
         '((#\x :control) (#\g) (#\l)))

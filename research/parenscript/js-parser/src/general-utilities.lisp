;;;; general-utilities.lisp
;;;
;;; Handy utility functions that don't really belong anywhere else
;;; (these aren't really specific to this project in any way)
;;;
;;; Copyright (c) 2006 James Wright
;;; See LICENSE for full licensing details.
;;;
(in-package :js-parser)

;;;; ======= Anaphoric conditionals ================================================================
(defmacro aif (test-form then-form &optional else-form)
  "Anaphoric IF expression; binds IT."
  `(let ((it ,test-form))
    (if it ,then-form ,else-form)))

(defmacro awhen (test-form &body body)
  "Anaphoric WHEN expression; binds IT."
  `(aif ,test-form
    (progn ,@body)))

(defmacro when-let ((bind-var test-form) &body body)
  "Anaphoric WHEN expression that allows the caller to specify the name of the bound variable"
  `(let ((,bind-var ,test-form))
    (when ,bind-var
      ,@body)))

;;;; ======= List handling =========================================================================
(defun postpend (list-arg atom-arg)
  "Appends a list containing ATOM-ARG to LIST-ARG.
   eg: (POSTPEND '(1 2) 3) ===> '(1 2 3)"
  (append list-arg (list atom-arg)))

(defun maptree (fn tree)
  "MAPTREE maps a function over a tree of cons cells.
   If TREE is NIL, returns NIL.
   If TREE is a cons cell, recursively calls MAPTREE on the CAR and CDR and returns a new cons cell
   whose CAR and CDR are the results.
   Otherwise, returns the result of applying FN to TREE."
  (cond
    ((consp tree)
     (cons (maptree fn (car tree))
           (maptree fn (cdr tree))))
    ((null tree)
     tree)
    (t
     (funcall fn tree))))

(defun prefix-p (string prefix)
  "return:  whether prefix is a prefix of the string."
  (and (<= (length prefix) (length string))
       (string= string prefix :end1 (length prefix))))

;;;; ======= File handling =========================================================================
(defun pathnames-equal (path1 path2)
  "Return non-NIL if PATH1 and PATH2 are equivalent.  This function avoids some of the
   complexity that pathnames can entail by comparing the namestrings of the two paths
   rather than the paths themselves.  That way we don't have to worry about spurious
   distinctions between :UNSPECIFIED and NIL, :NEWEST and NIL and some actual version number, etc."
  (equal (namestring (pathname path1))
         (namestring (pathname path2))))

(defun read-entire-file (path)
  "Reads the entire contents of the file located at PATH and returns it as a string"
  (with-open-file (in path :direction :input)
    (with-output-to-string (out)
      (loop for line = (read-line in nil :eof)
            until (eq line :eof)
            do (format out "~A~%" line)))))

(defun read-asdf-component-text (component-path)
  "Returns the contents of a file that is a component of a currently-loaded asdf system.
   COMPONENT-PATH is a path describing the location of the component to read.  It should
   have at least 2 elements.
   The first element is a symbol naming a system.
   The last element is a string naming a component.
   There may be intermediate strings naming intermediate modules.  Eg:
   
       (:JS-ON-CL-TESTS \"tests\" \"test-cps-transformation\")

   names the test-cps-transformation component, which is part of the tests module, which
   is part of the :JS-ON-CL-TESTS system."
  (let ((component (asdf:find-system (car component-path))))
    (dolist (name (cdr component-path))
      (setf component (asdf:find-component component name)))
    (read-entire-file (asdf:component-pathname component))))

;;;; ======= Backchannel communication =============================================================
(define-condition backchannel-message ()
  ((channel-name :initarg :channel-name :accessor channel-name)
   (message-value :initarg :message-value :accessor message-value))
  (:documentation
   "A condition that indicates that a function further down the call chain has a message
    to pass back.  BACKCHANNEL-MESSAGE should never be directly instantiated or used;
    use BIND-WITH-BACKCHANNELS and BACKCHANNEL-SIGNAL instead."))

(defun backchannel-signal (channel value)
  "Signals VALUE on backchannel CHANNEL.  Returns T if the message was received."
  (assert (keywordp channel))
  (restart-case (signal 'backchannel-message :channel-name channel :message-value value)
                (backchannel-message-received () t)))

(defmacro bind-with-backchannels ((&rest bindings) form &body body)
  (let* ((first-keyword (position-if #'keywordp bindings))
         (mv-bindings (subseq bindings 0 first-keyword))
         (gmv-bindings (mapcar #'(lambda (name)
                                   (cons name (gensym)))
                               mv-bindings)))
    (if first-keyword
      (let* ((channel-spec (subseq bindings first-keyword))
             (channel-names (loop with spec = channel-spec
                                  while spec
                                  collect (pop spec)
                                  do (pop spec)))
             (channel-bindings (loop with spec = channel-spec
                                     while spec
                                     do (pop spec)
                                     collect (pop spec)))
             (gvalue (gensym)))
        (flet ((make-clause (channel-name)
                 `(,channel-name
                   (push (message-value ,gvalue)
                         ,(getf channel-spec channel-name))
                   (invoke-restart 'backchannel-message-received))))
          `(let (,@mv-bindings
                 ,@channel-bindings)
            (handler-bind ((backchannel-message #'(lambda (,gvalue)
                                                    (case (channel-name ,gvalue)
                                                      ,@(mapcar #'make-clause channel-names)))))
              (multiple-value-bind (,@(mapcar #'cdr gmv-bindings))
                  ,form
                ,@(loop for (name . gname) in gmv-bindings
                        collect `(setf ,name ,gname))
                ,@(loop for name in channel-bindings
                        collect `(setf ,name (reverse ,name)))))
            ,@body)))
      `(multiple-value-bind ,mv-bindings ,form ,@body))))
         
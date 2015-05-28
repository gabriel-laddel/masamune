;;; Input/Output of buffers to and from streams.

(in-package :climacs-core)

(define-condition buffer-contains-noncharacter (buffer-writing-error)
  ()
  (:report (lambda (condition stream)
             (format stream "Buffer ~A contains non-character object"
                     (name (buffer condition)))))
  (:documentation "This error is signalled whenever an attempt is
made to save a buffer that contains a non-character object."))

(defun buffer-contains-noncharacter (buffer filepath)
  "Signal an error of type `buffer-contains-noncharacter' with
the buffer `buffer' and the filepath `filepath'."
  (error 'buffer-contains-noncharacter :buffer buffer :filepath filepath))

(defmethod check-buffer-writability ((application-frame climacs) (filepath pathname)
                                     (buffer drei-buffer))
  (do-buffer-region (object offset buffer 0 (size buffer))
    (unless (characterp object)
      (buffer-contains-noncharacter buffer filepath)))
  (call-next-method))

(defmethod frame-save-buffer-to-stream ((application-frame climacs) (buffer climacs-buffer) stream)
  (let ((seq (buffer-sequence buffer 0 (size buffer))))
    (if (every #'characterp seq)
        (write-sequence seq stream)
        (display-message "Cannot save to file, buffer contains non-character object"))))

(defun input-from-stream (stream buffer offset)
  (let* ((seq (make-string (file-length stream)))
         (count (#+mcclim read-sequence #-mcclim cl:read-sequence
                          seq stream)))
    (insert-buffer-sequence buffer offset
                            (if (= count (length seq))
                                seq
                                (subseq seq 0 count)))))

(defmethod frame-make-buffer-from-stream ((application-frame climacs) stream)
  (let* ((buffer (make-new-buffer)))
    (input-from-stream stream buffer 0)
    (clear-undo-history buffer)
    buffer))

;;; -*- Mode: Lisp -*-

;;;; streams-utilities.lisp --
;;;; get-unget-stream.lisp -- Original file name...

#| (in-package "CL.EXTENSIONS.STREAMS") |# ; Copied from there!

(in-package "HELAMBDAP")


(defvar *backtraceable-streams*
  (make-hash-table :test #'eq))

(defparameter *backlog-length* 1024)


(declaim (inline stream-backlog
                 (setf stream-backlog)
                 get-char
                 unget-char))

(defun stream-backlog (stream)
  (gethash stream *backtraceable-streams* ()))


(defun (setf stream-backlog) (v stream)
  (setf (gethash stream *backtraceable-streams*) v))


(defun clean-stream-backlog (stream)
  (remhash stream *backtraceable-streams*))


(defmacro with-backlogged-stream ((s) &body forms)
  `(unwind-protect
       ,@forms
     (remhash ,s *backtraceable-streams*)))
  

(defvar *whitespace-characters* ; XML characters.
  (list (code-char #x20)
        (code-char #x9)
        (code-char #xD)
        (code-char #xA)))


(defun look-at-char (&optional peek-type ; Roll it back in the original file!!!!
                               (stream *standard-input*)
                               (eof-error-p t)
                               eof-value
                               recursive-p)
  (let ((stream-backlog (stream-backlog stream)))
    (when stream-backlog
      (cond ((null peek-type)
             (return-from look-at-char (first stream-backlog)))

            ((eq peek-type t)
             (loop for c = (first stream-backlog)
                   while (and c
                              (member c *whitespace-characters*
                                      :test #'char=))
                   do (pop stream-backlog)
                   finally (when stream-backlog
                             (return-from look-at-char
                               (first stream-backlog))))
             )
            (t ; PEEK-TYPE is a character.
             (loop for c = (first stream-backlog)
                   while (and c (char/= c peek-type))
                   do (pop stream-backlog)
                   finally (when stream-backlog
                             (return-from look-at-char
                               (first stream-backlog)))))
            ))
    (peek-char peek-type stream eof-error-p eof-value recursive-p))
  )


(defun get-char (&optional (stream *standard-input*)
                           (eof-error-p t)
                           eof-value
                           recursive-p)
  (let ((stream-backlog (pop (stream-backlog stream))))
    (if stream-backlog
        stream-backlog
        (read-char stream eof-error-p eof-value recursive-p))))


(defun unget-char (char &optional
                        (stream *standard-output*)
                        (backlog-length *backlog-length*))
  (declare (type character char))
  (declare (ignore backlog-length))
  (push char (stream-backlog stream))
  nil
  )


(defmethod unget-chars ((chars list)
                        &optional
                        (stream *standard-output*)
                        (backlog-length *backlog-length*))
  
  (dolist (c (reverse chars))
    (unget-char c stream backlog-length)))


(defmethod unget-chars ((chars vector)
                        &optional
                        (stream *standard-output*)
                        (backlog-length *backlog-length*))
  
  (loop for c of-type character across (reverse chars)
        do (unget-char c stream backlog-length)))


;;; end of file -- get-unget-stream.lisp --

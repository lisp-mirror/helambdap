;;;; -*- Mode: Lisp -*-

;;;; text-utilities.lisp --
;;;; A few text utilities.
;;;;
;;;; See file COPYING for copyright and licensing information.

(in-package "HELAMBDAP")

(declaim (inline newlines newline))


(defun newlines (&optional (out *standard-output*) (n 1))
  (dotimes (i n) (terpri out)))


(defun newline (&optional (out *standard-output*))
  (newlines out 1))


(defun empty-string-p (s)
  (declare (type (or null string) s))
  (and s (string= "" s)))


(defun non-empty-string-p (s)
  (declare (type (or null string) s))
  (and s (not (null (string/= "" s)))))


(defun first-line-special (s)
  (let* ((empty-line-pos (search (format nil "~2%") s))
         (first-line (subseq s 0 empty-line-pos))
         )
    (if (<= (length first-line) 80)
        (values first-line
                (and empty-line-pos (subseq s (+ 2 empty-line-pos))))
        s)))


(defun split-all-lines (strings)
  (mapcan (lambda (s)
            (split-sequence:split-sequence #\Newline s))
          strings))


(defun split-lines-for-html (s)
  (loop with lines = (split-sequence:split-sequence #\Newline s)
        for l in (rest lines)
        collect (<:br) into result
        collect l into result
        finally (return (cons (first lines) result))))


(defun split-at-tex-paragraphs (s)
  (loop with parbreak = (format nil "~2%")
        for start = 0 then (+ 2 pbr)
        for pbr = (search parbreak s :start2 start)
        collect (subseq s start pbr)
        while pbr))


(defun sanitize-string-for-html (s)
  (with-output-to-string (r)
    (loop for c across s
          do (cond ((char= #\< c) (write-string "&lt;" r))
                   ((char= #\> c) (write-string "&gt;" r))
                   (t (write-char c r))))))


(defun sanitize-quotes-for-html (s &aux (sl (length s)))
  (declare (type string s)
           (type fixnum sl) ; Yeah!  This is wrong... la la la.
           )
  (with-output-to-string (r)
    (loop with skip = nil
          for i from 0 below sl
          for next = (1+ i)
          for c = (char s i)
          ;; do (format t "~D ~C ~S~%" i c skip)
          if skip
          do (setf skip nil)
          else
          do (if (< next sl)
                 (cond ((and (char= c #\\)
                             (char= (char s next) #\"))
                        (setf skip t)
                        (write-char #\\ r)
                        (write-char #\" r))
                       ((and (char= c #\\)
                             (char= (char s next) #\\))
                        (setf skip t)
                        (write-char #\\ r)
                        (write-char #\\ r))
                       ((char= c #\")
                        (write-char #\\ r)
                        (write-char #\" r))
                       (t
                        ; (format t ">> ~C~%" c)
                        (write-char c r))
                       )
                 (cond ((char= c #\")
                        (write-char #\\ r)
                        (write-char #\" r))
                       (t
                        ;; (format t ">> ~C~%" c)
                        (write-char c r)))
                 )
          )))
        

;;;; end of file -- text-utilities.lisp --

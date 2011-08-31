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


;;;; end of file -- text-utilities.lisp --

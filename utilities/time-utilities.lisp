;;;; -*- Mode: Lisp -*-

;;;; time-utilities.lisp --
;;;; A few functional utilities (to avoid dependencies).
;;;;
;;;; See file COPYING for copyright and licensing information.

(in-package "HELAMBDAP")

(defconstant +months+
  #("Jan" "Feb" "Mar" "Apr" "May" "Jun" "Jul" "Aug" "Sep" "Oct" "Nov" "Dec"))

(defconstant +weekdays+
  #("Mon" "Tue" "Wed" "Thu" "Fri" "Sat" "Sun"))

(defun text-timestamp ()
  "Produces a string of th form \"Day Mon Dat HH:MM:SS TZ YEAR\"."
  (multiple-value-bind (s mi h d mo y w dls z)
      (get-decoded-time)
    (declare (ignore dls))
    (format nil "~A ~A ~D ~D:~D:~D GMT~D ~D"
            (aref +weekdays+ w)
            (aref +months+ (1- mo))
            d
            h mi s
            z
            y)))
    
;;;; end of file -- time-utilities.lisp --

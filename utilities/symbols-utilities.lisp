;;;; -*- Mode: Lisp -*-

;;;; symbols-utilities.lisp --
;;;; A few functional utilities (to avoid dependencies).
;;;;
;;;; See file COPYING for copyright and licensing information.

(in-package "HELAMBDAP")

(defun external-symbol-p (s)
  (declare (type symbol s))
  (eq :external
      (nth-value 1
                 (find-symbol (symbol-name s)
                              (symbol-package s)))))
    
;;;; end of file -- symbols-utilities.lisp --

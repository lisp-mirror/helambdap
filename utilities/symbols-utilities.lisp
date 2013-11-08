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


(defun symbol-status-in-package (sd pd &aux (p (find-package pd)))
  (multiple-value-bind (s status)
      (find-symbol (string sd) p)
    (cond (status ; Symbol is "accessible" from P.
           (let ((sp (symbol-package s)))
             (if (eq sp p)
                 (values p status sp status)

                 ;; This is the interesing case.
                 (multiple-value-bind (s-in-sp s-status-in-sp)
                     (find-symbol (string sd) sp)
                   (declare (ignore s-in-sp))
                   (let ((s-p-vs-sp
                          (ecase status
                            (:internal :internal)
                            (:inherited :inherited)
                            (:external (ecase s-status-in-sp
                                         ((:inherited :internal)
                                          ;; Should never be :inherited.
                                          :imported-exported)
                                         (:external
                                          (if (find s (package-shadowing-symbols p))
                                              :external
                                              :re-exported))
                                         ))))
                         )
                     (values p s-p-vs-sp sp s-status-in-sp)))
                 )))
          ((symbolp sd)
           (let ((sp (symbol-package sd)))
             (multiple-value-bind (s-in-sp s-status-in-sp)
                 (find-symbol (string sd) sp)
               (declare (ignore s-in-sp))
               (values p status sp s-status-in-sp))))
          (t
           (values nil nil nil nil)))
    ))
    
;;;; end of file -- symbols-utilities.lisp --

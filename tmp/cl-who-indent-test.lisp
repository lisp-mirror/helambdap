;;;; -*- Mode: Lisp -*-

(in-package "HELAMBDAP")

(use-package "CL-WHO")

(defun who-test (x &optional (out *standard-output*) (indent 2))
  (let ((cl-who::*indent* indent))
    (declare (special cl-who::*indent*))
    (with-html-output (out out :indent 4 #| indent |#)
      (htm (:body (:frameset :rows "65px" :border 0 :noresize "noresize"
                   (frame-stuff x)))
           ))))

(defun frame-stuff (x &optional (out *standard-output*))
  (with-html-output (out out :indent 3)
    (htm (:frame :src x))))

;;;; -*- Mode: Lisp -*-

;;;; asdf-deps.lisp --
;;;; XHTMLambda ASDF dependent code.
;;;;
;;;; Copyright (c) 2013, Marco Antoniotti
;;;; See file COPYING for more information.

(in-package "ASDF")

(export '(files-in-system))

(defun files-in-system (s)
  (check-type s (or string symbol asdf:system))

  (typecase s
    ((or symbol string) (setf s (asdf:find-system s))))

  (let ((filenames ()))

    ;; Simple tree traversal.  If you need something more
    ;; sophisticated, please contribute it back.

    (labels ((collect-filenames (c)
               (typecase c
                 (cl-source-file
                  (push (component-pathname c) filenames))
                 (module
                  (dolist (sub-c (module-components c))
                    (collect-filenames sub-c)))
                 ))
             )
      (collect-filenames s)
      (nreverse filenames))))


;;;; asdf-deps.lisp --

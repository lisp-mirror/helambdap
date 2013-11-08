;;;; -*- Mode: Lisp -*-

;;;; asdf-deps.lisp --
;;;; XHTMLambda ASDF dependent code.
;;;;
;;;; See file COPYING for more information.


(in-package "ASDF")

#+asdf2
(export '(files-in-system) "ASDF")

#+asdf2
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


;;;===========================================================================
;;; Thanks to Fare` for these...

#+asdf3
(export '(system-input-files system-files) "ASDF")

#+asdf3
(defun system-input-files (system)
  (multiple-value-bind (i o) 
      (while-collecting (i o)
        (loop for (op . comp)
              in (plan-actions
                  (traverse-sub-actions 'load-op (find-system system)))
              do (map () #'i (input-files op comp))
              (map () #'o (output-files op comp))))
    (remove-if #'(lambda (f) (member f o :test 'pathname-equal))
               (remove-duplicates i :from-end t :test 'pathname-equal))))

;;; Of course, you could pray that no one is doing anything funky with
;;; input-files, and instead use the simpler:

#+asdf3
(defun system-files (system)
  (cons (system-source-file system)
        (mapcar 'component-pathname 
                (remove-duplicates
                 (required-components system
                                      :other-systems nil
                                      :component-type '(not system)) 
                 :from-end t))))

;;;; asdf-deps.lisp --

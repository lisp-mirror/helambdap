;;;; -*- Mode: Lisp -*-

;;;; package-utilities.lisp --
;;;; A few utilities for dealing wth packages.
;;;;
;;;; See file COPYING for copyright and licensing information.

(in-package "HELAMBDAP")

(defun move-package-symbols (from-package to-package)
  (declare (type package from-package to-package))
  ;; (assert (packagep from-package))
  ;; (assert (packagep to-package))
  
  (do-symbols (s from-package)
    (let ((ns (intern (string s) to-package)))
      (setf (symbol-plist ns)
            (append (symbol-plist s)
                    (symbol-plist ns)))

      (when (boundp s)
        (setf (symbol-value ns) (symbol-value s)))

      (when (fboundp s)
        (setf (symbol-function ns) (symbol-function s)))
      ))

  (do-external-symbols (s from-package t)
    (export (find-symbol (string s) to-package) to-package))
  )

;;;; end of file -- package-utilities.lisp --

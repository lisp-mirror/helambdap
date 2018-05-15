;;;; -*- Mode: Lisp -*-

;;;; package-utilities.lisp --
;;;; A few utilities for dealing wth packages.
;;;;
;;;; See file COPYING for copyright and licensing information.

(in-package "HELAMBDAP")

;;;; Conditions
;;;; ----------
;;;;
;;;; Different CLs do not agree about what to do when the reader sees
;;;; an unknown package.  This is yet another annoyance in the ANSI
;;;; spec, which must be ironed out to ensure manageable reading of forms.
;;;;
;;;; The first thing we do is to define a CONDITION which will be the
;;;; root of implementation dependent specialized "unknown package" conditions.

(define-condition unknown-package ()
  ((name :reader unknown-package-name
         :initarg :name
         :initform "I-AM-NOT-A-PACKAGE"))
  (:report (lambda (c s)
             (format s "HELambdaP error: unknown package ~S."
                     (unknown-package-name c))))
  )

(defparameter *unknown-package-condition-class* 'unknown-package
  "This parameter is bound to a condition class related to unknown packages.

The parameter is set by each implementation dependent code in order to
properly generate the most appropriate condition.")


(defgeneric make-unknown-package-condition (upc-class)
  (:documentation
   "Dispatches on argument UPC-CLASS to generate an UNKNOWN-PACKAGE instance.

This generic function relies on the presence of implementation
dependent subclasses of UNKNOWN-PACKAGE to ensure a compatible
treatment of this events.

See also:

The implementation dependent classes; e.g., UNKNOWN-PACKAGE-LW.")

  (:method (upc-thing)
   (warn "HELambdaP warning: making an UNKNOWN-PACKAGE condition from a ~S."
         upc-thing)
   (make-instance 'unknown-package))

  (:method ((s string))
   (make-instance 'unknown-package :name s))

  (:method ((upc-class (eql 'unknown-package)))
   (make-instance 'unknown-package))

  (:method ((upc unknown-package))
   upc))


;;;; Other utilities
;;;; ---------------

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

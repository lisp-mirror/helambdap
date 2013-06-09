;;;; -*- Mode: Lisp -*-

;;;; documentation-production.lisp --

(in-package "HELAMBDAP")

;;;;===========================================================================
;;;; Global special parameters controlling the documentation production.
;;;;
;;;; The *EVERYTHING*, *ONLY-DOCUMENTED*, *ONLY-EXPORTED* need the
;;;; following explanation.  When *EVERYTHING* is true, HELAMBDAP produces
;;;; documentation for every bit in the "search" path requested regardless
;;;; of the values of *ONLY-DOCUMENTED* and *ONLY-EXPORTED*.

(defparameter *supersede-documentation* t)

(defparameter *everything* nil)

(defparameter *only-documented* t)

(defparameter *only-exported* nil)


;;;;===========================================================================
;;;; Protocol.

(defgeneric produce-documentation (format element out doc-bits
                                          &key documentation-title
                                          &allow-other-keys)
  (:documentation "Produces documentation for ELEMENT according to FORMAT.")
  )


(defgeneric build-documentation (for-what
                                 format
                                 &key
                                 layout
                                 source
                                 destination
                                 &allow-other-keys)
  (:documentation
   "Produces the documentation according to a number of parameters"))


(defgeneric build-doc-skeleton (for-what
                                format
                                &key
                                layout
                                source
                                destination
                                &allow-other-keys)
  (:documentation "Produces a skeleton for the documentation.

The skeleton consists of a number of 'judiciously' editable files that
can be used as building blocks for the final documentation."))


(declaim (ftype (function (T &key &allow-other-keys) T) document))


;;;;===========================================================================
;;;; Implementation.


(defun document (for-what
                 &key
                 (documentation-title)
                 (format 'html)
                 (layout *default-documentation-structure*)
                 (source #P"")
                 (destination 
                  (make-pathname :directory '(:relative "doc" "html")))

                 ((:supersede *supersede-documentation*)
                  *supersede-documentation*)
                 ((:only-documented *only-documented*) *only-documented*)
                 ((:only-exported *only-exported*) *only-exported*)
                 ((:everything *everything*) *everything*)

                 ((:exclude-directories *exclude-directories*)
                  *exclude-directories*)
                 ((:exclude-files *exclude-files*)
                  *exclude-files*)
                 
                 &allow-other-keys
                 )
  "Produces the documentation for something.

The function is a wrapper for BUILD-DOCUMENTATION defaulting a few
parameters, in particular the output FORMAT (which defaults to HTML)."
  
  (when (and *everything* (or *only-documented* *only-exported*))
    (warn "EVERYTHNG is currently true: HELAMBDAP will produce all ~@
           documentation files regardless of the value of other 'limiting' ~@
           variables."))

  (build-documentation for-what
                       format
                       :layout layout
                       :source source
                       :destination destination
                       :documentation-title documentation-title))


;;;;---------------------------------------------------------------------------
;;;; Build documentation implementations.

(defmethod build-documentation ((p pathname)
                                (format (eql 'html))
                                &key
                                (documentation-title)
                                (layout *default-documentation-structure*)
                                (source #P".")
                                (destination
                                 (make-pathname :directory '(:relative "doc" "html")))
                                &allow-other-keys
                                )
  (declare (ignore source))

  (when documentation-title
    (setf (property layout :documentation-title) documentation-title))

  (let ((doc-bits (collect-documentation p)))
    (produce-documentation format
                           layout
                           destination
                           doc-bits
                           :documentation-title documentation-title))
  )
  

;;;; end of file -- documentation-production.lisp --

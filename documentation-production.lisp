;;;; -*- Mode: Lisp -*-

;;;; documentation-production.lisp --

(in-package "HELAMBDAP")

(defgeneric build-documentation (for-what
                                 format
                                 &key
                                 layout
                                 source
                                 destination
                                 &allow-other-keys)
  (:documentation "Produces the documentation according to a number of parameters"))


(defgeneric build-doc-skeleton (for-what
                                format
                                &key
                                layout
                                source
                                destination
                                &allow-other-keys)
  (:documentation "Produces a skeleton for the documentation.

The skeleton consists of a number of 'judiciously' editable files that
can be used asbuilding blocks for the final documentation."))


(defun document (for-what
                 &key
                 (format 'html)
                 (layout *default-documentation-structure*)
                 (source #P"")
                 (destination 
                  (make-pathname :directory '(:relative "doc" "html")))
                 )
  (build-documentation for-what
                       format
                       :layout layout
                       :source source
                       :destination destination))


;;;;---------------------------------------------------------------------------
;;;; Build documentation implementations.

(defmethod build-documentation ((p pathname)
                                (format (eql 'html))
                                &key
                                (layout *default-documentation-structure*)
                                (source #P".")
                                (destination
                                 (make-pathname :directory '(:relative "doc" "html")))
                                )
  (declare (ignore source))
  (let ((doc-bits (collect-documentation p)))
    (produce-documentation format
                           layout
                           destination
                           doc-bits))
  )
  

;;;; end of file -- documentation-production.lisp --
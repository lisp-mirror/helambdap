;;;; -*- Mode: Lisp -*-

;;;; doc-bit.lisp --

(in-package "HELAMBDAP")


;;; doc-bit struture --

(defstruct (doc-bit (:type list) :named)
  "The DOC-BIT Structure.

The structure of a documentation bit."

  (name nil :type (or symbol naming)) ; Either a name a CONS like (SETF s).
  kind ; As per CL:DOCUMENTATION second argument, with extra "naming"
       ; accepted, in the fashion of LW DSPEC package
  (doc-string "" :type string)
  (timestamp (get-universal-time) :type integer)
  location ; We assume that NAME is unique, hence LOCATION must be as well.
  )

;;; end of file -- doc-bit.lisp --

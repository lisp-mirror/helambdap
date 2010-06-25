;;;; -*- Mode: Lisp -*-

;;;; doc-bit.lisp --

(in-package "HELAMBDAP")


;;; doc-bit struture --
;;; The use of the :TYPE LIST option is for easiness of READ/WRITE.

(defstruct (doc-bit)
  "The DOC-BIT Structure.

The structure of a documentation bit."

  (name nil :type (or symbol naming) :read-only t) ; Either a name a CONS like (SETF s).
  (kind t :read-only t) ; As per CL:DOCUMENTATION second argument, with extra "naming"
                        ; accepted, in the fashion of LW DSPEC package
  (doc-string "" :type string)
  (timestamp (get-universal-time) :type integer)
  location ; We assume that NAME is unique, hence LOCATION must be as
           ; well.  doc-bit => location is 1-1.

  )


(defstruct (parameterized-doc-bit (:include doc-bit))
  (lambda-list ()))


(defstruct (function-doc-bit (:include parameterized-doc-bit)))


(defstruct (macro-doc-bit (:include parameterized-doc-bit)))


(defstruct (compiler-macro-doc-bit (:include parameterized-doc-bit)))


(defstruct (setf-expander-doc-bit (:include parameterized-doc-bit)))


(defstruct (modify-macro-doc-bit (:include macro-doc-bit)))


(defstruct (generic-function-doc-bit (:include parameterized-doc-bit)))


(defstruct (method-doc-bit (:include parameterized-doc-bit))
  qualifiers)


(defstruct (type-doc-bit (:include parameterized-doc-bit)))


(defstruct (class-doc-bit (:include type-doc-bit))
  superclasses)


(defstruct (condition-doc-bit (:include class-doc-bit))
  )


(defstruct (struct-doc-bit (:include type-doc-bit))
  include)


(defstruct (method-combination-doc-bit (:include type-doc-bit))
  )


(defstruct (package-doc-bit (:include doc-bit))
  use-list
  nicknames
  )

;;; end of file -- doc-bit.lisp --

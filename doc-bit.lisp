;;;; -*- Mode: Lisp -*-

;;;; doc-bit.lisp --

(in-package "HELAMBDAP")


;;; doc-bit struture --

(defstruct doc-bit
  "The DOC-BIT Structure.

The structure of a documentation bit."

  (name nil :type (or symbol naming) :read-only t) ; Either a name a CONS like (SETF s).
  (kind t :read-only t) ; As per CL:DOCUMENTATION second argument, with extra "naming"
                        ; accepted, in the fashion of LW DSPEC package
  (kind-tag "" :type string :read-only t)
  (doc-string "" :type string)
  (timestamp (get-universal-time) :type integer)
  location ; We assume that NAME is unique, hence LOCATION must be as
           ; well.  doc-bit => location is 1-1.

  )


(defstruct (variable-doc-bit (:include doc-bit (kind-tag "Variable")))
  (initial-value nil :read-only t))


(defstruct (parameter-doc-bit (:include variable-doc-bit (kind-tag "Parameter"))))

(defstruct (constant-doc-bit (:include variable-doc-bit (kind-tag "Constant"))))


(defstruct (parameterized-doc-bit (:include doc-bit)
                                  (:constructor nil))
  (lambda-list () :read-only t :type list))


(defstruct (function-doc-bit (:include parameterized-doc-bit (kind-tag "Function"))))


(defstruct (macro-doc-bit (:include parameterized-doc-bit (kind-tag "Macro"))))


(defstruct (compiler-macro-doc-bit (:include parameterized-doc-bit (kind-tag "Compiler Macro"))))


(defstruct (setf-expander-doc-bit (:include parameterized-doc-bit (kind-tag "SETF Expander"))))


(defstruct (modify-macro-doc-bit (:include macro-doc-bit (kind-tag "Modifier Macro"))))


(defstruct (generic-function-doc-bit (:include parameterized-doc-bit (kind-tag "Generic Function"))))


(defstruct (method-doc-bit (:include parameterized-doc-bit (kind-tag "Method")))
  (qualifiers () :type list :read-only t))


(defstruct (type-doc-bit (:include parameterized-doc-bit (kind-tag "Type"))))


(defstruct (class-doc-bit (:include type-doc-bit (kind-tag "Class")))
  (superclasses () :type list :read-only t))


(defstruct (condition-doc-bit (:include class-doc-bit (kind-tag "Condition")))
  )


(defstruct (struct-doc-bit (:include type-doc-bit (kind-tag "Structure")))
  (include nil :type symbol :read-only t)
  )


(defstruct (method-combination-doc-bit (:include type-doc-bit))
  )


(defstruct (package-doc-bit (:include doc-bit (kind-tag "Package")))
  (use-list '("CL") :type list :read-only t)
  (nicknames () :type list :read-only t)
  )


(defstruct (system-doc-bit (:include doc-bit (kind-tag "System"))
                           (:constructor nil))
  (depends-on () :type list :read-only t))


#+mk-defsystem
(defstruct (mk-system-doc-bit (:include system-doc-bit)))

#+asdf
(defstruct (asdf-system-doc-bit (:include system-doc-bit)))

#+lispworks
(defstruct (lw-system-doc-bit (:include system-doc-bit)))


;;; end of file -- doc-bit.lisp --

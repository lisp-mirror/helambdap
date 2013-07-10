;;;; -*- Mode: Lisp -*-

;;;; doc-bit.lisp --

(in-package "HELAMBDAP")


;;; doc-bit struture --

(defstruct doc-bit
  "The DOC-BIT Structure.

The structure of a documentation bit."

  (name nil :type (or symbol naming string list) :read-only t) ; Either a name a CONS like (SETF s).
  (kind t :read-only t) ; As per CL:DOCUMENTATION second argument, with extra "naming"
                        ; accepted, in the fashion of LW DSPEC package
  (kind-tag "" :type string :read-only t)
  (doc-string "" :type (or null string))
  (timestamp (get-universal-time) :type integer)
  location ; We assume that NAME is unique, hence LOCATION must be as
           ; well.  doc-bit => location is 1-1.

  )

(defun doc-bit-identifier (db &aux (dbn (doc-bit-name db)))
  (declare (type doc-bit))
  (etypecase dbn
    ((or symbol string) dbn)
    (naming (let ((n (naming-id dbn)))
              (if (listp n) ; (SETF X) et al.
                  (second n)
                  n)))
    (list ; (SETF X) et al.
     (second dbn))
    ))


(defun doc-bit-package (db &aux (dbi (doc-bit-identifier db)))
  (declare (type doc-bit))
  (typecase dbi
    (symbol (symbol-package dbi))
    (string (if (package-doc-bit-p db) dbi "CL-USER"))
    ))


(eval-when (:load-toplevel :compile-toplevel :execute)

(defmacro def-doc-bit (name include tag &body slots)
  `(defstruct (,name (:include ,include (kind-tag ,tag))) ,.slots))

)


#+old-version
(defun doc-bit-pathname-name (doc-bit)
  "Ensures that the resulting pathname does not contain 'problematic' characters."
  (let ((name (string (doc-bit-name doc-bit)))
        (kind (doc-bit-kind-tag doc-bit))
        )
    (with-output-to-string (result)
      (write-string kind result)
      (write-string "-" result)
      (loop for c across name
            if (char= #\* c)
            do (write-string "\\*" result)
            else if (char= #\Space c)
            do (write-char #\_ result)
            else
            do (write-char c result)))))

#+old-version
(defun doc-bit-pathname-name (doc-bit)
  (concatenate 'string
               (substitute #\_ #\Space (doc-bit-kind-tag doc-bit))
               "-"
               (string (doc-bit-name doc-bit))))


(defun doc-bit-pathname-name (doc-bit)
  (nsubstitute #\| #\/
               (nsubstitute #\_ #\Space
                            (format nil "~A-~A"
                                    (doc-bit-kind-tag doc-bit)
                                    (doc-bit-name doc-bit)))))


(defun make-doc-bit-pathname (doc-bit
                              &optional
                              (type "txt")
                              (where *default-pathname-defaults*))
  (make-pathname :name (doc-bit-pathname-name doc-bit)
                 :type type
                 :defaults where))
      

;;;;===========================================================================
;;;; Known DOC-BITS.

;;;;---------------------------------------------------------------------------
;;;; Standard CL doc bits (as per DOCUMENTATION, plus "systems").

(def-doc-bit variable-doc-bit doc-bit "Variable"
  (initial-value nil :read-only t))

#|
(defstruct (variable-doc-bit (:include doc-bit (kind-tag "Variable")))
  (initial-value nil :read-only t))
|#

(defstruct (parameter-doc-bit (:include variable-doc-bit (kind-tag "Parameter"))))

(defstruct (constant-doc-bit (:include variable-doc-bit (kind-tag "Constant"))))


(defstruct (parameterized-doc-bit (:include doc-bit)
                                  (:constructor nil))
  (lambda-list () :read-only t :type list))


(defstruct (function-doc-bit (:include parameterized-doc-bit (kind-tag "Function")))
  (values () :read-only t :type list))


(defstruct (macro-doc-bit (:include parameterized-doc-bit (kind-tag "Macro"))))


(defstruct (compiler-macro-doc-bit (:include parameterized-doc-bit (kind-tag "Compiler Macro"))))


(defstruct (setf-expander-doc-bit (:include parameterized-doc-bit (kind-tag "SETF Expander"))))


(defstruct (modify-macro-doc-bit (:include macro-doc-bit (kind-tag "Modifier Macro"))))


(defstruct (generic-function-doc-bit (:include function-doc-bit (kind-tag "Generic Function")))
  (methods () :type list))


(defstruct (method-doc-bit (:include function-doc-bit (kind-tag "Method")))
  (qualifiers () :type list :read-only t))


(defstruct (type-doc-bit (:include doc-bit (kind-tag "Type"))))


;;; Need multiple inheritance!

(defstruct (deftype-doc-bit
            (:include parameterized-doc-bit (kind-tag "Type"))))


(defstruct (slotted-doc-bit (:include type-doc-bit) (:constructor nil))
  (slots () :type list :read-only t))
  

(defstruct (class-doc-bit (:include slotted-doc-bit (kind-tag "Class")))
  (superclasses () :type list :read-only t))


(defstruct (condition-doc-bit (:include class-doc-bit (kind-tag "Condition")))
  )


(defstruct (struct-doc-bit (:include slotted-doc-bit (kind-tag "Structure")))
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


;;;---------------------------------------------------------------------------
;;; Using a "factory" generic function.

(defgeneric new-doc-bit (doc-bit-class &rest args))



;;;;---------------------------------------------------------------------------
;;;; "Document" doc bits; patterned after DocBook.

;;; Note.
;;; It may be worth to bite the bullet ad start defining a DTD-like or a
;;; XSD-like set of macros, but WTH!
;;;


;;; WHAT FOLLOWS IS UNUSED.

(def-doc-bit set-doc-bit doc-bit "Set"
  )


(def-doc-bit book-doc-bit doc-bit "Book"
  ;; dedication
  navigation
  divisions
  )


(def-doc-bit part-doc-bit doc-bit "Part"
  components
  )


(def-doc-bit chapter-doc-bit doc-bit "Chapter"
  components
  )


(def-doc-bit article-doc-bit doc-bit "Article"
  components
  )


(def-doc-bit literal-doc-bit doc-bit "Literal"
  (content nil :read-only t :type (or null string))
  )

;;; end of file -- doc-bit.lisp --

;;;; -*- Mode: Lisp -*-

;;;; doc-bit.lisp --

(in-package "HELAMBDAP")


;;; doc-bit struture --

(defstruct doc-bit
  "The DOC-BIT Structure.

The structure of a documentation bit."

  (name nil
        :type (or symbol naming string list)
        :read-only t) ; Either a name a CONS like (SETF s).
  (kind t
        :read-only t) ; As per CL:DOCUMENTATION second argument, with
                      ; extra "naming" accepted, in the fashion of LW
                      ; DSPEC package.
  (kind-tag ""
            :type string
            :read-only t)
  (doc-string ""
              :type (or null string))
  (timestamp (get-universal-time)
             :type integer)
  location ; We assume that NAME is unique, hence LOCATION must be as
           ; well.  doc-bit => location is 1-1.
  )


(defmethod print-object ((d doc-bit) stream)
  (print-unreadable-object (d stream :identity t)
    (format stream "~A ~A ~S ~S"
            (doc-bit-name d)
            (doc-bit-kind-tag d)
            (subseq (doc-bit-doc-string d)
                    0
                    (min 10 (length (doc-bit-doc-string d))))
            (doc-bit-timestamp d))))


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
    (string (if (package-doc-bit-p db) dbi (find-package "CL-USER")))))


(defun package-shortest-name (p &aux (ns (package-nicknames p)))
  (declare (type package p))
  (loop with sn = (package-name p)
        with l = (length sn)
        for x in ns
        for nl = (length x)
        when (< nl l)
        do (setf sn x l nl)
        finally (return sn)))


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


#+old-old-version
(defun doc-bit-pathname-name (doc-bit)
  "Ensures that the resulting pathname does not contain 'problematic' characters.

More specifically: #\\/ #\\Space #\\*"
  (nsubstitute #\= #\/
               (nsubstitute #\_ #\Space
                            (format nil "~A-~A"
                                    (doc-bit-kind-tag doc-bit)
                                    (doc-bit-name doc-bit)))))

#+old-new-version
(defun doc-bit-pathname-name (doc-bit
                              &aux
                              (dbpn (format nil "~A-~A"
                                            (doc-bit-kind-tag doc-bit)
                                            (doc-bit-name doc-bit))))
  "Ensures that the resulting pathname does not contain 'problematic' characters.

More specifically: #\\/ #\\Space #\\* #\%"
  (nsubstitute #\_ #\*
               (nsubstitute #\_ #\*
                            (nsubstitute #\= #\/
                                         (nsubstitute #\_ #\Space dbpn)))))

(defparameter *bad-chars-replacements*
  '((#\/ "=")
    (#\Space "_")
    (#\* "ast")
    (#\% "p100")
    (#\# "hash")
    (#\& "amp")
    (#\( "op")
    (#\) "cp")
    (#\? "qm")
    (#\! "em")

    ;; The following (plus some above) are bad characters for Windows; see
    ;; https://msdn.microsoft.com/en-us/library/windows/desktop/aa365247(v=vs.85).aspx#file_and_directory_names
    ;; Thanks to Mirko Vukovic for noticing this.
    (#\< "lt")
    (#\> "gt")
    (#\: "colon")
    (#\" "dq")
    (#\\ "bslash")
    (#\| "vb")
    )
  )

(defun doc-bit-pathname-name (doc-bit
                              &aux
                              (bad-chars *bad-chars-replacements*)
                              (dbpn (format nil "~A-~A-~A"
                                            (doc-bit-kind-tag doc-bit)
                                            (package-shortest-name
                                             (doc-bit-package doc-bit))
                                            (doc-bit-name doc-bit))))
  "Ensures that the resulting pathname does not contain 'problematic' characters.

More specifically: #\\/ #\\Space #\\* #\% #\( #\)"
  (with-output-to-string (result)
    (loop for c across dbpn
          for bad-char-n-repl = (find c bad-chars :key #'first :test #'char=)
          if bad-char-n-repl
          do (write-string (second bad-char-n-repl) result)
          else
          do (write-char c result))))


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
  (lambda-list () :read-only t :type list)
  (type-declarations () :read-only t :type list) ; From DECLARE.
  (ftype-declarations () :read-only t :type list) ; From DECLARE.
  )


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

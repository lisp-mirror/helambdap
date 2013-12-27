;;;; -*- Mode: Lisp -*-

;;;; extract-doc.lisp --
;;;;
;;;; See file COPYING for copyright and licensing information.

(in-package "HELAMBDAP")


;;; RETURNS special declaration.
;;; The RETURNS declaration can be used to declare (and document) the
;;; values returned by a 'function'.
;;; The syntax is
;;;
;;;    results-decl ::= '('RETURNS rdecls')'
;;;    rdecls       ::= ()
;;;                 |   rdecl rdecls
;;;    rdecl        ::= type
;;;                 |   '(' doc-string type optname ')'
;;;    type         ::= a CL type
;;;    doc-string   ::= a string
;;;    optname      ::= a symbol
;;;
;;; Only the first declaration is considered.  The others are ignored.

(eval-when (:load-toplevel :compile-toplevel :execute)
  (proclaim '(declaration returns))
  )


;;; extract-documentation --

(defgeneric extract-documentation (where-from)
  (:documentation
   "Extracts the documentation from a source WHERE-FROM."))


;;; extract-form-documentation --

(defgeneric extract-form-documentation (form-kind form)
  (:documentation
   "Extracts the documentation from a form tagged with a specific kind."))


;;; extract-named-form-documentation --

(defgeneric extract-named-form-documentation (form-kind name form)
  (:documentation
   "Extracts the documentation from a 'namd' form.

A 'named' form has the following structure: (<name> . <forms>).
This generic function is useful to add functionality in the presence
of symbols for which the package may not be present at runtime.

This function is called by the more general EXTRACT-FORM-DOCUMENTATION
method on (FORM-KIND SYMBOL).  The call has the form:

    (extract-named-form-documentation s (intern s \"HELAMBDAP\") form)

In this way it is possible to write special methods to handle
\"unpackaged\" symbols.
")
  (:method ((form-kind symbol) name form) nil)
  )


;;;; Package handling --

(defparameter *current-package* (find-package "COMMON-LISP-USER")
  "The variable holding the 'current' package.

Processing an IN-PACKAGE form sets this variable (if the package is
known) subsequent READs by the documentation extraction machinery
are done with *PACKAGE* bound to *CURRENT-PACKAGE*.")


;;; read-form --

(defun read-form (forms-stream &optional (eof (gensym "FORMS-STREAM-EOF-")))
  (handler-case
      (let ((*package* *current-package*))
        (read forms-stream nil eof))
    (simple-error (e)
      (format *error-output*
              "~%HELambdaP form reader: trying to read a form caused errors.~@
               ~?~@
               The result will be NIL, hence the form will be ignored.~2%"
              (simple-condition-format-control e)
              (simple-condition-format-arguments e)
              )
      nil)
    (error (e)
      (format *error-output*
              "~&HELambdaP form reader: trying to read a form caused errors; most likely a missing package.~@
               The error is ~S.~@
               The result will be NIL, hence the form will be ignored.~2%"
              e)
      nil)))


;;; extract-documentation --

(defmethod extract-documentation ((forms-stream stream))
  (let ((saved-package *package*)
        (*current-package* *package*)
        )
    (unwind-protect
        (loop with eof = (gensym "FORMS-STREAM-EOF-")
              for form = (read-form forms-stream eof)
              for form-doc = (form-documentation form)

              while (not (eq form eof))
                when form-doc
                  if (consp form-doc)
                    nconc (delete nil form-doc) into doc-bits
                  else
                    collect form-doc into doc-bits
                  end

              finally (return doc-bits))
      (setf *package* saved-package))))


(defmethod extract-documentation ((file pathname))
  (with-open-file (in file
                      :direction :input
                      :if-does-not-exist :error)
    (let ((file-doc-bits (extract-documentation in)))
      (dolist (fdb file-doc-bits file-doc-bits)
        (setf (doc-bit-location fdb) file)))))


(defmethod extract-documentation ((filename string))
  (extract-documentation (pathname filename)))


(defmethod extract-documentation :around ((forms-stream stream))
  (let ((doc-bits (delete nil (call-next-method))))
    (unless *everything*
      (when *only-documented*
        (setf doc-bits
              (delete-if (complement #'doc-bit-doc-string) doc-bits)))

      (when *only-exported*
        (setf doc-bits
              (delete-if (lambda (db &aux (dbn (doc-bit-identifier db)))
                           (and (symbolp dbn) (not (external-symbol-p dbn))))
                         doc-bits)))
      )
    doc-bits))


;;; Form handling.

(defun form-documentation (form)
  (when (consp form)
    (extract-form-documentation (first form) form)))


(defun is-declaration (f)
  (and (consp f) (eq (first f) 'declare)))


(defun extricate-doc-string (forms)
  "Given a list of FORMS finds a doc-string according to CL rules.

Cfr. ANSI 3.4.11 Syntactic Interaction of Documentation Strings and Declarations."
  (loop for (doc-or-decl . more-forms) on forms
        while (or (stringp doc-or-decl) (is-declaration doc-or-decl))
        when (and (stringp doc-or-decl) more-forms)
        return doc-or-decl))


(defun collect-declarations (forms)
  "Collects a list of declarations from a form list according to CL rules.

Cfr. ANSI 3.4.11 Syntactic Interaction of Documentation Strings and Declarations."
  ;; Not really really really right, but good enough FTTB.
  (loop for f in forms
        while (or (stringp f) (is-declaration f))
        when (is-declaration f) collect f))


;;; Documentation per form.

(defmacro define-documentation-extractor (spec &body forms)
  "Defines a specialized procedure to extract a doc string from a definition.
"
  `(defmethod extract-form-documentation ((_%FK%_ (eql ',(first spec)))
                                          (_%FORM%_ cons))
     ;; I know I should gensym these...
     (destructuring-bind ,spec
         _%FORM%_
       (declare (ignore ,(first spec)))
       ,.forms)))
       

;;; extract-form-documentation --

(defmethod extract-form-documentation ((fk symbol) (form cons))
  (let ((doc-bit
         (extract-named-form-documentation fk
                                           (intern (string fk) "HELAMBDAP")
                                           form))
        )
    (unless doc-bit
      (warn "Operator ~A not handled." fk))
    doc-bit))


(defmethod extract-form-documentation ((fk (eql 'deftype)) (form cons))
  (destructuring-bind (deftype name ll &rest forms)
      form
    (declare (ignore deftype))
    (make-deftype-doc-bit :name name
                          :kind 'type
                          :lambda-list ll
                          :doc-string (extricate-doc-string forms))))


(defmethod extract-form-documentation ((fk (eql 'defun)) (form cons))
  (destructuring-bind (defun name ll &rest forms)
      form
    (declare (ignore defun))
    (let* ((decls (collect-declarations forms))
           (values-decl (find 'returns (mapcan #'rest decls) :key #'first))
           )
      (make-function-doc-bit :name name
                             :kind 'function
                             :lambda-list ll
                             :values (rest values-decl)
                             :doc-string (extricate-doc-string forms)))))


(defmethod extract-form-documentation ((fk (eql 'defmacro)) (form cons))
  (destructuring-bind (defmacro name ll &rest forms)
      form
    (declare (ignore defmacro))
    (make-macro-doc-bit :name name
                        :kind 'macro ; This breaks the rule.  There is
                                     ; no MACRO in the
                                     ; CL:DOCUMENTATION reference.
                        :lambda-list ll
                        :doc-string (extricate-doc-string forms))))


(defmethod extract-form-documentation ((fk (eql 'define-compiler-macro)) (form cons))
  (destructuring-bind (define-compiler-macro name ll &rest forms)
      form
    (declare (ignore define-compiler-macro))
    (make-compiler-macro-doc-bit :name name
                                 :kind 'compiler-macro
                                 :lambda-list ll
                                 :doc-string (extricate-doc-string forms))))


(defmethod extract-form-documentation ((fk (eql 'define-setf-expander)) (form cons))
  (destructuring-bind (define-setf-expander name ll &rest forms)
      form
    (declare (ignore define-setf-expander))
    (make-setf-expander-doc-bit :name name
                                :kind 'setf
                                :lambda-list ll
                                :doc-string (extricate-doc-string forms))))


(defmethod extract-form-documentation ((fk (eql 'defclass)) (form cons))
  (destructuring-bind (defclass name supers slots &rest options)
      form
    (declare (ignore defclass))
    (make-class-doc-bit :name name
                        :kind 'type
                        :superclasses supers
                        :slots slots
                        :doc-string (second (find :documentation options
                                                  :key #'first)))))


(defmethod extract-form-documentation ((fk (eql 'define-condition)) (form cons))
  (destructuring-bind (define-condition name supers slots &rest options)
      form
    (declare (ignore define-condition slots))
    (make-condition-doc-bit :name name
                            :kind 'type
                            :superclasses supers
                            :doc-string (second (find :documentation options
                                                      :key #'first)))))


(defmethod extract-form-documentation ((fk (eql 'defgeneric)) (form cons))
  (destructuring-bind (defgeneric name ll &rest options-and-methods)
      form
    (declare (ignore defgeneric))
    (let* ((decls (collect-declarations options-and-methods))
           ;; This is essentially wrong.  It should return NIL (almost) always.
           (values-decl (find 'returns (mapcan #'rest decls) :key #'first))
           )
      (make-generic-function-doc-bit :name name
                                     :kind 'function
                                     :lambda-list ll
                                     :values (rest values-decl)
                                     :doc-string (second (find :documentation options-and-methods
                                                               :key #'first))))))


(defmethod extract-form-documentation ((fk (eql 'defpackage)) (form cons))
  (destructuring-bind (defpackage name &rest options)
      form
    (declare (ignore defpackage))
    ;; We must ensure that a package always has a doc string.
    (make-package-doc-bit :name name
                          :kind 'package
                          :use-list (rest (find :use options :key #'first))
                          :nicknames (rest (find :nicknames options :key #'first))
                          :doc-string (or (second (find :documentation options
                                                        :key #'first))
                                          (format nil "The ~A Package." name)
                                          ))))


(defvar *try-to-ensure-packages* t
  "Controls whether the system should try to create the packages it encouters.

DEFPACKAGE and IN-PACKAGE forms will be evaluated if non-NIL (default
T). Only top-level occurrences of these forms are considered.")


(defmethod extract-form-documentation :before ((fk (eql 'defpackage)) (form cons))
  (when (and *try-to-ensure-packages* (not (find-package (second form))))
    (eval form)))


(defmethod extract-form-documentation :before ((fk (eql 'in-package)) (form cons))
  (when *try-to-ensure-packages*
    (let* ((pkg-name (second form))
           (pkg (find-package pkg-name))
           )
      (when *try-to-ensure-packages*
        (unless pkg
          (make-package pkg-name))))))


(defmethod extract-form-documentation ((fk (eql 'in-package)) (form cons))
  nil)


(defmethod extract-form-documentation :after ((fk (eql 'in-package)) (form cons))
  (let ((pkg (find-package (second form))))
    (when pkg
      (setf *current-package* pkg))))


(defmethod extract-form-documentation ((fk (eql 'defmethod)) (form cons))
  (destructuring-bind (defmethod name &rest rest-method)
      form
    (declare (ignore defmethod))
    (let ((arglist-pos (position-if #'consp rest-method)))
      (make-method-doc-bit :name name
                           :kind 'method ; Incorrect.
                           :qualifiers (subseq rest-method 0 arglist-pos)
                           :lambda-list (nth arglist-pos rest-method)
                           :doc-string (extricate-doc-string
                                        (subseq rest-method (1+ arglist-pos)))))
    ))


#| Old simple one...
(defmethod extract-form-documentation ((fk (eql 'defstruct)) (form cons))
  (destructuring-bind (defstruct name-and-options &rest doc-string-and-slots)
      form
    (declare (ignore defstruct))
    (make-struct-doc-bit :name (if (symbolp name-and-options)
                                   name-and-options
                                   (first name-and-options))
                         :kind 'structure
                         :doc-string (when (stringp (first doc-string-and-slots))
                                       (first doc-string-and-slots))
                         :include (when (consp name-and-options)
                                    (second (find :include (remove-if #'symbolp name-and-options)
                                                  :key #'first)))
                         :slots (if (stringp (first doc-string-and-slots))
                                    (rest doc-string-and-slots)
                                    doc-string-and-slots)
                         )
    ))
|#


(defmethod extract-form-documentation ((fk (eql 'defstruct)) (form cons))
  (destructuring-bind (defstruct name-and-options &rest doc-string-and-slots)
      form
    (declare (ignore defstruct))
    (let* ((name-is-symbol (symbolp name-and-options))
           (name (if name-is-symbol
                     name-and-options
                     (first name-and-options)))
           (options (unless name-is-symbol
                      (rest name-and-options)))
           (doc-string-present (stringp (first doc-string-and-slots)))
           (doc-string (when doc-string-present (first doc-string-and-slots)))
           (slots (if doc-string-present
                      (rest doc-string-and-slots)
                      doc-string-and-slots))

           ;; Options unpacking.
           (default-conc-name (format nil "~A-" name))
           (conc-name (if name-is-symbol
                          (intern default-conc-name (symbol-package name))
                          (loop for opt in options
                                if (eq opt :conc-name)
                                return nil
                                else if (and (consp opt)
                                             (eq (first opt) :conc-name))
                                return (second opt)
                                finally
                                (return (intern default-conc-name (symbol-package name)))
                                )))
           (default-constructor-name (format nil "~A-~A" 'make name))
           )
      (labels ((extract-slot-names (slots)
                 (mapcar (lambda (slot-spec)
                           (typecase slot-spec
                             (symbol slot-spec)
                             (list (first slot-spec))))
                         slots)
                 )

               (build-doc-for-slots-fns (slots)
                 (loop for s in slots
                       if (symbolp s)
                       nconc (build-doc-for-single-slot-fns s nil t)
                       else
                       nconc
                       (destructuring-bind (sn vf
                                               &key
                                               read-only
                                               (type t)
                                               &allow-other-keys)
                           s
                         (declare (ignore vf))
                         (build-doc-for-single-slot-fns sn read-only type))))

               (build-doc-for-single-slot-fns (sn read-only type
                                                  &aux
                                                  (fns ())
                                                  (accessor-fn-name
                                                   (if conc-name
                                                       (intern (format nil "~A~A"
                                                                       conc-name
                                                                       sn)
                                                               (symbol-package sn))
                                                       sn))
                                                  )
                 (push (make-function-doc-bit
                        :name accessor-fn-name
                        :kind 'function
                        :lambda-list (list 'object)
                        :values (list type)
                        :doc-string
                        (format nil "Accessor for the~:[~; read-only~] slot ~A of an object of type ~A.~@
                                     ~@
                                     Arguments and Values:~@
                                     ~@
                                     OBJECT : a ~A~@
                                     result : a ~A"
                                read-only
                                sn
                                name
                                name
                                type
                                )
                        )
                       fns)
                 (unless read-only
                   (push (make-function-doc-bit
                          :name `(setf ,accessor-fn-name)
                          :kind 'function
                          :lambda-list (list 'v 'object)
                          :values (list type)
                          :doc-string
                          (format nil "Setter for the slot ~A of an object of type ~A.~@
                                     ~@
                                     Arguments and Values:~@
                                     ~@
                                     OBJECT : a ~A~@
                                     result : a ~A"
                                  sn
                                  name
                                  name
                                  type)
                          )
                         fns)
                   )
                 fns
                 )

               (build-default-constructor-doc-bit ()
                 (let ((sns (extract-slot-names slots)))
                   (make-function-doc-bit
                    :name (intern default-constructor-name
                                  (symbol-package name))
                    :kind 'function
                    :lambda-list (if sns
                                     (cons '&key
                                           (extract-slot-names slots))
                                     ()
                                     )
                    :values (list name)
                    :doc-string
                    (format nil "A constructor for the structure ~A." name)
                    )))

               (build-doc-for-constructors (opts)
                 (let ((constructor-opts
                        (loop for opt in opts
                              when (typecase opt
                                     (symbol (eq :constructor opt))
                                     (list (eq :constructor (first opt))))
                              collect (if (symbolp opt) (list opt) opt)))
                       )
                   (cond ((null constructor-opts)
                          (list (build-default-constructor-doc-bit)))
                         ((not (find '(:constructor nil) constructor-opts :test #'equal))
                          (loop for cons-opt in constructor-opts
                                collect
                                (destructuring-bind (cons-kwd
                                                     &optional
                                                     cons-name
                                                     boa-ll)
                                    cons-opt
                                  (declare (ignore cons-kwd))
                                  (if (null cons-name) ; constructor
                                                       ; opt is (:constructor)
                                      (build-default-constructor-doc-bit)
                                      (make-function-doc-bit
                                       :name cons-name
                                       :kind 'function
                                       :lambda-list (cond (boa-ll boa-ll)
                                                          (slots (cons '&key
                                                                       (extract-slot-names slots))))
                                       :values (list name)
                                       :doc-string
                                       (format nil
                                               "A constructor for the ~
                                                structure ~A." name)
                                       ))
                                  )))
                         (t ())
                         )))
               )

        (append
         (list
          (make-struct-doc-bit :name name
                               :kind 'structure
                               :doc-string doc-string
                               :include (when (consp name-and-options)
                                          (second (find :include (remove-if #'symbolp name-and-options)
                                                        :key #'first)))
                               :slots slots
                               ))
         (build-doc-for-slots-fns slots)
         (build-doc-for-constructors options)
         )))))


(defmethod extract-form-documentation ((fk (eql 'eval-when)) (form cons))
  (destructuring-bind (eval-when times &rest forms)
      form
    (declare (ignore eval-when times))
    (delete nil (mapcar #'form-documentation forms))))


(defmethod extract-form-documentation ((fk (eql 'progn)) (form cons))
  (destructuring-bind (progn &rest forms)
      form
    (declare (ignore progn))
    (delete nil (mapcar #'form-documentation forms))))


(defun extract-symbol-form-documentation (form)
  (fourth form))


(defmethod extract-form-documentation ((fk (eql 'defparameter)) (form cons))
  (let ((doc (extract-symbol-form-documentation form)))
    (make-parameter-doc-bit :name (second form)
                            :kind 'variable
                            :doc-string doc)))


(defmethod extract-form-documentation ((fk (eql 'defvar)) (form cons))
  (let ((doc (extract-symbol-form-documentation form)))
    (make-variable-doc-bit :name (second form)
                           :kind 'variable
                           :doc-string doc)))


(defmethod extract-form-documentation ((fk (eql 'defconstant)) (form cons))
  (let ((doc (extract-symbol-form-documentation form)))
    (make-constant-doc-bit :name (second form)
                           :initial-value (third form)
                           :kind 'constant
                           :doc-string doc)))


(define-documentation-extractor (declaim &rest forms)
  (declare (ignore forms))
  nil
  )


(define-documentation-extractor (proclaim &rest forms)
  (declare (ignore forms))
  nil
  )


(define-documentation-extractor (let bindings &rest forms) ; Hmmmmm!
  ;; LET is a 'special operator'; it should work, but...
  (declare (ignore bindings))
  (delete nil (mapcar #'form-documentation forms))
  )


(define-documentation-extractor (let* bindings &rest forms) ; Hmmmmm!
  ;; LET is a 'special operator'; it should work, but...
  (declare (ignore bindings))
  (mapcar #'form-documentation forms)
  )


(define-documentation-extractor (defsetf access-fn &rest form)
  (if (symbolp (first form))
      (let ((doc (second form)))
        (make-setf-expander-doc-bit :name access-fn
				    :kind 'setf
				    :doc-string doc))
      (let ((doc (extricate-doc-string (nthcdr 2 form)))
            (ll (first form))
            )
        (make-setf-expander-doc-bit :name access-fn
				    :kind 'setf
				    :lambda-list ll
				    :doc-string doc))))


(define-documentation-extractor (define-modify-macro name ll function
                                  &optional doc-string)
  (declare (ignore ll function))
  (make-doc-bit :name name
                :kind 'function
                :doc-string doc-string))


(define-documentation-extractor (define-method-combination name &rest rest-dmc)
  (let ((doc (if (keywordp (first rest-dmc)) ; Short form.
                 (getf rest-dmc :documentation)
                 (let ((options-and-forms (copy-list (nthcdr 2 rest-dmc)))) ; Paranoid!
                   (when (eq :arguments (first options-and-forms))
                     (pop options-and-forms))
                   (when (eq :generic-function (first options-and-forms))
                     (pop options-and-forms))
                   (extricate-doc-string options-and-forms))))
        )
    (make-doc-bit :name name
                  :kind 'method-combination
                  :doc-string doc)))


#+mk-defsystem
(define-documentation-extractor (mk:defsystem name &rest rest-system)
  ;; We must ensure that a system always has a doc string.
  (make-mk-system-doc-bit :name name
                          :kind 'mk::system
                          :depends-on (getf rest-system :depends-on)
                          :doc-string (or (second
                                           (member :documentation rest-system
                                                   :test #'eq))
                                          (format nil "The ~A System." name))
                                          ))


#+asdf
(define-documentation-extractor (asdf:defsystem name &rest rest-system)
  (make-asdf-system-doc-bit :name name
                            :kind 'asdf:system
                            :depends-on (getf rest-system :depends-on)
                            :doc-string (or (second
                                             (member :documentation rest-system
                                                     :test #'eq))
                                            (format nil "The ~A System." name))
                            ))


#+lispworks
(define-documentation-extractor (lw:defsystem name options &rest keys)
  (declare (ignore keys))
  (make-lw-system-doc-bit :name name
                          :kind 'scm:scm-system
                          :doc-string (getf options :documentation "")))


(defmethod extract-form-documentation :around ((fk symbol) form)  ; Catch all that fixes all problems.
  (let ((r (call-next-method)))
    (etypecase r
      (doc-bit r)
      (null nil)
      (list (delete nil r)))))


;;;;===========================================================================
;;;; Utilities.

(defun flatten-if (p l)
  (cond ((null l) nil)
        ((funcall p (first l))
         (cons (flatten-if p (first l))
               (flatten-if p (rest l))))
        (t
         (cons (first l)
               (flatten-if p (rest l))))
        ))


;;;; end of file -- extract-doc.lisp --

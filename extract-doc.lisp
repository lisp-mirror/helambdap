;;;; -*- Mode: Lisp -*-

;;;; extract-doc.lisp --

(in-package "HELAMBDAP")

;;; extract-documentation --

(defgeneric extract-documentation (where-from)
  (:documentation
   "Extracts the documentation from a source WHERE-FROM."))


;;; extract-form-documentation --

(defgeneric extract-form-documentation (form-kind form)
  (:documentation
   "Extracts the documentation from a form tagged with a specific kind."))


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
              "~%HELambdaP form reader: trying to read a form caused errors; most likely a missing package.~@
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

              finally (return
                       (delete-if (complement 'doc-bit-doc-string)
                                  doc-bits)))
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
  `(defmethod extract-form-documentation ((_%FK%_ (eql ',(first spec)))
                                          (_%FORM%_ cons))
     ;; I know I should gensym these...
     (destructuring-bind ,spec
         _%FORM%_
       (declare (ignore ,(first spec)))
       ,.forms)))
       

;;; extract-form-documentation --

(defmethod extract-form-documentation ((fk symbol) (form cons))
  (warn "Operator ~A not handled." fk)
  nil)


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
    (make-function-doc-bit :name name
                           :kind 'function
                           :lambda-list ll
                           :doc-string (extricate-doc-string forms))))


(defmethod extract-form-documentation ((fk (eql 'defmacro)) (form cons))
  (destructuring-bind (defmacro name ll &rest forms)
      form
    (declare (ignore defmacro))
    (make-macro-doc-bit :name name
                        :kind 'function
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
    (declare (ignore defclass slots))
    (make-class-doc-bit :name name
                        :kind 'type
                        :superclasses supers
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
    (make-generic-function-doc-bit :name name
                                   :kind 'function
                                   :lambda-list ll
                                   :doc-string (second (find :documentation options-and-methods
                                                             :key #'first)))))


(defmethod extract-form-documentation ((fk (eql 'defpackage)) (form cons))
  (destructuring-bind (defpackage name &rest options)
      form
    (declare (ignore defpackage))
    (make-package-doc-bit :name name
                          :kind 'package
                          :use-list (rest (find :use options :key #'first))
                          :nicknames (rest (find :nicknames options :key #'first))
                          :doc-string (second (find :documentation options
                                                    :key #'first)))))


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


(defmethod extract-form-documentation ((fk (eql 'defstruct)) (form cons))
  (destructuring-bind (defstruct name-and-options &rest doc-string-and-slots)
      form
    (declare (ignore defstruct))
    (when (stringp (first doc-string-and-slots))
      (make-struct-doc-bit :name (if (symbolp name-and-options)
                                     name-and-options
                                     (first name-and-options))
                           :kind 'structure
                           :doc-string (first doc-string-and-slots)
                           :include (when (consp name-and-options)
                                      (second (find :include (remove #'symbolp name-and-options)
                                                    :key #'first)))
                           ))))


(defmethod extract-form-documentation ((fk (eql 'eval-when)) (form cons))
  (destructuring-bind (eval-when times &rest forms)
      form
    (declare (ignore eval-when times))
    (mapcar #'form-documentation forms)))


(defmethod extract-form-documentation ((fk (eql 'progn)) (form cons))
  (destructuring-bind (progn &rest forms)
      form
    (declare (ignore progn))
    (mapcar #'form-documentation forms)))


(defun extract-symbol-form-documentation (form)
  (fourth form))


(defmethod extract-form-documentation ((fk (eql 'defparameter)) (form cons))
  (let ((doc (extract-symbol-form-documentation form)))
    (when doc
      (make-parameter-doc-bit :name (second form)
                              :kind 'variable
                              :doc-string doc))))


(defmethod extract-form-documentation ((fk (eql 'defvar)) (form cons))
  (let ((doc (extract-symbol-form-documentation form)))
    (when doc
      (make-variable-doc-bit :name (second form)
                             :kind 'variable
                             :doc-string doc))))


(defmethod extract-form-documentation ((fk (eql 'defconstant)) (form cons))
  (let ((doc (extract-symbol-form-documentation form)))
    (when doc
      (make-constant-doc-bit :name (second form)
                             :initial-value (third form)
                             :kind 'constant
                             :doc-string doc))))


(define-documentation-extractor (defsetf access-fn &rest form)
  (if (symbolp (first form))
      (let ((doc (second form)))
        (when doc
          (make-doc-bit :name access-fn
                        :kind 'setf
                        :doc-string doc)))
      (let ((doc (extricate-doc-string (nthcdr 2 form)))
            (ll (first form))
            )
        (when doc
          (make-doc-bit :name access-fn
                        :kind 'setf
                        :lambda-list ll
                        :doc-string doc)))))


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
    (when doc
      (make-doc-bit :name name
                    :kind 'method-combination
                    :doc-string doc))))


#+mk-defsystem
(define-documentation-extractor (mk:defsystem name &rest rest-system)
  (make-mk-system-doc-bit :name name
                          :kind 'mk::system
                          :depends-on (getf rest-system :depends-on)
                          :doc-string (second
                                       (member :documentation rest-system
                                               :test #'eq))))


#+asdf
(define-documentation-extractor (asdf:defsystem name &rest rest-system)
  (make-asdf-system-doc-bit :name name
                            :kind 'asdf:system
                            :depends-on (getf rest-system :depends-on)
                            :doc-string (second
                                         (member :description rest-system
                                                 :test #'eq))))


#+lispworks
(define-documentation-extractor (lw:defsystem name options &rest keys)
  (declare (ignore keys))
  (make-lw-system-doc-bit :name name
                          :kind 'scm:scm-system
                          :doc-string (getf options :documentation "")))


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

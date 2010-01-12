;;;; -*- Mode: Lisp -*-

(in-package "HELAMBDAP")


(defgeneric extract-documentation (where-from))

(defgeneric extract-form-documentation (form-kind form)
  (:documentation
   "Extracts the documentation from a form tagged with a specific kind."))


(defmethod extract-documentation ((forms-stream stream))
  (loop with eof = (gensym "FORMS-STREAM-EOF-")
        for form = (read forms-stream nil eof)
        while (not (eq form eof))
        collect (form-documentation form)))


(defmethod extract-documentation ((file pathname))
  (with-open-file (in file
                      :direction :input
                      :if-does-not-exist :error)
    (extract-documentation in)))


(defmethod extract-documentation ((filename string))
  (extract-documentation (pathname filename)))


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



;;; Documentation per form.

(defmacro define-documentation-extractor (spec &body forms)
  `(defmethod extract-form-documentation ((_%FK%_ (eql ',(first spec)))
                                          (_%FORM%_ cons))
     (destructuring-bind ,spec
         _%FORM%_
       (declare (ignore ,(first spec)))
       ,.forms)))
       

(defmethod extract-form-documentation ((fk symbol) (form cons))
  nil)


(defmethod extract-form-documentation ((fk (eql 'deftype)) (form cons))
  (destructuring-bind (deftype name ll &rest forms)
      form
    (declare (ignore deftype))
    (extricate-doc-string forms)))


(defmethod extract-form-documentation ((fk (eql 'defun)) (form cons))
  (destructuring-bind (defun name ll &rest forms)
      form
    (declare (ignore defun))
    (extricate-doc-string forms)))


(defmethod extract-form-documentation ((fk (eql 'define-compiler-macro)) (form cons))
  (destructuring-bind (define-compiler-macro name ll &rest forms)
      form
    (declare (ignore define-compiler-macro))
    (extricate-doc-string forms)))


(defmethod extract-form-documentation ((fk (eql 'define-setf-expander)) (form cons))
  (destructuring-bind (define-setf-expander name ll &rest forms)
      form
    (declare (ignore define-setf-expander))
    (extricate-doc-string forms)))


(defmethod extract-form-documentation ((fk (eql 'defclass)) (form cons))
  (destructuring-bind (defclass name supers slots &rest options)
      form
    (declare (ignore defclass))
    (second (find :documentation options-and-methods :key #'first))))


(defmethod extract-form-documentation ((fk (eql 'define-condition)) (form cons))
  (destructuring-bind (define-condition name supers slots &rest options)
      form
    (declare (ignore define-condition))
    (second (find :documentation options-and-methods :key #'first))))


(defmethod extract-form-documentation ((fk (eql 'defgeneric)) (form cons))
  (destructuring-bind (defgeneric name ll &rest options-and-methods)
      form
    (declare (ignore defgeneric))
    (second (find :documentation options-and-methods :key #'first))))


(defmethod extract-form-documentation ((fk (eql 'defpackage)) (form cons))
  (destructuring-bind (defpackage name &rest options)
      form
    (declare (ignore defpackage))
    (second (find :documentation options :key #'first))))


(defmethod extract-form-documentation ((fk (eql 'defmethod)) (form cons))
  (destructuring-bind (defmethod name &rest rest-method)
      form
    (declare (ignore defmethod))
    (extricate-doc-string (subseq rest-method
                                  (1+ (position-if #'consp rest-method))))))


(defmethod extract-form-documentation ((fk (eql 'defstruct)) (form cons))
  (destructuring-bind (defstruct name-and-options &rest doc-string-and-slots)
      form
    (declare (ignore defstruct))
    (when (stringp (first doc-string-and-slots))
      (first doc-string-and-slots))))


(defmethod extract-form-documentation ((fk (eql 'eval-when)) (form cons))
  (destructuring-bind (eval-when times &rest forms)
      form
    (declare (ignore eval-when))
    (mapcar #'form-documentation forms)))


(defun extract-symbol-form-documentation (form)
  (fourth form))


(defmethod extract-form-documentation ((fk (eql 'defparameter)) (form cons))
  (extract-symbol-form-documentation form))


(defmethod extract-form-documentation ((fk (eql 'defvar)) (form cons))
  (extract-symbol-form-documentation form))


(defmethod extract-form-documentation ((fk (eql 'defconstant)) (form cons))
  (extract-symbol-form-documentation form))


(define-documentation-extractor (defsetf access-fn &rest form)
  (if (symbolp (first form))
      (second form)
      (extricate-doc-string (nthcdr 2 form))))


(define-documentation-extractor (define-modify-macro name ll function
                                  &optional doc-string)
  (declare (ignore ll))
  doc-string)


(define-documentation-extractor (define-method-combination name &rest rest-dmc)
  (if (keywordp (first form)) ; Short form.
      (getf rest-dmc :documentation)
      (let ((options-and-forms (copy-list (nthcdr 2 rest-dmc)))) ; Paranoid!
        (when (eq :arguments (first options-and-forms))
          (pop options-and-forms))
        (when (eq :generic-function (first options-and-forms))
          (pop options-and-forms))
        (extricate-doc-string options-and-forms))))
  
;;;; end of file -- extract-doc.lisp --

;;;; -*- Mode: Lisp -*-

;;;; doc-formats.lisp --
;;;; HELambdaP produces documentation in a number of different
;;;; formats.
;;;; Some of these formats are small variations of other ones.  Here
;;;; we define them along a number of ancillary facilities to ease
;;;; programming.

;;;; See file COPYING for copyright and licensing information.


(in-package "HELAMBDAP")


;;;---------------------------------------------------------------------------
;;; Global definitions.
;;;
;;; First we define a set of structures representing formats; each of
;;; these structures will essentially be a singleton.  Next we
;;; define ways to easily retreive these singletons, which are what is
;;; eventually passed around in the code.

(defstruct (documentation-format
            (:constructor nil)
            (:conc-name format-))
  "The Documentation Format Structure.

The top of the 'doc formats' types.  This structure cannot be
instantiated."
  (tag nil :type symbol)
  (key :none :type keyword)
  )


(defmethod print-object ((df documentation-format) stream)
  (print-unreadable-object (df stream :type t :identity t)
    (let ((*print-escape* nil))
      (format stream "~A ~S"
              (format-tag df)
              (format-key df)))))


(defparameter *known-documentation-formats* ())


(defun register-doc-format (df)
  (pushnew df *known-documentation-formats*))


(defun list-doc-formats ()
  (copy-list *known-documentation-formats*))


(defun list-doc-format-tags ()
  (pairlis (mapcar #'format-tag *known-documentation-formats*)
           (mapcar #'format-key *known-documentation-formats*)))


(defun find-doc-format (id)
  (find-if #'(lambda (df)
               (or (eq id (format-key df))
                   (eq id (format-tag df))))
           *known-documentation-formats*))


(defun output-format (x) ; This function is a bit redundant.
  "Finds the DOCUMENTATION-FORMAT given a 'user' tag.

The result is an istance (singleton) of a DOCUMENTATION-FORMAT
structure, or NIL if none is found."
  (declare (type symbol x))
  (find-doc-format x))


(defgeneric output-format-tag (x)
  (:documentation
   "Translates from a 'user' tag to the canonical internal one.

The internal tags are symbols in the HELambdaP implementation packages
(which may, or may not be exported).

Examples:

(output-format-tag :html) ==> HLP:HTML

"
)


  #|
  (:method ((x (eql :html))) 'html)
  (:method ((x (eql 'html))) 'html)

  (:method ((x (eql :html5))) 'html5)
  (:method ((x (eql 'html5))) 'html5)

  (:method ((x (eql :texinfo))) 'texinfo)
  (:method ((x (eql 'texinfo))) 'texinfo)
  |#

  (:method ((x t))
    (warn "HELampdaP: unknown format  ~S; HTML assumed." x)
    (call-next-method :html))
)


#|
;;; texinfo-format
;;; --------------

(defstruct (texinfo-format
            (:include documentation-format
             (tag 'texinfo)
             (key :texinfo))
            (:constructor %texinfo ))
  "The Texinfo Documentation Format.")


(defparameter *texinfo-format* (%texinfo))

(eval-when (:load-toplevel :execute)
  (register-doc-format *texinfo-format*))

(defmethod output-format-tag ((x (eql :texinfo))) 'texinfo)
(defmethod output-format-tag ((x (eql 'texinfo))) 'texinfo)
|#


;;; define-doc-format

(defmacro define-doc-format (name tag key
                                  &key
                                  (derives-from 'documentation-format)
                                  (documentation
                                   (format nil
                                           "The ~A Documentation Format"
                                           name)))
  "Defines a Documentation Format.

This macro generates the appropriate declarations and definitions that
introduce a documentation format in the HELambdaP system.  In
particular it registers the new format in the internal tables and
makes the symbolic tags and keys used to retrieve the format known to
the system.

See Also:

OUTPUT-FORMAT, OUTPUT-FORMAT-TAG
"
  (let ((cons-name (gensym (format nil "%~A" name)))
        (parm-name (intern (format nil "*~A*" name)))
        )
    `(eval-when (:load-toplevel :compile-toplevel :execute)
       (defstruct (,name
                   (:include ,derives-from
                    (tag ',tag)
                    (key ,key))
                   (:constructor ,cons-name))
         ,documentation)
      
       (defparameter ,parm-name (,cons-name))

       (defmethod output-format-tag ((x (eql ',key))) ',tag)
       (defmethod output-format-tag ((x (eql ',tag))) ',tag)

       (register-doc-format ,parm-name)

       ',name)))


;;;; Known formats in file 'known-doc-formats.lisp'.

;;;; end of file -- doc-formats.lisp --

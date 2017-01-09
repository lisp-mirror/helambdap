;;;; -*- Mode: Lisp -*-

;;;; documentation-production.lisp --
;;;;
;;;; See file COPYING for copyright and licensing information.

(in-package "HELAMBDAP")


;;;;===========================================================================
;;;; Protocol.

(defgeneric produce-documentation (format element out doc-bits
                                          &key documentation-title
                                          &allow-other-keys)
  (:documentation "Produces documentation for ELEMENT according to FORMAT.")
  )


(defgeneric build-documentation (for-what
                                 format
                                 &key
                                 layout
                                 source
                                 destination
                                 &allow-other-keys)
  (:documentation
   "Produces the documentation according to a number of parameters"))


(defgeneric build-doc-skeleton (for-what
                                format
                                &key
                                layout
                                source
                                destination
                                &allow-other-keys)
  (:documentation "Produces a skeleton for the documentation.

The skeleton consists of a number of 'judiciously' editable files that
can be used as building blocks for the final documentation."))


#|
(declaim (ftype (function (T &key
			     documentation-title
			     format
			     layout
			     source
			     destination
			     supersede
			     only-documented
			     only-exported
			     everything
			     exclude-directories
			     exclude-files
			     &allow-other-keys)
			  T)
		document))
|#


;;;;===========================================================================
;;;; Implementation.


(defun document (for-what
                 &key
                 (documentation-title)
                 (format 'html)
                 (layout *default-documentation-structure*)
                 (source #P"")
                 (destination 
                  (make-pathname :directory '(:relative "docs" "html")))

                 ((:supersede *supersede-documentation*)
                  *supersede-documentation*)
                 ((:only-documented *only-documented*) *only-documented*)
                 ((:only-exported *only-exported*) *only-exported*)
                 ((:everything *everything*) *everything*)

                 ((:exclude-directories *exclude-directories*)
                  *exclude-directories*)
                 ((:exclude-files *exclude-files*)
                  *exclude-files*)

                 (special-methods-defs-files ())
                 (clear-documentation-db :before)
                 
                 &allow-other-keys
                 )
  "Produces the documentation for something.

The function is a wrapper for BUILD-DOCUMENTATION defaulting a few
parameters, in particular the output FORMAT (which defaults to HTML).

Arguments and Values:

FOR-WHAT --- what to document; can be a pathname or a 'system'.
DOCUMENTATION-TITLE --- a STRING which will appear as the documentation title.
FORMAT --- a SYMBOL designating the desired kind of output.
DESTINATION --- a (directory) PATHANME where the documentation will be produced.
ONLY-DOCUMENTED --- a BOOLEAN: whether to produce documentation only for documented items.
ONLY-EXPORTED --- a BOOLEAN: whether to produce documentation only for 'exported' items.
EVERYTHING --- a BOOLEAN: whether to produce documentation for everything, no matter what.
EXCLUDE-DIRECTORIES --- a LIST of directory pathnames not to be considered.
EXCLUDE-FILES --- a list of FILES not to be considered.
SPECIAL-METHODS-DEFS-FILES --- a list of FILES to be LOADed before running the parsers. 
CLEAR-DOCUMENTATION-DB --- a KEYWORD stating if and when the documentation db should be cleared.

Notes:

At the time of this writing, ASDF and MK-DEFSYSTEM are supported.

The arguments SOURCE and SUPERSEDE are, at the time of this writing, 
effectively ignored.

The argument SPECIAL-METHODS-DEFS-FILES is a list of Common Lisp files
that will be loaded before running the documentation parsers; it is
assumed that these files will contain mostly
DEFINE-DOCUMENTATION-EXTRACTOR and EXTRACT-NAMED-FORM-DOCUMENTATION
definitions.  The loading of these files is wrapped in an
IGNORE-ERRORS form: failure to load one of them will not completely
hamper the documentation procedure.
"
  
  (declare (special *supersede-documentation* ; SBCL may be right here.
                    *only-documented*
                    *only-exported*
                    *everything*
                    *exclude-directories*
                    *exclude-files*))
  
  (when (and *everything* (or *only-documented* *only-exported*))
    (warn "EVERYTHNG is currently true: HELAMBDAP will produce all ~@
           documentation files regardless of the value of other 'limiting' ~@
           variables."))

  (ignore-errors
    (dolist (smdf special-methods-defs-files)
      (load smdf)))

  (when (member clear-documentation-db '(t :before))
    (clear-doc-bits-db))

  (unwind-protect
      (build-documentation for-what
                           format
                           :layout layout
                           :source source
                           :destination destination
                           :documentation-title documentation-title)

    (when (member clear-documentation-db '(t :after))
      (clear-doc-bits-db))))


;;;;---------------------------------------------------------------------------
;;;; Build documentation implementations.

(defmethod build-documentation ((p pathname)
                                (format (eql 'html))
                                &key
                                (documentation-title)
                                (layout *default-documentation-structure*)
                                (source #P".")
                                (destination
                                 (make-pathname :directory '(:relative "docs" "html")))
                                &allow-other-keys
                                )
  (declare (ignore source))

  "Builds the documentation given a PATHNAME.

The pathname P can either denote a file or a folder.  If i is a folder
then it is recursively traversed.

See Also:

collect-documentation.
"
  (when documentation-title
    (setf (property layout :documentation-title) documentation-title))

  (let ((doc-bits (collect-documentation p)))
    (produce-documentation format
                           layout
                           destination
                           doc-bits
                           :documentation-title documentation-title))
  )


(defmethod build-documentation ((p pathname)
                                (format (eql 'html5))
                                &key
                                (documentation-title)
                                (layout *html5-documentation-structure*)
                                (source #P".")
                                (destination
                                 (make-pathname :directory '(:relative "docs" "html")))
                                &allow-other-keys
                                )
  (declare (ignore source))
  (when documentation-title
    (setf (property layout :documentation-title) documentation-title))

  (let ((doc-bits (collect-documentation p)))
    (produce-documentation format
                           layout
                           destination
                           doc-bits
                           :documentation-title documentation-title))
  )


#+asdf
(defmethod build-documentation ((s asdf:system)
                                (format (eql 'html))
                                &key
                                (documentation-title)
                                (layout *default-documentation-structure*)
                                (source #P".")
                                (destination
                                 (make-pathname :directory '(:relative "docs" "html")))
                                &allow-other-keys
                                )
  "Builds the documentation for a ASDF system."
  (declare (ignore source))

  (when documentation-title
    (setf (property layout :documentation-title) documentation-title))

  (let ((doc-bits (collect-documentation (asdf:files-in-system s))))
    (produce-documentation format
                           layout
                           destination
                           doc-bits
                           :documentation-title documentation-title))
  )


#+mk-defsystem
(defmethod build-documentation ((s mk::component)
                                (format (eql 'html))
                                &key
                                (documentation-title)
                                (layout *default-documentation-structure*)
                                (source #P".")
                                (destination
                                 (make-pathname :directory '(:relative "docs" "html")))
                                &allow-other-keys
                                )
  "Builds the documentation for a MK-DEFSYSTEM system."
  (declare (ignore source))

  (assert (eq :defsystem (mk::component-type s)))

  (when documentation-title
    (setf (property layout :documentation-title) documentation-title))

  (let ((doc-bits (collect-documentation (mapcar #'pathname (mk:files-in-system s)))))
    (produce-documentation format
                           layout
                           destination
                           doc-bits
                           :documentation-title documentation-title))
  )
  

;;;; end of file -- documentation-production.lisp --

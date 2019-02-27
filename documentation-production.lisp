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

(defvar *default-docs-destination-html*
  (make-pathname :directory '(:relative "docs" "html")))


(defvar *default-docs-destination-html5*
  (make-pathname :directory '(:relative "docs" "html5")))


(defvar *default-docs-destination-texinfo*
  (make-pathname :directory '(:relative "docs" "texinfo")))


(defvar *default-docs-destination-temp*
  (make-pathname :directory '(:relative "temp" "docs" "xhtmlx")))


(defgeneric select-doc-destination (format)
  (:method ((format (eql 'html))) *default-docs-destination-html*)
  (:method ((format (eql :html))) *default-docs-destination-html*)
  (:method ((format (eql 'html5))) *default-docs-destination-html5*)
  (:method ((format (eql :html5))) *default-docs-destination-html5*)
  )


(defun document (for-what
                 &key
                 (documentation-title "HE&Lambda;P Untitled Documentation")
                 (format :html)
                 (layout (select-doc-structure format))
                 (source #P"")
                 (destination (select-doc-destination format))

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
                           :destination (pathname destination)
                           :documentation-title documentation-title)

    (when (member clear-documentation-db '(t :after))
      (clear-doc-bits-db))))


;;;;---------------------------------------------------------------------------
;;;; Build documentation implementations.

(defmethod build-documentation ((for-what t)
                                (format t)
                                &key
                                &allow-other-keys)
  (error "HELAMBDAP: cannot produce documentation for a ~S (~S) and/or format ~S.
          The first argument can be a system object or a pathname, the second
          can be one of, for the time being, ~S."
         (type-of for-what)
         for-what
         format
         (list :html 'html)))


;;; The :before methods are used to do some common preprocessing
;;; (factored out from previous versions of the code).

(defmethod build-documentation :before ((for-what t)
                                        (format t)
                                        &key
                                        (documentation-title)
                                        (layout *default-documentation-structure*)
                                        &allow-other-keys
                                        )
  (when documentation-title
    (setf (property layout :documentation-title) documentation-title))
  )


(defmethod build-documentation :before ((for-what t)
                                        (format (eql 'html5))
                                        &key
                                        &allow-other-keys
                                        )
  (warn "HELambdaP: note that, due to the presence of Javascript and CORS, ~@
         the pages generated with the HTML5 format must be served by an HTTP server ~@
         (use your favourite one).")
  )


(defmethod build-documentation ((p pathname)
                                (format t)
                                &key
                                (documentation-title)
                                (layout *default-documentation-structure*)
                                (destination
                                 (make-pathname :directory '(:relative "docs" "html")))
                                &allow-other-keys
                                )
  "Builds the documentation given a PATHNAME in a given FORMAT.

The pathname P can either denote a file or a folder.  If it is a folder
then it is recursively traversed.

See Also:

COLLECT-DOCUMENTATION.
"

  (let ((doc-bits (collect-documentation p)))
    (produce-documentation format
                           layout
                           destination
                           doc-bits
                           :documentation-title documentation-title))
  )


;;; The :around method is used to proper "relativize" the excluded
;;; files and directories to the pathname P.

(defmethod build-documentation :around ((p pathname)
                                        (format t)
                                        &key
                                        &allow-other-keys
                                        )
  (declare (special *exclude-directories* *exclude-files*))
  (let ((*exclude-directories*
         (mapcar #'(lambda (ed) (merge-pathnames ed p)) *exclude-directories*))
        (*exclude-files*
         (mapcar #'(lambda (ef) (merge-pathnames ef p)) *exclude-files*))
        )
    (declare (special *exclude-directories* *exclude-files*))
    (call-next-method)
    ))


;;; "defsystem" methods

#+asdf
(defmethod build-documentation ((s asdf:system)
                                (format t)
                                &key
                                (documentation-title)
                                (layout *default-documentation-structure*)
                                (destination
                                 (make-pathname :directory '(:relative "docs" "html")))
                                &allow-other-keys
                                )
  "Builds the documentation for a ASDF system in a given FORMAT."

  (let ((doc-bits (collect-documentation (asdf:files-in-system s))))
    (produce-documentation format
                           layout
                           (pathname destination)
                           doc-bits
                           :documentation-title documentation-title))
  )


#+asdf
(defmethod build-documentation :around ((s asdf:system)
                                        (format t)
                                        &key
                                        &allow-other-keys
                                        )
  (declare (special *exclude-directories* *exclude-files*))
  (let* ((p (asdf:component-pathname s))
         (*exclude-directories*
         (mapcar #'(lambda (ed) (merge-pathnames ed p)) *exclude-directories*))
        (*exclude-files*
         (mapcar #'(lambda (ef) (merge-pathnames ef p)) *exclude-files*))
        )
    (declare (special *exclude-directories* *exclude-files*))
    (call-next-method)
    ))


#+mk-defsystem
(defmethod build-documentation ((s mk::component)
                                (format t)
                                &key
                                (documentation-title)
                                (layout *default-documentation-structure*)
                                (destination
                                 (make-pathname :directory '(:relative "docs" "html")))
                                &allow-other-keys
                                )
  "Builds the documentation for a MK-DEFSYSTEM system in a given FORMAT."

  (assert (eq :defsystem (mk::component-type s)))

  (let ((doc-bits (collect-documentation (mapcar #'pathname (mk:files-in-system s)))))
    (produce-documentation format
                           layout
                           (pathname destination)
                           doc-bits
                           :documentation-title documentation-title))
  )


#+mk-defsystem
(defmethod build-documentation :around ((s mk::component)
                                        (format t)
                                        &key
                                        &allow-other-keys
                                        )
  (declare (special *exclude-directories* *exclude-files*))

  (assert (eq :defsystem (mk::component-type s)))

  (let* ((p (directory-pathname (mk::component-full-pathname s :source)))
         (*exclude-directories*
         (mapcar #'(lambda (ed) (merge-pathnames ed p)) *exclude-directories*))
        (*exclude-files*
         (mapcar #'(lambda (ef) (merge-pathnames ef p)) *exclude-files*))
        )
    (declare (special *exclude-directories* *exclude-files*))
    (call-next-method)
    ))


;;;; end of file -- documentation-production.lisp --

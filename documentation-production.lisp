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

#|
(defgeneric select-doc-destination (format)
  (:method ((format (eql 'html))) *default-docs-destination-html*)
  (:method ((format (eql :html))) *default-docs-destination-html*)
  (:method ((format (eql 'html5))) *default-docs-destination-html5*)
  (:method ((format (eql :html5))) *default-docs-destination-html5*)
  )
|#


(defgeneric select-doc-source (for-what)
  (:documentation
   "Returns the source (a folder pathname) for the documentation.")
  
  (:method ((for-what t)) #P".")
  (:method ((for-what pathname)) for-what))


(defgeneric select-doc-destination (for-what source format)
  (:documentation
   "Returns a default destination (a folder) for the documentation.

The default destination is relative to SOURCE, depending on the final
FORMAT desired.  If FOR-WHAT is a pathname then SOURCE must be EQUAL
to it, otherwise a warning is issued.")
  )


(defmethod select-doc-destination ((for-what t)
				   (source pathname)
				   (format (eql :html)))
  (select-doc-destination for-what source 'html))


(defmethod select-doc-destination ((for-what t)
				   (source pathname)
				   (format (eql 'html)))
  (merge-pathnames *default-docs-destination-html* source))


(defmethod select-doc-destination :around ((for-what pathname)
					   (source pathname)
					   (format symbol))
  (unless (equal for-what source)
    (warn "HELambdaP: the folder to document and its presumed source differ. ~@
           Folder : '~A' ~@
           Source : '~A'"
	  for-what
	  source))
  (call-next-method))


(defmethod select-doc-destination ((for-what t)
				   (source pathname)
				   (format (eql :html5)))
  (select-doc-destination for-what source 'html5))


(defmethod select-doc-destination ((for-what t)
				   (source pathname)
				   (format (eql 'html5)))
  (merge-pathnames *default-docs-destination-html5* source))



(defun document (for-what
                 &key
                 (documentation-title "HE&Lambda;P Untitled Documentation")
                 (format :html)
                 (layout (select-doc-structure format))
                 (source (select-doc-source for-what))
                 (destination (select-doc-destination for-what source format))

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
parameters, in particular the output FORMAT.  The current default for
FORMAT is :HTML, and experimental :HTML5 is available and it can be
used to produce the documentation in HTML5 format.

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
                    *exclude-files*)
	   
	   (type pathname source destination)
	   )
  
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
                                        (layout (select-doc-structure format))
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
         the pages generated with the HTML5 format must be served by ~@
         an HTTP server (use your favourite one).")
  )


(defmethod build-documentation ((p pathname)
                                (format t)
                                &key
                                (documentation-title)
                                (layout (select-doc-structure format))
				(source (select-doc-source p))
                                (destination
				 (select-doc-destination p source format))
                                &allow-other-keys
                                )
  "Builds the documentation given a pathname P in a given FORMAT.

The pathname P can either denote a file or a folder.  If it is a folder
then it is recursively traversed.  The documentation is produced in
the DESTINATION directory/folder, which defaults to 'docs/html'
relative to the pathname P.

See Also:

COLLECT-DOCUMENTATION.
"
  (declare (type pathname source destination))
  
  (let ((doc-bits (collect-documentation source)))
    (produce-documentation format
                           layout
                           destination
                           doc-bits
                           :documentation-title documentation-title))
  )


;;; The :around method is used to proper "relativize" the excluded
;;; files and directories either to the pathname P (which should be
;;; EQUAL to SOURCE, unmentioned here).

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
                                (layout (select-doc-structure format))
				(source (select-doc-source s))
                                (destination
				 (select-doc-destination s source format))
                                &allow-other-keys
                                )
  "Builds the documentation for a ASDF system in a given FORMAT."

  (declare (type pathname source destination))

  (let ((doc-bits (collect-documentation (asdf:files-in-system s))))
    (produce-documentation format
                           layout
                           destination
                           doc-bits
                           :documentation-title documentation-title))
  )

#+asdf
(defmethod select-doc-source ((s asdf:system))
  (asdf:component-pathname s))


#+asdf
(defmethod build-documentation :around ((s asdf:system)
                                        (format t)
                                        &key
                                        &allow-other-keys
                                        )
  (declare (special *exclude-directories* *exclude-files*))
  (let* ((p (select-doc-source s))
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
				(layout (select-doc-structure format))
				(source (select-doc-source s))
                                (destination
				 (select-doc-destination s source format))
                                &allow-other-keys
                                )
  "Builds the documentation for a MK-DEFSYSTEM system in a given FORMAT."

  (declare (type pathname source destination))
  
  (assert (eq :defsystem (mk::component-type s)))

  (let ((doc-bits
	 (collect-documentation
	  (mapcar #'pathname (mk:files-in-system s))))
	)
    (produce-documentation format
                           layout
                           destination
                           doc-bits
                           :documentation-title documentation-title))
  )


#+mk-defsystem
(defmethod select-doc-source ((s mk::component))
  (directory-pathname (mk::component-full-pathname s :source)))


#+mk-defsystem
(defmethod build-documentation :around ((s mk::component)
                                        (format t)
                                        &key
                                        &allow-other-keys
                                        )
  (declare (special *exclude-directories* *exclude-files*))

  (assert (eq :defsystem (mk::component-type s)))

  (let* ((p (select-doc-source s))
         (*exclude-directories*
         (mapcar #'(lambda (ed) (merge-pathnames ed p)) *exclude-directories*))
        (*exclude-files*
         (mapcar #'(lambda (ef) (merge-pathnames ef p)) *exclude-files*))
        )
    (declare (special *exclude-directories* *exclude-files*))
    (call-next-method)
    ))


;;;; end of file -- documentation-production.lisp --

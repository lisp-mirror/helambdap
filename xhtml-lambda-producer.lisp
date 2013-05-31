;;;; -*- Mode: Lisp -*-

;;;; xhtml-producer.lisp --
;;;; Make a file out of a DOCUMENTATION-STRUCTURE and a set (list) of
;;;; DOC-BITs, using a (X)HTML outout format.

(in-package "HELAMBDAP")
;(use-package "XHTMLAMBDA")

;;;;===========================================================================
;;;; Prologue.

;;;; The (X)HTML producer makes essentially two kinds framesets: the "prose"
;;;; and "help" ones and the "dictionary" one.
;;;;
;;;; The "prose" and "help" ones are the "index", "downloads",
;;;; "mailing-lists"/"contact" and "links" framesets, which have the
;;;; following layout:
#|

+=MAIN========================================+
[+-HEADER------------------------------------+]
[|                                           |]
[+-------------------------------------------+]
[+=DOC-AREA==================================+]
[[+-NAV-++-INFO-AREA------------------------+]]
[[|     ||                                  |]]
[[|     ||                                  |]]
[[|     ||                                  |]]
[[|	||      			    |]]
[[|	||      			    |]]
[[|	||      			    |]]
[[+-----++----------------------------------+]]
[+===========================================+]
[+-FOOTER------------------------------------+]
[|                                           |]
[+-------------------------------------------+]
+=============================================+

MAIN and DOC-AREA (with other names in the code, depending on the
DOCUMENTATION-STRUCTURE) are HTML FRAMESETS, HEADER, NAV, INFO-AREA
and FOOTER are HTML FRAMES.  There should also be a Sidebar, but you
get the idea.

Each FRAMESET and FRAME is contained in a separate file.
|#
;;;; The "dictionary" frameset is essentially the same, but it
;;;; requires a more complex navigation scheme, reflected in the shape
;;;; of the NAV frame.
;;;;
;;;; The question is: what should we navigate through?  Well, let's
;;;; take our inspiration from doxygen (www.doxygen.org) and let's
;;;; tune it to CL, with the caveat that, for the time being, I will not
;;;; use "rendered" source files.
;;;;
;;;; There are, IMHO, a few "axis" or "indices" we'd like to see at
;;;; the top level in the navigation area.
;;;; o	Packages
;;;; o	Systems
;;;; o	Files (possibly ordered by directory)
;;;;
;;;; Systems could be seen as "more top-level", but, for the time
;;;; being, I'll just use them to narrow down on the list of files.
;;;; The structure of the "dictionary" navigation will therefore be
#|

+=NAV=====================+
[+-MENU------------------+]
[| Systems Files Packages|]
[|                       |]
[|                       |]
[|                       |]
[+-----------------------+]
[+-MENULIST--------------+]
[|                       |]
[|                       |]
[|                       |]
[|                       |]
[|                       |]
[|                       |]
[|                       |]
[|                       |]
[|                       |]
[|                       |]
[|                       |]
[|                       |]
[+-----------------------+]
+=========================+

|#




;;;;===========================================================================
;;;; Protocol.


(defgeneric produce-frame (format element out)
  )


(defgeneric produce-navigation-frame (format
                                      element
                                      frameset-stream
                                      where
                                      doc-bits
                                      doc-title)
  )


(defgeneric produce-navigation-file (frameset
                                     nav-element
                                     nav-pathname
                                     doc-bits
                                     documentation-title))


(defgeneric produce-header-frame (format
                                  frameset
                                  frameset-stream
                                  where
                                  doc-bits
                                  doc-title)
  )


(defgeneric produce-header-file (frameset
                                 header-pathname
                                 documentatio-title))


(defgeneric produce-footer-frame (format
                                  frameset
                                  frameset-stream
                                  where
                                  doc-bits
                                  doc-title)
  )

(defgeneric produce-footer-file (frameset
                                 footer-pathname
                                 documentatio-title))


(defparameter *xhtml-indent* 4) ; 4 is the actual initial value.


;;;;===========================================================================
;;;; Implementation.

(defmethod produce-documentation ((format (eql 'html))
                                  (structure documentation-structure)
                                  (where pathname)
                                  doc-bits
                                  &key
                                  (documentation-title)
                                  &allow-other-keys
                                  )
  (declare (ignorable documentation-title))
  (ensure-directories-exist where)
  (dolist (c (documentation-structure-structure structure))
    (produce-documentation 'html c where doc-bits
                           :documentation-title documentation-title)
    ))


(defmethod produce-documentation ((format (eql 'html))
                                  (structure style-file)
                                  (where pathname)
                                  doc-bits
                                  &key
                                  (documentation-title)
                                  &allow-other-keys
                                  )
  (declare (ignorable documentation-title))
  (let ((doc-directory where)
        (sfn (style-file-name structure))
        )
    (cl-fad:copy-file sfn
                      (make-pathname :directory (pathname-directory doc-directory)
                                     :name (pathname-name sfn)
                                     :type (pathname-type sfn))
                      :overwrite t)
    ))


(defmethod produce-documentation ((format (eql 'html))
                                  (structure doc-file)
                                  (where pathname)
                                  doc-bits
                                  &key
                                  (documentation-title)
                                  &allow-other-keys
                                  )
  (declare (ignorable documentation-title))
  (let* ((doc-directory where)
         (dfn (doc-file-name structure))
         (destination-path
          (make-pathname :directory (pathname-directory doc-directory)
                         :name (pathname-name dfn)
                         :type (pathname-type dfn)))
         )
    (cl-fad:copy-file dfn destination-path :overwrite nil)
    ))


(defmethod produce-documentation ((format (eql 'html))
                                  (structure doc-file)
                                  (where file-stream)
                                  doc-bits
                                  &key
                                  (documentation-title)
                                  &allow-other-keys
                                  )
  (declare (ignorable documentation-title))
  (let* ((doc-directory where)
         (dfn (doc-file-name structure))
         (destination-path
          (make-pathname :directory (pathname-directory doc-directory)
                         :name (pathname-name dfn)
                         :type (or (pathname-type dfn) *default-html-extension*)))
         )
    (if (probe-file dfn)
        (cl-fad:copy-file dfn destination-path :overwrite nil)
        (warn "File ~S from ~S cannot be copied to ~S."
              dfn
              structure
              destination-path)
        )))


#|
(defmethod produce-documentation ((format (eql 'html))
                                  (fss framesets)
                                  (where pathname)
                                  doc-bits)
  (let ((ds (element-doc-structure fss))
        (fs-order -1)
        )
    (dolist (fs (framesets-list fss))
      (let ((e (gethash fs (structure-table ds))))
        (when (frameset-p e)
          (setf (frameset-order e) (incf fs-order)))))))
|#


(defmethod produce-documentation ((format (eql 'html))
                                  (fss framesets)
                                  (where pathname)
                                  doc-bits
                                  &key
                                  (documentation-title)
                                  &allow-other-keys
                                  )
  (declare (ignorable documentation-title))
  (dolist (fs (framesets-list fss))
    (produce-documentation 'html fs where doc-bits
                           :documentation-title documentation-title)))


(defmethod produce-documentation ((format (eql 'html))
                                  (structure frameset)
                                  (where pathname)
                                  doc-bits
                                  &key
                                  (documentation-title)
                                  &allow-other-keys
                                  )
  (declare (ignorable documentation-title))
  (let* ((fs-location (frameset-location structure))
         (where (if fs-location
                    (merge-pathnames fs-location where)
                    where))

         (fs-name (frameset-name structure))
         (fs-pathname (make-pathname :name fs-name
                                     :type *default-html-extension*
                                     :defaults where))
         (fs-header (frameset-header structure))
         (fs-content (frameset-content structure))
         (fs-navigation (frameset-navigation structure))
         (fs-footer (frameset-footer structure))
         )
    (declare (ignore fs-footer fs-navigation fs-header))

    (ensure-directories-exist where)

    (with-open-file (fs-file fs-pathname
                             :direction :output
                             :if-does-not-exist :create
                             :if-exists :supersede)
      (<:with-html-syntax (fs-file :print-pretty t)
          (<:htmlize
           (<:document 
            (<:comment fs-name)
            (string #\Newline)
            
            +doctype-frameset-control-string+
            (string #\Newline)
            (<:html

             (<:head
              (<:title fs-name)
              (<:link :rel "stylesheet" :href (frameset-style structure)))
             
             ((<:frameset :rows "65px,*,65px" #| :border 0 |# :noresize "noresize")
              ;; HEADER ROW.
              (<:comment "HEADER ROW")
              (produce-header-frame 'html
                                    structure
                                    fs-file
                                    where
                                    doc-bits
                                    documentation-title)

              ;; NAVIGATION/CONTENT/SIDEBAR ROW.
              (<:comment "NAVIGATION/CONTENT/SIDEBAR ROW")
              (
               ;; (<:frameset :cols "*,*" :border 0)
               ;; (<:frameset :cols "150px,*" :border 0)
               ;; (<:frameset :cols "20%,80%" #| :border 0 |#)
               (<:frameset :cols "25%,75%" #| :border 0 |#)

               (<:comment "NAVIGATION FRAME")
               (produce-navigation-frame 'html
                                         structure
                                         fs-file
                                         where
                                         doc-bits
                                         documentation-title)
                         
               (<:comment "CONTENT " fs-content)
               (if fs-content
                   (progn
                     (produce-documentation format
                                            fs-content
                                            fs-file
                                            doc-bits)
                     (produce-frame format fs-content fs-file)
                     )
                   #|
                   (<:frame (:src (base-name
                                   (make-pathname
                                    :type *default-html-extension*
                                    :name (element-name structure)))))
                   |#
                   (<:frame (:name (format nil "~A_frame" (element-name structure))
                             :frameborder 0))
                   )
               )

              ;; FOOTER ROW.
              (<:comment () "FOOTER ROW")
              (produce-footer-frame 'html
                                    structure
                                    fs-file
                                    where
                                    doc-bits
                                    documentation-title)
              ;; (<:frame (:src (base-name fs-footer)))
              )
              
              (<:noframes
               (<:h2 "Frame Alert")
               (<:p
                "This document is designed to be viewed using the frames feature."
                "If you see this message, you are using a non-frame-capable web client."
                <:br
                "Link to "
                ((<:a :href "overview-summary.html") "Non-frame version.")))
              )
            (<:comment 
             (format nil "end of file : ~A.htm" fs-name))
            )
           :syntax :compact))
      )))


(defmethod produce-documentation ((format (eql 'html))
                                  (element frame)
                                  (where stream)
                                  doc-bits
                                  &key
                                  documentation-title
                                  &allow-other-keys)
  (declare (ignorable documentation-title))
  (<:frame (:src (frame-source element) :name (frame-name element) :frameborder 0)))
                                  

(defmethod produce-frame ((format (eql 'html))
                          (element doc-file)
                          (where stream)
                          )
  (<:frame (:src (namestring (file-pathname element)) :frameborder 0)
           (<:comment () "FRAME " (element-name element))))


(defmethod produce-frame ((format (eql 'html))
                          (element file-set)
                          (where stream)
                          )
  (<:frame (:src (concatenate 'string (file-set-name element) "." *default-html-extension*)
            :name (concatenate 'string (file-set-name element) "_frame")
            :frameborder 0
            )
           ;; (format nil "~&~%<!-- FRAME DOC FILE-SET ~S -->~2%" (element-name structure))
           ))



(defmethod produce-header-frame ((format (eql 'html))
                                 (fs frameset)
                                 (fs-file stream)
                                 (where pathname)
                                 doc-bits
                                 documentation-title
                                 )
  (let ((header (frameset-header fs)))
    (unless (or (null header) (string= header ""))
      (let ((header-pathname
             (merge-pathnames
              (merge-pathnames (frameset-header-name fs)
                               (make-pathname :type *default-html-extension*))
              where))
            )
             
        (unless (and (probe-file header-pathname)
                     (not *supersede-documentation*))
          (produce-header-file fs header-pathname documentation-title))

        (<:frame (:src (base-name header-pathname)
                  :frameborder 0
                  ))
        ))))


(defmethod produce-navigation-frame ((format (eql 'html))
                                     (element frameset)
                                     (out stream)
                                     (where pathname)
                                     doc-bits
                                     documentation-title
                                     )
  (let ((nav (frameset-navigation element)))
    (unless (or (null nav) (string= nav ""))
      (let ((nav-pathname
             (merge-pathnames
              (merge-pathnames (frameset-navigation-name element)
                               (make-pathname :type *default-html-extension*))
              where))
            )
        (declare (type pathname nav-pathname))
        (unless (and (probe-file nav-pathname)
                     (not *supersede-documentation*))
          (produce-navigation-file element
                                   (frameset-content element)
                                   nav-pathname
                                   doc-bits
                                   documentation-title))
        (<:frame (:src (base-name nav-pathname)
                  ;; :marginheight "5"
                  :frameborder "0"
                  ))
        ))))


(defmethod produce-footer-frame ((format (eql 'html))
                                 (fs frameset)
                                 (fs-file stream)
                                 (where pathname)
                                 doc-bits
                                 documentation-title
                                 )
  (let ((footer (frameset-footer fs)))
    (unless (or (null footer) (string= footer ""))
      (let ((footer-pathname
             (merge-pathnames
              (merge-pathnames (frameset-footer-name fs)
                               (make-pathname :type *default-html-extension*))
              where))
            )
        (unless (and (probe-file footer-pathname)
                     (not *supersede-documentation*))
          (produce-footer-file fs footer-pathname documentation-title))
        (<:frame (:src (base-name footer-pathname)
                  :frameborder 0
                  ))
        ))))


(defmethod produce-documentation ((format (eql 'html))
                                  (structure file-set)
                                  (where pathname)
                                  doc-bits
                                  &key
                                  documentation-title
                                  &allow-other-keys)
  (declare (ignorable documentation-title))
  (<:frame (:src (element-name structure)
            :frameborder 0
            )))


(defmethod produce-documentation ((format (eql 'html))
                                  (structure file-set)
                                  (where file-stream)
                                  doc-bits
                                  &key
                                  (documentation-title)
                                  &allow-other-keys
                                  )
  (declare (ignorable documentation-title))

  ;; Produce the documentation for the doc-bits.
  (dolist (doc-bit doc-bits)
    (let ((doc-bit-pathname
           (quote-wild-in-pathname-name
            (make-doc-bit-pathname doc-bit
                                   *default-html-extension*
                                   (pathname where))))
          )
      (with-open-file (doc-bit-stream doc-bit-pathname
                                      :direction :output
                                      :if-does-not-exist :create
                                      :if-exists :supersede)
        (produce-documentation 'html
                               doc-bit
                               doc-bit-stream
                               doc-bits))))
                           

  #|(<:frame (:src (concatenate 'string (file-set-name structure) "." *default-html-extension*)
            :name (concatenate 'string (file-set-name structure) "_frame")
            )
           ;; (format nil "~&~%<!-- FRAME DOC FILE-SET ~S -->~2%" (element-name structure))
           )|#)


;;;---------------------------------------------------------------------------
;;; Doc bits HTML production.

(defun paragraphize-doc-string (s)
  (loop for par in (split-at-tex-paragraphs s)
        when (string/= "" par)
        collect (<:p () par)))


(defun dump-doc-bit-html (n str-tag doc-string out)
  (let ((name (string-downcase n)))
    (<:with-html-syntax (out :print-pretty t)
        (<:htmlize
         (<:document
          (<:head
           (<:title (format nil "~A ~A" str-tag name))
           (<:link :rel "stylesheet" :href (namestring *helambdap-css-filename-up*)))
          (<:body
           (<:h1 (<:i (format nil "~A " str-tag)) (<:strong name))
           (<:h2 "Package: ")
           (<:p (package-name (symbol-package n)))
           (<:h2 "Description:") (paragraphize-doc-string doc-string))
          )
         :syntax :compact))))


(defmethod produce-documentation ((format (eql 'html))
                                  (doc-bit doc-bit)
                                  (out file-stream)
                                  doc-bits
                                  &key
                                  (documentation-title)
                                  &allow-other-keys
                                  )
  (declare (ignorable documentation-title))
  (dump-doc-bit-html (doc-bit-name doc-bit)
                     (doc-bit-kind-tag doc-bit)
                     (doc-bit-doc-string doc-bit)
                     out))


(defmethod produce-documentation ((format (eql 'html))
                                  (doc-bit package-doc-bit)
                                  (out file-stream)
                                  doc-bits
                                  &key
                                  ;; documentation-title
                                  &allow-other-keys)
  (let ((name (doc-bit-name doc-bit))
        (doc-string (doc-bit-doc-string doc-bit))
        )
    (<:with-html-syntax (out :print-pretty t)
        (<:htmlize
         (<:document
          (<:head
           (<:title "Package " name)
           (<:link :rel "stylesheet" :href (namestring *helambdap-css-filename-up*)))
          (<:body
           (<:h1 (<:i "Package ") (<:strong name))
           (<:h2 "Use list:") (<:p (package-doc-bit-use-list doc-bit))
           (<:h2 "Nicknames:") (<:p (package-doc-bit-nicknames doc-bit))
           (<:h2 "Description:") (paragraphize-doc-string doc-string))
          )
         :syntax :compact))))


(defmethod produce-documentation ((format (eql 'html))
                                  (doc-bit system-doc-bit)
                                  (out file-stream)
                                  doc-bits
                                  &key
                                  ;; documentation-title
                                  &allow-other-keys)
  (let ((name (doc-bit-name doc-bit))
        (doc-string (doc-bit-doc-string doc-bit))
        )
    (<:with-html-syntax (out :print-pretty t)
        (<:htmlize
         (<:document
          (<:head
           (<:title "System " name)
           (<:link :rel "stylesheet" :href (namestring *helambdap-css-filename-up*)))
          (<:body
           (<:h1 (<:i "System ") (<:strong name))
           (<:h2 "Depends on:")
           (<:p (mapcar (lambda (d) (<:i () d))
                        (system-doc-bit-depends-on doc-bit)))
           (<:h2 "Description:") (paragraphize-doc-string doc-string))
          )
         :syntax :compact))))


(defmethod produce-documentation ((format (eql 'html))
                                  (doc-bit parameterized-doc-bit)
                                  (out file-stream)
                                  doc-bits
                                  &key
                                  documentation-title
                                  &allow-other-keys)
  (let* ((db-name (doc-bit-name doc-bit))
         (name (string-downcase db-name))
         (kind (doc-bit-kind doc-bit))
         (doc-string (doc-bit-doc-string doc-bit))
         (ll (parameterized-doc-bit-lambda-list doc-bit))
         )
    (<:with-html-syntax (out :print-pretty t)
        (<:htmlize
         (<:document
          (<:head
           (<:title documentation-title ": " kind name)
           (<:link :rel "stylesheet" :href (namestring *helambdap-css-filename-up*)))
          (<:body
           (<:h1 (<:i kind) (<:strong name))

           (<:h2 "Package: ")
           (<:p (<:code (package-name (symbol-package db-name))))

           (<:h2 "Syntax:")
           (<:p
            (<:pre
             (format nil
                     "~&    ~A ~A~%"
                 (<:b () (<:span (:style "color: red") (<:strong () name)))
                 (format nil "~{ <i>~A</i>~}" ll))))

           (when ll
             (<:div ()
                    (<:h3 () "Arguments and Values:")
                    (<:ul ()
                          (loop for arg in ll
                                unless (member arg '(&optional &rest &key &allow-other-keys &whole &environment &aux))
                                collect
                                (<:htmlize
                                 (<:li (<:i (<:code (if (consp arg) (first arg) arg))) "---" (if (consp arg) (second arg) arg))
                                 :syntax :compact)))))

           (<:h2 "Description:")
           (paragraphize-doc-string doc-string))
          )
         :syntax :compact))))


(defmethod produce-documentation ((format (eql 'html))
                                  (doc-bit constant-doc-bit)
                                  (out file-stream)
                                  doc-bits
                                  &key
                                  ;; documentation-title
                                  &allow-other-keys)
  (let* ((name (doc-bit-name doc-bit))
         (kind (doc-bit-kind-tag doc-bit))
         (doc-string (doc-bit-doc-string doc-bit))
         (value (constant-doc-bit-initial-value doc-bit))
         (value-presented (if (stringp value)
                              (split-lines-for-html
                               (format nil "~S"
                                       (sanitize-string-for-html value)))
                              ;; Quiz: why do I need the FORMAT ~S?
                              ;; Because I want to ensure that the
                              ;; string contains all the necessary
                              ;; escapes (cfr. *PRINT-ESCAPE*).

                              value))
         )
    (<:with-html-syntax (out :print-pretty t)
        (<:htmlize
         (<:document
          (<:html
           +doctype-xhtml1-string-control-string+
           (string #\Newline)

           (<:head
            (<:title kind (string-downcase name))
            (<:link :rel "stylesheet" :href (namestring *helambdap-css-filename-up*)))
           (<:body
            (<:h1 (<:i kind) (<:strong (string-downcase name)))

            (<:h2 "Package:")
            (<:p (package-name (symbol-package name)))

            (<:h2 "Value:")
            (<:p (<:code value-presented))

            (<:h2 "Description:")
            (paragraphize-doc-string doc-string)))
          )
         :syntax :compact))))


(defmethod produce-documentation ((format (eql 'html))
                                  (doc-bit struct-doc-bit)
                                  (out file-stream)
                                  doc-bits
                                  &key
                                  ;; documentation-title
                                  &allow-other-keys)
  (let* ((name (doc-bit-name doc-bit))
         (kind (doc-bit-kind-tag doc-bit))
         (doc-string (doc-bit-doc-string doc-bit))
         (include (struct-doc-bit-include doc-bit))
         (slots (struct-doc-bit-slots doc-bit))
         )
    (<:with-html-syntax (out :print-pretty t)
        (<:htmlize
         (<:document
          (<:html
           +doctype-xhtml1-string-control-string+
           (string #\Newline)

           (<:head
            (<:title kind (string-downcase name))
            (<:link :rel "stylesheet" :href (namestring *helambdap-css-filename-up*)))
           (<:body
            (<:h1 (<:i kind) (<:strong (string-downcase name)))

            (<:h2 "Package: ")
            (<:p (<:code (package-name (symbol-package name))))

            (<:h2 "Class Precedence List:")
            (<:p (format nil "~A &rarr;~@[ ~A &rarr;~] ... &rarr; T" name include))

            (<:h2 "Slots:")
            (<:p (<:dl
                  (loop for s in slots
                        if (symbolp s)
                        collect
                        (<:dt () s)
                        and collect
                        (<:dd ()
                              (format nil
                                      "with initial value ~A of type ~A~@[; the slot is read-only~]."
                                      nil T nil))
                        else
                        nconc
                        (destructuring-bind (sn &optional sv &key read-only (type t))
                            s
                          (list
                           (<:dt () sn)
                           (<:dd ()
                                 (format nil "with initial value ~S of type ~A~@[; the slot is read-only~]."
                                         sv type read-only)))))
                  ))
            (<:h2 "Description:")
            (paragraphize-doc-string doc-string)))
          )
         :syntax :compact))))


(defmethod produce-documentation ((format (eql 'html))
                                  (doc-bit class-doc-bit)
                                  (out file-stream)
                                  doc-bits
                                  &key
                                  ;; documentation-title
                                  &allow-other-keys
                                  )
  (let* ((name (doc-bit-name doc-bit))
         (kind (doc-bit-kind-tag doc-bit))
         (doc-string (doc-bit-doc-string doc-bit))
         (superclasses (class-doc-bit-superclasses doc-bit))
         (slots (class-doc-bit-slots doc-bit))
         )
    (<:with-html-syntax (out :print-pretty t)
        (<:htmlize
         (<:document
          (<:html
           +doctype-xhtml1-string-control-string+
           (string #\Newline)

           (<:head
            (<:title kind (string-downcase name))
            (<:link :rel "stylesheet" :href (namestring *helambdap-css-filename-up*)))
           (<:body
            (<:h1 (<:i kind) (<:strong (string-downcase name)))
            
            (<:h2 "Package: ")
            (<:p (<:code (package-name (symbol-package name))))

            (<:h2 "Class Precedence List:")
            (<:p (format nil "~A &rarr;~@[~{ ~A~^&rarr;~}~] ... &rarr; T" name superclasses))
            
            (when slots
              (<:htmlize
               (<:div
                (<:h2 "Slots:")
                (<:p (<:dl
                      (loop for s in slots
                               if (symbolp s)
                               collect (<:dt () s)
                               else
                               nconc
                               (destructuring-bind (sn &key
                                                       type
                                                       documentation
                                                       allocation
                                                       initarg
                                                       initform
                                                       &allow-other-keys)
                                   s
                                 `(
                                   ,(<:dt () sn)
                                   ,@(when type (list (<:dd () "Type: " type)))
                                   ,@(when allocation (list (<:dd () "Allocation: " allocation)))
                                   ,@(when initarg (list (<:dd () "Initarg: " initarg)))
                                   ,@(when initform (list (<:dd () "Initform: " initform)))
                                   ,@(when documentation (list (<:dd () documentation)))
                                   )
                                 ))
                      )))
               :syntax :compact))
            
            (<:h2 "Description:")
            (paragraphize-doc-string doc-string)))
          )
         :syntax :compact))))
       


(defmethod produce-documentation ((format (eql 'html))
                                  structure
                                  (out stream)
                                  doc-bit
                                  &key
                                  (documentation-title)
                                  &allow-other-keys
                                  )
  (declare (ignorable documentation-title))
  (let ((name (doc-bit-name doc-bit))
        (doc-string (doc-bit-doc-string doc-bit))
        )

    (<:with-html-syntax (out :print-pretty t)
        (<:htmlize
         (<:document

          (<:head
           (<:title (format nil "DOC FOR ~A" (string-downcase name)))
           (<:link :rel "stylesheet" :href (namestring *helambdap-css-filename-up*)))

          (<:body
           (<:h1 (<:i "Function") (<:strong name))
           (<:h2 "Package:")
           (<:h2 "Description:" (<:br) doc-string)
           ))
         :syntax :compact))
    t))


#+version-using-MOP
(defmethod produce-documentation ((format (eql 'html))
                                  (doc-bit generic-function-doc-bit)
                                  (out file-stream)
                                  doc-bits
                                  &key
                                  documentation-title
                                  &allow-other-keys)
  "This specialized method produces the documentation for a generic function."
  (declare (ignorable documentation-title))
  (labels ((method-signature (m)
             (declare (type method m))
             (let ((ms  (closer-mop:method-specializers m))
                   (mll (closer-mop:method-lambda-list m))
                   )
               (list
                (mapcar (lambda (s)
                          (etypecase s
                            (closer-mop:eql-specializer
                             `(eql ,(closer-mop:eql-specializer-object s)))
                            (class (class-name s))))
                        (closer-mop:method-specializers m))
                (subseq mll (list-length ms))))
             )
           (method-signatures (gf)
             (declare (type generic-function gf))
             (mapcar #'method-signature (closer-mop:generic-function-methods gf))
             )
           )

    (let* ((gfname (doc-bit-name doc-bit))
           (name (string-downcase gfname))
           (kind (doc-bit-kind doc-bit))
           (doc-string (doc-bit-doc-string doc-bit))
           )
      (declare (ignore kind))
      (<:with-html-syntax (out :print-pretty t)
          (<:htmlize
           (<:document
            (<:head
             (<:title documentation-title ": " "Generic Function" name)
             (<:link :rel "stylesheet" :href (namestring *helambdap-css-filename-up*)))
            (<:body
             (<:h1 (<:i "Generic Function") (<:strong name))
             (<:h2 "Package:")
             (<:p (package-name (symbol-package gfname)))
             (<:h2 "Syntax:")
             (<:p (<:strong name)
                  (format nil "~{ <i>~A</i>~}" (parameterized-doc-bit-lambda-list doc-bit)))
             (<:h2 "Description:")
             (paragraphize-doc-string doc-string)
           
             (<:h3 "Known Methods:")
             (<:ul
              (loop for (specializers other) in (method-signatures (symbol-function gfname))
                    collect (<:htmlize
                             (<:li
                              (<:p (<:strong name)
                                   (format nil "~{ &lt;<i>~A</i>&gt;~}" specializers)
                                   (format nil "~{ <i>~A</i>~}" other)
                                   )
                              )
                             :syntax :compact)
                    ))
             ))
           :syntax :compact)))))

#-version-using-MOP
(defmethod produce-documentation ((format (eql 'html))
                                  (doc-bit generic-function-doc-bit)
                                  (out file-stream)
                                  doc-bits
                                  &key
                                  documentation-title
                                  &allow-other-keys)
  "This specialized method produces the documentation for a generic function."
  (declare (ignorable documentation-title))
  (labels ((method-signature (mdb)
             (declare (type method-doc-bit mdb))
             (let* ((mll (method-doc-bit-lambda-list mdb))
                    (regular-arg-pos (position-if #'symbolp mll))
                    )
               (list
                (mapcar (lambda (ll-elem
                                 &aux
                                 ;; (ll-elem-var (first ll-elem))
                                 (ll-elem-class (second ll-elem)))
                          (etypecase ll-elem-class
                            (cons ; An EQL specializer (or something else?)
                             ll-elem-class)
                            (symbol
                             ll-elem-class)
                            (class
                             (class-name ll-elem-class))
                            ))
                        (subseq mll 0 regular-arg-pos))
                (when regular-arg-pos (subseq mll regular-arg-pos)))
               ))
           )

    (let* ((gfname (doc-bit-name doc-bit))
           (name (string-downcase gfname))
           (kind (doc-bit-kind doc-bit))
           (doc-string (doc-bit-doc-string doc-bit))
           (f-values (function-doc-bit-values doc-bit))
           )
      (declare (ignore kind))
      (<:with-html-syntax (out :print-pretty t)
          (<:htmlize
           (<:document
            (<:head
             (<:title documentation-title ": " "Generic Function" name)
             (<:link :rel "stylesheet" :href (namestring *helambdap-css-filename-up*)))
            (<:body

             (<:h1 (<:i "Generic Function") (<:strong name))

             (<:h2 "Package:")
             (<:p (package-name (symbol-package gfname)))

             (<:h2 "Syntax:")
             (<:p
              (<:pre (format nil "~&    ~A~A~%"
                             (<:b () (<:span (:style "color: red") (<:strong () name)))
                             (format nil "~{ <i>~A</i>~}" (parameterized-doc-bit-lambda-list doc-bit)))))

             (<:h3 "Arguments and Values:")
             (loop for arg in (parameterized-doc-bit-lambda-list doc-bit)
                   unless (member arg '(&optional &rest &key &allow-other-keys &whole &environment &aux))
                   collect
                   (<:htmlize
                    (<:p (<:i (<:code (if (consp arg) (first arg) arg))) "---" (if (consp arg) (second arg) arg))
                    :syntax :compact))

             (if f-values
                 (loop for fv in f-values
                       for i from 1
                       collect
                       (<:htmlize
                        (<:p (<:i (<:code (format nil "result-~D" i))) "---" " a " (<:i fv) ".")
                        :syntax :compact))
                 (<:p () (<:i () (<:code () "result")) "--- a T."))

             (<:h2 "Description:")
             (paragraphize-doc-string doc-string)
           

             (let ((known-methods-els
                    (loop for mdb of-type method-doc-bit in (generic-function-doc-bit-methods doc-bit)
                          for (specializers other) = (method-signature mdb)
                          for mdb-doc = (doc-bit-doc-string mdb)
                          collect (<:htmlize
                                   (<:li
                                    (<:p
                                     (<:pre (format nil
                                                    "    ~A~A~A~A"
                                                    (<:b () (<:strong () name))
                                                    (format nil "~@[~{~A~^ ~}]~]" (method-doc-bit-qualifiers mdb))
                                                    (format nil "~{ &lt;<i>~A</i>&gt;~}" specializers)
                                                    (format nil "~{ <i>~A</i>~}" other))))
                                    (when mdb-doc
                                      (<:p ()  mdb-doc)))
                                   :syntax :compact)
                          ))
                   )
               (when known-methods-els
                 (<:div ()
                        (<:h3 () "Known Methods:")
                        (<:ul () known-methods-els)))
              
               )))
           :syntax :compact)))))


;;;---------------------------------------------------------------------------
;;; Auxiliary files production.

(defmethod frameset-head-title ((fs frameset))
  "FOOO")


(defmethod frameset-body-title ((fs frameset))
  "FFFFOOOOO")


(defmethod framesets-of ((fss framesets))
  (framesets-list fss))

(defmethod framesets-of ((e element)) ())

(defmethod framesets-of ((e documentation-structure)) ())


(defmethod produce-header-file ((fs frameset) header-pathname documentation-title)
  (declare (type pathname header-pathname))
  (declare (ignorable documentation-title))
  (let ((fs-order (frameset-order fs))
        (fs-head-title (frameset-head-title fs))
        (fs-body-title (frameset-body-title fs))
        (ed-fs (element-location-depth fs))
        )
    (labels ((select-link-style (i)
               (if (= i fs-order)
                   "navigation-link-selected"
                   "navigation-link"))

             (select-link-style-1 (fs-in-p)
               (if (eq fs fs-in-p)
                   "navigation-link-selected"
                   "navigation-link"))

             ;; The next function is hairy because it needs to produce
             ;; the href based on the position of the frameset in the
             ;; hierarchy.

             (produce-navigation-link (fs-in-parent)
               (let* ((fs-path (compute-element-path fs-in-parent))
                      (href (if (eq fs fs-in-parent)
                                (namestring
                                 (make-pathname :name (frameset-name fs-in-parent)
                                                :type *default-html-extension*
                                                :directory ()))
                                (namestring
                                 (merge-pathnames 
                                  fs-path
                                  (make-pathname
                                   :directory (cons :relative
                                                    (make-list ed-fs
                                                               :initial-element :up)))))))
                      )
                 (<:a (:href href ; :href (namestring (compute-element-path fs-in-parent)
                       :target "_parent"
                       :class (select-link-style-1 fs-in-parent))
                      (string-capitalize (frameset-name fs-in-parent)))))
             )
      (with-open-file (hs header-pathname
                          :direction :output
                          :if-exists :supersede
                          :if-does-not-exist :create)
        (<:with-html-syntax (hs :print-pretty t)
            (<:htmlize
             (<:document
              (<:comment (base-name header-pathname))
              +doctype-xhtml1-string-control-string+
              (<:html
               (<:head
                (<:title fs-head-title)
                (<:link :rel "stylesheet" :href (frameset-style fs)))

               ((<:body :style "margin: 0pt 0pt 0pt 0pt;")
                ((<:div :class "header"
                        :style "padding-left: 2em; padding-top: 5pt; color: #41286f; font-size: 14pt")
                 (<:strong (or documentation-title fs-body-title))
                 ((<:div :class "navigation"
                         :style "right: 2m")

                  (when (element-parent fs)
                    (let ((fss-in-p (framesets-of (element-parent fs))))
                      (loop for fs-in-p in (rest fss-in-p)
                            collect " | " into result
                            collect (produce-navigation-link fs-in-p) into result
                            finally (return (cons (produce-navigation-link (first fss-in-p))
                                                  result))
                            )))
                  #|
                  ((<:a :href "index.htm"
                        :class (select-link-style 0)
                        :target "_parent")
                   "Home")
                  "|"
                  ((<:a :href "dictionary/dictionary.htm"
                        :class (select-link-style 1)
                        :target "_parent")
                   "Dictionary")
                  "|"
                  ((<:a :href "mailing-lists.htm"
                        :class (select-link-style 2)
                        :target "_parent")
                   "Mailing Lists")
                  |#
                  )
                 )
                )))
             :syntax :compact))
        ))))


(defmethod produce-navigation-file ((fs frameset)
                                    (nav-element doc-file)
                                    nav-pathname
                                    doc-bits
                                    documentation-title)
  (declare (type pathname nav-pathname))
  (declare (ignorable doc-bits documentation-title))

  (let ((fs-order (frameset-order fs))
        (fs-head-title (frameset-head-title fs))
        (fs-body-title (frameset-body-title fs))
        )
    (declare (ignore fs-order fs-body-title))

    (with-open-file (ns nav-pathname
                        :direction :output
                        :if-exists :supersede
                        :if-does-not-exist :create)
      (<:with-html-syntax (ns :print-pretty t)
          (<:htmlize
           (<:document
            (<:comment (base-name nav-pathname))
            (string #\Newline)

            +doctype-xhtml1-string-control-string+
            (string #\Newline)
            (string #\Newline)

            (<:html
             (<:head
              (<:title fs-head-title)
              (<:link :rel "stylesheet" :href (frameset-style fs)))

             (<:body
              (<:ul))
             ))
           :syntax :compact))
      ))
  )

#|
#+original
(defmethod produce-navigation-file ((fs frameset)
                                    (nav-element file-set)
                                    nav-pathname
                                    doc-bits
                                    documentation-title)
  (declare (type pathname nav-pathname))
  (declare (ignorable documentation-title))

  (let ((fs-order (frameset-order fs))
        (fs-head-title (frameset-head-title fs))
        (fs-body-title (frameset-body-title fs))
        )
    (declare (ignore fs-order fs-body-title))

    (with-open-file (ns nav-pathname
                        :direction :output
                        :if-exists :supersede
                        :if-does-not-exist :create)
      (<:with-html-syntax (ns :print-pretty t)
          (<:htmlize
           (<:document
            (<:comment (base-name nav-pathname))
            +doctype-frameset-control-string+
            (<:html
             (<:head
              (<:title fs-head-title)
              (<:link :rel "stylesheet" :href (frameset-style fs)))

             (<:body
              (<:ul))
             ))
           :syntax :compact))
      ))
  )
|#


(defmethod produce-navigation-file ((fs frameset)
                                    (nav-element file-set)
                                    nav-pathname
                                    doc-bits
                                    documentation-title)
  (declare (type pathname nav-pathname))
  (declare (ignorable documentation-title))

  (let* ((fs-order (frameset-order fs))
         (fs-head-title (frameset-head-title fs))
         (fs-body-title (frameset-body-title fs))
         (fs-name (frameset-name fs))

         (nav-map-pathname
          (make-pathname :name (format nil
                                       "~A-navigation-map"
                                       fs-name)
                         :type *default-html-extension*
                         :defaults nav-pathname))
         (nav-list-pathname
          (make-pathname :name (format nil
                                       "~A-navigation-lists"
                                       fs-name)
                         :type *default-html-extension*
                         :defaults nav-pathname))
         )
    (declare (ignore fs-order fs-body-title nav-list-pathname))

    (with-open-file (ns nav-pathname
                        :direction :output
                        :if-exists :supersede
                        :if-does-not-exist :create)
      (<:with-html-syntax (ns :print-pretty t)
          (<:htmlize
           (<:document
            (<:comment (base-name nav-pathname))
            (string #\Newline)

            +doctype-frameset-control-string+
            (string #\Newline)
            (string #\Newline)
            (<:html
             (<:head
              (<:title fs-head-title)
              (<:link :rel "stylesheet" :href (frameset-style fs)))
            
             ((<:frameset :rows "20%,80%"
                          #| :frameborder 0 |#
                          :noresize "noresize")
              ((<:frame :name (format nil
                                      "~A_navigation_map"
                                      fs-name)
                        :src (base-name nav-map-pathname)
                        :frameborder 0
                        ))
              ((<:frame :name (format nil
                                      "~A_navigation_lists"
                                      fs-name)
                        ;; :src (namestring nav-list-pathname)
                        :frameborder 0
                        ))
              ))
            (<:comment (format nil "end of file : ~A"
                               (base-name nav-pathname))) 
            (string #\newline))
           :syntax :compact))) ; WITH-OPEN-FILE...

    (produce-navigation-map fs nav-element nav-map-pathname doc-bits)
    ))


(defun produce-navigation-map (fs nav-element nm-pathname doc-bits)
  (format t "~&>>> Producing NAV MAP file ~S ~S ~S~2%"
          fs nav-element nm-pathname doc-bits)
  (let ((nav-element-target (format nil "~A_frame" (element-name nav-element))))
    (with-open-file (nm nm-pathname
                        :direction :output
                        :if-exists :supersede
                        :if-does-not-exist :create)
      (<:with-html-syntax (nm :print-pretty t)
          (<:htmlize
           (<:document
            (<:comment (base-name nm-pathname))
            (string #\Newline)
            (string #\Newline)

            +doctype-xhtml1-string-control-string+
            (string #\Newline)

            (<:html
             (<:head
              (<:title "Navigation Map")
              (<:link :rel "stylesheet"
                      :href (namestring *helambdap-css-filename-up*)) ; TESTING!
              (<:style (format nil
                               ".helambdap_navmap li {~
                                  display: inline;~
                              }"))
              )

             (<:body
              ((<:div :class "helambdap_navmap")
               ;; (<:h4 "Systems and Packages")


               (<:p (<:strong ((<:script :type "text/javascript")
                               (format nil
                                       "document.write('<i>' ~
                                                      + document.getElementByName('~A').src ~
                                                      + '</i>' + Date())"
                                       (format nil "~A_navigation_list" (element-name fs))
                                       ))))

               (let ((syss (remove-if (complement #'system-doc-bit-p)
                                      doc-bits))
                     (pkgs (remove-if (complement #'package-doc-bit-p)
                                      doc-bits))
                     )
                 (list
                  (<:h4 () "Systems")
                  (<:div ()
                         (loop for s in (remove-duplicates syss
                                                           :test
                                                           (lambda (s1 s2)
                                                             (and (not (eq (type-of s1) (type-of s2)))
                                                                  (string-equal (doc-bit-name s1) (doc-bit-name s2)))))
                               ;; The above is kludgy!  It is meant to
                               ;; remove duplicate systems assuming
                               ;; that different kinds of systems are
                               ;; mutually exclusive.
                               ;; In practice it will not affect most people.
                               for s-doc-pathname
                               = (make-doc-bit-pathname s
                                                        *default-html-extension*
                                                        nm-pathname)

                               for s-filename = (base-name s-doc-pathname)
                               collect (<:p ()
                                            (<:a (:href s-filename
                                                  :target nav-element-target
                                                  #|
                                                  :onclick
                                                  (format nil
                                                          "parent.frames[1].location.href = '~A'"
                                                          s-filename
                                                          )
                                                  #|
                                                  (format nil
                                                          "document.getElementByName('~A').src = '~A'"
                                                          (format nil "~A_navigation_list" (element-name fs)) 
                                                          s-filename
                                                          )
                                                  |#
                                                  |#
                                                  )
                                                 (doc-bit-name s))
                                            ;; (type-of s)
                                            )))

                  (<:h4 () "Packages")
                  (<:div ()
                         (loop for p in pkgs

                               for p-doc-pathname =
                               (make-doc-bit-pathname p
                                                      *default-html-extension*
                                                      nm-pathname)
                               for p-filename = (base-name p-doc-pathname)

                               for p-list-pathname =
                               (make-pathname :name (format nil "~A-list"
                                                            (pathname-name p-doc-pathname))
                                              :type *default-html-extension*
                                              :defaults nm-pathname)
                               for p-list-filename = (base-name p-list-pathname)
                                                           
                               do (produce-package-navigation-list fs nav-element p p-list-pathname doc-bits)
                               collect (<:p ()
                                            (<:a (:href p-filename
                                                  :target nav-element-target
                                                  :onclick
                                                  (format nil
                                                          "parent.frames[1].location.href = '~A'"
                                                          p-list-filename
                                                          )
                                                  #|(format nil
                                                           "document.getElementByName('~A').src = '~A'"
                                                           (format nil "~A_navigation_list" (element-name fs))
                                                           p-list-filename
                                                           )|#
                                                  )
                                                 (doc-bit-name p)))))
                  )))
              ) ; </body>
             (<:comment (format nil "end of file : ~A"
                                (base-name nm-pathname)))
             (string #\newline)
             ))
           :syntax :compact)))))


(defun produce-package-navigation-list (fs nav-element pkg-doc-bit pkg-list-pathname doc-bits
                                           &aux
                                           (pkg (find-package (package-doc-bit-name pkg-doc-bit)))
                                           (target (format nil "~A_frame"
                                                           (element-name nav-element)))
                                           )
  (declare (ignore pkg))
  (format t "~&>>>> produce-package-navigation-list ~S ~S ~S~%"
          fs
          (package-doc-bit-name pkg-doc-bit)
          pkg-list-pathname)
          
  (destructuring-bind (systems
                       packages
                       constants
                       parameters
                       variables
                       types
                       classes
                       structs
                       conditions
                       generic-functions
                       methods
                       functions
                       macros
                       method-combinations
                       setf-expanders
                       modify-macros
                       others
                       )
      (sift-standard-doc-bits doc-bits)
    (declare (ignore systems packages methods others))
    (flet (#|(build-list (list-name doc-bits)
             (when doc-bits
               (list (<:h4 () list-name)
                     (<:ul ()
                           (loop for db in doc-bits
                                 for db-filename
                                 = (base-name
                                    (make-doc-bit-pathname db
                                                           *default-html-extension*
                                                           pkg-list-pathname))
                                 collect (<:li ()
                                               (<:a (:href db-filename
                                                     :target target)
                                                    (string-downcase (doc-bit-name db)))))
                           ))))|#
           (build-list (list-name doc-bits)
             (when doc-bits
               (list* (<:h4 () list-name)
                      (loop for db in doc-bits
                            for db-filename
                            = (base-name
                               (make-doc-bit-pathname db
                                                      *default-html-extension*
                                                      pkg-list-pathname))
                            collect (<:p (:class "navindex")
                                         (<:a (:href db-filename
                                               :target target)
                                              (string-downcase (doc-bit-name db)))))
                      )))
           )
      (with-open-file (ps pkg-list-pathname
                          :if-exists :supersede
                          :if-does-not-exist :create
                          :direction :output)

        (<:with-html-syntax (ps :print-pretty t)
            (<:htmlize
             (<:document
              (<:comment (base-name pkg-list-pathname))
              +doctype-xhtml1-string-control-string+

              (<:html
               (<:head
                (<:title (format nil "~A Package List" (doc-bit-name pkg-doc-bit)))
                (<:link :rel "stylesheet" :href (frameset-style fs))
                (<:style (format nil
                                 ".helambdap_navmap li {~
                                      display: inline;~
                                  }")))

               (<:body
                ((<:div :class "helambdap_navmap")
                ;; systems
                ;; packages
                (build-list "Constants" constants)
                (build-list "Parameters" parameters)
                (build-list "Variables" variables)
                (build-list "Types" types)
                (build-list "Classes" classes)
                (build-list "Structures" structs)
                (build-list "Conditions" conditions)
                (build-list "Generic Functions" generic-functions)
                ;; (build-list "Methods" methods)
                (build-list "Functions" functions)
                (build-list "Macros" macros)
                (build-list "Method Combinations" method-combinations)
                (build-list "Setf expanders" setf-expanders)
                (build-list "Modify Macros" modify-macros)
                ;; others
                ))))
             :syntax :compact
             )
            )))))




(defmethod produce-footer-file ((fs frameset) footer-pathname documentation-title)
  (declare (type pathname footer-pathname))
  (declare (ignorable documentation-title))
  (let ((fs-order (frameset-order fs))
        (fs-head-title (frameset-head-title fs))
        (fs-body-title (frameset-body-title fs))
        )
    (declare (ignore fs-order))

    (with-open-file (hs footer-pathname
                        :direction :output
                        :if-exists :supersede
                        :if-does-not-exist :create)
      (<:with-html-syntax (hs :print-pretty t)
          (<:htmlize
           (<:document
            (<:comment (base-name footer-pathname))
            +doctype-xhtml1-string-control-string+
            (<:html
             (<:head
              (<:title fs-head-title)
              (<:link :rel "stylesheet" :href (frameset-style fs)))

             ((<:body :style "margin: 0pt 0pt 0pt 0pt;")
              ((<:div :class "copyright"
                      #| :style "padding-left: 2em; padding-top: 5pt; color: #41286f; font-size: 14pt"|#)
               (<:strong (or documentation-title fs-body-title))
               "documentation produced with"
               "HE&Lambda;P"
               (<:br)
               (<:comment "hhmts start")
               "Last modified: " (text-timestamp)
               (<:comment "hhmts end")
               (<:br)

               (format nil "&copy; ~D, Marco Antoniotti, all rights reserved."
                       (nth-value 5 (decode-universal-time (get-universal-time)))))
              )))
           :syntax :compact)))
      ))


#|
<!-- header.html -->

<!DOCTYPE HTML PUBLIC
"-//W3C//DTD XHTML 1.0 Strict//EN"
"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd">

<html>
<head>
<title>HE&Lambda;P: The Common Lisp Help and Documentation Center</title>
<link rel="stylesheet" href="clstyle.css">
</head>

<body style="margin: 0pt 0pt 0pt 0pt;">

<div class="header"
  style="padding-left: 2em; padding-top: 5pt; color: #41286f; font-size: 14pt">
<!-- <div class="text"> -->
  <strong><i>HE&Lambda;P</i></strong><br/>
<!-- </div> -->


<div class="navigation" style="right: 2m">
  <a href="index.html" class="navigation-link-selected" target=_parent>Home</a>
  | <a href="dictionary/dictionary-frame.html" class="navigation-link" target=_parent>Dictionary</a>
  | <a href="downloads-frame.html" class="navigation-link" target=_parent>Downloads</a>
  | <a href="mailing-lists-frame.html" class="navigation-link" target=_parent>Mailing Lists</a>
  | <a href="links-frame.html" class="navigation-link" target=_parent>Links</a>
</div>

<!--
<div class="black-line"><img src="images/shim.gif" height="1" width="1"></div>
<div class="middle-bar"><img src="images/shim.gif" height="5" width="1"></div>
<div class="black-line"><img src="images/shim.gif" height="1" width="1"></div>
-->

</div>

</body>
</html>

<!-- end of file : header.html -->

|#


;;;; end of file -- xhtml-producer.lisp --

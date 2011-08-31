;;;; -*- Mode: Lisp -*-

;;;; xhtml-producer.lisp --
;;;; Make a file out of a doc-bit.

(in-package "HELAMBDAP")

;(use-package "XHTMLAMBDA")

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


(defgeneric produce-header-frame (format
                                  frameset
                                  frameset-stream
                                  where
                                  doc-bits
                                  doc-title)
  )


(defgeneric produce-footer-frame (format
                                  frameset
                                  frameset-stream
                                  where
                                  doc-bits
                                  doc-title)
  )


(defparameter *xhtml-indent* 4) ; 4 is the actual initial value.


;;;;===========================================================================
;;;; Implementation.

(defmethod produce-documentation ((format (eql 'html)) ;
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
                         :type (or (pathname-type dfn) "html")))
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


(declaim (type string
               +doctype-frameset-control-string+
               +doctype-xhtml1-string-control-string+))


(defconstant +doctype-frameset-control-string+
"<!DOCTYPE HTML PUBLIC
  \"-//W3C//DTD HTML 4.01 Frameset//EN\"
  \"http://www.w3.org/TR/html4/frameset.dtd\">"

"The standard 'DOCTYPE' w3c Frameset DTD (X)HTML string.")


(defconstant +doctype-xhtml1-string-control-string+
"<!DOCTYPE HTML PUBLIC
  \"-//W3C//DTD XHTML 1.0 Strict//EN\"
  \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">"

"The standard 'DOCTYPE' w3c DTD XHTML Strict DTD (X)HTML string.")


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

    (ensure-directories-exist where)

    (with-open-file (fs-file fs-pathname
                             :direction :output
                             :if-does-not-exist :create
                             :if-exists :supersede)
      (<:with-html-syntax (fs-file :print-pretty t)
          (<:htmlize
           (<:document 
            (format nil "<!-- ~A.htm -->~2%" fs-name)
            +doctype-frameset-control-string+
            (string #\newline)
            (<:html

             (<:head
              (<:title fs-name)
              (<:link :rel "stylesheet" :href "clstyle.css"))
             
             ((<:frameset :rows "65px,*" :border 0 :noresize "noresize")
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
              ((<:frameset :cols "150px,*" :border 0)
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
                                            (first fs-content)
                                            fs-file
                                            doc-bits)
                     (produce-frame format (first fs-content) fs-file)
                     )
                   (<:frame (:src (base-name
                                   (make-pathname
                                    :type *default-html-extension*
                                    :name (element-name structure))))))
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



(defmethod produce-frame ((format (eql 'html))
                          (element file-set)
                          (where stream)
                          )
  (<:frame (:src (element-name element))
           (format nil "~&<!-- FRAME ~A -->" (element-name element))))
      



(defmethod produce-frame ((format (eql 'html))
                          (element doc-file)
                          (where stream)
                          )
  (<:frame (:src (namestring (file-pathname element)))
           (<:comment () "FRAME " (element-name element))))


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

        (<:frame (:src (base-name header-pathname)))
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
        (unless (and (probe-file nav-pathname)
                     (not *supersede-documentation*))
          (produce-navigation-file element nav-pathname documentation-title))
        (<:frame (:src (base-name nav-pathname)))
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
        (<:frame (:src (base-name footer-pathname)))
        ))))


(defmethod produce-documentation ((format (eql 'html))
                                  (structure file-set)
                                  (where pathname)
                                  doc-bits
                                  &key
                                  documentation-title
                                  &allow-other-keys)
  (declare (ignorable documentation-title))
  (<:frame (:src (element-name structure))))


(defmethod produce-documentation ((format (eql 'html))
                                  (structure file-set)
                                  (where file-stream)
                                  doc-bits
                                  &key
                                  (documentation-title)
                                  &allow-other-keys
                                  )
  (declare (ignorable documentation-title))

  (<:frame (:src (element-name structure))
           (format nil "~&~%<!-- FRAME DOC FILE-SET ~S -->~2%" (element-name structure))
           ))


(defun dump-doc-bit-html (name str-tag doc-string out)
  (<:with-html-syntax (out :print-pretty t)
      (<:htmlize
       (<:document
        (<:head
         (<:title (format nil "~A ~A" str-tag (string-downcase name)))
         (<:link :rel "stylesheet" :href "../clstyle.css"))
        (<:body
         (<:h1 (<:i (format nil "~A " str-tag)) (<:strong name))
         (<:h2 "Package: ")
         (<:p (package-name (symbol-package name)))
         (<:h2 "Description:") (<:p doc-string))
        )
       :syntax :compact)))


(defmethod produce-documentation ((format (eql 'html))
                                  structure
                                  (out file-stream)
                                  (doc-bit function-doc-bit)
                                  &key
                                  (documentation-title)
                                  &allow-other-keys
                                  )
  (declare (ignorable documentation-title))
  (let ((name (doc-bit-name doc-bit))
        (doc-string (doc-bit-doc-string doc-bit))
        )
    (dump-doc-bit-html name "Function" doc-string out)))



(defmethod produce-documentation ((format (eql 'html))
                                  structure
                                  (out file-stream)
                                  (doc-bit macro-doc-bit)
                                  &key
                                  (documentation-title)
                                  &allow-other-keys
                                  )
  (declare (ignorable documentation-title))
  (let ((name (doc-bit-name doc-bit))
        (doc-string (doc-bit-doc-string doc-bit))
        )
    (dump-doc-bit-html name "Macro" doc-string out)))


(defmethod produce-documentation ((format (eql 'html))
                                  structure
                                  (out file-stream)
                                  (doc-bit compiler-macro-doc-bit)
                                  &key
                                  (documentation-title)
                                  &allow-other-keys
                                  )
  (declare (ignorable documentation-title))
  (let ((name (doc-bit-name doc-bit))
        (doc-string (doc-bit-doc-string doc-bit))
        )
    (dump-doc-bit-html name "Compiler Macro" doc-string out)))


(defmethod produce-documentation ((format (eql 'html))
                                  structure
                                  (out file-stream)
                                  (doc-bit setf-expander-doc-bit)
                                  &key
                                  (documentation-title)
                                  &allow-other-keys
                                  )
  (declare (ignorable documentation-title))
  (let ((name (doc-bit-name doc-bit))
        (doc-string (doc-bit-doc-string doc-bit))
        )
    (dump-doc-bit-html name "Setf Expander" doc-string out)))


(defmethod produce-documentation ((format (eql 'html))
                                  structure
                                  (out file-stream)
                                  (doc-bit modify-macro-doc-bit)
                                  &key
                                  (documentation-title)
                                  &allow-other-keys
                                  )
  (declare (ignorable documentation-title))
  (let ((name (doc-bit-name doc-bit))
        (doc-string (doc-bit-doc-string doc-bit))
        )
    (dump-doc-bit-html name "Modify Macro" doc-string out)))


(defmethod produce-documentation ((format (eql 'html))
                                  structure
                                  (out file-stream)
                                  (doc-bit generic-function-doc-bit)
                                  &key
                                  (documentation-title)
                                  &allow-other-keys
                                  )
  (declare (ignorable documentation-title))
  (let ((name (doc-bit-name doc-bit))
        (doc-string (doc-bit-doc-string doc-bit))
        )
    (dump-doc-bit-html name "Generic Function" doc-string out)))


(defmethod produce-documentation ((format (eql 'html))
                                  structure
                                  (out file-stream)
                                  (doc-bit type-doc-bit)
                                  &key
                                  (documentation-title)
                                  &allow-other-keys
                                  )
  (declare (ignorable documentation-title))
  (let ((name (doc-bit-name doc-bit))
        (doc-string (doc-bit-doc-string doc-bit))
        )
    (dump-doc-bit-html name "Type" doc-string out)))


(defmethod produce-documentation ((format (eql 'html))
                                  structure
                                  (out file-stream)
                                  (doc-bit class-doc-bit)
                                  &key
                                  (documentation-title)
                                  &allow-other-keys
                                  )
  (declare (ignorable documentation-title))
  (let ((name (doc-bit-name doc-bit))
        (doc-string (doc-bit-doc-string doc-bit))
        )
    (dump-doc-bit-html name "Class" doc-string out)))


(defmethod produce-documentation ((format (eql 'html))
                                  structure
                                  (out file-stream)
                                  (doc-bit condition-doc-bit)
                                  &key
                                  (documentation-title)
                                  &allow-other-keys
                                  )
  (declare (ignorable documentation-title))
  (let ((name (doc-bit-name doc-bit))
        (doc-string (doc-bit-doc-string doc-bit))
        )
    (dump-doc-bit-html name "Condition" doc-string out)))


(defmethod produce-documentation ((format (eql 'html))
                                  structure
                                  (out file-stream)
                                  (doc-bit method-combination-doc-bit)
                                  &key
                                  (documentation-title)
                                  &allow-other-keys
                                  )
  (declare (ignorable documentation-title))
  (let ((name (doc-bit-name doc-bit))
        (doc-string (doc-bit-doc-string doc-bit))
        )
    (dump-doc-bit-html name "Method Combination" doc-string out)))


(defmethod produce-documentation ((format (eql 'html))
                                  structure
                                  (out file-stream)
                                  (doc-bit package-doc-bit)
                                  &key
                                  (documentation-title)
                                  &allow-other-keys
                                  )
  (declare (ignorable documentation-title))
  (let ((name (doc-bit-name doc-bit))
        (doc-string (doc-bit-doc-string doc-bit))
        )
    (dump-doc-bit-html name "Package" doc-string out)))



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
           (<:link :rel "stylesheet" :href "../clstyle.css"))

          (<:body
           (<:h1 (<:i "Function") (<:strong name))
           (<:h2 "Package:")
           (<:h2 "Description:" (<:br) doc-string)
           ))
         :syntax :compact))
    t))

;;;---------------------------------------------------------------------------
;;; Auxiliary files production.

(defmethod frameset-head-title ((fs frameset))
  "FOOO")


(defmethod frameset-body-title ((fs frameset))
  "FFFFOOOOO")


(defun produce-header-file (fs header-pathname documentation-title)
  (declare (type frameset fs)
           (type pathname header-pathname))
  (declare (ignorable documentation-title))
  (let ((fs-order (frameset-order fs))
        (fs-head-title (frameset-head-title fs))
        (fs-body-title (frameset-body-title fs))
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
              (<:link :rel "stylesheet" :href "clstyle.css"))

             ((<:body :style "margin: 0pt 0pt 0pt 0pt;")
              ((<:div :class "header"
                      :style "padding-left: 2em; padding-top: 5pt; color: #41286f; font-size: 14pt")
               (<:strong (or documentation-title fs-body-title))
               ((<:div :class "navigation"
                       :style "right: 2m")
                ((<:a :href "index.htm"
                      :class "navigation-link-selected"
                      :target "_parent")
                 "Home"))
               )
              )))
           :syntax :compact))
      ))
  )


(defun produce-navigation-file (fs nav-pathname documentation-title)
  (declare (type frameset fs)
           (type pathname nav-pathname))
  (declare (ignorable documentation-title))
  (let ((fs-order (frameset-order fs))
        (fs-head-title (frameset-head-title fs))
        (fs-body-title (frameset-body-title fs))
        )
    (with-open-file (ns nav-pathname
                        :direction :output
                        :if-exists :supersede
                        :if-does-not-exist :create)
      (<:with-html-syntax (ns :print-pretty t)
          (<:htmlize
           (<:document
            (<:comment (base-name nav-pathname))
            +doctype-xhtml1-string-control-string+
            (<:html
             (<:head
              (<:title fs-head-title)
              (<:link :rel "stylesheet" :href "clstyle.css"))

             (<:body
              (<:ul))
             ))
           :syntax :compact))
      ))
  )


(defun produce-footer-file (fs footer-pathname documentation-title)
  (declare (type frameset fs)
           (type pathname footer-pathname))
  (declare (ignorable documentation-title))
  (let ((fs-order (frameset-order fs))
        (fs-head-title (frameset-head-title fs))
        (fs-body-title (frameset-body-title fs))
        )
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
              (<:link :rel "stylesheet" :href "clstyle.css"))

             ((<:body :style "margin: 0pt 0pt 0pt 0pt;")
              ((<:div :class "copyright"
                      #| :style "padding-left: 2em; padding-top: 5pt; color: #41286f; font-size: 14pt"|#)
               (<:strong (or documentation-title fs-body-title))
               "documentation produced with"
               "HE&Lambda;P"
               (<:br)
               (<:comment "hhmts start")
               "Last modified: Thu Aug 25 17:03:36 EEST 2011"
               (<:comment "hhmts end")
               (<:br)

               "&copy; 2011, Marco Antoniotti, all rights reserved.")
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

;;;; -*- Mode: Lisp -*-

;;;; xhtml-producer.lisp --
;;;; Make a file out of a doc-bit.

(in-package "HELAMBDAP")

(use-package "CL-WHO")

;;;;===========================================================================
;;;; Protocol.


(defgeneric produce-frame (format element out)
  )


(defgeneric produce-navigation-frame (format nav element out doc-bits)
  )


(defgeneric produce-header-frame (format frameset frameset-stream where doc-bits doc-title)
  )


(defgeneric produce-footer-frame (format frameset frameset-stream where doc-bits)
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
    (produce-documentation 'html c where doc-bits)
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
    (produce-documentation 'html fs where doc-bits)))


(defmethod produce-documentation ((format (eql 'html))
                                  (structure frameset)
                                  (where pathname)
                                  doc-bits
                                  &key
                                  (documentation-title)
                                  &allow-other-keys
                                  )
  (declare (ignorable documentation-title))
  (flet ((produce-frameset-file ()
           (let* ((fs-name (frameset-name structure))
                  (fs-filename (make-pathname :name fs-name
                                              :type "html"
                                              :defaults where))
                  (fs-header (frameset-header structure))
                  (fs-content (frameset-content structure))
                  (fs-navigation (frameset-navigation structure))
                  (fs-footer (frameset-footer structure))
                  (fs-location (frameset-location structure))
                  (where (if fs-location
                             (merge-pathnames fs-location where)
                             where))
                  #|(*xhtml-indent* (if (zerop *xhtml-indent*)
                                      *xhtml-indent*
                                      (+ 4 *xhtml-indent*)))
                  |#
                  ;; (cl-who::*indent* 1)
                  )

             ;; (format t ">>>> Indentation is ~D~%" cl-who::*indent*)

             (ensure-directories-exist where)

             (with-open-file (fs-file fs-filename
                                      :direction :output
                                      :if-does-not-exist :create
                                      :if-exists :supersede)
               (with-html-output (fs-file fs-file :indent 4)
                 (fmt "<!-- ~A.htm -->~2%" fs-name)
                 (fmt +doctype-frameset-control-string+)
                 (:html
                  (:head
                   (:title (str fs-name))
                   (:link :rel "stylesheet" :href "clstyle.css"))
                  (:body
                   (:frameset :rows "65px,*" :border 0 :noresize "noresize"
                    ;; HEADER ROW.
                    (let () ; ((cl-who::*indent* (+ 4 cl-who::*indent*)))
                      (produce-header-frame 'html
                                            structure
                                            fs-file
                                            where
                                            doc-bits
                                            documentation-title))
                    #|(when (non-empty-string-p fs-header)
                      (htm (:frame :src fs-header)))|#
                    
                    ;; NAVIGATION/CONTENT/SIDEBAR ROW.
                    (:frameset :cols "150px,*" :border 0
                     (when (non-empty-string-p fs-navigation)
                       (produce-navigation-frame format
                                                 fs-navigation
                                                 structure
                                                 fs-file
                                                 doc-bits))
                     
                     (fmt "~&<!-- CONTENT ~S -->~%" fs-content)
                     (if fs-content
                         (progn
                           (produce-documentation format (first fs-content) fs-file doc-bits)
                           (produce-frame format (first fs-content) fs-file)
                           ;; (htm (:frame :src (file-pathname (first fs-content))))
                           )
                         (htm (:frame :src (element-name structure)))))

                    ;; FOOTER ROW.
                    (when (non-empty-string-p fs-footer)
                      (htm (:frame :src fs-footer)))
                    
                    (:noframes
                     (:h2 "Frame Alert")
                     (:p (fmt "This document is designed to be viewed using the frames feature. If~@
                               you see this message, you are using a non-frame-capable web client.")
                      (:br)
                      "Link to "
                      (:a :href "overview-summary.html" "Non-frame version.")))
                    )))

                 (fmt "~&<!-- end of file : ~A.htm -->~%" fs-name)
                 ))))
         )
    (produce-frameset-file)
    ))


(defmethod produce-frame ((format (eql 'html))
                          (element file-set)
                          (where stream)
                          )
  (declare (ignorable documentation-title))
  (format where "~&~%<!-- FILE-SET ~S -->~2%" (element-name element))
  (with-html-output (out where :indent t)
    (htm (:frame :src (element-name element)))))


(defmethod produce-frame ((format (eql 'html))
                          (element doc-file)
                          (where stream)
                          )
  (declare (ignorable documentation-title))
  (format where "~&~%<!-- FRAME ~S -->~2%" (element-name element))
  (with-html-output (out where :indent t)
    (htm (:frame :src (file-pathname element)))))


(defmethod produce-header-frame ((format (eql 'html))
                                 (fs frameset)
                                 (fs-file stream)
                                 (where pathname)
                                 doc-bits
                                 documentation-title
                                 )
  (declare (ignorable documentation-title))
  (let ((header (frameset-header fs)))
    (unless (or (null header) (string= header ""))
      (let ((header-pathname
             (merge-pathnames header
                               (make-pathname :type "html"))
             #|(merge-pathnames
              (merge-pathnames header
                               (make-pathname :type "html"))
              where)
|#
             )
            )
             
        (unless (probe-file header-pathname)
          (produce-header-file fs header-pathname documentation-title))
        #+nodebug (format fs-file "~&~%<!-- FRAME HEADER ~S (~D) -->~%"
                         header-pathname
                         *xhtml-indent*)
        (with-html-output (out fs-file)
          (htm (:frame :src header-pathname)))
        ))))


(defmethod produce-navigation-frame ((format (eql 'html))
                                     nav
                                     element
                                     (where stream)
                                     doc-bits
                                     )
  (declare (ignorable documentation-title))
  (format where "~&~%<!-- FRAME NAVIGATION ~S -->~2%" (element-name element))
  (with-html-output (out where :indent t)
    (htm (:frame :src nav)))
  )


(defmethod produce-documentation ((format (eql 'html))
                                  (structure file-set)
                                  (where pathname)
                                  doc-bits
                                  &key
                                  documentation-title
                                  &allow-other-keys)
  (declare (ignorable documentation-title))
  (with-html-output (out where :indent t)
    (htm (:frame :src (element-name structure)))
    )
  )


(defmethod produce-documentation ((format (eql 'html))
                                  (structure file-set)
                                  (where file-stream)
                                  doc-bits
                                  &key
                                  (documentation-title)
                                  &allow-other-keys
                                  )
  (declare (ignorable documentation-title))
  (format where "~&~%<!-- FRAME DOC FILE-SET ~S -->~2%" (element-name structure))
  (with-html-output (out where :indent t)
    (htm (:frame :src (element-name structure)))
    )
  )


(defun dump-doc-bit-html (name str-tag doc-string out)
  (with-html-output (out out :indent t)
      (htm (:head
            (:title (fmt "~A ~A" str-tag (string-downcase name)))
            (:link :rel "stylesheet" :href "../clstyle.css")
           (:body
            (:h1 (:i (fmt "~A " str-tag)) (:strong (str name)))
            (:h2 "Package: ") (:p (str (package-name (symbol-package name))))
            (:h2 "Description:") (:p (str doc-string)))
            ))))


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

    (with-html-output (out out :indent t)
      (htm (:head
            (:title (fmt "DOC FOR ~A" (string-downcase name)))
            (:link :rel "stylesheet" :href "../clstyle.css")
           (:body
            (:h1 (:i "Function") (:strong (str name)))
            (:h2 "Package:")
            (:h2 "Description:" (terpri out) (str doc-string))
            )))))
    t)

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
      (with-html-output (hs hs)
        (fmt "<!-- ~A.html -->" (cl-fad:pathname-as-file header-pathname))
        (fmt +doctype-frameset-control-string+)
        (:html
         (:head
          (:title (str fs-head-title))
          (:link :rel "stylesheet" :href "clstyle.css"))
         (:body :style "margin: 0pt 0pt 0pt 0pt;"
          (:div
           :class "header"
           :style "padding-left: 2em; padding-top: 5pt; color: #41286f; font-size: 14pt"
           (:strong (str fs-body-title))
           (:div
            :class "navigation"
            :style "right: 2m"
            (:a
             :href "index.html"
             :class "navigation-link-selected"
             :target "_parent"
             (str "Home"))
            )))))
      )))


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

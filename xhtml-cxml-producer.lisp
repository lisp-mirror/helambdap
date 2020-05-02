;;;; -*- Mode: Lisp -*-

;;;; xhtml-cxml-producer.lisp --
;;;; Make a file out of a doc-bit.

(in-package "HELAMBDAP")

(eval-when (:compile-toplevel :execute)
  (use-package "XHTML-GENERATOR")
  (editor:setup-indent "with-html" 1 2 4))


;;;;===========================================================================
;;;; Protocol.


(defgeneric produce-frame (format element out)
  )


(defgeneric produce-navigation-frame (format nav element out where doc-bits doc-title)
  )


(defgeneric produce-navigation-file (content nav-pathname documentation-title)
  )


(defgeneric produce-header-frame (format frameset frameset-stream where doc-bits doc-title)
  )


(defgeneric produce-footer-frame (format frameset frameset-stream where doc-bits)
  )


(defparameter *xhtml-indent* 4) ; 4 is the actual initial value.


;;;;===========================================================================
;;;; Implementation.

(defvar *html-type-pathname* (make-pathname :type "html"))


;;;;---------------------------------------------------------------------------
;;;; PRODUCE-DOCUMENTATION methods.

;;; html documentation-structure --

(defmethod produce-documentation ((format (eql 'html)) ;
                                  (structure documentation-structure)
                                  (where pathname)
                                  doc-bits
                                  &key
                                  &allow-other-keys
                                  )
  (ensure-directories-exist where)
  (dolist (c (documentation-structure-structure structure))
    (produce-documentation 'html c where doc-bits)
    ))


;;; html style-file --

(defmethod produce-documentation ((format (eql 'html))
                                  (structure style-file)
                                  (where pathname)
                                  doc-bits
                                  &key
                                  &allow-other-keys
                                  )
  (let ((doc-directory where)
        (sfn (style-file-name structure))
        )
    (cl-fad:copy-file sfn
                      (make-pathname :directory (pathname-directory doc-directory)
                                     :name (pathname-name sfn)
                                     :type (pathname-type sfn))
                      :overwrite t)
    ))


;;; html doc-file --

(defmethod produce-documentation ((format (eql 'html))
                                  (structure doc-file)
                                  (where pathname)
                                  doc-bits
                                  &key
                                  &allow-other-keys
                                  )
  (let* ((doc-directory where)
         (dfn (doc-file-name structure))
         (destination-path
          (merge-pathnames
           (make-pathname :directory (pathname-directory doc-directory)
                          :name (pathname-name dfn)
                          :type (pathname-type dfn))
           *html-type-pathname*))
         )
    (when (probe-file dfn)
      (cl-fad:copy-file dfn destination-path :overwrite nil))
    ))


(defmethod produce-documentation ((format (eql 'html))
                                  (structure doc-file)
                                  (where cxml::sink)
                                  doc-bits
                                  &key
                                  &allow-other-keys
                                  )
  
  (produce-documentation 'html
                         structure
                         (runes::ystream-target-stream (cxml::sink-ystream where))
                         doc-bits
                         ))


(defmethod produce-documentation ((format (eql 'html))
                                  (structure doc-file)
                                  (where file-stream)
                                  doc-bits
                                  &key
                                  &allow-other-keys
                                  )
  (let* ((doc-directory where)
         (dfn (doc-file-name structure))
         (destination-path
          (merge-pathnames
           (make-pathname :directory (pathname-directory doc-directory)
                          :name (pathname-name dfn)
                          :type (or (pathname-type dfn) "html"))
           *html-type-pathname*))
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

;;; html framesets --

(defmethod produce-documentation ((format (eql 'html))
                                  (fss framesets)
                                  (where pathname)
                                  doc-bits
                                  &key
                                  &allow-other-keys
                                  )
  (dolist (fs (framesets-list fss))
    (produce-documentation 'html fs where doc-bits)))


;;; html frameset --

(defmethod produce-documentation ((format (eql 'html))
                                  (structure frameset)
                                  (where pathname)
                                  doc-bits
                                  &key
                                  &allow-other-keys
                                  )
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
                  (doc-title (property (element-doc-structure structure)
                                       :documentation-title
                                       (element-name structure)))
                  )

             (format t "~&>>>> DOC TITLE is ~S~%" doc-title)

             (ensure-directories-exist where)
             
             (with-open-file (fs-file fs-filename
                                      :direction :output
                                      :if-does-not-exist :create
                                      :if-exists :supersede)
               (let ((fs-file (cxml:make-character-stream-sink fs-file :indentation 4)))
                 (sax:start-document fs-file)
                 (write-doctype fs-file)
                 (with-html fs-file
                   ;; (fmt "<!-- ~A.htm -->~2%" fs-name)
                   ;; (fmt +doctype-frameset-control-string+)
                   (:html

                    (:head
                     (:title (:princ-safe doc-title))
                     ((:link :rel "stylesheet" :href "clstyle.css")))

                    (:body
                     ((:frameset :rows "65px,*" :border "0" :noresize "noresize")
                      ;; HEADER ROW.
                      (produce-header-frame 'html
                                            structure
                                            fs-file
                                            where
                                            doc-bits
                                            doc-title
                                            ))
                    
                     ;; NAVIGATION/CONTENT/SIDEBAR ROW.
                     ((:frameset :cols "150px,*" :border "0")

                      ;; NAVIGATION.
                      (when (non-empty-string-p fs-navigation)
                        (produce-navigation-frame format
                                                  fs-navigation
                                                  structure
                                                  fs-file
                                                  where
                                                  doc-bits
                                                  doc-title))
                     
                      ;; CONTENT.
                      (if fs-content
                          (progn
                            (produce-documentation format (first fs-content) where doc-bits)
                            (produce-frame format (first fs-content) fs-file))
                          (with-html fs-file ((:frame :src (element-name structure))))))

                     ;; FOOTER ROW.
                     (when (non-empty-string-p fs-footer)
                       (with-html fs-file ((:frame :src fs-footer))))
                    
                     (:noframes
                      (:h2 "Frame Alert")
                      (:p
                       "This document is designed to be viewed using the frames feature. If"
                       "you see this message, you are using a non-frame-capable web client."
                       :br
                       "Link to "
                       ((:a :href "overview-summary.html") "Non-frame version.")))
                     ) ; BODY
                    (:comment (format nil " end of file : ~A.htm " fs-name))
                    )

                 )
                 (sax:end-document fs-file)))))
         )
    (produce-frameset-file)
    ))




#|
(defmethod produce-documentation ((format (eql 'html))
                                  (structure file-set)
                                  (where pathname)
                                  doc-bits
                                  &key
                                  documentation-title
                                  &allow-other-keys)
  (declare (ignorable documentation-title))
  (with-html where
    (htm (:frame :src (element-name structure)))
    )
  )
|#

(defmethod produce-documentation ((format (eql 'html))
                                  (structure file-set)
                                  (where cxml::sink)
                                  doc-bits
                                  &key
                                  &allow-other-keys
                                  )
  (with-html where
    (:comment (format nil " FRAME DOC FILE-SET ~S " (element-name structure)))
    ((:frame :src (element-name structure)))
    )
  )


(defmethod produce-documentation ((format (eql 'html))
                                  structure
                                  (out cxml::sink)
                                  (doc-bit function-doc-bit)
                                  &key
                                  &allow-other-keys
                                  )
  (let ((name (doc-bit-name doc-bit))
        (doc-string (doc-bit-doc-string doc-bit))
        )
    (dump-doc-bit-html name "Function" doc-string out)))



(defmethod produce-documentation ((format (eql 'html))
                                  structure
                                  (out cxml::sink)
                                  (doc-bit macro-doc-bit)
                                  &key
                                  &allow-other-keys
                                  )
  (let ((name (doc-bit-name doc-bit))
        (doc-string (doc-bit-doc-string doc-bit))
        )
    (dump-doc-bit-html name "Macro" doc-string out)))


(defmethod produce-documentation ((format (eql 'html))
                                  structure
                                  (out cxml::sink)
                                  (doc-bit compiler-macro-doc-bit)
                                  &key
                                  &allow-other-keys
                                  )
  (let ((name (doc-bit-name doc-bit))
        (doc-string (doc-bit-doc-string doc-bit))
        )
    (dump-doc-bit-html name "Compiler Macro" doc-string out)))


(defmethod produce-documentation ((format (eql 'html))
                                  structure
                                  (out cxml::sink)
                                  (doc-bit setf-expander-doc-bit)
                                  &key
                                  &allow-other-keys
                                  )
  (let ((name (doc-bit-name doc-bit))
        (doc-string (doc-bit-doc-string doc-bit))
        )
    (dump-doc-bit-html name "Setf Expander" doc-string out)))


(defmethod produce-documentation ((format (eql 'html))
                                  structure
                                  (out cxml::sink)
                                  (doc-bit modify-macro-doc-bit)
                                  &key
                                  &allow-other-keys
                                  )
  (let ((name (doc-bit-name doc-bit))
        (doc-string (doc-bit-doc-string doc-bit))
        )
    (dump-doc-bit-html name "Modify Macro" doc-string out)))


(defmethod produce-documentation ((format (eql 'html))
                                  structure
                                  (out cxml::sink)
                                  (doc-bit generic-function-doc-bit)
                                  &key
                                  &allow-other-keys
                                  )
  (let ((name (doc-bit-name doc-bit))
        (doc-string (doc-bit-doc-string doc-bit))
        )
    (dump-doc-bit-html name "Generic Function" doc-string out)))


(defmethod produce-documentation ((format (eql 'html))
                                  structure
                                  (out cxml::sink)
                                  (doc-bit type-doc-bit)
                                  &key
                                  &allow-other-keys
                                  )
  (let ((name (doc-bit-name doc-bit))
        (doc-string (doc-bit-doc-string doc-bit))
        )
    (dump-doc-bit-html name "Type" doc-string out)))


(defmethod produce-documentation ((format (eql 'html))
                                  structure
                                  (out cxml::sink)
                                  (doc-bit class-doc-bit)
                                  &key
                                  &allow-other-keys
                                  )
  (let ((name (doc-bit-name doc-bit))
        (doc-string (doc-bit-doc-string doc-bit))
        )
    (dump-doc-bit-html name "Class" doc-string out)))


(defmethod produce-documentation ((format (eql 'html))
                                  structure
                                  (out cxml::sink)
                                  (doc-bit condition-doc-bit)
                                  &key
                                  &allow-other-keys
                                  )
  (let ((name (doc-bit-name doc-bit))
        (doc-string (doc-bit-doc-string doc-bit))
        )
    (dump-doc-bit-html name "Condition" doc-string out)))


(defmethod produce-documentation ((format (eql 'html))
                                  structure
                                  (out cxml::sink)
                                  (doc-bit method-combination-doc-bit)
                                  &key
                                  &allow-other-keys
                                  )
  (let ((name (doc-bit-name doc-bit))
        (doc-string (doc-bit-doc-string doc-bit))
        )
    (dump-doc-bit-html name "Method Combination" doc-string out)))


(defmethod produce-documentation ((format (eql 'html))
                                  structure
                                  (out cxml::sink)
                                  (doc-bit package-doc-bit)
                                  &key
                                  &allow-other-keys
                                  )
  (let ((name (doc-bit-name doc-bit))
        (doc-string (doc-bit-doc-string doc-bit))
        )
    (dump-doc-bit-html name "Package" doc-string out)))



(defmethod produce-documentation ((format (eql 'html))
                                  structure
                                  (out stream)
                                  doc-bit
                                  &key
                                  &allow-other-keys
                                  )
  (let ((name (doc-bit-name doc-bit))
        (doc-string (doc-bit-doc-string doc-bit))
        )

    (with-html out
               (:head
                (:title (format nil "DOC FOR ~A" (string-downcase name)))
                ((:link :rel "stylesheet" :href "../clstyle.css"))
                (:body
                 (:h1 (:i "Function") (:strong name))
                 (:h2 "Package:")
                 (:h2 "Description:" (terpri out) doc-string)
                 ))))
    t)


;;;---------------------------------------------------------------------------
;;; Frames production.

(defmethod produce-frame ((format (eql 'html))
                          (element file-set)
                          (where cxml::sink)
                          )

  (with-html where
    (:comment (format nil " FILE-SET ~S " (element-name element)))
    ((:frame :src (element-name element)))))


(defmethod produce-frame ((format (eql 'html))
                          (element doc-file)
                          (where cxml::sink)
                          )
  (with-html where
    (:comment (format nil " FRAME ~S " (element-name element)))
    ((:frame :src (namestring (file-pathname element))))))


(defmethod produce-header-frame ((format (eql 'html))
                                 (fs frameset)
                                 (fs-file cxml::sink)
                                 (where pathname)
                                 doc-bits
                                 documentation-title
                                 )
  (let ((header (frameset-header fs)))
    (unless (or (null header) (string= header ""))
      (let* ((header-file-pathname
              (merge-pathnames header
                               (make-pathname :type "html")))
             (header-full-pathname
              (merge-pathnames header-file-pathname where))
             )
             
        #+helambdap-check-overwrite
        (unless (probe-file header-full-pathname)
          (produce-header-file fs header-full-pathname documentation-title))

        #-helambdap-check-overwrite
        (produce-header-file fs header-full-pathname documentation-title)

        (with-html fs-file
          ((:frame :src (namestring header-file-pathname))))
        ))))


(defmethod produce-navigation-frame ((format (eql 'html))
                                     nav
                                     (element frameset)
                                     (out cxml::sink)
                                     (where pathname)
                                     doc-bits
                                     documentation-title
                                     )
  (when (and nav (string/= "" nav))
    (let* ((nav-file-pathname
            (merge-pathnames nav (make-pathname :type "html")))
           (nav-full-pathname
            (merge-pathnames nav-file-pathname where))
           )
            
      (let ((fs-content (frameset-content element)))
        (when fs-content
          (produce-navigation-file (first fs-content)
                                   nav-full-pathname
                                   documentation-title
                                   )))

      (with-html where
        (:comment (format nil " FRAME NAVIGATION ~S " (element-name element)))
        ((:frame :src nav)))
      )))


;;;---------------------------------------------------------------------------
;;; DOC-BIT production.

(defun dump-doc-bit-html (name str-tag doc-string out)
  (declaim (type string str-tag doc-string)
           (type cxml::sink out))
  (xhtml-generator:with-html out
      (:html
       (:head
        (:title (format nil "~A ~A" str-tag (string-downcase name)))
        ((:link :rel "stylesheet" :href "../clstyle.css"))
        (:body
         (:h1 (:i (format nil "~A " str-tag)) (:strong name))
         (:h2 "Package: ") (:p (package-name (symbol-package name)))
         (:h2 "Description:") (:p doc-string))
        ))))


;;;---------------------------------------------------------------------------
;;; Auxiliary files production.

(defmethod frameset-head-title ((fs frameset))
  "FOOO")


(defmethod frameset-body-title ((fs frameset))
  "FFFFOOOOO")


(defun produce-header-file (fs header-pathname documentation-title)
  (declare (type frameset fs)
           (type pathname header-pathname))
  (let ((fs-order (frameset-order fs))
        (fs-head-title documentation-title #|(frameset-head-title fs)|#)
        (fs-body-title documentation-title #|(frameset-body-title fs)|#)
        )
    (with-open-file (hs header-pathname
                        :direction :output
                        :if-exists :supersede
                        :if-does-not-exist :create)
      (let ((hs (cxml:make-character-stream-sink hs :indentation 4)))
        (sax:start-document hs)
        (xhtml-generator:with-html hs
          (:comment (:princ-safe (format nil " ~A.html "
                                         (cl-fad:pathname-as-file header-pathname)))))
        (xhtml-generator:write-doctype hs)
        (xhtml-generator:with-html hs
          (:html

           (:head
            (:title (:princ-safe fs-head-title))
            ((:link :rel "stylesheet" :href "clstyle.css")))

           ((:body :style "margin: 0pt 0pt 0pt 0pt;")
            ((:div
              :class "header"
              :style "padding-left: 2em; padding-top: 5pt; color: #41286f; font-size: 14pt")
             (:strong fs-body-title)
             ((:div
              :class "navigation"
              :style "right: 2m")
              ((:a
               :href "index.html"
               :class "navigation-link-selected"
               :target "_parent")
               "Home")
              "|"
              ((:a
               :href "dictionary/dictionary-frame.html"
               :class "navigation-link"
               :target "_parent")
               "Dictionary")
              )
             )))
          )
        (sax:end-document hs)))))


(defmethod produce-navigation-file ((content doc-file)
                                    (nav-pathname pathname)
                                    documentation-title)
  (let ((ns-head-title documentation-title #|(frameset-head-title fs)|#)
        (ns-body-title documentation-title #|(frameset-body-title fs)|#)
        )
    (with-open-file (ns nav-pathname
                        :direction :output
                        :if-exists :supersede
                        :if-does-not-exist :create)
      (let ((ns (cxml:make-character-stream-sink ns :indentation 4)))
        (sax:start-document ns)
        (xhtml-generator:with-html ns
          (:comment (:princ-safe (format nil " ~A.html "
                                         (cl-fad:pathname-as-file nav-pathname)))))
        (xhtml-generator:write-doctype ns)
        (xhtml-generator:with-html ns
          (:html

           (:head
            (:title (:princ-safe ns-head-title))
            ((:link :rel "stylesheet" :href "clstyle.css")))

           (:body
            (:ul
             (:li "==> Navigation")
             )
            ))
          )
        (sax:end-document ns)))))



(defmethod produce-navigation-file ((content file-set)
                                    (nav-pathname pathname)
                                    documentation-title)
  (let ((ns-head-title documentation-title #|(frameset-head-title fs)|#)
        (ns-body-title documentation-title #|(frameset-body-title fs)|#)
        )
    (with-open-file (ns nav-pathname
                        :direction :output
                        :if-exists :supersede
                        :if-does-not-exist :create)
      (let ((ns (cxml:make-character-stream-sink ns :indentation 4)))
        (sax:start-document ns)
        (xhtml-generator:with-html ns
          (:comment (:princ-safe (format nil " ~A.html "
                                         (cl-fad:pathname-as-file nav-pathname)))))
        (xhtml-generator:write-doctype ns)
        (xhtml-generator:with-html ns
          (:html

           (:head
            (:title (:princ-safe ns-head-title))
            ((:link :rel "stylesheet" :href "clstyle.css")))

           (:body
            (:ul
             (:li "==> Items list")
             )
            ))
          )
        (sax:end-document ns)))))


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

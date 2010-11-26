;;;; -*- Mode: Lisp -*-

;;;; xhtml-producer.lisp --
;;;; Make a file out of a doc-bit.

(in-package "HELAMBDAP")


;;;; Protocol.

(defgeneric produce-documentation (format element out doc-bits)
  (:documentation "Produces documentation for ELEMENT according to FORMAT.")
  )


(defgeneric produce-frame (format element out)
  )


(defgeneric produce-navigation-frame (format nav element out doc-bits)
  )


(defgeneric produce-header-frame (format frameset frameset-stream where doc-bits)
  )


(defgeneric produce-footer-frame (format frameset frameset-stream where doc-bits)
  )


;;;; Implementation.

(defmethod produce-documentation ((format (eql 'html)) ;
                                  (structure documentation-structure)
                                  (where pathname)
                                  doc-bits)
  (ensure-directories-exist where)
  (dolist (c (documentation-structure-structure structure))
    (produce-documentation 'html c where doc-bits)
    ))


(defmethod produce-documentation ((format (eql 'html))
                                  (structure style-file)
                                  (where pathname)
                                  doc-bits)
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
                                  doc-bits)
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
                                  doc-bits)
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


(defmethod produce-documentation ((format (eql 'html))
                                  (structure framesets)
                                  (where pathname)
                                  doc-bits)
  )


(declaim (type string +doctype-frameset-control-string+))

(defconstant +doctype-frameset-control-string+
"<!DOCTYPE HTML PUBLIC
  \"-//W3C//DTD HTML 4.01 Frameset//EN\"
  \"http://www.w3.org/TR/html4/frameset.dtd\">"

  "The standard 'DOCTYPE' w3c frameset DTD (X)HTML string.")


(defmethod produce-documentation ((format (eql 'html))
                                  (structure frameset)
                                  (where pathname)
                                  doc-bits)
  (flet ((produce-frameset-file ()
           (let* ((fs-name (frameset-name structure))
                  (fs-filename (make-pathname :name fs-name
                                              :type "htm"
                                              :defaults where))
                  (fs-header (frameset-header structure))
                  (fs-content (frameset-content structure))
                  (fs-navigation (frameset-navigation structure))
                  (fs-footer (frameset-footer structure))
                  (fs-location (frameset-location structure))
                  (where (if fs-location
                             (merge-pathnames fs-location where)
                             where))
                  )

             (ensure-directories-exist where)

             (with-open-file (fs-file fs-filename
                                      :direction :output
                                      :if-does-not-exist :create
                                      :if-exists :supersede)
               (with-html-output (fs-file fs-file :indent t)
                 (fmt "<!-- ~A.htm -->~2%" fs-name)
                 (fmt +doctype-frameset-control-string+)
                 (:html
                  (:head
                   (:title (str fs-name))
                   (:link :rel "stylesheet" :href "clstyle.css"))
                  (:body
                   (:frameset :rows "65px,*" :border 0 :noresize "noresize"
                    (produce-header-frame 'html structure fs-file where)
                    #|(when (non-empty-string-p fs-header)
                      (htm (:frame :src fs-header)))|#
                    
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
  (with-html-output (out where :indent cl-who::*indent*)
    (htm (:frame :src (element-name element)))))


(defmethod produce-frame ((format (eql 'html))
                          (element doc-file)
                          (where stream)
                          )
  (with-html-output (out where :indent cl-who::*indent*)
    (htm (:frame :src (file-pathname element)))))


(defmethod produce-header-frame ((format (eql 'html))
                                 (fs frameset)
                                 (fs-file stream)
                                 (where pathname)
                                 doc-bits
                                 )
  (let ((header (frameset-header fs)))
    (unless (or (null header) (string= ""))
      (let ((header-pathname
             (merge-pathnames
              (merge-pathnames header
                               (make-pathname :type "html"))
              where))
            )
             
        (unless (probe-file header-pathname)
          (produce-header-file fs header-pathname))
        (with-html-output (out fs-file :indent cl-who::*indent*)
          (htm (:frame :src header)))
        ))))


(defmethod produce-navigation-frame ((format (eql 'html))
                                     nav
                                     element
                                     (where stream)
                                     doc-bits
                                     )
  (with-html-output (out where :indent cl-who::*indent*)
    (htm (:frame :src nav)))
  )


(defmethod produce-documentation ((format (eql 'html))
                                  (structure file-set)
                                  (where pathname)
                                  doc-bits)
  (with-html-output (out where :indent cl-who::*indent*)
    (htm (:frame :src (element-name structure)))
    )
  )


(defmethod produce-documentation ((format (eql 'html))
                                  (structure file-set)
                                  (where file-stream)
                                  doc-bits)
  (with-html-output (out where :indent cl-who::*indent*)
    (htm (:frame :src (element-name structure)))
    )
  )


(defun dump-doc-bit-html (name str-tag doc-string out)
  (with-html-output (out)
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
                                  (doc-bit function-doc-bit))
  (let ((name (doc-bit-name doc-bit))
        (doc-string (doc-bit-doc-string doc-bit))
        )
    (dump-doc-bit-html name "Function" doc-string out)))



(defmethod produce-documentation ((format (eql 'html))
                                  structure
                                  (out file-stream)
                                  (doc-bit macro-doc-bit))
  (let ((name (doc-bit-name doc-bit))
        (doc-string (doc-bit-doc-string doc-bit))
        )
    (dump-doc-bit-html name "Macro" doc-string out)))


(defmethod produce-documentation ((format (eql 'html))
                                  structure
                                  (out file-stream)
                                  (doc-bit compiler-macro-doc-bit))
  (let ((name (doc-bit-name doc-bit))
        (doc-string (doc-bit-doc-string doc-bit))
        )
    (dump-doc-bit-html name "Compiler Macro" doc-string out)))


(defmethod produce-documentation ((format (eql 'html))
                                  structure
                                  (out file-stream)
                                  (doc-bit setf-expander-doc-bit))
  (let ((name (doc-bit-name doc-bit))
        (doc-string (doc-bit-doc-string doc-bit))
        )
    (dump-doc-bit-html name "Setf Expander" doc-string out)))


(defmethod produce-documentation ((format (eql 'html))
                                  structure
                                  (out file-stream)
                                  (doc-bit modify-macro-doc-bit))
  (let ((name (doc-bit-name doc-bit))
        (doc-string (doc-bit-doc-string doc-bit))
        )
    (dump-doc-bit-html name "Modify Macro" doc-string out)))


(defmethod produce-documentation ((format (eql 'html))
                                  structure
                                  (out file-stream)
                                  (doc-bit generic-function-doc-bit))
  (let ((name (doc-bit-name doc-bit))
        (doc-string (doc-bit-doc-string doc-bit))
        )
    (dump-doc-bit-html name "Generic Function" doc-string out)))


(defmethod produce-documentation ((format (eql 'html))
                                  structure
                                  (out file-stream)
                                  (doc-bit type-doc-bit))
  (let ((name (doc-bit-name doc-bit))
        (doc-string (doc-bit-doc-string doc-bit))
        )
    (dump-doc-bit-html name "Type" doc-string out)))


(defmethod produce-documentation ((format (eql 'html))
                                  structure
                                  (out file-stream)
                                  (doc-bit class-doc-bit))
  (let ((name (doc-bit-name doc-bit))
        (doc-string (doc-bit-doc-string doc-bit))
        )
    (dump-doc-bit-html name "Class" doc-string out)))


(defmethod produce-documentation ((format (eql 'html))
                                  structure
                                  (out file-stream)
                                  (doc-bit condition-doc-bit))
  (let ((name (doc-bit-name doc-bit))
        (doc-string (doc-bit-doc-string doc-bit))
        )
    (dump-doc-bit-html name "Condition" doc-string out)))


(defmethod produce-documentation ((format (eql 'html))
                                  structure
                                  (out file-stream)
                                  (doc-bit method-combination-doc-bit))
  (let ((name (doc-bit-name doc-bit))
        (doc-string (doc-bit-doc-string doc-bit))
        )
    (dump-doc-bit-html name "Method Combination" doc-string out)))


(defmethod produce-documentation ((format (eql 'html))
                                  structure
                                  (out file-stream)
                                  (doc-bit package-doc-bit))
  (let ((name (doc-bit-name doc-bit))
        (doc-string (doc-bit-doc-string doc-bit))
        )
    (dump-doc-bit-html name "Package" doc-string out)))






(defmethod produce-documentation ((format (eql 'html)) structure (out stream) doc-bit)
  (let ((name (doc-bit-name doc-bit))
        (doc-string (doc-bit-doc-string doc-bit))
        )

    (with-html-output (out)
      (htm (:head
            (:title (fmt "DOC FOR ~A" (string-downcase name)))
            (:link :rel "stylesheet" :href "../clstyle.css")
           (:body
            (:h1 (:i "Function") (:strong (str name)))
            (:h2 "Package:")
            (:h2 "Description:" (terpri out) (str doc-string))
            )))))
    t)


;;;; end of file -- xhtml-producer.lisp --

;;;; -*- Mode: Lisp -*-

;;;; xhtml-lambda-producer.lisp --
;;;; Make a file out of a DOCUMENTATION-STRUCTURE and a set (list) of
;;;; DOC-BITs, using a (X)HTML output format.
;;;;
;;;; See file COPYING for copyright and licensing information.

(in-package "HELAMBDAP")


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

;;;; Notes:
;;;; The layout already somewhat reflects HTML5 elements.


;;;;===========================================================================
;;;; Protocol.

#| Moved in separate file.
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

(defparameter *formatted-section-right-margin* 256)
|#


;;;;===========================================================================
;;;; Implementation.

(defmethod produce-documentation ((format (eql :html))
                                  element
                                  out
                                  doc-bits
                                  &key
                                  documentation-title
                                  &allow-other-keys
                                  )
  "This specialized method produces the (X)HTML documentation for an ELEMENT."
  (produce-documentation 'html element out doc-bits
                         :documentation-title documentation-title))


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
  (declare (ignorable doc-bits documentation-title))
  (let ((doc-directory where)
        (sfn (style-file-name structure))
        )
    (cl-fad:copy-file sfn
                      (make-pathname
                       :directory (pathname-directory doc-directory)
                       :name (pathname-name sfn)
                       :type (pathname-type sfn))
                      :overwrite t)
    ))


(defmethod produce-documentation ((format (eql 'html))
                                  (structure doc-file)
                                  (where pathname)
                                  doc-bits
                                  &key
                                  (documentation-title "")
                                  &allow-other-keys
                                  )
  (declare (ignorable doc-bits))
  (let* ((doc-directory where)
         (dfname (doc-file-name structure))
         (destination-path
          (make-pathname :directory (pathname-directory doc-directory)
                         :name (pathname-name dfname)
                         :type (pathname-type dfname)))
         )
    (cond ((probe-file dfname)
           (cl-fad:copy-file dfname destination-path :overwrite nil))
          ((not (probe-file destination-path))
           (produce-info-area-placeholder format
                                          structure
                                          destination-path
                                          documentation-title))
          )))


(defmethod produce-documentation ((format (eql 'html))
                                  (structure doc-file)
                                  (where file-stream)
                                  doc-bits
                                  &key
                                  (documentation-title "")
                                  &allow-other-keys
                                  )
  (declare (ignorable doc-bits))
  (let* ((doc-directory where)
         (dfname (doc-file-name structure))
         (destination-path
          (make-pathname :directory (pathname-directory doc-directory)
                         :name (pathname-name dfname)
                         :type (or (pathname-type dfname)
                                   *default-html-extension*)))
         )
    (cond ((probe-file dfname)
           (cl-fad:copy-file dfname destination-path :overwrite nil))
          ((not (probe-file destination-path))
           (produce-info-area-placeholder format
                                          structure
                                          destination-path
                                          documentation-title))
          )))


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
  (let* ((fs-location (frameset-location structure))
         (where (if fs-location
                    (merge-pathnames fs-location where)
                    where))

         (fs-name (frameset-name structure))
         (fs-title (or documentation-title fs-name))
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

      (when fs-content
        (produce-documentation format
                               fs-content
                               fs-file
                               doc-bits
                               :documentation-title documentation-title))

      (<:with-html-syntax-output (fs-file :print-pretty t :syntax :compact)
          (<:document 
           (<:comment fs-name)
           (string #\Newline)
            
           +doctype-frameset-control-string+
           (string #\Newline)
           (<:html

            (<:head
             (<:title fs-title)
             (<:meta :http-equiv "Content-Type"
                     :content "text/html" :charset "UTF-8")
             (<:link :rel "stylesheet"
                     :href (frameset-style structure)))
             
            ((<:frameset :rows "65px,*,65px"
                         #| :border 0 |#
                         :noresize "noresize")
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
                    #|(produce-documentation format
                                           fs-content
                                           fs-file
                                           doc-bits
                                           :documentation-title documentatio-title)|#
                    (produce-frame format fs-content fs-file)
                    )
                  (<:frame (:name (format nil "~A_frame" (element-name structure))
                            :frameborder 0
                            #| :class "helambda_dict_entry" |#
                            ))
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
           ))
      )))


(defmethod produce-documentation ((format (eql 'html))
                                  (element frame)
                                  (where stream)
                                  doc-bits
                                  &key
                                  documentation-title
                                  &allow-other-keys)
  (declare (ignorable doc-bits documentation-title))
  (<:frame (:src (frame-source element)
            :name (frame-name element)
            :frameborder 0))) 
                                  

(defmethod produce-frame ((format (eql 'html))
                          (element doc-file)
                          (where stream)
                          )
  (<:frame (:src (namestring (file-pathname element nil)) ; force the name as-is.
            :name (concatenate 'string
                               (pathname-name
                                (file-pathname element))
                               "_frame")
            :frameborder 0
            :class "helambdap_dict_entry"
            )
           (<:comment () "FRAME " (element-name element))))


(defmethod produce-frame ((format (eql 'html))
                          (element file-set)
                          (where stream)
                          )
  (<:frame (:src (concatenate 'string
                              (file-set-name element) "."
                              *default-html-extension*) 
            :name (concatenate 'string
                               (file-set-name element)
                               "_frame") 
            :frameborder 0
            :class "helambdap_dict_entry"
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
  (declare (ignorable doc-bits))
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
          (produce-navigation-file format
                                   element
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
  (declare (ignorable doc-bits))
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
  (declare (ignorable doc-bits documentation-title))
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

  (produce-info-area-placeholder format structure where)

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
                               doc-bits
                               :documentation-title documentation-title))))
  )


(defmethod produce-info-area-placeholder ((format (eql 'html))
                                          (doc-file doc-file)
                                          doc-file-pathname 
                                          &optional (documentation-title ""))
  (declare (type pathname doc-file-pathname))
  (let ((dfname (doc-file-name doc-file)))
    (with-open-file (dffs doc-file-pathname
                          :direction :output
                          :if-does-not-exist :create
                          :if-exists :supersede)
      (<:with-html-syntax-output (dffs :print-pretty t :syntax :compact)
          (<:document
           (<:comment dfname)
           (<:html

            +doctype-xhtml1-string-control-string+
            (string #\Newline)

            (<:head
             (<:title dfname)
             (<:meta :http-equiv "Content-Type"
                     :content "text/html" :charset "UTF-8")
             (<:link :rel "stylesheet" :href *helambdap-css-filename*))
            
            (<:body
             (<:h1 documentation-title dfname)
             (<:p "This is a placeholder for information pertaining "
                  documentation-title)
             (<:p (format nil
                          "Please edit the file '~A', to complete ~
                           the documentation."
                          doc-file-pathname))
             )
            )
           (<:comment "end of file : " (string dfname)))
          ))
    ))


(defmethod produce-info-area-placeholder ((format (eql 'html))
                                          (file-set file-set)
                                          where
                                          &optional
                                          documentation-title)
  (declare (ignore documentation-title)
           (type (or stream pathname) where)
           )
  (let ((fsn (file-set-name file-set))
        (file-set-pathname
         (make-pathname :name (file-set-name file-set)
                        :type *default-html-extension*
                        :defaults (pathname where)))
        )
    (with-open-file (fsfs file-set-pathname
                          :direction :output
                          :if-does-not-exist :create
                          :if-exists :supersede)
      (<:with-html-syntax-output (fsfs :print-pretty t :syntax :compact)
          (<:document
           (<:comment (file-set-name file-set))
           (<:html

            +doctype-xhtml1-string-control-string+
            (string #\Newline)

            (<:head
             (<:title fsn)
             (<:meta :http-equiv "Content-Type"
                     :content "text/html" :charset "UTF-8")
             (<:link :rel "stylesheet"
                     :href (namestring *helambdap-css-filename-up*)))
            
            (<:body
             (<:h1 "Dictionary Entries")
             (<:p "Click and/or scroll on the menus on the side to choose "
                  "what information to display.")
             )
            )
           (<:comment "end of file : " (file-set-name file-set)))
          ))
    ))


;;;---------------------------------------------------------------------------
;;; Doc bits HTML production.

(defmethod process-doc-string
           ((s string)
            (input-syntax (eql 'text/hyperspec))
            (output-format (eql 'html))
            &optional
            args-n-values-p
            lambda-list
            result-p
            returns-decl
            type-decls
            skip-description-header-p
            )
  (declare (ignorable type-decls))
  ;; Try to process Hyperspec-style.

  (multiple-value-bind (syntax-pars
                        args-n-values-pars
                        description-pars
                        examples-pars
                        affected-by-pars
                        see-also-pars
                        notes-pars
                        except-pars
                        side-effects-pars
                        )
      (parse-doc-hyperspec-style s)
    (declare (ignore syntax-pars))
    
    (let ((elements ()))
      (flet ((push-pars (subsection-header pars)
               (when pars
                 (when subsection-header
                   (push (<:h2 () subsection-header) elements))
                 (dolist (par pars)
                   (when (string/= "" par)
                     (push (<:p () par) elements)))))

             (format-example-section-paragraphs (example-pars)
               (dolist (ep example-pars)
                 (if (string= ";;;" ep :end1 3 :end2 (min 3 (length ep)))
                     (let* ((ep-lines (split-sequence:split-sequence #\Newline ep))
                            (comment-text
                             (format nil "~{~A~}"
                                     (mapcar (lambda (epl)
                                               (string-left-trim '(#\;) epl))
                                             ep-lines)))
                            )
                       (push (<:p () comment-text) elements))
                     (push (<:pre () (sanitize-string-for-html ep)) elements))))
             )

        (when args-n-values-p
          (cond (args-n-values-pars ; User supplied directly in doc-string.
                 (push (<:h3 () "Arguments and Values:") elements)
                 (push (<:ul () (process-arg-n-value-pars args-n-values-pars))
                       elements))
                (t
                 (let ((ll-vars (and lambda-list (ll-vars lambda-list))))
                   (when (or ll-vars result-p)
                     (push (<:h3 () "Arguments and Values:") elements)
                     (push (<:ul (:style "list-style-type: none")
                                 (append
                                  (loop for arg in ll-vars
                                        for arg-type-decl
                                        = (or (find-if (lambda (type-decl)
                                                         (member arg
                                                                 (cddr type-decl)))
                                                       type-decls)
                                              `(type t ,arg))

                                        collect
                                        (<:li (:style "list-style-type: none")
                                              (<:i () (<:code () (arg-name arg)))
                                              " : "
                                              ;; "a T." ; To be FIXED.
                                              "a "
                                              (second arg-type-decl)
                                              ))
                                  (and result-p
                                       (process-returns-declaration returns-decl))))
                           elements))))
                ))
        
        (push-pars (and (not skip-description-header-p)
                        "Description:")
                   description-pars)

        (when examples-pars
          (push (<:h2 () "Examples:") elements)
          #|
          (push (<:pre () (sanitize-string-for-html
                           (format nil "~{~A~2%~}" examples-pars)))
                elements)
          |#
          (format-example-section-paragraphs examples-pars)
          )

        (push-pars (and affected-by-pars "Affected By:") affected-by-pars)

        (push-pars (and side-effects-pars "Side Effects:") side-effects-pars)

        (push-pars (and except-pars "Exceptional Situations:") except-pars)

        (push-pars (and see-also-pars "See Also:") see-also-pars)
    
        (push-pars (and notes-pars "Notes:") notes-pars)

        (nreverse elements)
        ))))


(defun dump-doc-bit-html (doc-bit n str-tag doc-string out)
  (let ((name (string-downcase n)))
    (<:with-html-syntax-output (out :print-pretty t :syntax :compact)
        (<:document
         (<:head
          (<:title (format nil "~A ~A" str-tag name))
          (<:meta :http-equiv "Content-Type"
                     :content "text/html" :charset "UTF-8")
          (<:link :rel "stylesheet"
                  :href (namestring *helambdap-css-filename-up*)))
         (<:body
          (produce-doc-bit-title-name doc-bit)
          ;; (<:h1 (<:i (format nil "~A " str-tag)) (<:strong name))

          (<:h2 "Package: ")
          (<:p (package-name (symbol-package n)))
          ;; (<:h2 "Description:")
          ;; (paragraphize-doc-string doc-string)
          (process-doc-string doc-string 'text/hyperspec 'html)
          )))))



#| Moved to 'protocol' file
(defgeneric render-lambda-list (lambda-list-type lambda-list))
|#


(defmethod render-lambda-list ((format (eql 'html))
                               (llt (eql :ordinary))
                               (ll list))
  (render-lambda-list format :ordinary (parse-ll :ordinary ll)))


(defmethod render-lambda-list ((format (eql 'html))
                               (llt (eql :ordinary))
                               (ll t_lambda-list))
  (let* ((pll ll)
         (rvs (ll-ordinary-vars pll))
         (ovs (ll-optional-vars pll))
         (rv  (ll-rest-var pll))
         (kvs (ll-keyword-vars pll))
         ;; (avs (ll-auxiliary-vars pll))
         (aok (ll-allow-other-keys pll))

         (rendered-ll ())
         )
              
    (dolist (rv rvs)
      (push (<:i () (llv-name rv)) rendered-ll))

    (when ovs
      (push (<:span (:style "color: blue") (string '&optional))
            rendered-ll)

      (dolist (ov ovs)
	(push (<:i () (llv-name ov)) rendered-ll)))

    (when rv
      (push (<:span (:style "color: blue") (string '&rest))
            rendered-ll)

      (push (<:i () (llv-name (first rv))) rendered-ll))

    (when kvs
      (push (<:span (:style "color: blue") (string '&key))
            rendered-ll)

      (dolist (kv kvs)
	(push (<:i () (llv-name kv)) rendered-ll) ; Fix this. No need to expose
					          ; internal variables.
	))

    (when aok
      (push (<:span (:style "color: blue") (string '&allow-other-keys))
            rendered-ll))

    (nreverse rendered-ll)
    ))


(defmethod render-lambda-list ((format (eql 'html))
                               (llt (eql :macro))
                               (ll list))
  (render-lambda-list format :macro (parse-ll :macro ll)))


(defmethod render-lambda-list ((format (eql 'html))
                               (llt (eql :macro))
                               (ll macro-lambda-list))
  (let* ((pll ll)
         (wv (macro-lambda-list-whole-var pll))
         (ev (macro-lambda-list-env-var pll))
         (bv (macro-lambda-list-body-var pll))
         
         (rvs (ll-ordinary-vars pll))
         (ovs (ll-optional-vars pll))
         (rv  (ll-rest-var pll))
         (kvs (ll-keyword-vars pll))
         ;; (avs (ll-auxiliary-vars pll))
         (aok (ll-allow-other-keys pll))

         (rendered-ll ())
         )

    (when (and bv rv)
      (warn "HELambdaP: parsing a macro lambda list that has 
             both &rest and &body variables."))

    (labels ((render-ll-item (lli)
             (etypecase lli
               (lambda-list-item
                (let ((llv-n (lli-name lli)))
                  (etypecase llv-n
                    (symbol (push (<:i () llv-n) rendered-ll))
                    (t_lambda-list
                     (push (render-lambda-list format :macro llv-n)
                           rendered-ll))
                    (list (mapc #'render-ll-item lli)
			  rendered-ll)
                    )))
               (t_lambda-list
                (push (render-lambda-list format :macro lli)
                      rendered-ll))
               ))
	     )
    
      (when wv
        (push (<:span (:style "color: blue") (string '&whole))
              rendered-ll)
        (render-ll-item (first wv))
        ;; (push (<:i () (llv-name (first wv))) rendered-ll)
        )

      (dolist (rv rvs)
        (render-ll-item rv)
        ;; (push (<:i () (llv-name rv)) rendered-ll)
        )

      (when ovs
        (push (<:span (:style "color: blue") (string '&optional))
              rendered-ll))

      (dolist (ov ovs)
        (render-ll-item ov)
        ;; (push (<:i () (llv-name ov)) rendered-ll)
        )

      (when rv
        (push (<:span (:style "color: blue") (string '&rest))
              rendered-ll))

      (when rv
        (render-ll-item (first rv))
        ;; (push (<:i () (llv-name (first rv))) rendered-ll)
        )


      (when bv
        (push (<:span (:style "color: blue") (string '&body))
              rendered-ll))

      (when bv
        (render-ll-item (first bv))
        ;; (push (<:i () (llv-name (first rv))) rendered-ll)
        )

      (when kvs
        (push (<:span (:style "color: blue") (string '&key))
              rendered-ll))

      (dolist (kv kvs)
        (render-ll-item kv)
        ;; (push (<:i () (llv-name kv)) rendered-ll) ; Fix this. No need to expose
                                                     ; internal variables.
        )

      (when aok
        (push (<:span (:style "color: blue") (string '&allow-other-keys))
              rendered-ll))

      (when ev
        (push (<:span (:style "color: blue") (string '&environment))
              rendered-ll)
        ;; (push (<:i () (llv-name (first ev))) rendered-ll)
        (render-ll-item (first ev))
        )

      (nreverse rendered-ll)
      )))


(defmethod render-lambda-list ((format (eql 'html))
                               (llt (eql :generic-function))
                               (ll list))
  (render-lambda-list format
		      :generic-function
		      (parse-ll :generic-function ll)))


(defmethod render-lambda-list ((format (eql 'html))
                               (llt (eql :generic-function))
                               (ll generic-function-lambda-list))
  (let* ((pll ll)
         (rvs (ll-ordinary-vars pll))
         (ovs (ll-optional-vars pll))
         (rv  (ll-rest-var pll))
         (kvs (ll-keyword-vars pll))
         (aok (ll-allow-other-keys pll))

         (rendered-ll ())
         )
    (declare (type list rendered-ll))

    (dolist (rv rvs)
      (push (<:i () (llv-form rv)) rendered-ll) ; LLV-FORM, not LLV-NAME.
      )

    (when ovs
      (push (<:span (:style "color: blue") (string '&optional))
            rendered-ll)

      (dolist (ov ovs)
        (push (<:i () (llv-name ov)) rendered-ll)
        ))

    (when rv
      (push (<:span (:style "color: blue") (string '&rest))
            rendered-ll)

      (push (<:i () (llv-name (first rv))) rendered-ll))

    (when kvs
      (push (<:span (:style "color: blue") (string '&key))
            rendered-ll)

      (dolist (kv kvs)
        (push (<:i () (llv-name kv)) rendered-ll) ; Fix this. No need to expose
                                                  ; internal variables.
        ))

    (when aok
      (push (<:span (:style "color: blue") (string '&allow-other-keys))
            rendered-ll))

    (nreverse rendered-ll)
    ))


(defmethod produce-documentation ((format (eql 'html))
                                  (doc-bit doc-bit)
                                  (out file-stream)
                                  doc-bits
                                  &key
                                  (documentation-title)
                                  &allow-other-keys
                                  )
  (declare (ignorable doc-bits documentation-title))
  (dump-doc-bit-html doc-bit
                     (doc-bit-name doc-bit)
                     (doc-bit-kind-tag doc-bit)
                     (doc-bit-doc-string doc-bit)
                     out))


(defmethod produce-documentation ((format (eql 'html))
                                  (doc-bit package-doc-bit)
                                  (out file-stream)
                                  doc-bits
                                  &key
                                  (documentation-title "")
                                  &allow-other-keys)
  ;; "This specialized method produces the documentation for a package."
  (declare (ignorable doc-bits documentation-title))
  (let ((name (doc-bit-name doc-bit))
        (doc-string (doc-bit-doc-string doc-bit))
        )
    (<:with-html-syntax-output (out :print-pretty t :syntax :compact)
        (<:document
         (<:head
          (<:title "Package " name)
          (<:meta :http-equiv "Content-Type"
                  :content "text/html" :charset "UTF-8")
          (<:link :rel "stylesheet"
                  :href (namestring *helambdap-css-filename-up*)))

         (<:body
          (<:h1 (<:i "Package ") (<:strong name))

          (<:h2 "Use list:")
          (<:p (mapcan (lambda (up) (list up (<:br)))
                       (package-doc-bit-use-list doc-bit)))

          (when (package-doc-bit-nicknames doc-bit)
            (list
             (<:h2 () "Nicknames:")
             (<:p () (mapcan (lambda (pn) (list pn (<:br)))
                         (package-doc-bit-nicknames doc-bit)))))

          ;; (<:h2 "Description:")
          ;; (paragraphize-doc-string doc-string)
          (process-doc-string doc-string 'text/hyperspec 'html)
         )))))


(defmethod produce-documentation ((format (eql 'html))
                                  (doc-bit system-doc-bit)
                                  (out file-stream)
                                  doc-bits
                                  &key
                                  ;; documentation-title
                                  &allow-other-keys)
  (declare (ignorable doc-bits))
  (let ((name (doc-bit-name doc-bit))
        (doc-string (doc-bit-doc-string doc-bit))
        (deps-on (system-doc-bit-depends-on doc-bit))
        )
    (<:with-html-syntax-output (out :print-pretty t :syntax :compact)
        (<:document
         (<:head
          (<:title "System " name)
          (<:meta :http-equiv "Content-Type"
                  :content "text/html" :charset "UTF-8")
          (<:link :rel "stylesheet"
                  :href (namestring *helambdap-css-filename-up*)))
         (<:body
          (<:h1 (<:i "System ") (<:strong name))
          (when deps-on
            (list
             (<:h2 () "Depends on:")
             (<:p () (mapcan (lambda (d) (list (<:i () d) (<:br))) deps-on))))

          ;; (<:h2 "Description:")
          ;; (paragraphize-doc-string doc-string)
          (process-doc-string doc-string 'text/hyperspec 'html)
          )
         ))))


#|
(defgeneric render-syntax-section (format doc-bit &optional lambda-list values))
|#


(defmethod render-syntax-section
           ((format (eql 'html))
            (doc-bit parameterized-doc-bit)
            &optional
            (ll (parameterized-doc-bit-lambda-list doc-bit))
            (values ()))

  (declare (ignore values))

  (let ((db-name (doc-bit-name doc-bit))
        (*print-pretty* t)
        (*print-right-margin* *formatted-section-right-margin*)
        )
    (<:htmlise (:syntax :compact)
        (<:div
         (<:p 
          (<:pre
           (with-output-to-string (pre-string)
             (pprint-logical-block (pre-string
                                    (list (<:strong (:style "color: red")
                                                    (format nil "~(~A~)" db-name))
                                          (render-lambda-list format :ordinary ll))
                                    )
               (write-string "  " pre-string)
               (bypass-pprint pre-string (pprint-pop) nil nil)
                                          
               (write-char #\Space pre-string)
               (pprint-indent :block 8 pre-string)
               (pprint-newline :linear pre-string)
               (pprint-logical-block (pre-string (pprint-pop))
                 (loop (pprint-exit-if-list-exhausted)
                       (bypass-pprint pre-string (pprint-pop) nil nil)
                       (write-char #\Space pre-string)
                       (pprint-newline :linear pre-string)
                       ))
               (pprint-indent :block 4 pre-string)
               (pprint-newline :linear pre-string)
               (write-string "&rarr; " pre-string)
               (write-string "<i>result</i>" pre-string)
               ))
           )) ; <:/pre <:/p
         ))
    ))


(defmethod render-syntax-section
           ((format (eql 'html))
            (doc-bit deftype-doc-bit)
            &optional
            (ll (deftype-doc-bit-lambda-list doc-bit))
            (values ())
            )

  (declare (ignore values))

  (let ((db-name (doc-bit-name doc-bit))
        (*print-pretty* t)
        (*print-right-margin* *formatted-section-right-margin*)
        )
    (<:htmlise (:syntax :compact)
        (<:div
         (<:p 
          (<:pre
           (with-output-to-string (pre-string)
             (pprint-logical-block (pre-string
                                    (list (<:strong (:style "color: red")
                                                    (format nil "~(~A~)" db-name))
                                          (render-lambda-list format :ordinary ll))
                                    )
               (write-string "  " pre-string)
               (bypass-pprint pre-string (pprint-pop) nil nil)
                                          
               (write-char #\Space pre-string)
               (pprint-indent :block 8 pre-string)
               (pprint-newline :linear pre-string)
               (pprint-logical-block (pre-string (pprint-pop))
                 (loop (pprint-exit-if-list-exhausted)
                       (bypass-pprint pre-string (pprint-pop) nil nil)
                       (write-char #\Space pre-string)
                       (pprint-newline :linear pre-string)
                       ))
               ))
           )) ; <:/pre <:/p
         ))
    ))


(defmethod render-syntax-section
           ((format (eql 'html))
            (doc-bit macro-doc-bit)
            &optional
            (ll (macro-doc-bit-lambda-list doc-bit))
            (values ()))

  (declare (ignore values))

  (let ((db-name (doc-bit-name doc-bit))
        (*print-pretty* t)
        (*print-right-margin* *formatted-section-right-margin*)
        )
    (<:htmlise (:syntax :compact)
        (<:div
         (<:p 
          (<:pre
           (with-output-to-string (pre-string)
             ;; (format t "==> ~A~%" pre-string)
             (pprint-logical-block (pre-string
                                    (list (<:strong (:style "color: red")
                                                    (format nil "~(~A~)" db-name))
                                          (render-lambda-list format :macro ll))
                                    )
               ;; (format t "==> ~A~%" pre-string)
               (write-string "  " pre-string)
               (bypass-pprint pre-string (pprint-pop) nil nil)
                                          
               (write-char #\Space pre-string)
               (pprint-indent :block 8 pre-string)
               (pprint-newline :linear pre-string)

               (labels ((pprint-lli (ll nested)
                          (assert ll) ; Being paranoid.
                          (let ((prefix (if nested "(" ""))
                                (suffix (if nested ")" ""))
                                )
                            (pprint-logical-block (pre-string ll
                                                              :prefix prefix
                                                              :suffix suffix)
                              (loop (pprint-exit-if-list-exhausted)
                                    (let ((lli (pprint-pop)))
                                      (if (listp lli)
                                          (pprint-lli lli t)
                                          (bypass-pprint pre-string lli nil nil))
                                      (pprint-exit-if-list-exhausted)
                                      (write-char #\Space pre-string)
                                      (pprint-newline :linear pre-string)
                                      )))))
                        )
                 (pprint-lli (pprint-pop) nil))

               (pprint-indent :block 4 pre-string)
               (pprint-newline :linear pre-string)
               (write-string " &rArr; " pre-string)
               (write-string "<i>form</i>" pre-string)
               ))
           )) ; <:/pre <:/p
         ))
    ))
            

(defmethod produce-documentation ((format (eql 'html))
                                  (doc-bit parameterized-doc-bit)
                                  (out file-stream)
                                  doc-bits
                                  &key
                                  (documentation-title "")
                                  &allow-other-keys)
  (declare (ignorable doc-bits))
  (let* ((db-name (doc-bit-name doc-bit))
         (name (format nil "~(~A~)" db-name))
         (kind (doc-bit-kind doc-bit))
         (doc-string (doc-bit-doc-string doc-bit))
         (ll (parameterized-doc-bit-lambda-list doc-bit))
         )
    (<:with-html-syntax-output (out :print-pretty t :syntax :compact)
        (<:document
         (<:head
          (<:title documentation-title ": " kind name)
          (<:meta :http-equiv "Content-Type"
                  :content "text/html" :charset "UTF-8")
          (<:link :rel "stylesheet" :href (namestring *helambdap-css-filename-up*)))

         (<:body
          ;; (<:h1 (<:i kind) (<:strong name))
          (produce-doc-bit-title-name doc-bit)

          (<:h2 "Package: ")
          (<:p (package-name (doc-bit-package doc-bit)))

          (<:h2 "Syntax:")
          (render-syntax-section format doc-bit)

          ;; (<:h2 "Description:")
          ;; (paragraphize-doc-string doc-string)
          (process-doc-string doc-string 'text/hyperspec 'html
                              t (parse-ll :ordinary ll))
          )))))


(defmethod produce-documentation ((format (eql 'html))
                                  (doc-bit function-doc-bit)
                                  (out file-stream)
                                  doc-bits
                                  &key
                                  (documentation-title "")
                                  &allow-other-keys)
  (declare (ignorable doc-bits))
  (let* ((db-name (doc-bit-name doc-bit))
         (name (format nil "~(~A~)" db-name))
         (kind (doc-bit-kind doc-bit))
         (doc-string (doc-bit-doc-string doc-bit))
         (ll (parameterized-doc-bit-lambda-list doc-bit))
         (returns (function-doc-bit-values doc-bit))
         (type-decls (function-doc-bit-type-declarations doc-bit))
         )
    (declare (ignorable type-decls))

    (<:with-html-syntax-output (out :print-pretty t :syntax :compact)
        (<:document
         (<:head
          (<:title documentation-title ": " kind name)
          (<:meta :http-equiv "Content-Type"
                  :content "text/html" :charset "UTF-8")
          (<:link :rel "stylesheet"
                  :href (namestring *helambdap-css-filename-up*)))

         (<:body
          (produce-doc-bit-title-name doc-bit)
          ;; (<:h1 (<:i kind) (<:strong name))

          (<:h2 "Package: ")
          (<:p (package-name (doc-bit-package doc-bit)))

          (<:h2 "Syntax:")
          (render-syntax-section format doc-bit)

          ;; (<:h2 "Description:")
          ;; (paragraphize-doc-string doc-string)
          (process-doc-string doc-string 'text/hyperspec 'html
                              t
                              (parse-ll :ordinary ll)
                              t
                              returns
                              type-decls)
          )))))


(defmethod produce-documentation ((format (eql 'html))
                                  (doc-bit macro-doc-bit)
                                  (out file-stream)
                                  doc-bits
                                  &key
                                  (documentation-title "")
                                  &allow-other-keys)
  (declare (ignorable doc-bits))
  (let* ((db-name (doc-bit-name doc-bit))
         (name (format nil "~(~A~)" db-name))
         (kind (doc-bit-kind doc-bit))
         (doc-string (doc-bit-doc-string doc-bit))
         (ll (parameterized-doc-bit-lambda-list doc-bit))
         )
    (<:with-html-syntax-output (out :print-pretty t :syntax :compact)
        (<:document
         (<:head
          (<:title documentation-title ": " kind name)
          (<:meta :http-equiv "Content-Type"
                  :content "text/html" :charset "UTF-8")
          (<:link :rel "stylesheet"
                  :href (namestring *helambdap-css-filename-up*)))

         (<:body
          (produce-doc-bit-title-name doc-bit)
          ;; (<:h1 (<:i kind) (<:strong name))

          (<:h2 "Package: ")
          (<:p (package-name (doc-bit-package doc-bit)))

          (<:h2 "Syntax:")
          (render-syntax-section 'html doc-bit (macro-doc-bit-lambda-list doc-bit))

          ;; (<:h2 "Description:")
          ;; (paragraphize-doc-string doc-string)
          (process-doc-string doc-string 'text/hyperspec 'html
                              t
                              (parse-ll :macro ll)
                              t
                              '(form)
                              )
          )))))


(defmethod produce-documentation ((format (eql 'html))
                                  (doc-bit constant-doc-bit)
                                  (out file-stream)
                                  doc-bits
                                  &key
                                  ;; documentation-title
                                  &allow-other-keys)
  (declare (ignorable doc-bits))
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
    (<:with-html-syntax-output (out :print-pretty t :syntax :compact)
        (<:document
         (<:html
          +doctype-xhtml1-string-control-string+
          (string #\Newline)

          (<:head
           (<:title kind (string-downcase name))
           (<:meta :http-equiv "Content-Type"
                   :content "text/html" :charset "UTF-8")
           (<:link :rel "stylesheet"
                   :href (namestring *helambdap-css-filename-up*)))
          (<:body
           (produce-doc-bit-title-name doc-bit)
           ;; (<:h1 (<:i kind) (<:strong (string-downcase name)))

           (<:h2 "Package:")
           (<:p (package-name (symbol-package name)))

           (<:h2 "Value:")
           (<:p (<:code value-presented))

           ;; (<:h2 "Description:")
           ;; (paragraphize-doc-string doc-string)
           (process-doc-string doc-string 'text/hyperspec 'html)
         ))))))


(defmethod produce-documentation ((format (eql 'html))
                                  (doc-bit struct-doc-bit)
                                  (out file-stream)
                                  doc-bits
                                  &key
                                  ;; documentation-title
                                  &allow-other-keys)
  (declare (ignorable doc-bits))
  (let* ((name (doc-bit-name doc-bit))
         (kind (doc-bit-kind-tag doc-bit))
         (doc-string (doc-bit-doc-string doc-bit))
         (include (struct-doc-bit-include doc-bit))
         (slots (struct-doc-bit-slots doc-bit))
         )
    (<:with-html-syntax-output (out :print-pretty t :syntax :compact)
        (<:document
         (<:html
          +doctype-xhtml1-string-control-string+
          (string #\Newline)

          (<:head
           (<:title kind (string-downcase name))
           (<:meta :http-equiv "Content-Type"
                   :content "text/html" :charset "UTF-8")
           (<:link :rel "stylesheet"
                   :href (namestring *helambdap-css-filename-up*))) 
          (<:body
           (produce-doc-bit-title-name doc-bit)
           ;; (<:h1 (<:i kind) (<:strong (string-downcase name)))

           (<:h2 "Package: ")
           (<:p (package-name (symbol-package name)))

           (<:h2 "Class Precedence List:")
           (<:p (format nil "~A &rarr;~@[ ~A &rarr;~] ... &rarr; T"
                        name
                        include))

           (<:h2 "Slots:")
           (<:p (<:dl
                 (loop for s in slots
                       if (symbolp s)
                       collect
                       (<:dt () s)
                       and collect
                       (<:dd ()
                             (format nil
                                     "with initial value ~A of type ~
                                      ~A~@[; the slot is read-only~]."
                                     nil T nil))
                       else
                       nconc
                       (destructuring-bind (sn &optional sv &key read-only (type t))
                           s
                         (list
                          (<:dt () sn)
                          (<:dd ()
                                (format nil
                                        ;; "with initial value ~S ~
                                        ;;  of type ~A~@[; the slot is read-only~]."
                                        "with initial value ~A ~
                                         of type ~A~@[; the slot is read-only~]."
                                        sv type read-only)))))
                 ))
           ;; (<:h2 "Description:")
           ;; (paragraphize-doc-string doc-string)
           (process-doc-string doc-string 'text/hyperspec 'html)
           ))))))


(defmethod produce-documentation ((format (eql 'html))
                                  (doc-bit class-doc-bit)
                                  (out file-stream)
                                  doc-bits
                                  &key
                                  ;; documentation-title
                                  &allow-other-keys
                                  )
  (declare (ignorable doc-bits))
  (let* ((name (doc-bit-name doc-bit))
         (kind (doc-bit-kind-tag doc-bit))
         (doc-string (doc-bit-doc-string doc-bit))
         (superclasses (class-doc-bit-superclasses doc-bit))
         (slots (class-doc-bit-slots doc-bit))
         )
    (<:with-html-syntax-output (out :print-pretty t :syntax :compact)
        (<:document
         (<:html
          +doctype-xhtml1-string-control-string+
          (string #\Newline)

          (<:head
           (<:title kind (string-downcase name))
           (<:meta :http-equiv "Content-Type"
                   :content "text/html" :charset "UTF-8")
           (<:link :rel "stylesheet"
                   :href (namestring *helambdap-css-filename-up*)))

          (<:body
           (produce-doc-bit-title-name doc-bit)
           ;; (<:h1 (<:i kind) (<:strong (string-downcase name)))
            
           (<:h2 "Package: ")
           (<:p (package-name (symbol-package name)))

           (<:h2 "Class Precedence List:")
           (<:p (format nil "~A &rarr;~@[~{ ~A~^&rarr;~}~] ... &rarr; T"
                        name superclasses))
           
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
            
           ;; (<:h2 "Description:")
           ;; (paragraphize-doc-string doc-string)
           (process-doc-string doc-string 'text/hyperspec 'html)
           ))))))
       

(defmethod produce-documentation ((format (eql 'html))
                                  structure
                                  (out stream)
                                  doc-bit
                                  &key
                                  (documentation-title)
                                  &allow-other-keys
                                  )
  (declare (ignorable structure documentation-title))
  (let ((name (doc-bit-name doc-bit))
        (doc-string (doc-bit-doc-string doc-bit))
        )

    (<:with-html-syntax-output (out :print-pretty t :syntax :compact)
        (<:document

         (<:head
          (<:title (format nil "DOC FOR ~A" (string-downcase name)))
          (<:meta :http-equiv "Content-Type"
                  :content "text/html" :charset "UTF-8")
          (<:link :rel "stylesheet" :href (namestring *helambdap-css-filename-up*)))

         (<:body
          (<:h1 (<:i "Function") (<:strong name))
          (<:h2 "Package:")
          (<:h2 "Description:" (<:br) doc-string)
          )))
    t))


#||
#+helambdap.version-using-MOP
(defmethod produce-documentation ((format (eql 'html))
                                  (doc-bit generic-function-doc-bit)
                                  (out file-stream)
                                  doc-bits
                                  &key
                                  documentation-title
                                  &allow-other-keys)
  ;; "This specialized method produces the documentation for a generic function."
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
      (<:with-html-syntax-output (out :print-pretty t :syntax :compact)
          (<:document
           (<:head
            (<:title documentation-title ": " "Generic Function" name)
            (<:link :rel "stylesheet" :href (namestring *helambdap-css-filename-up*)))
           (<:body
            (produce-doc-bit-title-name doc-bit)
            ;; (<:h1 (<:i "Generic Function") (<:strong name))

            (<:h2 "Package:")
            (<:p (package-name (symbol-package gfname)))

            (<:h2 "Syntax:")
            (<:p (<:strong name)
                 (format nil "~{ <i>~A</i>~}"
                         (parameterized-doc-bit-lambda-list doc-bit)))
            ;; (<:h2 "Description:")
            ;; (paragraphize-doc-string doc-string)
            (process-doc-string doc-string 'text/hyperspec 'html
                                t
                                ;; (parse-ll :generic-function ll)
                                )
           
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
            ))))))
||#


;;; The next method should be reworked in order to take advantage of
;;; RENDER-SYNTAX-SECTION.  However, it works and I need to do other
;;; things now (20130618).

;;; It is not 2020 and this has finally gotten back to bit me.
;;; Time to fix it (20200428).

#-helambdap.version-using-MOP
(defmethod produce-documentation ((format (eql 'html))
                                  (doc-bit generic-function-doc-bit)
                                  (out file-stream)
                                  doc-bits
                                  &key
                                  documentation-title
                                  &allow-other-keys)
  ;; "This specialized method produces the documentation for a generic function."

  (declare (ignorable doc-bits documentation-title))
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

           (render-lambda-list (ll)
             (loop for lle in ll
                   if (member lle +lambda-list-kwds+ :test #'eq)
                   collect (<:span (:style "color: blue") (string lle))
                   else
                   collect (<:i () lle))
             )

           #|
           (pre-format-syntax-entry (name ll)
             (let ((*print-right-margin* 75))
               (format nil
                       "  ~A ~@<~@/pprint-linear/~:>~%    &rarr; <i>result</i>"
                       (<:span (:style "color: red") (<:strong () name))
                       (render-lambda-list ll))
               ))
           |#


           (quoted-exp-p (exp) (and (listp exp) (eq 'quote (first exp))))

           (eql-spec-p (sp) (and (listp sp) (eq 'eql (first sp))))

           (render-method-specializers (specializers)
             (loop for sp in specializers
                   for s
                   = (if (eql-spec-p sp)
                         (cond ((quoted-exp-p (second sp))
                                (<:i () (format nil "(EQL '~A)"
                                                (second (second sp)))))
                               ((keywordp (second sp))
                                (<:i () (format nil "(EQL :~A)"
                                                (second sp))))
                               (t
                                (<:i () sp)))
                       (<:i () (format nil "&lt;~A&gt;" sp)))
                   collect s))

           (render-known-methods (name doc-bit)
             (let ((*print-right-margin* *formatted-section-right-margin*))
               (loop for mdb of-type method-doc-bit
                     in (generic-function-doc-bit-methods doc-bit)

                     for (specializers other) = (method-signature mdb)

                     ;; for mdb-doc = (doc-bit-doc-string mdb)
                     for mdb-doc = (process-doc-string (doc-bit-doc-string mdb)
                                                       'text/hyperspec
                                                       'html
                                                       nil
                                                       nil
                                                       t
                                                       nil
                                                       t
                                                       t
                                                       )

                     for qualfs = (method-doc-bit-qualifiers mdb)

                     collect
                     (<:htmlise (:syntax :compact)
                         (<:li
                          (<:p
                           (<:pre
                            (let ((*print-pretty* t))
                              (with-output-to-string (pre-string)
                                (pprint-logical-block
                                    (pre-string
                                     (list (<:strong () name)
                                           qualfs
                                           ;; (list :around)
                                           (nconc
                                            (render-method-specializers specializers)
                                            (render-lambda-list other))
                                           ))
                                  (write-string " " pre-string)
                                  (bypass-pprint pre-string (pprint-pop) nil nil)
                                  ;; (write (pprint-pop) :stream pre-string)
                                  (write-char #\Space pre-string)

                                  (let ((qualfs (pprint-pop)))
                                    (when qualfs
                                      (pprint-linear pre-string qualfs nil)))
                                          
                                  (write-char #\Space pre-string)
                                  (pprint-indent :block 8 pre-string)
                                  (pprint-newline :linear pre-string)
                                  (pprint-logical-block (pre-string (pprint-pop))
                                    (loop (pprint-exit-if-list-exhausted)
                                          (bypass-pprint pre-string (pprint-pop) nil nil)
                                          (write-char #\Space pre-string)
                                          (pprint-newline :linear pre-string)
                                          ))
                                  (pprint-indent :block 4 pre-string)
                                  (pprint-newline :linear pre-string)
                                  (write-string "&rarr; " pre-string)
                                  (write-string "<i>result</i>" pre-string)
                                  )))
                            )) ; <:/pre <:/p
                          (when mdb-doc
                            (<:p () mdb-doc))
                          ) ; <:li
                         ))))
           )

    (let* ((gfname (doc-bit-name doc-bit))
           (name (string-downcase gfname))
           (kind (doc-bit-kind doc-bit))
           (doc-string (doc-bit-doc-string doc-bit))
           (f-values (function-doc-bit-values doc-bit))
           (ll (parameterized-doc-bit-lambda-list doc-bit))
           )
      (declare (ignore kind))

      (<:with-html-syntax-output (out :print-pretty t :syntax :compact)
          (<:document
           (<:head
            (<:title documentation-title ": " "Generic Function" name)
            (<:meta :http-equiv "Content-Type"
                    :content "text/html" :charset "UTF-8")
            (<:link :rel "stylesheet"
                    :href (namestring *helambdap-css-filename-up*)))
	   
           (<:body

            (produce-doc-bit-title-name doc-bit)
            ;; (<:h1 (<:i "Generic Function") (<:strong name))

            (<:h2 "Package:")
            (<:p (package-name (symbol-package gfname)))

            (<:h2 "Syntax:")
	    (render-syntax-section format doc-bit)

            ;; (<:h2 "Description:")
            ;; (paragraphize-doc-string doc-string)
            (process-doc-string doc-string 'text/hyperspec 'html
                                t (parse-ll :generic-function ll)
                                t f-values
                                )
           
            (let ((known-methods-els (render-known-methods name doc-bit)))
              (when known-methods-els
                (<:div ()
                       (<:h2 () "Known Documented Methods:")
                       (<:ol () known-methods-els)))
              
              ))
           )))))


;;;#+do-not-use ; Now we can use it 20200429 MA.
(defmethod render-syntax-section
           ((format (eql 'html))
            (doc-bit generic-function-doc-bit)
            &optional
            (ll (parameterized-doc-bit-lambda-list doc-bit))
            (values ()))

  (declare (ignore values))

  (let ((db-name (doc-bit-name doc-bit))
        (*print-pretty* t)
        (*print-right-margin* *formatted-section-right-margin*)
        )
    (<:htmlise (:syntax :compact)
        (<:div
         (<:p 
          (<:pre
           (with-output-to-string (pre-string)
             (pprint-logical-block (pre-string
                                    (list (<:strong (:style "color: red")
                                                    (format nil "~(~A~)" db-name))
                                          (render-lambda-list format
                                                              :generic-function
                                                              ll))
                                    )
               (write-string "  " pre-string)
               (bypass-pprint pre-string (pprint-pop) nil nil)
                                          
               (write-char #\Space pre-string)
               (pprint-indent :block 8 pre-string)
               (pprint-newline :linear pre-string)
               (pprint-logical-block (pre-string (pprint-pop))
                 (loop (pprint-exit-if-list-exhausted)
                       (bypass-pprint pre-string (pprint-pop) nil nil)
                       (write-char #\Space pre-string)
                       (pprint-newline :linear pre-string)
                       ))
               (pprint-indent :block 4 pre-string)
               (pprint-newline :linear pre-string)
               (write-string "&rarr; " pre-string)
               (write-string "<i>result</i>" pre-string)
               ))
           )) ; <:/pre <:/p
         ))
    ))



;;;---------------------------------------------------------------------------
;;; Auxiliary files production.

#| Moved to 'xhtml-lambda-producer-protocol'
(defgeneric frameset-head-title (fs)
  (:method ((fs frameset)) "Frameset head placeholder title"))


(defgeneric frameset-body-title (fs)
  (:method ((fs frameset)) "Frameset body placeholder title"))


(defgeneric framesets-of (e)
  (:method ((fss framesets)) (framesets-list fss))
  (:method ((e element)) ())
  (:method ((e documentation-structure)) ())
  )
|#


(defmethod produce-header-file ((fs frameset)
                                header-pathname
                                documentation-title)
  (declare (type pathname header-pathname))
  (declare (ignorable documentation-title))
  (let ((fs-order (frameset-order fs))
        (fs-head-title (frameset-head-title fs))
        (fs-body-title (frameset-body-title fs))
        (ed-fs (element-location-depth fs))
        )
    (declare (ignorable fs-order))
    (labels (
	     #| Commented to placate SBCL. 
                Note comments underneath the actual usage.

	     (select-link-style (i)
               (if (= i fs-order)
                   "navigation-link-selected"
                   "navigation-link"))
	     |#

             (select-link-style-1 (fs-in-p)
               (if (eq fs fs-in-p)
                   "navigation-link-selected"
                   "navigation-link"))

             ;; The next function is hairy because it needs to produce
             ;; the href based on the position of the frameset in the
             ;; hierarchy.

             (produce-navigation-link (fs-in-parent)
               (let* ((fs-path (compute-element-path fs-in-parent))
                      (href (progn
                              #+helambdap-debugging
                              (format t ">>>> Producing NAV link.~@
                                         >>>> FS-in-p ~S~@
                                         >>>> EQ ~A~@
                                         >>>> ED-FS ~A~@
                                         >>>> FS Path ~S~%"
                                      fs-in-parent
                                      (eq fs fs-in-parent)
                                      ed-fs
                                      fs-path)
                              (if (eq fs fs-in-parent)
                                (namestring
                                 (make-pathname :name (frameset-name fs-in-parent)
                                                :type *default-html-extension*
                                                :directory ()))
                                (namestring
                                 (merge-pathnames 
                                  fs-path
                                  (make-pathname
                                   :directory (cons :relative
                                                    (make-list
                                                     ed-fs
                                                     :initial-element :up))))))
                              ))
                      )
                 #+helambdap-debugging (format t ">>>> HREF ~S~2%" href)
                 (<:a (:href href ; :href (namestring (compute-element-path fs-in-parent)
                       :target "_parent"
                       :class (select-link-style-1 fs-in-parent))
                      (string-capitalize (frameset-name fs-in-parent)))))
             )
      (with-open-file (hs header-pathname
                          :direction :output
                          :if-exists :supersede
                          :if-does-not-exist :create)
        (<:with-html-syntax-output (hs :print-pretty t :syntax :compact)
            (<:document
             (<:comment (base-name header-pathname))
             +doctype-xhtml1-string-control-string+
             (<:html
              (<:head
               (<:title fs-head-title)
               (<:meta :http-equiv "Content-Type"
                       :content "text/html" :charset "UTF-8")
               (<:link :rel "stylesheet" :href (frameset-style fs)))

              ((<:body :style "margin: 0pt 0pt 0pt 0pt;")
               ((<:div :class "header"
                       :style "padding-left: 2em; padding-top: 5pt; color: #41286f; font-size: 14pt")
                (<:strong (or documentation-title fs-body-title))
                ((<:div :class "navigation"
                        :style "right: 2m")

                 (when (element-parent fs)
                   (let ((fss-in-p (framesets-of (element-parent fs))))
                     #+helambdap-debugging
                     (format t ">>> Producing NAV links.~@
                                >>> Header pathname ~S~@
                                >>> Frameset ~S~@
                                >>> FS Parent ~S~@
                                >>> FSs of parent ~S~2%"
                             header-pathname
                             fs
                             (element-parent fs)
                             (framesets-of (element-parent fs))
                             )
                     (loop for fs-in-p in (rest fss-in-p)
                           collect " | " into result
                           collect (produce-navigation-link fs-in-p) into result
                           finally (return
                                    (cons (produce-navigation-link (first fss-in-p))
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
               ))))
        ))))


(defmethod produce-navigation-file ((format (eql 'html))
                                    (fs frameset)
                                    (nav-element doc-file)
                                    nav-pathname
                                    doc-bits
                                    documentation-title)
  (declare (type pathname nav-pathname))
  (declare (ignorable doc-bits documentation-title))

  (let* ((fs-order (frameset-order fs))
         (fs-head-title (frameset-head-title fs))
         (fs-body-title (frameset-body-title fs))
         (doc-file-pathname (file-pathname nav-element nav-pathname))
         (sections (extract-sections doc-file-pathname 'html))
         (section-names (extract-section-names sections))
        )
    (declare (ignore fs-order fs-body-title))

    (flet ((make-nav-links ()
             (loop for sn in section-names
                   collect
                   (<:li (:style "list-style-type: none")
                         (<:a (:href (format nil "~A#~A"
                                             (base-name doc-file-pathname)
                                             sn)
                               :target (format nil "~A_frame"
                                               (element-name nav-element)))
                              sn))))
           )
      (with-open-file (ns nav-pathname
                          :direction :output
                          :if-exists :supersede
                          :if-does-not-exist :create)
        (<:with-html-syntax-output (ns :print-pretty t :syntax :compact)
            (<:document
             (<:comment (base-name nav-pathname))
             (string #\Newline)

             +doctype-xhtml1-string-control-string+

             (string #\Newline)
             (string #\Newline)

             (<:html
              (<:head
               (<:title fs-head-title)
               (<:meta :http-equiv "Content-Type"
                       :content "text/html" :charset "UTF-8")
               (<:link :rel "stylesheet" :href (frameset-style fs)))

              (<:body
               (<:ul (make-nav-links))))
             (string #\Newline)
             (<:comment "end of file :" (base-name nav-pathname))
             ))))
    ))


(defmethod produce-navigation-file ((format (eql 'html))
                                    (fs frameset)
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
      (<:with-html-syntax-output (ns :print-pretty t :syntax :compact)
          (<:document
           (<:comment (base-name nav-pathname))
           (string #\Newline)

           +doctype-frameset-control-string+
           (string #\Newline)
           (string #\Newline)
           (<:html
            (<:head
             (<:title fs-head-title)
             (<:meta :http-equiv "Content-Type"
                     :content "text/html" :charset "UTF-8")
             (<:link :rel "stylesheet" :href (frameset-style fs)))
            
            ((<:frameset :rows "25%,75%"
                         #| :frameborder 0 |#
                         :noresize "noresize")
             ((<:frame :name (format nil
                                     "~A_navigation_map"
                                     fs-name)
                       :src (base-name nav-map-pathname)
                       :frameborder 0
                       :scrolling :auto
                       :style "border-bottom-style: dotted;
                               border-bottom-width: 1px"
                       #|
                       :style "border-bottom-style: dotted;
                               border-bottom-width: 1px;
                               border-right-style: dotted;
                               border-right-width: 1px
                               "
                       |#
                       ;; :class "helambdap_navmap"
                       ))
             ((<:frame :name (format nil
                                     "~A_navigation_lists"
                                     fs-name)
                       ;; :src (namestring nav-list-pathname)
                       :frameborder 0
                       :scrolling :auto
                       #|
                       :style "border-right-style: dotted;
                               border-right-width: 1px
                               "
                       |#
                       ))
             ))
           (<:comment (format nil "end of file : ~A"
                              (base-name nav-pathname))) 
           (string #\newline)))) ; WITH-OPEN-FILE...

    (produce-navigation-map format
                            fs
                            nav-element
                            nav-map-pathname
                            doc-bits
                            documentation-title)
    ))


(defmethod produce-navigation-map ((format (eql 'html))
                                   (fs frameset)
                                   (nav-element file-set)
                                   nm-pathname
                                   doc-bits
                                   doc-title)
  (declare (ignore doc-title))
  (format t "~&HELAMBDAP: producing NAV MAP file~%~:
           ~S~%~:
           ~S~%~:
           ~S~2%"
          fs nav-element nm-pathname)
  (let ((nav-element-target
         (format nil "~A_frame" (element-name nav-element))))
    (with-open-file (nm nm-pathname
                        :direction :output
                        :if-exists :supersede
                        :if-does-not-exist :create)
      (<:with-html-syntax-output (nm :print-pretty t :syntax :compact)
          (<:document
           (<:comment (base-name nm-pathname))
           (string #\Newline)
           (string #\Newline)

           +doctype-xhtml1-string-control-string+
           (string #\Newline)

           (<:html
            (<:head
             (<:title "Navigation Map")
             (<:meta :http-equiv "Content-Type"
                     :content "text/html" :charset "UTF-8")
             (<:link :rel "stylesheet"
                     :href (namestring *helambdap-css-filename-up*)) ; TESTING!
             (<:style (format nil
                              ".helambdap_navmap li {
                                  display: inline;
                              }"))
             )

            (<:body
             ((<:div :class "helambdap_navmap"
                     ;; :style "border-bottom-style: dotted"
                     )
              (<:h3 "Systems and Packages")

              (let ((syss (remove-if (complement #'system-doc-bit-p)
                                     doc-bits))
                    (pkgs (remove-if (complement #'package-doc-bit-p)
                                     doc-bits))
                    )
                (list
                 (<:h4 () "Systems")
                 (<:div ()
                        (loop for s in (remove-duplicates
                                        syss
                                        :test
                                        (lambda (s1 s2)
                                          (and (not (eq (type-of s1)
                                                        (type-of s2)))
                                               (string-equal (doc-bit-name s1)
                                                             (doc-bit-name s2)))))
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
                                                           
                              do (produce-package-navigation-list format
                                                                  fs
                                                                  nav-element
                                                                  p
                                                                  p-list-pathname
                                                                  doc-bits)
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
            ))))))


(defmethod produce-package-navigation-list ((format (eql 'html))
                                            (fs frameset)
                                            nav-element
                                            pkg-doc-bit
                                            pkg-list-pathname
                                            doc-bits)
  (let* ((pkg (find-package (package-doc-bit-name pkg-doc-bit)))
         (target (format nil "~A_frame"
                         (element-name nav-element)))
         (pkg-doc-bits
          (remove-if #'system-doc-bit-p
                     (remove-if (lambda (db &aux (dbn (doc-bit-name db)))
                                  (and (not (system-doc-bit-p db))
                                       (not (package-doc-bit-p db))
                                       (typecase dbn
                                         (symbol
                                          (not (eq pkg (symbol-package dbn))))
                                         (cons ; Mostly (SETF NAME)
                                          (not (eq pkg (symbol-package (second dbn)))))
                                         )))
                                doc-bits))
          ;; This is an horrendous kludge. Alas, I FTTB I will not
          ;; chase down instances of people defining systems with symbols.
          )
         )
    (format t "~&HELAMBDAP: produce-package-navigation-list HTML~%~:
           ~S~%~:
           ~S~%~:
           ~S~%~:
           ~S~2%"
            fs
            (if pkg (package-name pkg) "#<not-yet-defined package>")
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
        (sift-standard-doc-bits pkg-doc-bits)

      (declare (ignore systems packages methods others))
      ;; (format t "~&HELAMBDAP:~{~} ~%") ; Count stuff.
      (flet ((build-list (list-name doc-bits)
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
                                                (format nil
                                                        "~(~A~)"
                                                        (doc-bit-name db)))))
                        )))
             )
        (with-open-file (ps pkg-list-pathname
                            :if-exists :supersede
                            :if-does-not-exist :create
                            :direction :output)

          (<:with-html-syntax-output (ps :print-pretty t :syntax :compact)
              (<:document
               (<:comment (base-name pkg-list-pathname))

               +doctype-xhtml1-string-control-string+

               (<:html
                (<:head
                 (<:title (format nil "~A Package List" (doc-bit-name pkg-doc-bit)))
                 (<:meta :http-equiv "Content-Type"
                         :content "text/html" :charset "UTF-8")
                 (<:link :rel "stylesheet" :href (frameset-style fs))
                 (<:style (format nil
                                  ".helambdap_navmap li {
                                      display: inline;
                                  }")))

                (if pkg-doc-bits
                    (<:htmlise (:syntax :compact)
                        (<:body
                         (<:h3 "Package interface" <:br
                               (package-name pkg))

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
                          )) ; body
                        )

                    (<:htmlise (:syntax :compact)
                        (<:body
                         (<:h3 "Package interface" <:br
                               (package-name pkg))

                         (<:p (<:i "No published interface."))
                         ) ; body
                        )
                    )
                ) ; html
               ) ; document
              )))
      )))


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
      (<:with-html-syntax-output (hs :print-pretty t :syntax :compact)
          (<:document
           (<:comment (base-name footer-pathname))
           +doctype-xhtml1-string-control-string+
           (<:html
            (<:head
             (<:title fs-head-title)
             (<:meta :http-equiv "Content-Type"
                     :content "text/html" :charset "UTF-8")
             (<:link :rel "stylesheet" :href (frameset-style fs)))

            ((<:body :style "margin: 0pt 0pt 0pt 0pt;")
             ((<:div :class "copyright"
                     #|
                     :style "padding-left: 2em;
                             padding-top: 5pt;
                             color: #41286f;
                             font-size: 14pt"
                     |#
                     )
              (<:strong (or documentation-title fs-body-title))
              "(X)HTML documentation produced with"
              ((<:a :href *helambdap-site* :target "_blank") "HE&Lambda;P")
              (<:br)
              (<:comment "hhmts start")
              "Last modified: " (text-timestamp)
              (<:comment "hhmts end")
              (<:br)

              (format nil "&copy; ~D, Marco Antoniotti, all rights reserved."
                      (nth-value 5 (decode-universal-time (get-universal-time)))))
             )))))
    ))


;;;;===========================================================================
;;;; Utilities.
        

;;;;===========================================================================
;;;; Example...

#| Example output...  May change.
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


;;;; end of file -- xhtml-lambda-producer.lisp --

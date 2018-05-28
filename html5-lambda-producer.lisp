;;;; -*- Mode: Lisp -*-

;;;; html5-producer.lisp --
;;;; Make a file out of a DOCUMENTATION-STRUCTURE and a set (list) of
;;;; DOC-BITs, using a HTML5 output format.
;;;;
;;;; See file COPYING for copyright and licensing information.

(in-package "HELAMBDAP")


;;;;===========================================================================
;;;; Prologue.

;;;; The HTML5 producer makes essentially two kinds of "documents": the "prose"
;;;; and "help" ones and the "dictionary" one.
;;;;
;;;; The "prose" and "help" ones are the "index", and possibly the "downloads",
;;;; "mailing-lists"/"contact" and "links" documents, which have the
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
DOCUMENTATION-STRUCTURE) are HTML5 MAIN and ARTICLE elements; HEADER,
NAV, INFO-AREA and FOOTER are similar HTML5 elements.  There should
also be a Sidebar, but you get the idea.

Each ARTICLE and NAV is contained in a separate file.
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
;;;; The layout already reflects HTML5 elements.


;;;;===========================================================================
;;;; Protocol.


;;;;===========================================================================
;;;; Implementation.


(defmethod produce-documentation ((format (eql :html5))
                                  element
                                  out
                                  doc-bits
                                  &key
                                  documentation-title
                                  &allow-other-keys
                                  )
  (produce-documentation 'html5 element out doc-bits
                         :documentation-title documentation-title))


(defmethod produce-documentation :before ((format (eql 'html5))
                                          element
                                          out
                                          doc-bits
                                          &key
                                          documentation-title
                                          &allow-other-keys)
  (declare (ignorable element out doc-bits documentation-title))
  (warn "HELAMBDAP: HTML5 documentation production is still experimental.
           THe results may not be completely satisfactory."))


(defmethod produce-documentation ((format (eql 'html5))
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
    (produce-documentation 'html5 c where doc-bits
                           :documentation-title documentation-title)
    ))


(defmethod produce-documentation ((format (eql 'html5))
                                  (structure style-file)
                                  (where pathname)
                                  doc-bits
                                  &key
                                  (documentation-title)
                                  &allow-other-keys
                                  )
  (declare (ignorable doc-bits documentation-title))
  (let ((doc-directory where)
        (sfn (element-name structure))
        )
    (cl-fad:copy-file sfn
                      (make-pathname
                       :directory (pathname-directory doc-directory)
                       :name (pathname-name sfn)
                       :type (pathname-type sfn))
                      :overwrite t)
    ))


(defmethod produce-documentation ((format (eql 'html5))
                                  (structure js-file)
                                  (where pathname)
                                  doc-bits
                                  &key
                                  (documentation-title)
                                  &allow-other-keys
                                  )
  (declare (ignorable doc-bits documentation-title))
  (let ((doc-directory where)
        (jsfn (element-name structure))
        )
    (cl-fad:copy-file jsfn
                      (make-pathname
                       :directory (pathname-directory doc-directory)
                       :name (pathname-name jsfn)
                       :type (pathname-type jsfn))
                      :overwrite t)
    ))


;;; produce-documentation html5 doc-area
;;; This actually produces the 'index.html' and the 'dictionary.html'
;;; for HTML5.

(defmethod produce-documentation ((format (eql 'html5))
                                  (mv main-view)
                                  (where pathname)
                                  doc-bits
                                  &key
                                  (documentation-title)
                                  &allow-other-keys
                                  )

  (let* ((index-name (element-name mv))
         (index-title (or documentation-title index-name))
         (index-pathname (make-pathname :name "index" ; (element-name mv)
                                        :type *default-html-extension*
                                        :defaults where))
         (<:*html5-no-self-closing-tag* t) ; Just testing.
         )

    (with-open-file (index-file index-pathname
                                :direction :output
                                :if-does-not-exist :create
                                :if-exists :supersede)
      (<:with-html-syntax-output (index-file :print-pretty t :syntax :compact)
          (<:document
           +doctype-html5-control-string+
           (string #\Newline)

           (<:comment index-name)
           (string #\Newline)

           (<:html
	    
            (<:head
             (<:title index-title)
	     
             ((<:script :type "text/javascript" :src "helambdap-support.js"))

             #|	     ((<:script :type "text/javascript")
              "
// -*- Mode: JavaScript -*-

// helambdap-js-support.js --
// Javascript support code for HELambdaP.
//
// See file COPYING in top level folder for licence and copying
// information.

var xhttp = new XMLHttpRequest();


function set_nav_mode(mode) {
    str = '<div id=\"nav_index\"></div>';
    if (mode === 1) {
	str = '<div id=\"nav_map\"></div><div id=\"nav_list\"></div>';
    }
    document.getElementById('nav').innerHTML = str;
    
    str = '<div id=\"introduction_frame\"></div>';
    if (mode === 1) {
	str = '<div id=\"dictionary-entries_frame\"></div>';
    }
    document.getElementById('main').innerHTML = str;
};


function load_section(id, filename) {
    xhttp.open('GET', filename, false); 
    xhttp.send();
    document.getElementById(id).innerHTML = xhttp.responseText;
};


function load_index() {
    set_nav_mode(0);
    load_section('nav_index', 'introduction-navigation.html');
    load_section('main', 'introduction.html');
};


function load_dictionary() {
    set_nav_mode(1);

    load_section('nav_map',
		 'dictionary/dictionary-navigation.html');
    
    load_section('dictionary/dictionary-entries_frame',
		 'dictionary/dictionary-entries.html')
};

// end of file -- helambdap-js-support.js
"
	      )
|#
	     
             (<:link :rel "stylesheet" :href (element-style mv))
             ) ; HEAD

	    #|START OF HTML5 GENERATION|#
	    (<:body
             ((<:div :class "helambdap_doc")
              (<:header
               ((<:div :class "hlpdoctitle")
                (<:strong (or documentation-title (head-title mv))))
               ((<:nav :class "topnavigation" :id "topnavigation")

                ;; !!!!! ADD "topnavigation" and "mainnav" to helambdap5.css.

                ((<:a :href "#" :onclick "hlp_load_introduction()") "Index")
                "|"
                ;; ((<:a :href "#" :onclick "load_dictionary()") "Dictionary")
                ((<:a :href "#" :onclick "hlp_load_dictionary()") "Dictionary")
                )) ; HEADER
              	     
              #|
              (produce-navigation 'html5
                                  da
                                  da-file
                                  where
                                  doc-bits
                                  documentation-title)
              |#

              ((<:nav :class "mainnav" :id "mainnav") "")
	     
              ((<:main :id "main") "") ; ADDED "main" TAG GENERATION IN (X)HTMLambda

              
              (<:footer
               (<:strong (or documentation-title (body-title mv)))
               "HTML5 documentation produced with"
               ((<:a :href *helambdap-site* :target "_blank") "HE&Lambda;P")
               (<:br)
               (<:comment "hhmts start")
               "Last modified: " (text-timestamp)
               (<:comment "hhmts end")
               (<:br)
               (format nil "&copy; ~D, Marco Antoniotti, all rights reserved."
                       (nth-value 5 (decode-universal-time (get-universal-time))))
               ) ; FOOTER

              ;; Do load the 'introduction'.
              (<:script "hlp_load_introduction()")
              )
             )
            ) ; HTML END

           (string #\Newline)

           (<:comment 
            (format nil "end of file : ~A.html" index-name))
           ) ; DOCUMENT END
          )
      )

    (dolist (e (elements-of mv))
      (format t "~&>>> HLP: producing doc for ~S.~%" (element-name e))
      (produce-documentation 'html5 e where doc-bits
                             :documentation-title documentation-title)
      )))


(defmethod produce-documentation ((format (eql 'html5))
                                  (da doc-area)
                                  (where pathname)
                                  doc-bits
                                  &key
                                  (documentation-title)
                                  &allow-other-keys
                                  )
  (let* ((da-location (element-location da))
         (where (if da-location
                    (merge-pathnames da-location where)
                    where))

         (da-name (element-name da))
         (da-title (or documentation-title da-name))
         (da-pathname (make-pathname :name da-name
                                     :type *default-html-extension*
                                     :defaults where))
         (da-content (doc-area-content da))
         (da-navigation (doc-area-navigation da))
         )
    (declare (ignore da-title da-pathname da-navigation)) ; Come back later to fix this.

    (ensure-directories-exist where)

    (when da-content
      (produce-documentation format
                             da-content
                             where
                             doc-bits
                             :documentation-title documentation-title)

      (produce-navigation 'html5
                          da
                          ;; da-file
                          where
                          doc-bits
                          documentation-title))
    ))


(defmethod produce-navigation ((format (eql 'html5))
                               (element doc-area)
                               ;; (out stream)
                               (where pathname)
                               doc-bits
                               documentation-title
                               )
  (let ((nav (doc-area-navigation element)))
    (unless (or (null nav) (and (stringp nav) (string= nav "")))
      (let ((nav-pathname
             (merge-pathnames
              (merge-pathnames (element-name nav)
                               (make-pathname :type *default-html-extension*))
              where))
            )
        (declare (type pathname nav-pathname))
        (unless (and (probe-file nav-pathname)
                     (not *supersede-documentation*))
          (produce-navigation-links format
                                    element
                                    (doc-area-content element)
                                    nav-pathname
                                    doc-bits
                                    documentation-title))
        ))))


(defmethod produce-documentation ((format (eql 'html5))
                                  (structure doc-file)
                                  (where pathname)
                                  doc-bits
                                  &key
                                  (documentation-title "")
                                  &allow-other-keys
                                  )
  (declare (ignorable doc-bits))
  (let* ((doc-directory where)
         (dfname (element-name structure))
         (destination-path
          (make-pathname :directory (pathname-directory doc-directory)
                         :name (pathname-name dfname)
                         :type (pathname-type dfname)))
         )
    (cond ((probe-file dfname)
           (cl-fad:copy-file dfname destination-path :overwrite nil))

          ((not (probe-file destination-path))
           (produce-doc-file-placeholder structure
                                         destination-path
                                         documentation-title))
          )))


(defmethod produce-documentation ((format (eql 'html5))
                                  (structure doc-file)
                                  (where file-stream)
                                  doc-bits
                                  &key
                                  (documentation-title "")
                                  &allow-other-keys
                                  )
  (declare (ignorable doc-bits))
  (let* ((doc-directory where)
         (dfname (element-name structure))
         (destination-path
          (make-pathname :directory (pathname-directory doc-directory)
                         :name (pathname-name dfname)
                         :type (or (pathname-type dfname)
                                   *default-html-extension*)))
         )
    (cond ((probe-file dfname)
           (cl-fad:copy-file dfname destination-path :overwrite nil))
          ((not (probe-file destination-path))
           (produce-doc-file-placeholder structure
                                         destination-path
                                         documentation-title))
          )))


(defmethod produce-documentation ((format (eql 'html5))
                                  (structure file-set)
                                  ;; (where file-stream)
                                  (where pathname)
                                  doc-bits
                                  &key
                                  (documentation-title)
                                  &allow-other-keys
                                  )
  (declare (ignorable documentation-title))

  (produce-file-set-placeholder structure where)

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
        (produce-documentation 'html5
                               doc-bit
                               doc-bit-stream
                               doc-bits
                               :documentation-title documentation-title))))
  )


(defun produce-doc-file-placeholder (doc-file
                                     doc-file-pathname
                                     &optional (documentation-title ""))
  (declare (type doc-file doc-file)
           (type pathname doc-file-pathname)
           )
  (let ((dfname (element-name doc-file)))
    (with-open-file (dffs doc-file-pathname
                          :direction :output
                          :if-does-not-exist :create
                          :if-exists :supersede)
      (<:with-html-syntax-output (dffs :print-pretty t :syntax :compact)
          (<:div
           (<:h1 documentation-title dfname)
           (<:p "This is a placeholder for information pertaining "
                documentation-title)
           (<:p (format nil
                        "Please edit the file '~A', to complete ~
                         the documentation."
                        doc-file-pathname)))
          ))
    ))


(defun produce-file-set-placeholder (file-set where)
  (declare (type file-set file-set)
           (type stream where)
           )



  (let ((file-set-pathname
         (make-pathname :name (element-name file-set)
                        :type *default-html-extension*
                        :defaults (pathname where)))
        )
    (format t "HELAMBDAP: producing HTML5 file set placeholder~%~:
               ~S~%~:
               ~S~%~:
               ~S~2%"
            file-set where file-set-pathname)
    (with-open-file (fsfs file-set-pathname
                          :direction :output
                          :if-does-not-exist :create
                          :if-exists :supersede)
      (<:with-html-syntax-output (fsfs :print-pretty t :syntax :compact)
          (<:div
           (<:h1 "Dictionary Entries")
           (<:p "Click and/or scroll on the menus on the side to choose "
                "what information to display."))
          ))
    ))


;;;---------------------------------------------------------------------------
;;; Doc bits HTML production.

(defmethod process-doc-string
           ((s string)
            (input-syntax (eql 'text/hyperspec))
            (output-format (eql 'html5))
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
                 (if (string= ";;;" ep :end1 3 :end2 3)
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
                                        for arg-type-decl = (or (find-if (lambda (type-decl)
                                                                           (member arg (cddr type-decl)))
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


(defmethod render-doc-bit ((format (eql 'html5))
                           doc-bit
                           out
                           n
                           str-tag
                           doc-string)
  (let ((name (string-downcase n)))
    (declare (ignore name))
    (<:with-html-syntax-output (out :print-pretty t :syntax :compact)
      ((<:div :class "innertube")
       (produce-doc-bit-title-name doc-bit)
       (<:h2 "Package: ")
       (<:p (package-name (symbol-package n)))
       (process-doc-string doc-string 'text/hyperspec format)
       ))))


(defmethod render-lambda-list ((format (eql 'html5)) llt ll)
  ;; FTTB.
  (render-lambda-list 'html llt ll))



(defmethod produce-documentation ((format (eql 'html5))
                                  (doc-bit doc-bit)
                                  (out file-stream)
                                  doc-bits
                                  &key
                                  (documentation-title)
                                  &allow-other-keys
                                  )
  (declare (ignorable doc-bits documentation-title))
  (render-doc-bit format
                  doc-bit
                  out
                  (doc-bit-name doc-bit)
                  (doc-bit-kind-tag doc-bit)
                  (doc-bit-doc-string doc-bit)
                  ))


(defmethod produce-documentation ((format (eql 'html5))
                                  (doc-bit package-doc-bit)
                                  (out file-stream)
                                  doc-bits
                                  &key
                                  (documentation-title "")
                                  &allow-other-keys)
  "This specialized method produces the documentation for a package."
  (declare (ignorable doc-bits documentation-title))
  (let ((name (doc-bit-name doc-bit))
        (doc-string (doc-bit-doc-string doc-bit))
        )
    (<:with-html-syntax-output (out :print-pretty t :syntax :compact)
      ((<:div :class "innertube")
       (<:h1 (<:i "Package ") (<:strong name))

       (<:h2 "Use list:")
       (<:p (mapcan (lambda (up) (list up (<:br)))
		    (package-doc-bit-use-list doc-bit)))

       (<:h2 "Nicknames:")
       (<:p (mapcan (lambda (pn) (list pn (<:br)))
		    (package-doc-bit-nicknames doc-bit)))

       (process-doc-string doc-string 'text/hyperspec format))
      )))


(defmethod produce-documentation ((format (eql 'html5))
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
        ((<:div :class "innertube")
         (<:h1 (<:i "System ") (<:strong name))
         (when deps-on
           (list
            (<:h2 () "Depends on:")
            (<:p () (mapcan (lambda (d) (list (<:i () d) (<:br))) deps-on))))

         (process-doc-string doc-string 'text/hyperspec 'html))
        )))



#|
(defgeneric render-syntax-section (format doc-bit &optional lambda-list values))
|#


(defmethod render-syntax-section
           ((format (eql 'html5))
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
           ((format (eql 'html5))
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
           ((format (eql 'html5))
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


(defmethod produce-documentation ((format (eql 'html5))
                                  (doc-bit parameterized-doc-bit)
                                  (out file-stream)
                                  doc-bits
                                  &key
                                  ;; (documentation-title "")
                                  &allow-other-keys)
  (declare (ignorable doc-bits))
  (let* ((db-name (doc-bit-name doc-bit))
         (name (format nil "~(~A~)" db-name))
         (kind (doc-bit-kind doc-bit))
         (doc-string (doc-bit-doc-string doc-bit))
         (ll (parameterized-doc-bit-lambda-list doc-bit))
         )
    (declare (ignore name kind))
    (<:with-html-syntax-output (out :print-pretty t :syntax :compact)
        ((<:div :class "innertube")
         (produce-doc-bit-title-name doc-bit)

         (<:h2 "Package: ")
         (<:p (package-name (doc-bit-package doc-bit)))
       
         (<:h2 "Syntax:")
         (render-syntax-section format doc-bit)

         (process-doc-string doc-string 'text/hyperspec 'html
                             t (parse-ll :ordinary ll)))
	)))


(defmethod produce-documentation ((format (eql 'html5))
                                  (doc-bit function-doc-bit)
                                  (out file-stream)
                                  doc-bits
                                  &key
                                  ;; (documentation-title "")
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
    (declare (ignorable type-decls kind name))

    (<:with-html-syntax-output (out :print-pretty t :syntax :compact)
        ((<:div :class "innertube")
         (produce-doc-bit-title-name doc-bit)

         (<:h2 "Package: ")
         (<:p (package-name (doc-bit-package doc-bit)))

         (<:h2 "Syntax:")
         (render-syntax-section format doc-bit)

         (process-doc-string doc-string 'text/hyperspec 'html
                             t
                             (parse-ll :ordinary ll)
                             t
                             returns
                             type-decls))
        )))


(defmethod produce-documentation ((format (eql 'html5))
                                  (doc-bit macro-doc-bit)
                                  (out file-stream)
                                  doc-bits
                                  &key
                                  (documentation-title "")
                                  &allow-other-keys)
  (declare (ignorable doc-bits documentation-title))
  (let* ((db-name (doc-bit-name doc-bit))
         (name (format nil "~(~A~)" db-name))
         (kind (doc-bit-kind doc-bit))
         (doc-string (doc-bit-doc-string doc-bit))
         (ll (parameterized-doc-bit-lambda-list doc-bit))
         )
    (declare (ignore name kind))
    (<:with-html-syntax-output (out :print-pretty t :syntax :compact)
        ((<:div :class "innertube")
         (produce-doc-bit-title-name doc-bit)

         (<:h2 "Package: ")
         (<:p (package-name (doc-bit-package doc-bit)))

         (<:h2 "Syntax:")
         (render-syntax-section 'html doc-bit (macro-doc-bit-lambda-list doc-bit))

         (process-doc-string doc-string 'text/hyperspec 'html
                             t
                             (parse-ll :macro ll)
                             t
                             '(form)
                             ))
        )))


(defmethod produce-documentation ((format (eql 'html5))
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
    (declare (ignore kind))
    (<:with-html-syntax-output (out :print-pretty t :syntax :compact)
        ((<:div :class "innertube")
         (produce-doc-bit-title-name doc-bit)

         (<:h2 "Package:")
         (<:p (package-name (symbol-package name)))

         (<:h2 "Value:")
         (<:p (<:code value-presented))

         (process-doc-string doc-string 'text/hyperspec 'html))
        )))


(defmethod produce-documentation ((format (eql 'html5))
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
    (declare (ignore kind))
    (<:with-html-syntax-output (out :print-pretty t :syntax :compact)
        ((<:div :class "innertube")
         (produce-doc-bit-title-name doc-bit)

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
                                   "with initial value ~A ~
                                    of type ~A~@[; the slot is read-only~]."
                                   nil T nil))
                     else
                     nconc
                     (destructuring-bind (sn &optional sv &key read-only (type t))
                         s
                       (list
                        (<:dt () sn)
                        (<:dd ()
                              (format nil
                                      "with initial value ~A ~
                                       of type ~A~@[; the slot is read-only~]."
                                      sv type read-only)))))
               ))
         (process-doc-string doc-string 'text/hyperspec format))
        )))


(defmethod produce-documentation ((format (eql 'html5))
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
    (declare (ignore kind))
    (<:with-html-syntax-output (out :print-pretty t :syntax :compact)
        ((<:div :class "innertube")
         (produce-doc-bit-title-name doc-bit)

         (<:h2 "Package: ")
         (<:p (package-name (symbol-package name)))

         (<:h2 "Class Precedence List:")
         (<:p (format nil "~A &rarr;~@[~{ ~A~^&rarr;~}~] ... &rarr; T"
                      name
                      superclasses))

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

         (process-doc-string doc-string 'text/hyperspec format))
        )))
       

(defmethod produce-documentation ((format (eql 'html5))
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
        ((<:div :class "innertube")
         (<:h1 (<:i "Function") (<:strong name))
         (<:h2 "Package:")
         (<:h2 "Description:" (<:br) doc-string))
        )
    t))



;;; The next method should be reworked in order to take advantage of
;;; RENDER-SYNTAX-SECTION.  However, it works and I need to do other
;;; things now (20130618).


;;;---------------------------------------------------------------------------
;;; Auxiliary files production.

;;; NAVIGATION FILE FOR INDEX - 1 PART NAVIGATION

#| Pre-cleanup
(defmethod produce-navigation-file ((fs frameset)
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

    #|FRAME TO BE LINKED --- DONE|# 
    (flet ((make-nav-links ()
             (loop for sn in section-names
                   collect (<:li (:style "list-style-type: none")
                                 (<:a (:href (format nil "#~A"      ;"~A#~A"
                                                     ;(base-name doc-file-pathname)
                                                     sn)
				       ;:target (format nil "~A_frame"
				       ;              (element-name nav-element))
					     )
				      sn)
				      )))
           )
      (with-open-file (ns nav-pathname
                          :direction :output
                          :if-exists :supersede
                          :if-does-not-exist :create)
        (<:with-html-syntax-output (ns :print-pretty t :syntax :compact)
	  ((<:div :class "innertube")
	   (<:ul (make-nav-links)))
            #| OLD FRAME PRODUCTION
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
               (<:ul (make-nav-links))))
             (string #\Newline)
             (<:comment "end of file :" (base-name nav-pathname))
             )|#)))
    ))
|#


(defmethod produce-navigation-links ((format (eql 'html5))
                                     (da doc-area)
                                     (nav-element doc-file)
                                     nav-pathname
                                     doc-bits
                                     documentation-title)
  (declare (type pathname nav-pathname))
  (declare (ignorable doc-bits documentation-title))
  
  (let* ((doc-file-pathname (file-pathname nav-element nav-pathname))
         (sections (extract-sections doc-file-pathname 'html))
         (section-names (extract-section-names sections))
         )

    (flet ((make-nav-links ()
             (loop for sn in section-names
                   collect (<:li (:style "list-style-type: none")
                                 (<:a (:href (format nil "#~A" sn)
				       ;; :target (format nil "~A_frame"
				       ;;                 (element-name nav-element))
                                       )
				      sn)
                                 )))
           )
      (with-open-file (ns nav-pathname
                          :direction :output
                          :if-exists :supersede
                          :if-does-not-exist :create)
        (<:with-html-syntax-output (ns :print-pretty t :syntax :compact)
            (<:document
             (<:comment (base-name nav-pathname))
             ((<:div :class "nav_doc_file_links")
              (<:ul (make-nav-links)))
             (<:comment "end of file -- " (base-name nav-pathname))
             )))
      )))


;;; NAVIGATION FILE FOR DICTIONARY - 2 PART NAVIGATION

(defmethod produce-navigation-links ((format (eql 'html5))
                                     (da doc-area)
                                     (nav-element file-set)
                                     nav-pathname
                                     doc-bits
                                     documentation-title)
  (declare (type pathname nav-pathname))
  (declare (ignorable documentation-title))

  (format t "~&HELAMBDAP: producing HTML5 NAV LINKS file~%~:
           ~S~%~:
           ~S~%~:
           ~S~2%"
          da nav-element nav-pathname)

  (let* ((da-name (element-name da))

         (nav-map-pathname
          (make-pathname :name (format nil
                                       "~A-navigation-map"
                                       da-name)
                         :type *default-html-extension*
                         :defaults nav-pathname))
         (nav-list-pathname
          (make-pathname :name (format nil
                                       "~A-navigation-lists"
                                       da-name)
                         :type *default-html-extension*
                         :defaults nav-pathname))
         )
    (declare (ignore nav-list-pathname))
    
    (produce-navigation-map format
                            da
                            nav-element
                            nav-map-pathname
                            doc-bits
                            documentation-title)
    ))


#| Made into a generic function (see protocol and xhtml-lambda-producer) |#

(defmethod produce-navigation-map ((format (eql 'html5))
                                   (da doc-area)
                                   (nav-element file-set)
                                   nm-pathname
                                   doc-bits
                                   documentation-title)
  (format t "~&HELAMBDAP: producing HTML5 NAV MAP file~%~:
           ~S~%~:
           ~S~%~:
           ~S~2%"
          da nav-element nm-pathname)
  (let ((nav-element-target (format nil "~A_frame" (element-name nav-element))))
    (with-open-file (nm nm-pathname
                        :direction :output
                        :if-exists :supersede
                        :if-does-not-exist :create)
      (<:with-html-syntax-output (nm :print-pretty t :syntax :compact)
          (<:document
           (<:comment (base-name nm-pathname))
           ((<:div :class "nav_file_set_links")
            ((<:div :class "nav_menu")
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
			 
                             #|FRAME TO BE LINKED --- DONE|#
                             for s-filename = (base-name s-doc-pathname)
                             collect (<:p ()
                                          (<:a (:href "#"
                                                :onclick
                                                (format nil
                                                        "load_section('~A', '~A')"
                                                        nav-element-target
                                                        s-filename))
                                               ;; (:href s-filename
                                               ;;	:target nav-element-target
                                               ;;	)
                                               (doc-bit-name s))
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

                             #|FRAME TO BE LINKED --- DONE|#
                             do (produce-package-navigation-list format
                                                                 da
                                                                 nav-element
                                                                 p
                                                                 p-list-pathname
                                                                 doc-bits)
                             collect (<:p ()
                                          (<:a (:href "#"
                                                :onclick
                                                (format nil
                                                        "load_section('~A', '~A');
                                                         load_section('nav_list', '~A');"
                                                        nav-element-target
                                                        p-filename
                                                        p-list-filename)
                                                )
                                               ;; (:href p-filename
                                               ;;		:target nav-element-target
                                               ;;		:onclick
                                               ;;		(format nil
                                               ;;			"parent.frames[1].location.href = '~A'"
                                               ;;			p-list-filename
                                               ;;			)
                                               ;;		)
                                               (doc-bit-name p)))))
                ))))
           (<:comment "end of file -- " (base-name nm-pathname)))
          ))))


(defmethod produce-package-navigation-list ((format (eql 'html5))
                                            fs
                                            nav-element
                                            pkg-doc-bit
                                            pkg-list-pathname
                                            doc-bits)
  (declare (type frameset fs))
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
          ;; This is an horrendous kludge. Alas, I FTTB will not
          ;; chase down instances of people defining systems with symbols.
          )
         )
    (format t "~&HELAMBDAP: produce-package-navigation-list HTML5 ~%~:
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

      #|FRAME TO BE LINKED --- DONE|#
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
                                           (<:a (:href "#"
						 :onclick
						 (format nil
							 "load_section('~A', '~A')"
							 target
							 db-filename))
					       ;; (:href db-filename
                                               ;;  :target target)
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
              (if pkg-doc-bits
                  (<:htmlise (:syntax :compact)
                      ((<:div :class "innertube")
                       (<:h3 "Package interface" <:br
                             (package-name pkg))

                       ((<:div #| :class "helambdap_navmap" |#)
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
                        (build-list "Functions" functions)
                        (build-list "Macros" macros)
                        (build-list "Method Combinations" method-combinations)
                        (build-list "Setf expanders" setf-expanders)
                        (build-list "Modify Macros" modify-macros)
                        )
                       ))

                  (<:htmlise (:syntax :compact)
                      ((<:div :class "innertube")
                       (<:h3 "Package interface" <:br
                             (package-name pkg))

                       (<:p (<:i "No published interface."))
                       ))
                  )
              )))
      )))


;;;; end of file -- html5-lambda-producer.lisp --

;;;; -*- Mode: Lisp -*-

;;;; texinfo-producer.lisp --
;;;; Make a file out of a DOCUMENTATION-STRUCTURE and a set (list) of
;;;; DOC-BITs, using a TeXinfo output format.

(in-package "HELAMBDAP")

;;;;===========================================================================
;;;; Prologue.



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

(defparameter *formatted-section-right-margin* 80)


;;;;===========================================================================
;;;; Implementation.

(defmethod produce-documentation ((format (eql 'texinfo))
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
    (produce-documentation 'texinfo c where doc-bits
                           :documentation-title documentation-title)
    ))


(defmethod produce-documentation ((format (eql 'texinfo))
                                  (structure texinfo-file)
                                  (where pathname)
                                  &key
                                  documentation-title
                                  &allow-other-keys)
  (let ((tf-location (texinfo-file-location structure))
        (where (if tf-location
                   (merge-pathnames tf-location where)
                   where))
        (tf-name (texinfo-file-name structure))
        (tf -pathname (make-pathname :name tf-name
                                     :type *default-texinfo-extension*
                                     :defaults where))
        )

    (ensure-directories-exist where)
    
    (with-open-file (tf-file tf-pathname
                             :direction :output
                             :if-does-not-exist :create
                             :if-exists :supersede)
      (format tf-file "\\input texinfo @c -*- Mode: texinfo -*-~@
                       @c %**start of header~@
                       @setfilename ~A~@
                       @settitle ~A~@
                       @c %**end of header~%"
              tf-name
              documentation-title
              )
      )))


(defmethod produce-documentation ((format (eql 'texinfo))
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


(defmethod produce-documentation ((format (eql 'texinfo))
                                  (structure file-set)
                                  (where file-stream)
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
        (produce-documentation 'html
                               doc-bit
                               doc-bit-stream
                               doc-bits))))
                           

  )


(defun produce-file-set-placeholder (file-set where)
  (declare (type file-set file-set)
           (type stream where)
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
             (<:link :rel "stylesheet"
                     :href (namestring *helambdap-css-filename-up*)))
            
            (<:body
             (<:h1 "Dictionary Entries")
             (<:p "Click on the menu on the side to choose what information to display.")
             )
            )
           (<:comment "end of file : " (file-set-name file-set)))
          ))
    ))

;;;---------------------------------------------------------------------------
;;; Doc bits HTML production.

;;; Usual SBCL appeasement.

#-sbcl
(defconstant +lambda-list-kwds+
  '(&optional &rest &key &allow-other-keys &whole &environment &aux))

#+sbcl
(defparameter +lambda-list-kwds+
  '(&optional &rest &key &allow-other-keys &whole &environment &aux))


(defun arg-name (arg)
  (if (consp arg)
      (let ((a1 (first arg)))
        ;; Is it a KWD spec?
        (if (consp a1)
            (first a1) ; The "visible" bit.
            a1
            ))
      arg))


(defun paragraphize-doc-string (s)
  (loop for par in (split-at-tex-paragraphs s)
        when (string/= "" par)
        collect (<:p () par)))


(defgeneric process-doc-string (s input-syntax output-format)
  (:documentation
   "Processes a 'doc string'.

The processing is done (or rather a best effort is made to parse)
according to a give 'input-syntax' and the result is built in a
given 'output-format'."))


(defun parse-doc-hyperspec-style (s)
  (declare (type string s))
  (let* ((syntax-header "Syntax:")
         (shl (length syntax-header))
         (args-n-values "Arguments and Values:")
         (anvl (length args-n-values))
         (description "Description:")
         (dl (length description))
         (examples "Examples:")
         (el (length examples))
         (affected-by "Affected By:")
         (abl (length affected-by))
         (see-also "See Also:")
         (sal (length see-also))
         (notes "Notes:")
         (nl (length notes))
         (pars (split-at-tex-paragraphs s))
         )
    ;; Assumes that the doc string follows the Hyperspec sectioning
    ;; conventions. However, it ignores the 'Syntax:' section and it
    ;; assumes that - at a minimum - everything is 'Description:'.

    (loop ; with syntax-pars = ()
;;;;           with args-n-values-pars = ()
;;;;           with description-pars = ()
;;;;           with examples-pars = ()
;;;;           with affected-by-pars = ()
;;;;           with see-also-pars = ()
;;;;           with notes-pars = ()
          with state = 'description
          
          for p in pars
          for pl = (length p)
          
          do (format t ">>> ~A ~S~%" state p)
          
          if (string= p syntax-header :end1 (min pl shl))
          do (setf state 'syntax-header)
          else if (string= p args-n-values :end1 (min pl anvl))
          do (setf state 'args-n-values)
          else if (string= p description :end1 (min pl dl))
          do (setf state 'description)
          else if (string= p examples :end1 (min pl dl))
          do (setf state 'examples)
          else if (string= p affected-by :end1 (min pl abl))
          do (setf state 'affected-by)
          else if (string= p see-also :end1 (min pl sal))
          do (setf state 'see-also)
          else if (string= p notes :end1 (min pl nl))
          do (setf state 'notes)
          
          else
          
          if (eq state 'syntax-header)
          collect p into syntax-pars
          
          else if (eq state 'args-n-values)
          collect p into args-n-values-pars
          
          else if (eq state 'description)
          collect p into description-pars
          
          else if (eq state 'examples)
          collect p into examples-pars
          
          else if (eq state 'affected-by)
          collect p into affected-by-pars
          
          else if (eq state 'see-also)
          collect p into see-also-pars
          
          else if (eq state 'notes)
          collect p into notes-pars

          ;; else collect p into description-pars ; ? Should I leave this?
          end

          finally
          (return (values syntax-pars
                          args-n-values-pars
                          description-pars
                          examples-pars
                          affected-by-pars
                          see-also-pars
                          notes-pars))
          )))


(defmethod process-doc-string
           ((s string)
            (input-syntax (eql 'text/hyperspec))
            (output-format (eql 'html)))
  ;; Try to process Hyperspec-style.

  (multiple-value-bind (syntax-pars
                        args-n-values-pars
                        description-pars
                        examples-pars
                        affected-by-pars
                        see-also-pars
                        notes-pars)
      (parse-doc-hyperspec-style s)
    (declare (ignore syntax-pars args-n-values-pars))
    
    (let ((elements ()))
      (flet ((push-pars (subsection-header pars)
               (when subsection-header
                 (push (<:h2 () subsection-header) elements))
               (dolist (par pars)
                 (when (string/= "" par)
                   (push (<:p () par) elements))))
             )
        (push-pars nil description-pars)

        (when examples-pars
          (push (<:h2 () "Examples:") elements)
          (push (<:pre () (sanitize-string-for-html
                           (format nil "~{~A~2%~}" examples-pars)))
                elements))

        (push-pars (and affected-by-pars "Affected By:") affected-by-pars)

        (push-pars (and see-also-pars "See Also:") see-also-pars)
    
        (push-pars (and notes-pars "Notes:") notes-pars)

        (nreverse elements)
        ))))


(defun dump-doc-bit-html (n str-tag doc-string out)
  (let ((name (string-downcase n)))
    (<:with-html-syntax-output (out :print-pretty t :syntax :compact)
        (<:document
         (<:head
          (<:title (format nil "~A ~A" str-tag name))
          (<:link :rel "stylesheet" :href (namestring *helambdap-css-filename-up*)))
         (<:body
          (<:h1 (<:i (format nil "~A " str-tag)) (<:strong name))
          (<:h2 "Package: ")
          (<:p (package-name (symbol-package n)))
          (<:h2 "Description:")
          ;; (paragraphize-doc-string doc-string)
          (process-doc-string doc-string 'text/hyperspec 'texinfo)
          )))))



(defgeneric render-lambda-list (lambda-list-type lambda-list))

(defmethod render-lambda-list ((llt (eql :ordinary)) (ll list))
  (loop for lle in ll
        if (member lle +lambda-list-kwds+ :test #'eq)
        collect (<:span (:style "color: blue") (string lle))
        else
        collect (<:i () lle))
  )


(defgeneric render-syntax-section (format doc-bit &optional lambda-list values))


(defmethod produce-documentation ((format (eql 'texinfo))
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


(defmethod produce-documentation ((format (eql 'texinfo))
                                  (doc-bit package-doc-bit)
                                  (out file-stream)
                                  doc-bits
                                  &key
                                  ;; documentation-title
                                  &allow-other-keys)
  "This specialized method produces the documentation for a package."
  (let ((name (doc-bit-name doc-bit))
        (doc-string (doc-bit-doc-string doc-bit))
        )
    (<:with-html-syntax-output (out :print-pretty t :syntax :compact)
        (<:document
         (<:head
          (<:title "Package " name)
          (<:link :rel "stylesheet" :href (namestring *helambdap-css-filename-up*)))
         (<:body
          (<:h1 (<:i "Package ") (<:strong name))
          (<:h2 "Use list:") (<:p (package-doc-bit-use-list doc-bit))
          (<:h2 "Nicknames:") (<:p (package-doc-bit-nicknames doc-bit))
          (<:h2 "Description:")
          ;; (paragraphize-doc-string doc-string)
          (process-doc-string doc-string 'text/hyperspec 'texinfo)
         )))))


(defmethod produce-documentation ((format (eql 'texinfo))
                                  (doc-bit system-doc-bit)
                                  (out file-stream)
                                  doc-bits
                                  &key
                                  ;; documentation-title
                                  &allow-other-keys)
  (let ((name (doc-bit-name doc-bit))
        (doc-string (doc-bit-doc-string doc-bit))
        )
    (<:with-html-syntax-output (out :print-pretty t :syntax :compact)
        (<:document
         (<:head
          (<:title "System " name)
          (<:link :rel "stylesheet" :href (namestring *helambdap-css-filename-up*)))
         (<:body
          (<:h1 (<:i "System ") (<:strong name))
          (<:h2 "Depends on:")
          (<:p (mapcar (lambda (d) (<:i () d))
                       (system-doc-bit-depends-on doc-bit)))
          (<:h2 "Description:")
          ;; (paragraphize-doc-string doc-string)
          (process-doc-string doc-string 'text/hyperspec 'texinfo)
          )
         ))))


(defmethod render-syntax-section
           ((format (eql 'texinfo))
            (doc-bit parameterized-doc-bit)
            &optional
            (ll (parameterized-doc-bit-lambda-list doc-bit))
            (values ()))

  (declare (ignore values))

  (let ((db-name (doc-bit-name doc-bit)))
    (<:htmlise (:syntax :compact)
        (<:div
         (<:h2 "Syntax:")
         ((<:p :class "syntax_section")
          (<:b ((<:span :style "color: red") (<:strong () (string-downcase db-name))))
          (render-lambda-list :ordinary ll)))
        )))


(defmethod produce-documentation ((format (eql 'texinfo))
                                  (doc-bit parameterized-doc-bit)
                                  (out file-stream)
                                  doc-bits
                                  &key
                                  documentation-title
                                  &allow-other-keys)
  (let* ((db-name (doc-bit-name doc-bit))
         (name (format nil "~(~A~)" db-name))
         (kind (doc-bit-kind doc-bit))
         (doc-string (doc-bit-doc-string doc-bit))
         (ll (parameterized-doc-bit-lambda-list doc-bit))
         )
    (flet ((render-lambda-list (ll)
             (loop for lle in ll
                   if (member lle +lambda-list-kwds+ :test #'eq)
                   collect (<:span (:style "color: blue") (string lle))
                   else
                   collect (<:i () lle))
             )
           )
      (<:with-html-syntax-output (out :print-pretty t :syntax :compact)
          (<:document
           (<:head
            (<:title documentation-title ": " kind name)
            (<:link :rel "stylesheet" :href (namestring *helambdap-css-filename-up*)))

           (<:body
            (<:h1 (<:i kind) (<:strong name))

            (<:h2 "Package: ")
            (<:p (<:code (package-name (doc-bit-package doc-bit))))

            (<:h2 "Syntax:")
            (<:p
             (<:pre
              (format nil
                      "~&    ~A~A~%"
                      (<:b () (<:span (:style "color: red") (<:strong () name)))
                      (format nil "~{ ~A~}" (render-lambda-list ll)))))

            (when ll
              (<:div ()
                     (<:h3 () "Arguments and Values:")
                     (loop for arg in ll
                           unless (member arg +lambda-list-kwds+ :test #'eq)
                           collect
                           (<:htmlize
                            (<:p (<:i (<:code (arg-name arg)))
                                 "---"
                                 "a T." ; To be FIXED.
                                 )
                            :syntax :compact))))

            (<:h2 "Description:")
            ;; (paragraphize-doc-string doc-string)
            (process-doc-string doc-string 'text/hyperspec 'texinfo)
           ))))))


(defmethod produce-documentation ((format (eql 'texinfo))
                                  (doc-bit macro-doc-bit)
                                  (out file-stream)
                                  doc-bits
                                  &key
                                  documentation-title
                                  &allow-other-keys)
  (let* ((db-name (doc-bit-name doc-bit))
         (name (format nil "~(~A~)" db-name))
         (kind (doc-bit-kind doc-bit))
         (doc-string (doc-bit-doc-string doc-bit))
         (ll (parameterized-doc-bit-lambda-list doc-bit))
         )
    (labels ((render-lambda-list (ll) ; Rather kludgy. Should dig out
                                      ; proper lambda list parsing.
               (loop with state = t
                     for lle in ll
                     if (member lle +lambda-list-kwds+ :test #'eq)
                       collect (<:span (:style "color: blue") (string lle))
                     else if (symbolp lle)
                       collect (<:i () lle)
                     else if (and (eq state t) (consp lle)) ; This is
                                                            ; not fully correct either.
                       collect (render-lambda-list lle)
                     else if (consp lle)
                       collect (format nil "(~{<i>~A</i>~^ ~})" lle)
                     else
                       collect (<:i () lle)
                     end
                     do (case lle
                          (&optional (setf state '&optional))
                          (&key (setf state '&key))
                          (&aux (setf state '&aux))
                          (&rest (setf state '&rest))
                          ((&whole &environment) (setf state t)) ; Other kludge!
                          )
                     ))
             )
      (<:with-html-syntax-output (out :print-pretty t :syntax :compact)
          (<:document
           (<:head
            (<:title documentation-title ": " kind name)
            (<:link :rel "stylesheet" :href (namestring *helambdap-css-filename-up*)))

           (<:body
            (<:h1 (<:i kind) (<:strong name))

            (<:h2 "Package: ")
            (<:p (<:code (package-name (doc-bit-package doc-bit))))

            (<:h2 "Syntax:")
            (<:p
             (<:pre
              (format nil
                      "~&    ~A~A~%"
                      (<:b () (<:span (:style "color: red") (<:strong () name)))
                      (format nil "~{ ~A~}" (render-lambda-list ll)))))

            (when ll
              (<:div ()
                     (<:h3 () "Arguments and Values:")
                     (loop for arg in ll
                           unless (member arg +lambda-list-kwds+ :test #'eq)
                           collect
                           (<:p () (<:i () (<:code () (arg-name arg)))
                                "---"
                                "a T." ; To be FIXED.
                                )
                           )))

            (<:h2 "Description:")
            ;; (paragraphize-doc-string doc-string)
            (process-doc-string doc-string 'text/hyperspec 'texinfo)
           ))))))


(defmethod produce-documentation ((format (eql 'texinfo))
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
    (<:with-html-syntax-output (out :print-pretty t :syntax :compact)
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
           ;; (paragraphize-doc-string doc-string)
            (process-doc-string doc-string 'text/hyperspec 'texinfo)
         ))))))


(defmethod produce-documentation ((format (eql 'texinfo))
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
    (<:with-html-syntax-output (out :print-pretty t :syntax :compact)
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
           ;; (paragraphize-doc-string doc-string)
            (process-doc-string doc-string 'text/hyperspec 'texinfo)
         ))))))


(defmethod produce-documentation ((format (eql 'texinfo))
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
    (<:with-html-syntax-output (out :print-pretty t :syntax :compact)
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
           ;; (paragraphize-doc-string doc-string)
            (process-doc-string doc-string 'text/hyperspec 'texinfo)
         ))))))
       

(defmethod produce-documentation ((format (eql 'texinfo))
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

    (<:with-html-syntax-output (out :print-pretty t :syntax :compact)
        (<:document

         (<:head
          (<:title (format nil "DOC FOR ~A" (string-downcase name)))
          (<:link :rel "stylesheet" :href (namestring *helambdap-css-filename-up*)))

         (<:body
          (<:h1 (<:i "Function") (<:strong name))
          (<:h2 "Package:")
          (<:h2 "Description:" (<:br) doc-string)
          )))
    t))


#+helambdap.version-using-MOP
(defmethod produce-documentation ((format (eql 'texinfo))
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
      (<:with-html-syntax-output (out :print-pretty t :syntax :compact)
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
            ;; (paragraphize-doc-string doc-string)
            (process-doc-string doc-string 'text/hyperspec 'texinfo)
           
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


#-helambdap.version-using-MOP
(defmethod produce-documentation ((format (eql 'texinfo))
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

           (render-lambda-list (ll)
             (loop for lle in ll
                   if (member lle +lambda-list-kwds+ :test #'eq)
                   collect (<:span (:style "color: blue") (string lle))
                   else
                   collect (<:i () lle))
             )

#|
           (pre-format-syntax-entry (name ll)
             (let ((*print-right-margin* 80))
               (format nil
                       (concatenate 'string
                                    "~A~<"
                                    (format nil "~A"
                                            (make-string (length name)
                                                         :initial-element #\Space))
                                    "~;~@{ ~W~_~}~:>")
                       (<:span (:style "color: red") (<:strong () name))
                       (render-lambda-list ll))
               ))|#


           (pre-format-syntax-entry (name ll)
             (let ((*print-right-margin* 75))
               (format nil
                       "  ~A ~@<~@/pprint-linear/~:>~%    &rarr; <i>result</i>"
                       (<:span (:style "color: red") (<:strong () name))
                       (render-lambda-list ll))
               ))

           #|
           (render-known-methods (name doc-bit)
             (loop for mdb of-type method-doc-bit in (generic-function-doc-bit-methods doc-bit)
                   for (specializers other) = (method-signature mdb)
                   for mdb-doc = (doc-bit-doc-string mdb)
                   collect (<:htmlise (:syntax :compact)
                            (<:li
                             (<:p
                              (<:pre
                               (format nil
                                       "  ~A~A~A~A"
                                       (<:strong () name)
                                       (format nil "~@[~{~A~^ ~}]~]" (method-doc-bit-qualifiers mdb))
                                       (format nil "~{ &lt;<i>~A</i>&gt;~}" specializers)
                                       (format nil "~{ ~A~}" (render-lambda-list other)))))
                             (when mdb-doc
                               (<:p ()  mdb-doc))))
                   ))|#
           #|
           (render-known-methods (name doc-bit)
             (let ((*print-right-margin* 80))
               (loop for mdb of-type method-doc-bit in (generic-function-doc-bit-methods doc-bit)
                     for (specializers other) = (method-signature mdb)
                     for mdb-doc = (doc-bit-doc-string mdb)
                     for qualfs = (method-doc-bit-qualifiers mdb)
                     collect (<:htmlise (:syntax :compact)
                                 (<:li
                                  (<:p
                                   (<:pre
                                    (let ((*print-right-margin* 70)
                                          (*print-pretty* t)
                                          )
                                      (format nil
                                              "  ~@<~A~@/pprint-linear/~-8I~@/pprint-linear/~:>"
                                              (<:strong () name)
                                              qualfs
                                              (nconc
                                               (loop for s in specializers
                                                     collect (<:i () s))
                                               (render-lambda-list other))
                                              ))))
                                  (when mdb-doc
                                    (<:p ()  mdb-doc)))
                                 ) ; <:li
                     )))|#

           (bypass-pprint (s e &optional (colon-p t) at-sign-p)
             (declare (ignore colon-p at-sign-p))
             (let ((*print-pretty* nil))
               (format s "~A" e)))

           (quoted-exp-p (exp) (and (listp exp) (eq 'quote (first exp))))

           (eql-spec-p (sp) (and (listp sp) (eq 'eql (first sp))))

           (render-method-specializers (specializers)
             (loop for sp in specializers
                   for s = (if (eql-spec-p sp)
                               (if (quoted-exp-p (second sp))
                                   (<:i () (format nil "(EQL '~A)" (second (second sp))))
                                   (<:i () sp))
                               (<:i () (format nil "&lt;~A&gt;" sp)))
                   collect s))

           (render-known-methods (name doc-bit)
             (let ((*print-right-margin* *formatted-section-right-margin*))
               (loop for mdb of-type method-doc-bit in (generic-function-doc-bit-methods doc-bit)
                     for (specializers other) = (method-signature mdb)
                     for mdb-doc = (doc-bit-doc-string mdb)
                     for qualfs = (method-doc-bit-qualifiers mdb)
                     collect (<:htmlise (:syntax :compact)
                                 (<:li
                                  (<:p
                                   (<:pre
                                    (let ((*print-pretty* t))
                                      (with-output-to-string (pre-string)
                                        (pprint-logical-block (pre-string
                                                               (list (<:strong () name)
                                                                     qualfs
                                                                     ;; (list :around)
                                                                     (nconc
                                                                      (render-method-specializers specializers)
                                                                      (render-lambda-list other))
                                                                     ))
                                          (write-string "  " pre-string)
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
                                      #|
                                      (format nil
                                              "  [1~@<[2~@<~/helambdap::bypass-pprint/~
                                                      ~@[*[3~@<~@{ ~/helambdap::bypass-pprint/~^~_~}~:>3]~]2]~:>~
                                                      ~@[~_~-10I~@{ ~/helambdap::bypass-pprint/~^~_~}~]~:>1]"
                                              (<:strong () name)
                                              ;qualfs
                                              (list :around :before)
                                              (nconc
                                               (loop for s in specializers
                                                     collect (<:i () s))
                                               (render-lambda-list other))
                                              )
                                      |#
                                    )) ; <:/pre <:/p
                                  (when mdb-doc
                                    (<:p ()  mdb-doc))
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
            (<:link :rel "stylesheet" :href (namestring *helambdap-css-filename-up*)))
           (<:body

            (<:h1 (<:i "Generic Function") (<:strong name))

            (<:h2 "Package:")
            (<:p (package-name (symbol-package gfname)))

            (<:h2 "Syntax:")
            (<:p
             #|
             (<:pre (format nil "~&    ~A~A~%"
                            (<:b () (<:span (:style "color: red") (<:strong () name)))
                            (format nil "~{ ~A~}" (render-lambda-list ll))))
             |#
             (<:pre (pre-format-syntax-entry name ll)
                    ))

            (<:h3 "Arguments and Values:")
            (loop for arg in ll
                  unless (member arg +lambda-list-kwds+ :test #'eq)
                  collect
                  (<:htmlize
                   (<:p (<:i (<:code (arg-name arg)))
                        "---"
                        (if (consp arg)
                            (format nil "~A." (second arg))
                            "T.")
                        )
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
            ;; (paragraphize-doc-string doc-string)
            (process-doc-string doc-string 'text/hyperspec 'texinfo)
           

            (let ((known-methods-els (render-known-methods name doc-bit))
                  )
              (when known-methods-els
                (<:div ()
                       (<:h3 () "Known Documented Methods:")
                       (<:ol () known-methods-els)))
              
              ))
           )))))


;;;---------------------------------------------------------------------------
;;; Auxiliary files production.

(defgeneric frameset-head-title (fs)
  (:method ((fs frameset)) "Frameset head placeholder title"))


(defgeneric frameset-body-title (fs)
  (:method ((fs frameset)) "Frameset body placeholder title"))


(defgeneric framesets-of (e)
  (:method ((fss framesets)) (framesets-list fss))
  (:method ((e element)) ())
  (:method ((e documentation-structure)) ())
  )


(defmethod produce-header-file ((fs frameset) header-pathname documentation-title)
  (declare (type pathname header-pathname))
  (declare (ignorable documentation-title))
  (let ((fs-order (frameset-order fs))
        (fs-head-title (frameset-head-title fs))
        (fs-body-title (frameset-body-title fs))
        (ed-fs (element-location-depth fs))
        )
    (declare (ignorable fs-order))
    (labels (
	     #| Commented to placate SBCL.  Note commemts below of actual usage.
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
        (<:with-html-syntax-output (hs :print-pretty t :syntax :compact)
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
               ))))
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
             (<:link :rel "stylesheet" :href (frameset-style fs)))

            (<:body
             (<:ul))
            )))
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
             (<:link :rel "stylesheet" :href (frameset-style fs)))
            
            ((<:frameset :rows "25%,75%"
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
           (string #\newline)))) ; WITH-OPEN-FILE...

    (produce-navigation-map fs nav-element nav-map-pathname doc-bits)
    ))


(defun produce-navigation-map (fs nav-element nm-pathname doc-bits)
  (format t "~&HELAMBDAP: producing NAV MAP file ~S ~S ~S~2%"
          fs nav-element nm-pathname)
  (let ((nav-element-target (format nil "~A_frame" (element-name nav-element))))
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
             (<:link :rel "stylesheet"
                     :href (namestring *helambdap-css-filename-up*)) ; TESTING!
             (<:style (format nil
                              ".helambdap_navmap li {~
                                  display: inline;~
                              }"))
             )

            (<:body
             ((<:div :class "helambdap_navmap")
              (<:h3 "Systems and Packages")


              (<:p
               (<:strong ((<:script :type "text/javascript")
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
            ))))))


(defun produce-package-navigation-list (fs
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
    (format t "~&HELAMBDAP: produce-package-navigation-list ~S ~S ~S ~S~%"
            fs
            (package-name pkg)
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
                 (<:link :rel "stylesheet" :href (frameset-style fs))
                 (<:style (format nil
                                  ".helambdap_navmap li {~
                                      display: inline;~
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
             )))))
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

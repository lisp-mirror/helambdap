;;;; -*- Mode: Lisp -*-

;;;; doc-structure.lisp --
;;;; Collecting the documentation about a library entails a number of
;;;; steps, with the goal of producing a "structured" web site
;;;; organized as a (static) set of pages.
;;;;
;;;; The final organization of the the documentation web site is stored in a
;;;; "template file" which may or may not exist.
;;;;
;;;; In any case, the assumption is that a "library" is organized in a
;;;; directory/folder tree, where "source files" reside, alongside some
;;;; specialized files containing other information.
;;;;
;;;; The following are "source file" extensions recognized, plus
;;;; "specialized files" used by the documentation system.
;;;;
;;;; Extensions: lisp, lsp, asd, system, cl
;;;;
;;;; Specialized files:
;;;;     info.hela   : file containing the "information about the
;;;;                   content of a directory"; usually XHTML.
;;;;     intro.hela  : same as 'info.hlam'.
;;;;     struct.hela : "template" file.
;;;;     doc.hlam    : file containing the "doc bits" of a directory;
;;;;                   this is a Lisp source file containing READable
;;;;                   S-Expressions.

(in-package "HELAMBDAP")


;;;---------------------------------------------------------------------------
;;; Global definitions.

(defparameter *helambdap-css-filename* "helambdap.css"
  "The default name for the (x)html .css file.")


(defparameter *helambdap-css-pathname*
  (make-pathname :name "helambdap"
                 :type "css"
                 :defaults *load-pathname*)
    "The default pathname for the (X)HTML .css file.

The default directory is set to the location of the source file.")


(defparameter *helambdap5-css-filename* "helambdap5.css"
  "The default name for the HTML5 .css file.")


(defparameter *helambdap5-css-pathname*
  (make-pathname :name "helambdap5"
		 :type "css"
		 :defaults *load-pathname*)
  "The default pathname for the (X)HTML .css file.

The default directory is set to the location of the source file.")

(defparameter *default-html-extension* "html")


(defparameter *helambdap-js-pathname*
  (merge-pathnames (make-pathname :name "helambdap-support"
                                  :type "js"
                                  :directory (list :relative "js"))
                   *load-pathname*))


;;; Useful macro

(eval-when (:compile-toplevel :load-toplevel :execute)

(defmacro def-element-class (name superclasses slots &rest options)
  (let ((pred-name (intern (format nil "~A-P" name) (symbol-package name))))
    `(progn
       (defclass ,name ,superclasses ,slots ,@options)
       (defgeneric ,pred-name (x)
         (:method ((x ,name)) t)
         (:method ((x t)) nil))
       (defun ,name (name &rest keys &key &allow-other-keys)
         (apply #'make-instance ',name :name name keys))
       )))
)


;;;---------------------------------------------------------------------------
;;; Known output formats.

(defparameter *known-output-formats*
  (list 'html
        'html5
        ;; 'texinfo
        )
  "A list of known output formats.")


(defgeneric output-format-tag (x)
  (:documentation
   "Translates from a 'user' tag to the canonical internal one.

The internal tags are symbols in the HELambdaP implementation packages
(which may, or may not be exported).

Examples:

(output-format-tag :html) ==> HLP:HTML

")
  (:method ((x (eql :html))) 'html)
  (:method ((x (eql 'html))) 'html)

  (:method ((x (eql :html5))) 'html5)
  (:method ((x (eql 'html5))) 'html5)

  ;;   (:method ((x (eql :texinfo))) 'texinfo)
  ;;   (:method ((x (eql 'texinfo))) 'texinfo)
  )


;;;---------------------------------------------------------------------------
;;; Template/structure file management.

;;; documentation-structure --

(defclass documentation-structure ()
  ((name :reader documentation-structure-name :initarg :name)
   (root :reader documentation-structure-root :initarg :root)
   (structure :reader documentation-structure-structure
              :initarg :structure
              :type list)
   (structure-table :reader structure-table
                    :initform (make-hash-table :test #'equal))
   (property-table :reader property-table
                   :initform (make-hash-table :test #'equal))
   )
  (:documentation "The Document Structure Class."))



(defun property (ds p &optional default)
  (declare (type documentation-structure ds)
           (type symbol p))
  (gethash p (property-table ds) default))


(defun (setf property) (v ds p)
  (declare (type documentation-structure ds)
           (type symbol p))
  (setf (gethash p (property-table ds)) v))


(defun set-property (ds p v)
  (declare (type documentation-structure ds)
           (type symbol p))
  (setf (property ds p) v))



;;;---------------------------------------------------------------------------
;;; Elements of a doc structure (template).
;;;
;;; 20170215 MA: Reworked with classes.

;;; element --

(defclass element ()
  ((name :initform (symbol-name (gensym "ELEMENT-"))
         :reader element-name
         :reader name
         :initarg :name)
   (doc-structure :initform nil
                  :initarg :doc-structure
                  :accessor element-doc-structure
                  :type (or null documentation-structure))
   (parent :initform nil
           :initarg :parent
           :accessor element-parent
           :type (or null element))
   (style :initform nil
          :initarg :style
          :accessor element-style
          :type (or null string pathname))
   (source :initform ""
           :initarg :source
           :initarg :location
           :accessor element-source
           :accessor element-location
           :type (or string pathname))
   )
  )


(defgeneric element-p (x)
  (:method ((x element)) t)
  (:method ((x t)) nil))


(defun is-element (x) (element-p x))


#|(defgeneric initialize-element (e &key parent doc-structure &allow-other-keys)
  (:documentation "Initializes some parts of an ELEMENT.

This function may and should be called by constructors of structures
that include ELEMENT."))
|#


(defgeneric pprint-element (os e)
  (:documentation "Generic function used in the PPRINT dispatch table.")
  (:argument-precedence-order e os))


#|
(defmethod pprint-element ((os stream) (e element))
   (if *print-readably*
       (print-object e os)
       (format os "Element ~S." (element-name e))))

(eval-when (:load-toplevel :compile-toplevel :execute)
  (set-pprint-dispatch 'element 'pprint-element))
|#


;;;---------------------------------------------------------------------------
;;; Logical documentation structure.
;;;
;;; Will be mostly mapped to HTML5 semantic elements.

;;; composite --

(defclass composite ()
  ((elements :initform ()
             :initarg :elements
             :accessor elements-of
             )
   )
  )


(defgeneric composite-p (x)
  (:method ((x composite)) t)
  (:method ((x t)) nil))


(defmethod initialize-instance :after ((c composite)
                                       &key elements
                                       &allow-other-keys)
  (dolist (e elements)
    (setf (element-parent e) c)))


;;; site --

(defclass site (element composite) ())


(defgeneric site-p (x)
  (:method ((x site)) t)
  (:method ((x t)) nil))


(defun site (name &rest keys &key &allow-other-keys)
  (apply #'make-instance 'site :name name keys))


;;; pages --

(defclass pages (element composite) ())


(defgeneric pages-p (x)
  (:method ((x pages)) t)
  (:method ((x t)) nil))


(defun pages (name &rest keys &key &allow-other-keys)
  (apply #'make-instance 'pages :name name keys))


;;; view --

(defclass view (element composite) ())


(defgeneric view-p (x)
  (:method ((x view)) t)
  (:method ((x t)) nil))


;;; main-view --

(defclass main-view (view)
  ((name :type string))
  )


(defgeneric main-view-p (x)
  (:method ((x main-view)) t)
  (:method ((x t)) nil))


(defun main-view (name style &rest elements)
  (make-instance 'main-view
                 :name name
                 :style style
                 :elements elements
                 ))


;;; header --

(defclass header (element)
  ((name :type string)))


(defgeneric header-p (x)
  (:method ((x header)) t)
  (:method ((x t)) nil))


(defun header (name &rest keys &key &allow-other-keys)
  (apply #'make-instance :name name keys))


;;; footer --

(defclass footer (element)
  ((name :type string)))


(defgeneric footer-p (x)
  (:method ((x footer)) t)
  (:method ((x t)) nil))


(defun footer (name &rest keys &key &allow-other-keys)
  (apply #'make-instance :name name keys))


;;; doc-area --

(defclass doc-area (view)
  ((nav :accessor doc-area-navigation
        :initarg :navigation)
   (content :accessor doc-area-content
            :initarg :content
            :initarg :info-area)
   ))


(defgeneric doc-area-p (x)
  (:method ((x doc-area)) t)
  (:method ((x t)) nil))


(defun doc-area (name &rest keys &key &allow-other-keys)
  (apply #'make-instance 'doc-area :name name keys))


;;; navigation --

(defclass navigation (element)
  ((name :type string)))


(defgeneric navigation-p (x)
  (:method ((x navigation)) t)
  (:method ((x t)) nil))


(defun navigation (name &rest keys &key &allow-other-keys)
  (apply #'make-instance 'navigation :name name keys))


;;; info-area --

(defclass info-area (view)
  ((name :type string))
  )


(defgeneric info-area-p (x)
  (:method ((x info-area)) t)
  (:method ((x t)) nil))


(defun info-area (name &rest keys &key &allow-other-keys)
  (apply #'make-instance 'info-area :name name keys))


;;; section --

(defclass section (element)
  ((name :type string))
  )


(defgeneric section-p (x)
  (:method ((x section)) t)
  (:method ((x t)) nil))


(defun section (name &rest keys &key &allow-other-keys)
  (apply #'make-instance 'section :name name keys))


;;; article --

(defclass article (element)
  ((name :type string))
  )


(defgeneric article-p (x)
  (:method ((x article)) t)
  (:method ((x t)) nil))


(defun article (name &rest keys &key &allow-other-keys)
  (apply #'make-instance 'article :name name keys))


;;;---------------------------------------------------------------------------
;;; Physical / "File" System

;;; file-system-entity --

(defclass file-system-entity (element)
  ((name :type (or string pathname)))
  )

(defgeneric file-system-entity-p (x)
  (:method ((x file-system-entity)) t)
  (:method ((x t)) nil))


;;; file --

(defclass file (file-system-entity)
  ((name :reader file-name)))


(defgeneric file-p (x)
  (:method ((x file)) t)
  (:method ((x t)) nil))


(defun file (name &rest keys &key &allow-other-keys)
  (apply #'make-instance 'file :name name keys))


(defgeneric file-pathname (fd &optional defaults)
  (:method ((f file)
            &optional (defaults *default-pathname-defaults*))
   (merge-pathnames (pathname (file-name f))) defaults))


(defmethod pprint-element ((os stream) (f file))
  (if *print-readably*
      (print-object f os)
      (format os "File ~S." (file-pathname f))))


;;; style-file --

(defclass style-file (file)
  ((name :initform (pathname *helambdap-css-pathname*)
         :accessor style-file-name)))


(defgeneric style-file-p (x)
  (:method ((x style-file)) t)
  (:method ((x t)) nil))


(defun style-file (&optional (name (pathname *helambdap-css-pathname*)))
  (make-instance 'style-file :name name))


;;; js-file --

(defclass js-file (file)
  ((name :initform (pathname *helambdap-js-pathname*)
         :accessor jf-file-name)))


(defgeneric js-file-p (x)
  (:method ((x js-file)) t)
  (:method ((x t)) nil))


(defun js-file (&optional (name (pathname *helambdap-js-pathname*)))
  (make-instance 'js-file :name name))


;;; doc-file --

(defclass doc-file (file)
  ((name :accessor doc-file-name)))


(defgeneric doc-file-p (x)
  (:method ((x doc-file)) t)
  (:method ((x t)) nil))


(defun doc-file (name)
  (make-instance 'doc-file :name name))


(defun doc-file-pathname-type ()
  (make-pathname :type *default-html-extension* :directory ()))


(defmethod file-pathname ((df doc-file)
                          &optional
                          (defaults *default-pathname-defaults*))
  (merge-pathnames
   (merge-pathnames (file-name df)
                    (doc-file-pathname-type))
   defaults))


;;; file-set --

(defclass file-set (element composite)
  ((elements :initform () :accessor file-set-files)
   (index :accessor file-set-index)
   (name :accessor file-set-name)
   ))


(defgeneric file-set-p (x)
  (:method ((x file-set)) t)
  (:method ((x t)) nil))


(defmethod print-object ((fs file-set) stream)
  (print-unreadable-object (fs stream)
    (format stream "FILE-SET ~S" (file-set-name fs))))


(defmethod pprint-element ((o stream) (f file-set))
  (if *print-pretty*
      (print-object f o)
      (format o "File Set ~S." (element-name f))))


(defun file-set (name &rest list)
  (make-instance 'file-set :name name :elements list))


;;; folder --

(defclass folder (file-set)
  ((elements :accessor folder-files)))


(defgeneric folder-p (x)
  (:method ((x folder)) t)
  (:method ((x t)) t))


(defun folder (name &rest files)
  (make-instance 'folder :name name :elements files))
  

;;;---------------------------------------------------------------------------
;;; Hybrid (XHTML) documentation structure.

;;; frame --

(defclass frame (view)
  ((name :type string
         :accessor frame-name)
   (source :accessor frame-source)
   )
  )


(defgeneric frame-p (x)
  (:method ((x frame)) t)
  (:method ((x t)) nil))


(defun frame (name &rest keys &key &allow-other-keys)
  (apply #'make-instance 'frame :name name keys))


;;; framesets --

(defclass framesets (pages)
  ((elements :accessor framesets-list)))


(defgeneric framsets-p (x)
  (:method ((x framesets)) t)
  (:method ((x t)) nil))


(defun framesets (name style &rest framesets)
  (make-instance 'framesets
                 :name name
                 :style style 
                 :elements framesets))


(defmethod print-object ((fs framesets) o)
  (declare (type stream o))
  (if *print-pretty*
      (format o "Frameset ~S." (element-name fs))
      (call-next-method)))


;;; frameset --

(defclass frameset (view)
  ((name :initform (symbol-name (gensym "FRAMESET-"))
         :accessor frameset-name)
   (style :accessor frameset-style)
   (header :initform t
           :reader frameset-header
           :initarg :header
           :type t) ; :type (or null string)
   (footer :initform t
           :reader frameset-footer
           :initarg :footer
           :type t) ; :type (or null string)y
   (navigation :initform t
               :reader frameset-navigation
               :initarg :navigation
               :type t) ; :type (or string frameset)
   (sidebar :reader frameset-sidebar
            :initarg :sidebar
            :type t) ; :type (or string frameset)
   (content :accessor frameset-content
            :initarg :content)
   (source :initform #P"./"
           :reader frameset-location
           )
   (order :initform 0
          :accessor frameset-order
          :initarg :order
          :type (integer 0 #.most-positive-fixnum))
   )
  )


(defun frameset (name
                 &rest
                 keys
                 &key
                 style
                 (header t)
                 (footer t)
                 (navigation t)
                 sidebar
                 content
                 (location #P"./")
                 &allow-other-keys)
  (apply #'make-instance 'frameset
         :name name
         :style style
         :header header
         :footer footer
         :navigation navigation
         :sidebar sidebar
         :content content
         :location location
         :source location
         keys))


(defmethod initialize-instance :after ((fs frameset)
                                       &key
                                       (content nil csp)
                                       &allow-other-keys)
  (declare (ignore content))
  (unless csp
    (setf (frameset-content fs)
          (doc-file (concatenate 'string (name fs) "-frame"))))
  )


(defun frameset-header-name (fs)
  (let ((fsn (frameset-header fs)))
    (if (and fsn (stringp fsn))
        fsn
        (concatenate 'string (frameset-name fs) "-header"))))


(defun frameset-footer-name (fs)
  (let ((fsn (frameset-footer fs)))
    (if (and fsn (stringp fsn))
        fsn
        (concatenate 'string (frameset-name fs) "-footer"))))


(defun frameset-navigation-name (fs)
  (let ((fsn (frameset-navigation fs)))
    (if (and fsn (stringp fsn))
        fsn
        (concatenate 'string (frameset-name fs) "-navigation"))))


(defun frameset-sidebar-name (fs)
  (let ((fsn (frameset-sidebar fs)))
    (if (and fsn (stringp fsn))
        fsn
        (concatenate 'string (frameset-name fs) "-sidebar"))))


(defmethod print-object ((fs frameset) stream)
  (declare (type stream stream))
  (print-unreadable-object (fs stream :identity t)
    (format stream "FRAMESET ~S ~S"
            (frameset-name fs)
            (frameset-location fs))))


(defmethod pprint-element ((o stream) (fs frameset))
  (if *print-readably*
      (print-object fs o)
      (pprint-logical-block (o nil)
        (format o "Frameset ~S~:@_" (element-name fs))
        (pprint-indent :block 8 o)
        (format o "Location   ~S~:@_" (frameset-location fs))
        (format o "Header     ~S~:@_" (frameset-header fs))
        (format o "Navigation ~S~:@_" (frameset-navigation fs))
        (pprint-linear o (frameset-content fs))
        (format o "~:@_Footer     ~S~:@_" (frameset-footer fs)))))


;;;---------------------------------------------------------------------------
;;; Elements utilities.

(defun element-location-path (e)
  (let ((source (element-source e))
        (p (element-parent e))
        )
    (if (and p (string= ""
                        (typecase source
                          (string source)
                          (pathname (namestring source)))))
        (element-location-path p)
        source)))


(defun element-location-depth (e)
  (let* ((path (parse-namestring (element-location-path e)))
         ;; Bad assumption, but WTH. The path could be URL.
         (d (pathname-directory path))
         )
    (assert (or (null d) (eq :relative (first d))))
    (if (null d)
        0
        (list-length (rest d)))
    ))


(defgeneric compute-element-path (e)
  (:method ((e element))
   (let ((ep (element-location-path e)))
     ep))

  (:method ((fs frameset))
   (let ((fp (element-location-path fs)))
     (make-pathname :name (frameset-name fs)
                    :type *default-html-extension*
                    :defaults fp)))
  )


;;;---------------------------------------------------------------------------
;;; Texinfo documentation structure.

;;; texinfo-file --

(defclass texinfo-file (element composite)
  ((location :initform #P"./"
             :accessor texinfo-file-location
             :type (or pathname string))
   )
  )


(defgeneric texinfo-file-p (x)
  (:method ((x texinfo-file)) t)
  (:method ((x t)) nil))


(defun texinfo-file (name &rest contents)
  (make-instance 'texinfo-file :name name :elements contents))


;;;---------------------------------------------------------------------------
;;; Implementation.

(defgeneric register-element (element parent doc-structure))


(defmethod initialize-instance :after ((ds documentation-structure) &key)
  ;; Fix the header structure

  (dolist (e (documentation-structure-structure ds))
    (register-element e nil ds))

  ;; Register the ds.
  (register-doc-structure ds)
  )


(defun make-documentation-structure (name root &rest structure)
  (make-instance 'documentation-structure
                 :name name
                 :root root
                 :structure structure))


(defparameter *doc-structures* ())


(defun find-doc-structure (name)
  (find name *doc-structures*
        :key #'documentation-structure-name
        :test #'string-equal))


(defun erase-doc-structure (ds)
  (setf *doc-structures*
        (remove (documentation-structure-name ds)
                *doc-structures*
                :key #'documentation-structure-name
                :test #'string-equal)))


(defun register-doc-structure (ds &key (if-exists :replace))
  (declare (type documentation-structure ds))
  (let ((old-ds (find-doc-structure (documentation-structure-name ds))))
    (when (and old-ds (eq if-exists :replace))
      (erase-doc-structure old-ds))
    (push ds *doc-structures*)))


(defmethod register-element ((e element) parent doc-structure)
  (declare (type documentation-structure doc-structure))
  (setf (element-doc-structure e) doc-structure
        (element-parent e) parent
        (gethash (element-name e) (structure-table doc-structure)) e)
  (unless (element-style e)
    (let ((p (element-parent e)))
      (when p (setf (element-style e) (element-style p)))))
  e)


(defmethod register-element :after ((e framesets) parent doc-structure)
  (declare (ignorable parent))
  (loop for fs in (framesets-list e)
        for i from 0
        do (register-element fs e doc-structure)
        do (setf (frameset-order fs) i)))


(defmethod register-element :after ((e file-set) parent doc-structure)
  (declare (ignorable parent))
  (loop for fs in (file-set-files e)
        for i from 0
        do (register-element fs e doc-structure)))


(defun read-structure-file (&optional
                            (struct-pathname (pathname
                                              *structure-file-name*)))
  (declare (special *default-documentation-structure*)) ; Defined later.
  (if (probe-file struct-pathname)
      (with-open-file (sfs struct-pathname :direction :input)
        (read sfs))
      *default-documentation-structure*))


(defun pprint-documentation-structure (structure &optional (out *standard-output*))
  (pprint (documentation-structure-structure structure) out))


;;;---------------------------------------------------------------------------
;;; Auxiliary files production.

(defgeneric head-title (e)
  (:method ((e doc-area)) "Doc area head placeholder title")
  (:method ((e view)) "View head placeholder title"))


(defgeneric body-title (fs)
  (:method ((e doc-area)) "Doc area body placeholder title")
  (:method ((e view)) "View body placeholder title"))


;;;===========================================================================
;;; Documentation structures.

(defparameter *helambdap-css-filename-up*
  (make-pathname :directory '(:relative :up)
                 :defaults (pathname *helambdap-css-filename*)))


(defparameter *helambdap5-css-filename-up*
  (make-pathname :directory '(:relative :up)
                 :defaults (pathname *helambdap5-css-filename*)))


(defparameter *xhtml-frame-documentation-structure*
  (make-documentation-structure
   "standard-xhtml"
   "index"
   (style-file)
   (framesets "doc-framesets"
              *helambdap-css-filename*
              (frameset "index"
                        :content (doc-file "introduction"))
              (frameset "dictionary"
                        :location #P"dictionary/"
                        :style (namestring *helambdap-css-filename-up*)
                        :content (file-set "dictionary-entries"))
              (frameset "downloads")
              (frameset "mailing-lists")
              (frameset "links")
              )
   )
  "The XHTML frame-based documentation structure.")


(defparameter *xhtml-simple-frame-documentation-structure*
  (make-documentation-structure
   "standard-xhtml-simple"
   "index"
   (style-file)
   (framesets "doc-framesets"
              *helambdap-css-filename*
              (frameset "index"
                        :content (doc-file "introduction"))
              (frameset "dictionary"
                        :location #P"dictionary/"
                        :style (namestring *helambdap5-css-filename-up*)
                        :content (file-set "dictionary-entries"))
              ;; (frameset "downloads")
              ;; (frameset "mailing-lists")
              ;; (frameset "links")
              )
   )
  "The XHTML simple frame-based documentation structure.

A minimal documentation structure that contains only the main index
(and introduction and the dictionary of of the system/package/library.")





;;;; WORK IN PROGRESS

(defparameter *html5-documentation-structure*
  (make-documentation-structure
   "standard-html5"
   "index"
   (style-file (pathname *helambdap5-css-pathname*))
   (js-file)
   (main-view "index"
              *helambdap5-css-filename*
              (doc-area "intro"
                        :style (namestring *helambdap5-css-filename*)
                        :navigation (navigation "introduction-navigation")
                        :content (doc-file "introduction")
                        )
              (doc-area "dictionary"
                        :location #P"dictionary/"
                        :style (namestring *helambdap5-css-filename-up*)
                        :navigation (navigation "dictionaty-navigation")
                        :content (file-set "dictionary-entries")
                        )
              )
   )
  "The HTML5 documentation structure.

A minimal documentation structure that contains only the main index
(and introduction and the dictionary of the system/package/library).")

;;;; WIP




(defparameter *texinfo-documentation-structure*
  (make-documentation-structure
   "texinfo"
   "top"
   (texinfo-file "top"
                 (doc-file "introduction")
                 (file-set "dictionary-entries")))
  "The Texinfo documentation structure.")


(defparameter *default-documentation-structure* 
  *xhtml-simple-frame-documentation-structure*
  "The variable containing the default documentation structure.")


;;;; end of file -- doc-structure.lisp --

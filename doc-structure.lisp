;;;; -*- Mode: Lisp -*-

;;;; doc-structure.lisp --
;;;; Collecting the documentation about a library entails a number of
;;;; steps, with the goal of producing a "structured" web site
;;;; organized as a (static) setf of pages.
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


;;; element --

(defstruct element
  ;; (name "" :read-only t :type string)
  (name "" :read-only t)
  (doc-structure nil
                 :type (or null documentation-structure))
  (parent nil
          :type (or null element))
  )


(defgeneric initialize-element (e &key parent doc-structure &allow-other-keys)
  (:documentation "Initializes some parts of an ELEMENT.

This function may and should be called by constructors of structures
that include ELEMENT."))


(defmethod initialize-element ((e element)
                               &key parent doc-structure
                               &allow-other-keys)
  (declare (type element e)
           (type (or null documentation-structure) doc-structure)
           (type (or null element) parent))
  (when parent
    (setf (element-parent e) parent))
  (when doc-structure
    (setf (element-doc-structure e) doc-structure))
  e)


#|
(defgeneric pprint-element (os e)
  (:documentation "Generic function used in the PPRINT dispatch table.")
  (:argument-precedence-order e os))


(defmethod pprint-element ((os stream) (e element))
   (if *print-readably*
       (print-object e os)
       (format os "Element ~S." (element-name e))))

(eval-when (:load-toplevel :compile-toplevel :execute)
  (set-pprint-dispatch 'element 'pprint-element))
|#


;;;---------------------------------------------------------------------------
;;; XHTML documentation structure.

;;; framesets --  

(defstruct (framesets (:include element)
                      (:constructor %make-framesets (name list)))
  (list ()))


(defun framesets (name &rest framesets)
  (let ((fss (%make-framesets name framesets)))
    (initialize-element fss)
    (dolist (fs framesets fss)
      (setf (element-parent fs) fss))))


(defmethod print-object ((fs framesets) o)
  (declare (type stream o))
  (if *print-pretty*
      (format o "Frameset ~S." (element-name fs))
      (call-next-method)))


;;; frameset --

(defstruct (frameset (:include element)
                     (:constructor %make-frameset))
  (header nil :read-only t :type t) ; :type (or null string)
  (footer nil :read-only t :type t) ; :type (or null string)y
  (navigation nil :read-only t :type t) ; :type (or string frameset)
  (sidebar nil :read-only t :type t) ; :type (or string frameset)
  (content () :read-only t :type list)
  (location #P"./" :read-only t :type (or null pathname))
  (order 0 :type (integer 0 #.most-positive-fixnum))
  )


(defun frameset (name
                 &key
                 (header t)
                 footer
                 (navigation t)
                 sidebar
                 (content ())
                 (location #P"./"))
  (initialize-element
   (%make-frameset :name name
                   :header header
                   :footer footer
                   :navigation navigation
                   :sidebar sidebar
                   :content content
                   :location location)))


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


;;; file --

(defstruct (file (:constructor %make-file (name))
                 (:include element (name "" :type (or string pathname)))))

(defmethod file-pathname ((f file)) (pathname (file-name f)))


(defmethod pprint-element ((os stream) (f file))
  (if *print-readably*
      (print-object f os)
      (format os "File ~S." (file-pathname f))))


(defun file (name)
  (initialize-element (%make-file name)))


;;; style-file --

(defstruct (style-file
            (:include file)
            (:constructor %make-style-file (&optional (name (pathname "clstyle.css")))))
  )

(defun style-file (&optional (name (pathname "clstyle.css")))
  (initialize-element (%make-style-file name)))


;;; doc-file --

(defstruct (doc-file
            (:include file)
            (:constructor %make-doc-file (name)))
  )


(defun doc-file (name)
  (initialize-element (%make-doc-file name)))


(defvar *default-html-extension* "htm")

(defun doc-file-pathname-type ()
  (make-pathname :type *default-html-extension* :directory ()))


(defmethod file-pathname ((df doc-file))
  (merge-pathnames (file-name df)
                   (doc-file-pathname-type)))


;;; file-set --

(defstruct (file-set (:include element)
                     (:constructor %make-file-set (name files)))
  (files ()))


(defmethod print-element ((o stream) (f file-set))
  (if *print-pretty*
      (print-object f o)
      (format o "File Set ~S." (element-name f))))


(defun file-set (name &rest list)
  (initialize-element (%make-file-set name list)))



;;;---------------------------------------------------------------------------
;;; Texinfo documentation structure.


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
  e)


(defmethod register-element :after ((e framesets) parent doc-structure)
  (loop for fs in (framesets-list e)
        for i from 0
        do (register-element fs e doc-structure)
        do (setf (frameset-order fs) i)))
  


(defmethod register-element :after ((e file-set) parent doc-structure)
  (loop for fs in (file-set-files e)
        for i from 0
        do (register-element fs e doc-structure)))


;;;---------------------------------------------------------------------------
;;; Documentation structures.

(defparameter *xhtml-frame-documentation-structure*
  (make-documentation-structure
   "standard"
   "index"
   (style-file)
   (framesets "doc-framesets"
              (frameset "index"
                        :content (list (doc-file "introduction")))
              (frameset "dictionary"
                        :location #P"dictionary/"
                        :content (list (file-set "dictionary")))
              (frameset "downloads")
              (frameset "mailing-lists")
              (frameset "links")
              )
   )
  "The XHTML frame-based documentation structure.")


(defparameter *texinfo-documentation-structure*
  (make-documentation-structure
   "texinfo"
   "top")
  "The Texinfo documentation structure.")


(defparameter *default-documentation-structure* 
  *xhtml-frame-documentation-structure*
  "The variable containing the default documentation structure.")


(defun read-structure-file (&optional
                            (struct-pathname (pathname
                                              *structure-file-name*)))
  (if (probe-file struct-pathname)
      (with-open-file (sfs struct-pathname :direction :input)
        (read sfs))
      *default-documentation-structure*))


(defun pprint-documentation-structure (structure &optional (out *standard-output*))
  (pprint (documentation-structure-structure structure) out))


;;;; end of file -- doc-structure.lisp --

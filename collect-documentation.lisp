;;;; -*- Mode: Lisp -*-

;;;; collect-doc.lisp --
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

(defgeneric collect-documentation (where-from))


(defparameter *source-extensions*
  (list "lisp" "lsp" "asd" "system" "cl")
  "List of possible 'source extensions' where Lisp code is contained.")


(defparameter *info-file-name* "info.hela")

(defparameter *intro-file-name* "intro.hela") ; Synonym with 'info.hela'

(defparameter *structure-file-name* "struct.hela")

;;; (defparameter *doc-bits-db-file* "doc.hlam") ; defined elsewhere.

;;;---------------------------------------------------------------------------
;;; Template/structure file management.


(defstruct (framesets (:constructor framesets (&rest list)))
  (list ()))


(defstruct (frameset (:constructor frameset (name
                                             &key header navigation content location)))
  (name "" :read-only t :type string)
  (header "" :read-only t :type string)
  (navigation "" :read-only t :type t) ; :type (or string frameset)
  (content () :read-only t :type list)
  (location "." :read-only t)
  )


(defstruct (file)
  (name "" :type (or string pathname)))


(defstruct (style-file (:include file)
                       (:constructor style-file (&optional (name (pathname "clstyle.css")))))
  )


(defstruct (doc-file (:include file)
                     (:constructor doc-file (name)))
  )


(defstruct (file-set (:constructor file-set))
  (name "")
  (files ()))


(defclass documentation-structure ()
  ((name :reader documentation-structure-name :initarg :name)
   (structure :reader documentation-structure-structure
              :initarg :structure
              :type list)))


(defmethod initialize-instance :after ((ds documentation-structure) &key)
  ;; Fix the header structure
  ;; Register the ds.
  (register-doc-structure ds)
  )


(defun make-documentation-structure (name &rest structure)
  (make-instance 'documentation-structure
                 :name name
                 :structure structure))


(defparameter *doc-structures* ())

(defun find-doc-structure (name)
  (find name *doc-structures*
        :key #'documentation-structure-name
        :test #'string-equal))


(defun erase-doc-structure (ds)
  (setf *doc-structures*
        (remove (documentation-structure-name ds)
                :key #'documentation-structure-name
                :test #'string-equal)))


(defun register-doc-structure (ds &key (if-exists :replace))
  (declare (type documentation-structure ds))
  (let ((old-ds (find-doc-structure (documentation-structure-name ds))))
    (when (and old-ds (eq if-exists :replace))
      (erase-doc-structure old-ds))
    (push ds *doc-structures*)))


(defparameter *default-documentation-structure*
  (make-documentation-structure
   "standard"
   (style-file)
   (framesets "index"
              "dictionary"
              "downloads"
              "mailing-lists"
              "links")
   (frameset "index"
             :content (list "introduction"))
   (doc-file "introduction")
   (frameset "dictionary"
             :location "dictionary"
             :content (list (file-set :name "dictionary")))
   (frameset "downloads")
   (frameset "mailing-lists")
   (frameset "links")
   )
  "The default documentation structure.")


(defun read-structure-file (&optional
                            (struct-pathname (pathname
                                              *structure-file-name*)))
  (if (probe-file struct-pathname)
      (with-open-file (sfs struct-pathname :direction :input)
        (read sfs))
      *default-documentation-structure*))





;;;;===========================================================================
;;;; Collection.
;;;; Essentially a directory traversal.  DFS with 'nodes' being
;;;; pathanmes indexed on the namestring.
;;;;
;;;; CL-FAD:WALK-DIRECTORY does not do a proper DFS with links on some
;;;; FS; so I roll my own.

(defmethod collect-documentation ((p pathname))
  (let ((color-table (make-hash-table :test #'equal))
        (doc-bits ())
        )
    (labels ((is-grey (ntp) (eq :grey (gethash ntp color-table)))

             (is-black (ntp) (eq :black (gethash ntp color-table)))

             (is-white (ntp) (eq :white (gethash ntp color-table :white)))

             (grey (ntp) (setf (gethash ntp color-table) :grey))

             (blacken (ntp) (setf (gethash ntp color-table) :black))

             (whiten (ntp) (setf (gethash ntp color-table) :white))
                                                  
             (dfs (p)
               (let ((ntp (namestring (truename p))))
                 (when (is-white ntp)
                   (format t "Considering ~S~%" p)
                   (grey ntp)
                   (cond ((cl-fad:directory-pathname-p p)
                          (dolist (f (directory-source-files p))
                            (format t "Considering source file ~S~%" f)
                            (dfs f))
                          (dolist (sd (subdirectories p))
                            (dfs sd))
                          )
                         (t
                          (format t "Considering file ~S~%" p)
                          (setf doc-bits
                                (nconc (extract-documentation p)
                                       doc-bits)))
                         )
                   (blacken ntp))
                 ))
             )
      (dfs p)
      doc-bits)))


;;;;===========================================================================
;;;; Utilities.

(defun subdirectories (p)
  (declare (type (or string pathname) p))
  (delete-if (complement #'cl-fad:directory-pathname-p)
             (directory (make-pathname :name :wild :defaults p))))


(defun directory-source-files (p)
  (declare (type (or string pathname) p))
  (loop for se of-type string in *source-extensions*
        append (directory
                (merge-pathnames (make-pathname :name :wild
                                                :type se)
                                 p))))


;;;; end of file -- collect-doc.lisp --

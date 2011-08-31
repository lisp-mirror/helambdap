;;;; -*- Mode: Lisp -*-

;;;; collect-documentation.lisp --
;;;; Collecting the documentation about a library entails a number of
;;;; steps, with the goal of producing a "structured" web site
;;;; organized as a (static) setf of pages.

(in-package "HELAMBDAP")


(defgeneric collect-documentation (where-from))


(defparameter *source-extensions*
  (list "lisp" "lsp" "asd" "system" "cl")
  "List of possible 'source extensions' where Lisp code is contained.")


(defparameter *info-file-name* "info.hela")

(defparameter *intro-file-name* "intro.hela") ; Synonym with 'info.hela'

(defparameter *structure-file-name* "struct.hela")

;;; (defparameter *doc-bits-db-file* "doc.hlam") ; defined elsewhere.


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
                   (format t "~&Considering ~S " p)
                   (grey ntp)
                   (cond ((cl-fad:directory-pathname-p p)
                          (format t "[D]~%")
                          (dolist (f (directory-source-files p))
                            (dfs f))
                          (dolist (sd (subdirectories p))
                            (dfs sd))
                          )
                         (t
                          (format t "[F]~%" p)
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

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


(defmethod collect-documentation :around ((where-from t))
  (let ((doc-bits (call-next-method))
        (gfs-table (make-hash-table :test #'eq))
        (gfs ())
        (methods ())
        )

    (loop for db in doc-bits
          when (generic-function-doc-bit-p db)
          do (pushnew db gfs :key #'doc-bit-name :test #'eq)
          when (method-doc-bit-p db)
          do (pushnew db methods :key #'doc-bit-name :test #'eq)
          )
    (dolist (gfdb gfs)
      (setf (gethash (doc-bit-name gfdb) gfs-table) gfdb))
    (dolist (m methods)
      (pushnew m (generic-function-doc-bit-methods
                  (gethash (doc-bit-name m) gfs-table))))
    doc-bits))
  


;;;----------------------------------------------------------------------------
;;; Standard dictionary sorting...

(defun sift-standard-doc-bits (doc-bits)
  (declare (type list doc-bits)) ; (type (list doc-bits) doc-bits)

  (flet ((sort-doc-bits (doc-bits)
           (sort (copy-list doc-bits) #'string<= :key 'doc-bit-name))
         )

    ;; This is when you want ITERATE...
    (let (systems
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
      (macrolet ((put-in (list)
                   `(push doc-bit ,list)))
        (dolist (doc-bit doc-bits)
          (typecase doc-bit
            (system-doc-bit (put-in systems))
            (package-doc-bit (put-in packages))
            (constant-doc-bit (put-in constants))
            (parameter-doc-bit (put-in parameters))
            (variable-doc-bit (put-in variables))
            (class-doc-bit (put-in classes))
            (struct-doc-bit (put-in structs))
            (type-doc-bit (put-in types))
            (condition-doc-bit (put-in conditions))
            (generic-function-doc-bit (put-in generic-functions))
            (method-doc-bit (put-in methods))
            (function-doc-bit (put-in functions))
            (macro-doc-bit (put-in macros))
            (method-combination-doc-bit (put-in method-combinations))
            (setf-expander-doc-bit (put-in setf-expanders))
            (modify-macro-doc-bit (put-in modify-macros))
            (t (put-in others))
            ))
        (mapcar #'sort-doc-bits
                (list systems
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
                      ))
        ))))



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

;;;; -*- Mode: Lisp -*-

;;;; collect-documentation.lisp --
;;;; Collecting the documentation about a library entails a number of
;;;; steps, with the goal of producing a "structured" web site
;;;; organized as a (static) setf of pages.

(in-package "HELAMBDAP")


(defgeneric collect-documentation (where-from
                                   &key
                                   exclude-directories
                                   exclude-files
                                   &allow-other-keys))



(defparameter *info-file-name* "info.hela")

(defparameter *intro-file-name* "intro.hela") ; Synonym with 'info.hela'

(defparameter *structure-file-name* "struct.hela")

;;; (defparameter *doc-bits-db-file* "doc.hlam") ; defined elsewhere.


;;;;===========================================================================
;;;; Collection.

(defmethod collect-documentation ((pathname-list list)
                                  &key
                                  ((:exclude-directories *exclude-directories*)
                                   *exclude-directories*)
                                  ((:exclude-files *exclude-files*)
                                   *exclude-files*)
                                  &allow-other-keys)
  (declare (special *exclude-directories* *exclude-files*))
  (mapcan #'collect-documentation pathname-list))
  

;;;; The next is essentially a directory traversal.  DFS with 'nodes' being
;;;; pathanmes indexed on the namestring.
;;;;
;;;; CL-FAD:WALK-DIRECTORY does not do a proper DFS with links on some
;;;; FSs; so I roll my own.

#+pre-truename-fix
(defmethod collect-documentation ((p pathname)
                                  &key
                                  ((:exclude-directories *exclude-directories*)
                                   *exclude-directories*)
                                  ((:exclude-files *exclude-files*)
                                   *exclude-files*)
                                  &allow-other-keys
                                  )
  (declare (special *exclude-directories* *exclude-files*))
  (let ((color-table (make-hash-table :test #'equal))
        (doc-bits ())
        (exclude-dir-names (mapcar #'directory-last-name *exclude-directories*))
        (exclude-file-names (mapcar #'(lambda (exf)
                                        (namestring (pathname exf)))
                                    *exclude-files*))
        )
    (declare (type hash-table color-table)
	     (type list
		   doc-bits
		   exclude-dir-names
		   exclude-file-names)
	     )
    (labels (
	     ;; (is-grey (ntp) (eq :grey (gethash ntp color-table)))

             ;; (is-black (ntp) (eq :black (gethash ntp color-table)))

             (is-white (ntp) (eq :white (gethash ntp color-table :white)))

             (grey (ntp) (setf (gethash ntp color-table) :grey))

             (blacken (ntp) (setf (gethash ntp color-table) :black))

             ;; (whiten (ntp) (setf (gethash ntp color-table) :white))

             (dfs (p)
               (let* ((ntp (namestring (truename p)))
                      (ed (enclosing-directory (truename p)))
                      (trunc-p (make-pathname :directory ed :defaults p))
                     )
		 (declare (type string ntp))
                 (when (is-white ntp)
                   (format t "~&HELAMBDAP: Considering .../~A " trunc-p)
                   (grey ntp)
                   (cond ((cl-fad:directory-pathname-p p)
                          (format t "[D]")
                          (if (member (directory-last-name p)
                                      exclude-dir-names
                                      :test #'string-equal)
                              (format t " excluded.~%")
                              (progn
                                (dolist (f (directory-source-files p))
                                  (dfs f))
                                (dolist (sd (subdirectories p))
                                  (dfs sd)))
                            ))
                         (t ; A file.
                          (format t "[F]")
                          (if (member ntp exclude-file-names :test #'equal)
                              (format t " excluded.~%")
                              (setf doc-bits
                                    (nconc (extract-documentation p)
                                           doc-bits))))
                         )
                   (blacken ntp))
                 ))
             )
      (dfs p)
      doc-bits)))


;;; #+post-truename-fix
(defmethod collect-documentation ((p pathname)
                                  &key
                                  ((:exclude-directories *exclude-directories*)
                                   *exclude-directories*)
                                  ((:exclude-files *exclude-files*)
                                   *exclude-files*)
                                  &allow-other-keys
                                  )
  (declare (special *exclude-directories* *exclude-files*))
  (flet ((truename-or-nil (p)
           (declare (type (or string pathname) p))
           (ignore-errors (truename p)))
         )
    (let ((color-table (make-hash-table :test #'equal))
          (doc-bits ())
          (exclude-dir-names
           (delete nil (mapcar #'truename-or-nil *exclude-directories*)))
          (exclude-file-names
           (delete nil (mapcar #'truename-or-nil *exclude-files*)))
          )
      (declare (type hash-table color-table)
               (type list
                     doc-bits
                     exclude-dir-names
                     exclude-file-names)
               )
      (labels (
	     ;; (is-grey (ntp) (eq :grey (gethash ntp color-table)))

             ;; (is-black (ntp) (eq :black (gethash ntp color-table)))

               (is-white (ntp) (eq :white (gethash ntp color-table :white)))

               (grey (ntp) (setf (gethash ntp color-table) :grey))

               (blacken (ntp) (setf (gethash ntp color-table) :black))

               ;; (whiten (ntp) (setf (gethash ntp color-table) :white))

               (dfs (p)
                 (declare (type pathname p))
                 (let* ((tp (truename p))
                        (ntp (namestring tp))
                        (ed (enclosing-directory tp))
                        (trunc-p (make-pathname :directory ed :defaults p))
                        )
                   (declare (type string ntp)
                            (type pathname tp))
                   (when (is-white ntp)
                     (format t "~&HELAMBDAP: Considering .../~A " trunc-p)
                     (grey ntp)
                     (cond ((cl-fad:directory-pathname-p p)
                            (format t "[D]")
                            (if (member (truename p) exclude-dir-names :test #'equal)
                                (format t " excluded.~%")
                                (progn
                                  (dolist (f (directory-source-files p))
                                    (dfs f))
                                  (dolist (sd (subdirectories p))
                                    (dfs sd)))
                                ))
                           (t ; A file.
                            (format t "[F]")
                            (if (member tp exclude-file-names :test #'equal)
                                (format t " excluded.~%")
                                (setf doc-bits
                                      (nconc (extract-documentation p)
                                             doc-bits))))
                           )
                     (blacken ntp))
                   ))
               )
        (dfs p)
        doc-bits))))


(defmethod collect-documentation :around ((where-from t)
                                          &key &allow-other-keys)
  (let ((doc-bits (call-next-method))
        (gfs-table (make-hash-table :test #'equal))
        (gfs ())
        (methods ())
        )
    (declare (type hash-table gfs-table)
	     (type list doc-bits gfs methods))

    (loop for db in doc-bits
          when (generic-function-doc-bit-p db)
          do (pushnew db gfs :key #'doc-bit-name :test #'equal)
          when (method-doc-bit-p db)
          ;; do (pushnew db methods :key #'doc-bit-name :test #'eq)
          do (push db methods) ; Need all the methods.
          )
    (dolist (gfdb gfs)
      (setf (gethash (doc-bit-name gfdb) gfs-table) gfdb))
    (dolist (m methods)
      (when (gethash (doc-bit-name m) gfs-table)
        (pushnew m (generic-function-doc-bit-methods
                    (gethash (doc-bit-name m) gfs-table)))))

    (dolist (db doc-bits) (insert-doc-bit db))
    (save-doc-bits-db "tmp/dbdb.lisp")
    doc-bits))


;;;----------------------------------------------------------------------------
;;; Standard dictionary sorting...

(defun sift-standard-doc-bits (doc-bits)
  (declare (type list doc-bits)) ; (type (list doc-bits) doc-bits)

  (flet ((sort-doc-bits (doc-bits)
	   (declare (type list doc-bits))
           (sort (copy-list doc-bits) #'string<= :key 'doc-bit-identifier))
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
      (declare (type list
		     systems
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
		     others))
		     
      (macrolet ((put-in (list)
                   `(push doc-bit ,list)))
        (dolist (doc-bit doc-bits)
          (typecase doc-bit
            (system-doc-bit (put-in systems))
            (package-doc-bit (put-in packages))
            (constant-doc-bit (put-in constants))
            (parameter-doc-bit (put-in parameters))
            (variable-doc-bit (put-in variables))
            (condition-doc-bit (put-in conditions))
            (class-doc-bit (put-in classes))
            (struct-doc-bit (put-in structs))

            (method-combination-doc-bit (put-in method-combinations))

            ((or type-doc-bit deftype-doc-bit) (put-in types)) ; This would shadow the above.
            (generic-function-doc-bit (put-in generic-functions))
            (method-doc-bit (put-in methods))
            (function-doc-bit (put-in functions))
            (modify-macro-doc-bit (put-in modify-macros))
            (macro-doc-bit (put-in macros))
            (setf-expander-doc-bit (put-in setf-expanders))
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

(defun directory-last-name (p)
  (declare (type (or string pathname) p))
  (let ((p-dir (pathname-directory (pathname p))))
    
    (declare (type (or null string list (member :wild :unspecific))
                   p-dir))

    ;; P-DIR is now a proper directory pathname component.
    ;; SBCL complains with a note here about "unreachable code"; but
    ;; that is because it is too eager in its optimizations to pay
    ;; respect Section 19.2.2.4.3 of the ANSI spec.

    (cond ((null p-dir) ".")
          
          ((eq :unspecific p-dir) ".")
          
          ((eq :wild p-dir) "*")
          
          ((stringp p-dir) p-dir)

          ((listp p-dir)
           (let ((p-dir-l (list-length p-dir)))
             ;; Note that the result may be :wild, :wild-inferiors,
             ;; :up, :back, a symbol or a string.
             ;; It suffices for the purpose.
             (ecase (first p-dir)
               (:absolute (if (= p-dir-l 1)
                              "/"
                              (first (last p-dir))))
               (:relative (if (= p-dir-l 1)
                              "."
                              (first (last p-dir))))))
           ))
    ))


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

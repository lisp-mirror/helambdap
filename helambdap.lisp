;;;; -*- Mode: Lisp -*-

;;;; helambdap.lisp --
;;;;
;;;; See file COPYING for copyright and licensing information.

(in-package "HELAMBDAP")


;;;;---------------------------------------------------------------------------
;;;; Doc bits data base.

(defstruct (doc-bits-data-base
            (:constructor make-doc-bits-data-base ()))
  "The Doc-bits Data Base structure.

An 'opaque' wrapper around the actual doc-bits data base."
  (dbdb (make-hash-table :test #'equal) :type hash-table :read-only t))


(defun dbdb (dbdb)
  (declare (type doc-bits-data-base dbdb))
  (doc-bits-data-base-dbdb dbdb))


(defmethod print-object ((dbdb doc-bits-data-base) stream)
  (print-unreadable-object (dbdb stream :identity t :type t)
    (format stream "with ~D doc-bits; id" (hash-table-count (dbdb dbdb)))))


;;; *doc-bits-db* --

(defvar *doc-bits-db* (make-hash-table :test #'equal)
  "The Data Base of doc bits.

The doc bits db is indexed on the NAME of a DOC-BIT.")

;;; NOTE: we can make a doc-bit db a more structured object, thinking
;;; of linking it to a set files containing the actual doc strings.


;;; *doc-bits-db-file*

(defvar *doc-bits-db-file*
  "doc.hlam")


;;; init-doc-bits-db --

(defun init-doc-bits-db ()
  "Initializes the doc-bits data base."
  (if *doc-bits-db*
      (clear-doc-bits-db *doc-bits-db*)
      (setf *doc-bits-db* (make-hash-table :test #'equal))))


;;; clear-doc-bits-db --

(defun clear-doc-bits-db (&optional (dbdb *doc-bits-db*))
  "Clears the doc-bits data base."
  (clrhash dbdb)
  dbdb)


;;; insert-doc-bit --

(defun insert-doc-bit (doc-bit &optional (dbdb *doc-bits-db*))
  "Inserts a doc-bit in the doc-bits data base."
  (declare (type doc-bit doc-bit))
  (setf (gethash (doc-bit-name doc-bit) dbdb)
        (nconc (gethash (doc-bit-name doc-bit) dbdb) (list doc-bit)))
  doc-bit)


;;; save-doc-bits-db --

(defgeneric save-doc-bits-db (where &optional doc-bits-db)
  (:documentation "Saves the doc-bits data bases to a file.

The optional argument DOC-BITS-DB defaults to the 'current' doc bits
data base.")
  )


(defmethod save-doc-bits-db ((out stream)
                             &optional (db *doc-bits-db*))
  
  (format out ";;;; -*- Mode: Lisp -*-~2%;;;; DOC-BITS DB File.~2%")
  (format out ";;;; The actual documentation strings can be modifed.~2%")

  (loop for doc-bits being the hash-value of db
        do (map nil (lambda (doc-bit) (pprint doc-bit out)) doc-bits))

  (format out "~2%;;;; end of file -- DOC BITS DB File. --~%"))


(defmethod save-doc-bits-db ((f pathname)
                             &optional (db *doc-bits-db*))
  (ensure-directories-exist f)
  (with-open-file (out f
                       :direction :output
                       :if-exists :supersede
                       :if-does-not-exist :create)
    (save-doc-bits-db out db)))


(defmethod save-doc-bits-db ((f string)
                             &optional (db *doc-bits-db*))
  (save-doc-bits-db (parse-namestring f) db))


;;; load-doc-bits-db --

(defgeneric load-doc-bits-db (where &optional doc-bits-db)
  (:documentation "Loads a doc-bits data base from a source.

The SOURCE can be either a STREAM or a File Designator.  The optional
DOC-BITS-DB parameter defaults to the current doc-bits database."))


(defmethod load-doc-bits-db ((f pathname)
                             &optional (db *doc-bits-db*))
  (with-open-file (in f
                      :direction :input
                      :if-does-not-exist :error)
    (loop with eof = '%%DBDB-EOF%%
          for doc-bit = (read in nil eof)
          while (not (eq eof doc-bit))
          do (destructuring-bind (doc-tag name kind doc-string timestamp)
                 doc-bit
               (declare (ignore doc-tag kind doc-string timestamp))
               ;; The destructuring is already in place for further
               ;; developments. 
               (push doc-bit (gethash name db))))))


(defmethod load-doc-bits-db ((f string)
                             &optional (db *doc-bits-db*))
  (load-doc-bits-db (parse-namestring f) db))


;;; get-doc-bits --

(defun get-doc-bits (name &optional (dbdb *doc-bits-db*))
  (gethash name dbdb))


;;; doc bits creation --

(defun fill-doc-bits-db (f &optional (dbdb *doc-bits-db*))
  ;; MK User Manual similar stuff.
  (declare (ignore dbdb))
  (extract-documentation f)
  )


;;;---------------------------------------------------------------------------
;;; DOCUMENTATION shadowing.
;;; Cfr. CLHS DOCUMENTATION.

(defgeneric documentation (name kind
                                &key
                                doc-bits-db
                                db-store
                                reload)
  (:documentation "The DOCUMENTATION Generic Function.

The DOCUMENTATION generic function shadows CL:DOCUMENTATION in order
to provide a few hooks for manipulating the underlying DOC-BITS DB.

If no documentation string is found in the doc bits db, then
DOCUMENTATION falls back on CL:DOCUMENTATION.")
  )


(defmethod documentation ((x t) (doc-type symbol)
                          &key
                          ((:doc-bits-db dbdb) *doc-bits-db*)
                          (db-store *doc-bits-db-file*)
                          (reload nil)
                          )
  (declare (ignore db-store reload))
  (let ((doc-bits (get-doc-bits x dbdb)))
    (or (find doc-type doc-bits :key #'doc-bit-kind)
        (cl:documentation x doc-type))))


(defmethod documentation :before ((x t) (doc-type symbol)
                                  &key
                                  ((:doc-bits-db dbdb) *doc-bits-db*)
                                  (db-store *doc-bits-db-file*)
                                  (reload nil))
  (when reload
    (load-doc-bits-db db-store dbdb)))


;;;; end of file -- helambdap.lisp --

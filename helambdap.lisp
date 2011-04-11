;;;; -*- Mode: Lisp -*-

;;;; helambdap.lisp --

(in-package "HELAMBDAP")


;;;;---------------------------------------------------------------------------
;;;; Doc bits data base.

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
  (if *doc-bits-db*
      (clear-doc-bits-db *doc-bits-db*)
      (setf *doc-bits-db* (make-hash-table :test #'equal))))


;;; clear-doc-bits-db --

(defun clear-doc-bits-db (&optional (dbdb *doc-bits-db*))
  (clrhash dbdb)
  dbdb)


;;; save-doc-bits-db --

(defgeneric save-doc-bits-db (where &optional doc-bits-db))


(defmethod save-doc-bits-db ((out stream)
                             &optional (db *doc-bits-db*))
    (format out ";;;; -*- Mode: Lisp -*-~2%;;;; DOC-BITS DB File.~2%")
    (loop for doc-bits being the hash-value of db
          do (map nil (lambda (doc-bit) (prin1 doc-bit out)) doc-bits))
    (format out "~2%;;;; end of file -- DOC BITS DB File. --~%"))


(defmethod save-doc-bits-db ((f pathname)
                             &optional (db *doc-bits-db*))
  (with-open-file (out f
                       :direction :output
                       :if-exists :supersede
                       :if-does-not-exist :create)
    (save-doc-bits-db out db)))


(defmethod save-doc-bits-db ((f string)
                             &optional (db *doc-bits-db*))
  (save-doc-bits-db (parse-namestring f) db))


;;; load-doc-bits-db --

(defgeneric load-doc-bits-db (where &optional doc-bits-db))

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

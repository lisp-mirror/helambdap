;;;; -*- Mode: Lisp -*-

;;;; xhtml-producer.lisp --
;;;; Make a file out of a doc-bit.

(in-package "HELAMBDAP")

(declaim (inline newlines newline))


(defun newlines (&optional (out *standard-output*) (n 1))
  (dotimes (i n)
    (terpri out)))


(defun newline (&optional (out *standard-output*))
  (newlines out 1))



(defgeneric produce-documentation (format structure out doc-bits)
  )


(defmethod produce-documentation ((format (eql 'html))
                                  (structure documentation-structure)
                                  (out stream)
                                  doc-bits)
  (dolist (c (documentation-structure-structure structure))
    (produce-documentation'html c out doc-bits)
    ))


(defmethod produce-documentation ((format (eql 'html))
                                  (structure style-file)
                                  (out file-stream)
                                  doc-bits)
  (let ((doc-directory
         (pathname (directory-namestring (truename out))))
        (sfn (style-file-name structure))
        )
    (cl-fad:copy-file sfn
                      (make-pathname :name (pathname-name sfn)
                                     :type (pathname-type sfn))
                      :overwrite nil)
    ))


(defmethod produce-documentation ((format (eql 'html))
                                  (structure doc-file)
                                  (out file-stream)
                                  doc-bits)
  (let ((doc-directory
         (pathname (directory-namestring (truename out))))
        (dfn (doc-file-name structure))
        )
    (cl-fad:copy-file sfn
                      (make-pathname :name (pathname-name dfn)
                                     :type (pathname-type dfn))
                      :overwrite nil)
    ))



(defmethod produce-documentation ((format (eql 'html))
                                  structure
                                  (out file-stream)
                                  (doc-bit doc-bit))
  (format out (doc-bit-doc-string doc-bit))
  )


(defun dump-doc-bit-html (name str-tag doc-string out)
  (with-html-output (out)
      (htm (:head
            (:title (fmt "~A ~A" str-tag (string-downcase name)))
            (:link :rel "stylesheet" :href "../clstyle.css")
           (:body
            (:h1 (:i (fmt "~A " str-tag)) (:strong (str name)))
            (:h2 "Package: ") (:p (str (package-name (symbol-package name))))
            (:h2 "Description:" (:p (str doc-string)))
            )))))


(defmethod produce-documentation ((format (eql 'html))
                                  structure
                                  (out file-stream)
                                  (doc-bit function-doc-bit))
  (let ((name (doc-bit-name doc-bit))
        (doc-string (doc-bit-doc-string doc-bit))
        )
    (dump-doc-bit-html name "Function" doc-string out)))



(defmethod produce-documentation ((format (eql 'html))
                                  structure
                                  (out file-stream)
                                  (doc-bit macro-doc-bit))
  (let ((name (doc-bit-name doc-bit))
        (doc-string (doc-bit-doc-string doc-bit))
        )
    (dump-doc-bit-html name "Macro" doc-string out)))


(defmethod produce-documentation ((format (eql 'html))
                                  structure
                                  (out file-stream)
                                  (doc-bit compiler-macro-doc-bit))
  (let ((name (doc-bit-name doc-bit))
        (doc-string (doc-bit-doc-string doc-bit))
        )
    (dump-doc-bit-html name "Compiler Macro" doc-string out)))


(defmethod produce-documentation ((format (eql 'html))
                                  structure
                                  (out file-stream)
                                  (doc-bit setf-expander-doc-bit))
  (let ((name (doc-bit-name doc-bit))
        (doc-string (doc-bit-doc-string doc-bit))
        )
    (dump-doc-bit-html name "Setf Expander" doc-string out)))


(defmethod produce-documentation ((format (eql 'html))
                                  structure
                                  (out file-stream)
                                  (doc-bit modify-macro-doc-bit))
  (let ((name (doc-bit-name doc-bit))
        (doc-string (doc-bit-doc-string doc-bit))
        )
    (dump-doc-bit-html name "Modify Macro" doc-string out)))


(defmethod produce-documentation ((format (eql 'html))
                                  structure
                                  (out file-stream)
                                  (doc-bit generic-function-doc-bit))
  (let ((name (doc-bit-name doc-bit))
        (doc-string (doc-bit-doc-string doc-bit))
        )
    (dump-doc-bit-html name "Generic Function" doc-string out)))


(defmethod produce-documentation ((format (eql 'html))
                                  structure
                                  (out file-stream)
                                  (doc-bit type-doc-bit))
  (let ((name (doc-bit-name doc-bit))
        (doc-string (doc-bit-doc-string doc-bit))
        )
    (dump-doc-bit-html name "Type" doc-string out)))


(defmethod produce-documentation ((format (eql 'html))
                                  structure
                                  (out file-stream)
                                  (doc-bit class-doc-bit))
  (let ((name (doc-bit-name doc-bit))
        (doc-string (doc-bit-doc-string doc-bit))
        )
    (dump-doc-bit-html name "Class" doc-string out)))


(defmethod produce-documentation ((format (eql 'html))
                                  structure
                                  (out file-stream)
                                  (doc-bit condition-doc-bit))
  (let ((name (doc-bit-name doc-bit))
        (doc-string (doc-bit-doc-string doc-bit))
        )
    (dump-doc-bit-html name "Condition" doc-string out)))


(defmethod produce-documentation ((format (eql 'html))
                                  structure
                                  (out file-stream)
                                  (doc-bit method-combination-doc-bit))
  (let ((name (doc-bit-name doc-bit))
        (doc-string (doc-bit-doc-string doc-bit))
        )
    (dump-doc-bit-html name "Method Combination" doc-string out)))


(defmethod produce-documentation ((format (eql 'html))
                                  structure
                                  (out file-stream)
                                  (doc-bit package-doc-bit))
  (let ((name (doc-bit-name doc-bit))
        (doc-string (doc-bit-doc-string doc-bit))
        )
    (dump-doc-bit-html name "Package" doc-string out)))






(defmethod produce-documentation ((format (eql 'html)) structure (out stream) doc-bit)
  (let ((name (doc-bit-name doc-bit))
        (doc-string (doc-bit-doc-string doc-bit))
        )

    (with-html-output (out)
      (htm (:head
            (:title (fmt "DOC FOR ~A" (string-downcase name)))
            (:link :rel "stylesheet" :href "../clstyle.css")
           (:body
            (:h1 (:i "Function") (:strong (str name)))
            (:h2 "Package:")
            (:h2 "Description:" (terpri out) (str doc-string))
            )))))
    t)


;;;; end of file -- xhtml-producer.lisp --

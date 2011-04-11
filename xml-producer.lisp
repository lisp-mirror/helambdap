;;;; -*- Mode: Lisp -*-

;;;; xml-producer.lisp --
;;;; Make XML documents out of DOC-BITs.

(in-package "HELAMBDAP")


(defgeneric translate-doc-bit-to-xml (doc-bit out
                                              &key (print-pretty)
                                              &allow-other-keys))


(defparameter *indent-n-spaces* 8)


#|
(defun dump-doc-bit-to-xml (name str-tag doc-string out)
  (format out "<helambdap:docbit name=\"~A\" kind=\"~A\" package=\"~A\">~%"
          (string-downcase name)
          str-tag
          (package-name (symbol-package name)))
  (pprint-indent :block *indent-n-spaces* out)
  (format out "<helambdap:description>~%")
  (write-string doc-string out)
  (pprint-newline :mandatory out)
  (format out "</helambdap:description>~%")
  (format out "</helambdap:docbit>~%")
  )
|#


(defmethod translate-doc-bit-to-xml ((db doc-bit)
                                     (out stream)
                                     &key (print-pretty *print-pretty*)
                                     &allow-other-keys)
  (let ((name (doc-bit-name db))
        (doc-string (doc-bit-doc-string db))
        (kind-tag (doc-bit-kind-tag db))
        (*print-pretty* print-pretty)
        )
    (when (and doc-string (string/= doc-string ""))
      (pprint-logical-block (out nil)
        (format out "<helambdap:docbit name=\"~A\" kind=\"~A\" package=\"~A\">~%"
                (string-downcase name)
                kind-tag
                (package-name (symbol-package name)))
        (pprint-logical-block (out nil)
          (pprint-newline :mandatory out)
          (pprint-indent :block *indent-n-spaces* out)
          (format out "<helambdap:description>~%")
          (write-string doc-string out)
          (pprint-newline :mandatory out)
          (format out "</helambdap:description>~%"))
        (format out "</helambdap:docbit>~%")
        ))))


#|
(defmethod translate-doc-bit-to-xml ((db doc-bit)
                                     (out stream)
                                     &key (print-pretty *print-pretty*)
                                     &allow-other-keys)
  (let ((name (doc-bit-name db))
        (doc-string (doc-bit-doc-string db))
        (kind-tag (doc-bit-kind-tag db))
        (*print-pretty* print-pretty)
        )
    (when (and doc-string (string/= doc-string ""))
      (pprint-logical-block (out nil)
        (format out "<helambdap:docbit name=\"~A\" kind=\"~A\" package=\"~A\">~%"
                (string-downcase name)
                kind-tag
                (package-name (symbol-package name)))
        (pprint-indent :block *indent-n-spaces* out)
        (pprint-logical-block (out nil
                                   :prefix "<helambdap:description>"
                                   :suffix "</helambdap:description>")
          (pprint-newline :mandatory out)
          (write-string doc-string out)
          (pprint-newline :mandatory out)
          )
        (format out "</helambdap:docbit>~%")
        ))))
|#



(defmethod translate-doc-bit-to-xml ((db package-doc-bit)
                                     (out stream)
                                     &key (print-pretty *print-pretty*)
                                     &allow-other-keys)
  (let ((name (package-doc-bit-name db))
        (doc-string (package-doc-bit-doc-string db))
        (kind-tag (package-doc-bit-kind-tag db))
        (*print-pretty* print-pretty)
        )
    (when (and doc-string (string/= doc-string ""))
      (pprint-logical-block (out nil)
        (format out "<helambdap:docbit name=\"~A\" kind=\"~A\">"
                name
                kind-tag)
        (pprint-logical-block (out nil)
          (pprint-newline :mandatory out)
          (pprint-indent :block *indent-n-spaces* out)
          (format out "<helambdap:description>~%")
          (write-string doc-string out)
          (pprint-newline :mandatory out)
          (format out "</helambdap:description>~%"))
        (format out "</helambdap:docbit>~%")
        ))))


(defmethod translate-doc-bit-to-xml ((db system-doc-bit)
                                     (out stream)
                                     &key (print-pretty *print-pretty*)
                                     &allow-other-keys)
  (let ((name (system-doc-bit-name db))
        (doc-string (system-doc-bit-doc-string db))
        (kind-tag (system-doc-bit-kind-tag db)) 
        (*print-pretty* print-pretty)
        )
    (when (and doc-string (string/= doc-string ""))
      (pprint-logical-block (out nil)
        (format out "<helambdap:docbit name=\"~A\" kind=\"~A\">"
                name
                kind-tag)
        (pprint-logical-block (out nil)
          (pprint-newline :mandatory out)
          (pprint-indent :block *indent-n-spaces* out)
          (format out "<helambdap:description>~%")
          (write-string doc-string out)
          (pprint-newline :mandatory out)
          (format out "</helambdap:description>~%"))
        (format out "</helambdap:docbit>~%")
        ))))


;;;; end of file -- xml-producer.lisp --

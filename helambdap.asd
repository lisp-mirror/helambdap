;;;; -*- Mode: Lisp -*-

(eval-when (:load-toplevel :compile-toplevel :execute)
  ;; (pushnew :helambdap.with-cxml *features*)
  ;; (pushnew :helambdap.with-ediware *features*)
  (pushnew :helambdap.with-xhtmlambda *features*)
  )


(asdf:defsystem "HELAMBDAP"
  :description "The HELambdaP System."
  :components ((:file "helambdap-pkg")
               (:module "utilities"
                :depends-on ("helambdap-pkg")
                :components ((:file "text-utilities")
                             (:file "filename-utilities")
                             (:file "time-utilities")
                             (:file "streams-utilities")
                             (:file "symbols-utilities")
                             ))
               (:file "naming")
               (:file "doc-bit" :depends-on ("naming"))
               (:file "extract-doc")
               (:file "helambdap")
               (:file "collect-documentation")
               (:file "doc-structure")
               (:file "documentation-production")

               (:file "xhtml-common-definitions")

               #+helambdap.with-ediware
               (:file "xml-producer"
                :depends-on ("doc-structure" "utilities" "documentation-production"
                                             "xhtml-common-definitions"))

               #+helambdap.with-ediware
               (:file "xhtml-producer"
                :depends-on ("doc-structure" "utilities" "documentation-production"
                                             "xhtml-common-definitions"))

               #+helambdap.with-cxml
               (:file "xhtml-cxml-producer"
                :depends-on ("doc-structure" "utilities" "documentation-production"
                                             "xhtml-common-definitions"))

               #+helambdap.with-xhtmlambda
               (:file "xhtml-lambda-producer"
                :depends-on ("doc-structure" "utilities" "documentation-production"
                                             "xhtml-common-definitions"))

	       (:module "impl-dependent"
		:depends-on ("helambdap-pkg")
		:components (
			     #+lispworks
			     (:file "lispworks")
			     ))
               )
  :depends-on ("cl-fad"
               "SPLIT-SEQUENCE"
               #+helambdap.with-cxml "CXML"
               #+helambdap.with-ediware "CL-WHO"
               #+helambdap.with-ediware "HTML-TEMPLATE"
               #+helambdap.with-xhtmlambda "XHTMLAMBDA"
               )
  )


;;;; end of file -- helambdap.asd --

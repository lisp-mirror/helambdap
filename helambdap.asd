;;;; -*- Mode: Lisp -*-

(eval-when (:load-toplevel :compile-toplevel :execute)
  ;; (pushnew :helambdap.with-cxml *features*)
  ;; (pushnew :helambdap.with-ediware *features*)
  (pushnew :helambdap.with-xhtmlambda *features*)
  )


(asdf:defsystem "helambdap"
  :description "The HELambdaP System."

  :author "Marco Antoniotti <mantoniotti (you-know-the-drill) common-lisp.net>"

  :license "BSD like"

  :components (
	       (:file "helambdap-pkg")
               (:file "helambdap-globals" :depends-on ("helambdap-pkg"))
               (:module "utilities"
                :depends-on ("helambdap-pkg")
                :components ((:file "text-utilities")
                             (:file "filename-utilities")
                             (:file "time-utilities")
                             (:file "streams-utilities")
                             (:file "symbols-utilities")
                             (:file "lambda-list-parsing")
                             ))
               (:file "naming" :depends-on ("helambdap-pkg"))
               (:file "doc-bit" :depends-on ("naming" "helambdap-globals"))
               (:file "extract-doc" :depends-on ("doc-bit"))
               (:file "helambdap" :depends-on ("doc-bit"))
               (:file "collect-documentation" :depends-on ("doc-bit"))
               (:file "doc-structure" :depends-on ("helambdap-pkg" "utilities"))
               (:file "documentation-production"
                :depends-on ("impl-dependent" "doc-structure"))

               (:file "xhtml-common-definitions"
                :depends-on ("helambdap-pkg"))

               #+helambdap.with-ediware
               (:file "xml-producer"
                :depends-on ("doc-structure"
                             "utilities"
                             "documentation-production"
                             "xhtml-common-definitions"))

               #+helambdap.with-ediware
               (:file "xhtml-producer"
                :depends-on ("doc-structure"
                             "utilities"
                             "documentation-production"
                             "xhtml-common-definitions"))

               #+helambdap.with-cxml
               (:file "xhtml-cxml-producer"
                :depends-on ("doc-structure"
                             "utilities"
                             "documentation-production"
                             "xhtml-common-definitions"))

               #+helambdap.with-xhtmlambda
               (:file "html5-lambda-producer"
                :depends-on ("doc-structure"
                             "utilities"
                             "documentation-production"
                             "xhtml-common-definitions"))

	       (:module "impl-dependent"
		:depends-on ("helambdap-pkg")
		:components (
			     #+lispworks
			     (:file "lispworks")
			     #+sbcl
                             (:file "sbcl")
                             #+asdf
                             (:file "asdf-deps")
                             #+mk-defsystem
                             (:file "mk-deps")
			     ))
               )
  :depends-on ("cl-fad"
               "split-sequence"
               #+helambdap.version-using-MOP "closer-mop"
               #+helambdap.with-cxml "cxml"
               #+helambdap.with-ediware "cl-WHO"
               #+helambdap.with-ediware "html-template"
               #+helambdap.with-xhtmlambda "xhtmlambda"
               )
  )


;;;; end of file -- helambdap.asd --

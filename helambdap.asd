;;;; -*- Mode: Lisp -*-

;;;; helambdap.asd --
;;;;
;;;; Main ASDF spec for HELambdaP.
;;;;
;;;; See file COPYING for copyright and licensing information.


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
               (:file "setup" :depends-on ("helambdap-globals"))
               (:module "utilities"
                :depends-on ("helambdap-pkg")
                :components ((:file "text-utilities")
                             (:file "package-utilities")
                             (:file "filename-utilities")
                             (:file "time-utilities")
                             (:file "streams-utilities")
                             (:file "symbols-utilities")
                             (:file "lambda-list-parsing")
                             ))
               (:file "naming" :depends-on ("setup"))
               (:file "doc-bit" :depends-on ("naming" "setup"))
               (:file "extract-doc" :depends-on ("doc-bit"))
               (:file "helambdap" :depends-on ("doc-bit"))
               (:file "collect-documentation" :depends-on ("doc-bit"))
               (:file "doc-structure"
                :depends-on ("setup"
                             "utilities"
                             "collect-documentation"))
               (:file "documentation-production"
                :depends-on ("impl-dependent"
                             "helambdap"
                             "doc-structure"))

               (:file "xhtml-common-definitions"
                :depends-on ("setup"))

               (:file "doc-string-handling"
                :depends-on ("xhtml-common-definitions"))

               (:file "html-source-handling"
                :depends-on ("xhtml-common-definitions"))

               (:file "xhtml-lambda-producer-protocol"
                :depends-on ("xhtml-common-definitions"))

               #+helambdap.with-ediware
               (:file "xml-producer"
                :depends-on ("doc-structure"
                             "utilities"
                             "documentation-production"
                             "doc-string-handling"
                             "html-source-handling"
                             "xhtml-lambda-producer-protocol"
                             ))

               #+helambdap.with-ediware
               (:file "xhtml-producer"
                :depends-on ("doc-structure"
                             "utilities"
                             "documentation-production"
                             "doc-string-handling"
                             "html-source-handling"
                             "xhtml-lambda-producer-protocol"
                             ))

               #+helambdap.with-cxml
	       (:file "xhtml-cxml-producer"
                :depends-on ("doc-structure"
                             "utilities"
                             "documentation-production"
                             "doc-string-handling"
                             "html-source-handling"
                             "xhtml-lambda-producer-protocol"
                             ))

               #+helambdap.with-xhtmlambda
	       (:file "xhtml-lambda-producer"
		:depends-on ("doc-structure"
                             "utilities"
                             "documentation-production"
                             "doc-string-handling"
                             "html-source-handling"
                             "xhtml-lambda-producer-protocol"
                             ))

	       #+(and helambdap.with-xhtmlambda helambdap.html5-working)
	       (:file "html5-lambda-producer"
		:depends-on ("doc-structure"
                             "utilities"
                             "documentation-production"
                             "doc-string-handling"
                             "html-source-handling"
                             "xhtml-lambda-producer-protocol"
                             ))

	       (:module "impl-dependent"
		:depends-on ("helambdap-pkg")
		:components (
			     #+lispworks
			     (:file "lispworks")
			     #+sbcl
                             (:file "sbcl")
                             #+ccl
                             (:file "ccl")

                             #+asdf
                             (:file "asdf-deps")
                             #+mk-defsystem
                             (:file "mk-deps")
			     ))
               )
  :depends-on ("cl-fad"
               "clad"
               "split-sequence"
               #+helambdap.version-using-MOP "closer-mop"
               #+helambdap.with-cxml "cxml"
               #+helambdap.with-ediware "cl-WHO"
               #+helambdap.with-ediware "html-template"
               #+helambdap.with-xhtmlambda "xhtmlambda"
               )
  )


;;;; end of file -- helambdap.asd --

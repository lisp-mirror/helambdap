;;;; -*- Mode: Lisp -*-

;;;; helambdap.asd --

(asdf:defsystem :helambdap
  :documentation "The HELambdaP System."
  :components ("helambdap-pkg"
               (:module "utilities"
                :depends-on ("helambdap-pkg")
                :components ("text-utilities"))
               "doc-bit"
               "extract-doc"
               "helambdap"
               "collect-documentation"
               "doc-structure"
               "documentation-production"
               (:file "xml-producer"
                :depends-on ("doc-structure" "utilities" "documentation-production"))
               (:file "xhtml-producer"
                :depends-on ("doc-structure" "utilities" "documentation-production"))
               )
  :depends-on ("CL-FAD"
               "CL-WHO"
               "HTML-TEMPLATE"))

;;;; end of file -- helambdap.asd --
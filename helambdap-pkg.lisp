;;;; -*- Mode: Lisp -*-

;;;; helambdap-pkg.lisp --
;;;; Separate doc string for CL elements.
;;;;
;;;; See file COPYING for copyright and licensing information.

(defpackage "IT.UNIMIB.DISCO.MA.CL.HELambdaP" (:use "CL"
                                               ;; #+helambdap.with-ediware "CL-WHO"
                                               ;; #+helambdap.with-cxml "CXML"
                                               ;; #+helambdap.with-xhtmlambda "XHTMLAMBDA"
                                               )
  (:nicknames "HELAMBDAP" "HELambdaP")
  (:shadow "DOCUMENTATION")
  (:shadow "FRAMESET" "FRAME")
  ;; (:shadowing-import-from "XHTMLAMBDA" "MAP" "TIME")

  (:export
   "DOCUMENT"
   "RESULTS"

   "BUILD-DOCUMENTATION"
   "PRODUCE-DOCUMENTATION"
   )
  (:documentation "The HELambdaP Package.

The package containing the implementation of the 'semi-offline'
documentation system for Common Lisp.")
  )

;;;; end of file -- helambda-pkg.lisp --



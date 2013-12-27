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
   "DOCUMENTATION")

  (:export
   "*EVERYTHING*"
   "*ONLY-DOCUMENTED*"
   "*ONLY-EXPORTED*"

   "*EXCLUDE-DIRECTORIES*"
   "*EXCLUDE-FILES*"

   "DOCUMENT"
   "RESULTS"

   "EXTRACT-DOCUMENTATION"
   "DEFINE-DOCUMENTATION-EXTRACTOR"
   "EXTRACT-FORM-DOCUMENTATION"
   "EXTRACT-NAMED-FORM-DOCUMENTATION"
   "BUILD-DOCUMENTATION"
   "PRODUCE-DOCUMENTATION"
   )

  (:export
   "DOC-BITS-DATA-BASE"
   "DOC-BITS-DATA-BASE-P"

   "INIT-DOC-BITS-DB"
   "CLEAR-DOC-BITS-DB"
   ;; "LOAD-DOC-BITS-DB"
   "SAVE-DOC-BITS-DB"
   "GET-DOC-BITS"
   "INSERT-DOC-BIT"
   )

  (:export
   "+DOCTYPE-FRAMESET-CONTROL-STRING+"
   "+DOCTYPE-XHTML1-STRING-CONTROL-STRING+"
   "+DOCTYPE-HTML5-CONTROL-STRING+"
   "+FRAMESET-DOCTYPE-PUBLIC-ID+"
   "+FRAMESET-DOCTYPE-SYSTEM-ID+"
   )

  (:documentation "The HELambdaP Package.

The package containing the implementation of the 'semi-offline'
documentation system for Common Lisp.")
  )

;;;; end of file -- helambda-pkg.lisp --



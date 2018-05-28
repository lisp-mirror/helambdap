;;;; -*- Mode: Lisp -*-

;;;; document-helambdap.lisp --
;;;; The call that was used to produce the HELAMBDAP documentation
;;;; itself.
;;;; This must be invoked from the main HELambdaP directory.
;;;;
;;;; See file COPYING for copyright and licensing information.

(in-package "CL-USER")

(hlp:document #P"./"
              :documentation-title "HE&Lambda;P"
              :format :html
              :exclude-files (list
                              "html5-lambda-producer-saved-1.lisp"
                              "html5-lambda-producer-saved-2.lisp")
              :only-documented t
              :only-exported t
              )


;;;; end of file -- document-helambdap.lisp --

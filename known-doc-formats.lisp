;;;; -*- Mode: Lisp -*-

;;;; known-doc-formats.lisp --
;;;;
;;;; Separate the definition of known fromats from the supporting code
;;;; definitions in 'doc-formats.lisp'.

;;;; See file COPYING for copyright and licensing information.


(in-package "HELAMBDAP")


;;; texinfo-format
;;; --------------

(define-doc-format texinfo-format texinfo :texinfo) ; Experimental.


;;; HTML and HTML5 formats.
;;; -----------------------

(define-doc-format html-format html :html)

(define-doc-format html5-format html5 :html5
                   :derives-from html-format)

;;;; end of file -- known-doc-formats.lisp --

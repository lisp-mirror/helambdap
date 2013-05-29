;;;; -*- Mode: Lisp -*-

;;;; xhtml-common-definitions.lisp --

(declaim (type string
               +doctype-frameset-control-string+
               +doctype-xhtml1-string-control-string+
               +frameset-doctype-public-id+
               +frameset-doctype-system-id+
               ))


(defconstant +doctype-frameset-control-string+
"<!DOCTYPE HTML PUBLIC
  \"-//W3C//DTD HTML 4.01 Frameset//EN\"
  \"http://www.w3.org/TR/html4/frameset.dtd\">"

"The standard 'DOCTYPE' w3c Frameset DTD (X)HTML string.")


(defconstant +doctype-xhtml1-string-control-string+
"<!DOCTYPE HTML PUBLIC
  \"-//W3C//DTD XHTML 1.0 Strict//EN\"
  \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">"

"The standard 'DOCTYPE' w3c DTD XHTML Strict DTD (X)HTML string.")


(defconstant +frameset-doctype-public-id+
  "-//W3C//DTD HTML 4.01 Frameset//EN")


(defconstant +frameset-doctype-system-id+
  "http://www.w3.org/TR/html4/frameset.dtd")

;;;; end of file -- xhtml-common-definitions.lisp --

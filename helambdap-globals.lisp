;;;; -*- Mode: Lisp -*-

;;;; helambdap-globals.lisp --
;;;;
;;;; See file COPYING for copyright and licensing information.

(in-package "HELAMBDAP")

;;;;===========================================================================
;;;; Global special parameters controlling the documentation production.


;;;; Filesystem parameters.
;;;; ----------------------

;;;; *helambdap-site* --

(defparameter *helambdap-site*
  "http://helambdap.sourceforge.net"
  "The site where HELambdaP currently resides.")


;;;; *helambdap-source-location*

(defparameter *helambdap-source-location*
  nil
  "The location where HELambdaP source resides.

This parameter is set during HELambdaP installation.  If not it is NIL
and it indicates that HELambdaP is not properly installed.")


;;;; *helambdap-data-folder*

(defparameter *helambdap-data-folder*
  (clad:ensure-app-or-library-data-folder "HELambdaP")
  ;; CLAD dependency.
  "The user HELambdaP data folder.

Notes:

You are advised to handle this parameter with care.  If you change
this parameter you should be prepared to change most other HELambdaP
pathnames.

See Also:

*HELAMBDAP-CSS-PATHNAME*, *HELAMBDAP5-CSS-PATHNAME*, *HELAMBDAP-JS-PATHNAME*")


;;;; *helambdap-css-filename*

(defparameter *helambdap-css-filename* "helambdap.css"
  "The default name for the (x)html .css file.")


;;;; *helambdap-css-pathname*

(defparameter *helambdap-css-pathname*
  (make-pathname :name "helambdap"
                 :type "css"
                 :defaults *helambdap-data-folder*)
    "The default pathname for the (X)HTML .css file.

The default directory is set to the location of the source file.")


;;;; *helambdap5-css-filename*

(defparameter *helambdap5-css-filename* "helambdap5.css"
  "The default name for the HTML5 .css file.")


;;;; *helambdap5-css-pathname*

(defparameter *helambdap5-css-pathname*
  (make-pathname :name "helambdap5"
		 :type "css"
		 :defaults *helambdap-data-folder*)
  "The default pathname for the (X)HTML .css file.

The default directory is set to the location of the source file.")


;;;; *default-html-extension*

(defparameter *default-html-extension* "html")


;;;; *helambdap-js-relative-pathname*

(defparameter *helambdap-js-relative-pathname*
  (make-pathname :name "helambdap-support"
                 :type "js"
                 :directory (list :relative "js")))


;;;; *helambdap-js-pathname*

(defparameter *helambdap-js-pathname*
  (merge-pathnames *helambdap-js-relative-pathname*
                   *helambdap-data-folder*))


(defparameter *helambdap-js-data-folder*
  (merge-pathnames (make-pathname :directory (list :relative "js"))
                   *helambdap-data-folder*))


;;;; Execution parameters.
;;;; ---------------------

;;;; *supersede-documentation* --

(defparameter *supersede-documentation* t)


;;;; *everything*, *only-documented*, *only-exported* --
;;;;
;;;; The *EVERYTHING*, *ONLY-DOCUMENTED*, *ONLY-EXPORTED* need the
;;;; following explanation.  When *EVERYTHING* is true, HELAMBDAP produces
;;;; documentation for every bit in the "search" path requested regardless
;;;; of the values of *ONLY-DOCUMENTED* and *ONLY-EXPORTED*.

(defparameter *everything* nil
  "Controls whether the system must produce documentation for everything. 

When *EVERYTHING* is true, HELAMBDAP produces documentation for every
bit in the 'search' path requested regardless of the values of
*ONLY-DOCUMENTED* and *ONLY-EXPORTED*.

See Also:

*ONLY-DOCUMENTED* and *ONLY-EXPORTED*.
"
  )


(defparameter *only-documented* t
  "Controls whether the system must produce documentation only for
user-documented items.

When *ONLY-DOCUMENTED* is true, then the system produces documentation
only for those items the user has actually written a documentation
string.

See Also:

*EVERYTHING* and *ONLY-EXPORTED*.
"
  )

(defparameter *only-exported* nil
  "Controls whether the system must produce documentation only for
'exported' items (i.e., symbols).

When *ONLY-EXPORTED* is true, then the system produces documentation
only for those items (symbols) that are exported from their package.

See Also:

*EVERYTHING* and *ONLY-DOCUMENTED*.
")


;;;; *exclude-directories*, *exclude-files --
;;;;
;;;; The *EXCLUDE-DIRECTORIES* and *EXCLUDE-FILES* lists contain
;;;; pathnames that the documentation extraction procedures will not
;;;; consider.

(defparameter *exclude-directories*
  (list #P".git/" #P"CVS/" #P"svn/" #P"tmp/"))

(defparameter *exclude-files*
  ())


;;;; *source-extensions* --

(defparameter *source-extensions*
  (list "lisp" "lsp" "l" #+asdf "asd" #+mk-defsystem "system" "cl")
  "List of possible 'source extensions' of files where Lisp code may be found.")


;;;; *helambdap-configation-parameters*

(defvar *helambdap-configuration-parameters*
  '(
    *helambdap-source-location*
    *helambdap-data-folder*
    *helambdap-css-filename*
    *helambdap-css-pathname*
    *helambdap5-css-filename*
    *helambdap5-css-pathname*
    *helambdap-js-pathname*
    *helambdap-js-data-folder*

    *default-html-extension*

    *supersede-documentation*
    *everything*
    *only-exported*
    *only-documented*
    *exclude-files*
    *exclude-directories*
    *source-extensions*
    )
  "A list of HELambdaP most important parameters.")


;;;; end of file -- helambdap-globals.lisp --

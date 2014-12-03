;;;; -*- Mode: Lisp -*-

;;;; helambdap-globals.lisp --
;;;;
;;;; See file COPYING for copyright and licensing information.

(in-package "HELAMBDAP")

;;;;===========================================================================
;;;; Global special parameters controlling the documentation production.


;;;; *helambdap-site* --

(defparameter *helambdap-site*
  "http://helambdap.sourceforge.net"
  "The site where HELambdaP currently resides.")


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
  (list "lisp" "lsp" "l" "asd" "system" "cl")
  "List of possible 'source extensions' where Lisp code is contained.")


;;;; end of file -- helambdap-globals.lisp --

;;;; -*- Mode: Lisp -*-

;;;; xhtml-lambda-producer-protocol.lisp --
;;;; Make a file out of a DOCUMENTATION-STRUCTURE and a set (list) of
;;;; DOC-BITs, using a (X)HTML or a HTML5 output format, using
;;;; XHTMLambda.
;;;;
;;;; This file contains the main "protocol" definitions, i.e., mostly
;;;; the top-level DEFGENERICs that will be instantiated in the
;;;; implementation files.
;;;;
;;;; See file COPYING for copyright and licensing information.

(in-package "HELAMBDAP")


;;;;===========================================================================
;;;; Prologue.

;;;; The (X)HTML/HTML5 producer makes essentially two kinds framesets:
;;;; the "prose" and "help" ones and the "dictionary" one.
;;;;
;;;; The "prose" and "help" ones are the "index", "downloads",
;;;; "mailing-lists"/"contact" and "links" framesets or HTML5
;;;; elements, which have the following layout:
#|

+=MAIN========================================+
[+-HEADER------------------------------------+]
[|                                           |]
[+-------------------------------------------+]
[+=DOC-AREA==================================+]
[[+-NAV-++-INFO-AREA------------------------+]]
[[|     ||                                  |]]
[[|     ||                                  |]]
[[|     ||                                  |]]
[[|	||      			    |]]
[[|	||      			    |]]
[[|	||      			    |]]
[[+-----++----------------------------------+]]
[+===========================================+]
[+-FOOTER------------------------------------+]
[|                                           |]
[+-------------------------------------------+]
+=============================================+

MAIN and DOC-AREA (with other names in the code, depending on the
DOCUMENTATION-STRUCTURE) are HTML FRAMESETS, HEADER, NAV, INFO-AREA
and FOOTER are HTML FRAMES.  There should also be a Sidebar, but you
get the idea.

Each FRAMESET and FRAME is contained in a separate file.
|#
;;;; The "dictionary" frameset is essentially the same, but it
;;;; requires a more complex navigation scheme, reflected in the shape
;;;; of the NAV frame.
;;;;
;;;; The question is: what should we navigate through?  Well, let's
;;;; take our inspiration from doxygen (www.doxygen.org) and let's
;;;; tune it to CL, with the caveat that, for the time being, I will not
;;;; use "rendered" source files.
;;;;
;;;; There are, IMHO, a few "axis" or "indices" we'd like to see at
;;;; the top level in the navigation area.
;;;; o	Packages
;;;; o	Systems
;;;; o	Files (possibly ordered by directory)
;;;;
;;;; Systems could be seen as "more top-level", but, for the time
;;;; being, I'll just use them to narrow down on the list of files.
;;;; The structure of the "dictionary" navigation will therefore be
#|

+=NAV=====================+
[+-MENU------------------+]
[| Systems Files Packages|]
[|                       |]
[|                       |]
[|                       |]
[+-----------------------+]
[+-MENULIST--------------+]
[|                       |]
[|                       |]
[|                       |]
[|                       |]
[|                       |]
[|                       |]
[|                       |]
[|                       |]
[|                       |]
[|                       |]
[|                       |]
[|                       |]
[+-----------------------+]
+=========================+

|#

;;;; Notes:
;;;; The layout already reflects HTML5 elements.


;;;;===========================================================================
;;;; Protocol.


;;; New protocol.
;;; -------------
;;;
;;; The new protocol is more abstract and directly uses the layout
;;; naming (which happens to follow HTML5 conventions).

(defgeneric produce-main (format element out))

(defgeneric produce-header (format element out))

(defgeneric produce-footer (format element out))

(defgeneric produce-doc-area (format element out))

(defgeneric produce-navigation (format
                                element
                                ;; out
                                where
                                doc-bits
                                doc-title))

(defgeneric produce-navigation-links (format
                                      element
                                      nav-element
                                      nav-pathname
                                      doc-bits
                                      documentation-title))

(defgeneric produce-navigation-map (format
                                    element
                                    nav-element
                                    navmap-pathname
                                    doc-bits
                                    doc-title))


(defgeneric produce-package-navigation-list (format
                                             element
                                             nav-element
                                             package-doc-bit
                                             package-nav-list-pathname
                                             doc-bits
                                             ))


(defgeneric produce-info-area-placeholder (format
                                           element
                                           destination
                                           &optional
                                           documentation-title))


(defgeneric render-doc-bit (format doc-bit out n str-tag doc-string))

(defgeneric render-lambda-list (format lambda-list-type lambda-list))



;;; Old protocol.
;;; -------------

(defgeneric produce-frame (format element out)
  )


(defgeneric produce-navigation-frame (format
                                      element
                                      frameset-stream
                                      where
                                      doc-bits
                                      doc-title)
  )


(defgeneric produce-navigation-file (format
                                     frameset
                                     nav-element
                                     nav-pathname
                                     doc-bits
                                     documentation-title))


(defgeneric produce-header-frame (format
                                  frameset
                                  frameset-stream
                                  where
                                  doc-bits
                                  doc-title)
  )


(defgeneric produce-header-file (frameset
                                 header-pathname
                                 documentation-title))


(defgeneric produce-footer-frame (format
                                  frameset
                                  frameset-stream
                                  where
                                  doc-bits
                                  doc-title)
  )

(defgeneric produce-footer-file (frameset
                                 footer-pathname
                                 documentation-title))


(defparameter *xhtml-indent* 4) ; 4 is the actual initial value.

(defparameter *formatted-section-right-margin* 256)


;;;;===========================================================================
;;;; Implementation.




;;;---------------------------------------------------------------------------
;;; Auxiliary files production.

(defgeneric frameset-head-title (fs)
  (:method ((fs frameset)) "Frameset head placeholder title"))


(defgeneric frameset-body-title (fs)
  (:method ((fs frameset)) "Frameset body placeholder title"))


(defgeneric framesets-of (e)
  (:method ((fss framesets)) (framesets-list fss))
  (:method ((e element)) ())
  (:method ((e documentation-structure)) ())
  )


;;;; end of file -- xhtml-lambda-producer-protocol.lisp --

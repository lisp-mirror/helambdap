;;;; -*- Mode: Lisp -*-

;;;; filename-utilities.lisp --
;;;; A few utilities for dealing wth filenames.
;;;;
;;;; See file COPYING for copyright and licensing information.

(in-package "HELAMBDAP")


(defun base-name (file-n-or-p &optional (type nil)
                             &aux (file-pathname (pathname file-n-or-p)))
  (declare (type (or string pathname) file-n-or-p)
           (type (or null string) type)
           (type pathname file-pathname))
  (if (and type (string-equal type (pathname-type file-pathname)))
      (namestring
       (make-pathname :device nil
                      :directory ()
                      :type nil
                      :defaults file-pathname))
      (namestring
       (make-pathname :device nil
                      :directory ()
                      :defaults file-pathname))
      ))


(defun directory-name (file-n-or-p &aux (file-pathname (pathname file-n-or-p)))
  (declare (type (or string pathname) file-n-or-p)
           (type pathname file-pathname))
  (namestring
   (make-pathname :device nil
                  :name nil
                  :type nil
                  :defaults file-pathname)))


;;;; end of file -- filename-utilities.lisp --

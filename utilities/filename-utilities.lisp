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


(defun enclosing-directory (file-n-or-p &aux (file-pathname (pathname file-n-or-p)))
  (declare (type (or string pathname) file-n-or-p)
           (type pathname file-pathname))
  (let* ((tn file-pathname)
         (tnd (pathname-directory tn))
         (tnn (pathname-name tn))
         (tnt (pathname-type tn))
         )
    (flet ((get-directory (dir n &aux (l (length dir)))
             (when (and dir (listp dir))
               (cond ((> l 2)
                      (cons :relative (last dir (min (- l 2) n))))
                     ((= l 2)
                      (if (eq :absolute (first dir))
                          dir
                          (list :relative (second dir))))
                     (t dir))))
           )
                     
      (if (or (and (null tnn) (null tnt))
              (and (eq tnn :unspecific) (eq tnt :unspecific))
              (and (null tnn) (eq tnt :unspecific))
              (and (eq tnn :unspecific) (null tnt)))
          ;; Pathname names - most likely, these being CL pathnames - a directory.
          (get-directory tnd 2)
          ;; Pathname names a regular file.
          (get-directory tnd 1)
          ))))


(defun quote-wild-in-pathname-name (name) ; Can't use SUBSTITUTE.
  (declare (type (or string pathname) name))
  (flet ((quote-wild-cs (n)
           (declare (type string n))
           (with-output-to-string (r)
             (loop for c across n
                   if (char= #\* c)
                   do (write-string "\\*" r)
                   else if (char= #\Space c)
                   do (write-char #\_ r)
                   else
                   do (write-char c r)))))
    (etypecase name
      (string (quote-wild-cs name))
      (pathname
       (make-pathname :name (quote-wild-cs (pathname-name name))
                      :defaults name)))
    ))


;;;; end of file -- filename-utilities.lisp --

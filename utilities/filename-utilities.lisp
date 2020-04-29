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

      ;; Both HOST and DEVICE must be explicitely set to NIL because,
      ;; as we all know, it is FUC#%&G "implementation dependent"
      ;; what goes in there.
      (namestring
       (make-pathname :host nil
                      :device nil
                      :directory ()
                      :type nil
                      :defaults file-pathname))
      (namestring
       (make-pathname :host nil
                      :device nil
                      :directory ()
                      :defaults file-pathname))
      ))


(defun directory-pathname (file-n-or-p &aux (file-pathname (pathname file-n-or-p)))
  (declare (type (or string pathname) file-n-or-p)
           (type pathname file-pathname))

  ;; Both HOST and DEVICE must be explicitely set to NIL because,
  ;; as we all know, it is FUC#%&G "implementation dependent"
  ;; what goes in there.

  (make-pathname :host nil
                 :device nil
                 :name nil
                 :type nil
                 :defaults file-pathname))


(defun directory-name (file-n-or-p &aux (file-pathname (pathname file-n-or-p)))
  (declare (type (or string pathname) file-n-or-p)
           (type pathname file-pathname))
  (namestring (directory-pathname file-pathname)))


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


;;;; Fooling around.

(defun path-full (host device directory name type version)
  (make-pathname :host host
                 :device device
                 :directory directory
                 :name name
                 :type type
                 :version version))


(defgeneric path (x &key)
  (:method ((x string) &key) (pathname x))
  (:method ((x pathname) &key) x)
  (:method ((x list) &key (defaults *default-pathname-defaults*) (relative t))
   (if (null x)
       (if relative
           (setf x (cons :relative x))
           (setf x (cons :absolute x)))
       (let ((x1 (first x)))
         (unless (or (eq x1 :relative) (eq x1 :absolute))
           (if relative
               (setf x (cons :relative x))
               (setf x (cons :absolute x))))))
   (make-pathname :directory x
                  :defaults defaults))
  )

(defgeneric conc-paths (p1 p2 &optional warn)
  (:method ((p1 pathname) (p2 pathname) &optional warn)
   (when (and warn (pathname-name p1))
     (warn "Pathname ~S is not a pure folder pathname." p1))
   (when (and (pathname-directory p2)
              (eq :absolute (first (pathname-directory p2))))
     (error "Pathname ~S is an absolute pathname." p2))
   (merge-pathnames p2 p1)))


(defun conc-pathnames (p1 p2 &rest more-paths)
  (reduce #'conc-paths more-paths
          :initial-value (conc-paths p1 p2)))

;;;; end of file -- filename-utilities.lisp --

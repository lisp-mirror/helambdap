;;;; -*- Mode: Lisp -*-

;;;; setup.lisp --
;;;; Code executed to ensure a sane operating environment.  Some
;;;; ancillary functions are here as well.
;;;;
;;;; See file COPYING for copyright and licensing information.

(in-package "HELAMBDAP")

;;;; Debugging parameters.
;;;; ---------------------

(define-symbol-macro *hlp-dbg-level* *current-debug-tag*)

(defvar *hlp-dbg-reader* (* +max-debug-tag+ 0.8))

(defvar *hlp-dbg-trace*  (/ +max-debug-tag+ 2))

(defvar *hlp-dbg-warn*   (* +max-debug-tag+ 0.9))

(defvar *hlp-dbg-msg*    +max-debug-tag+)



;;;; Configuration parameters.
;;;; -------------------------

;;;; HELambdaP source location: the key parameter.

(defun helambdap-source-location ()
  *helambdap-source-location*)


;;;; *configuration-readme*

(defvar *configuration-readme*
  "HELambdaP Configuration.
========================
Date: ~A.

This folder contains some files that are used by HELambdaP to produce
libraries and applications documentations.

The files are:

* helambdap.css  : style file for HTML doc production.
* helambdap5.css : style file for HTML5 doc production.

And the folder:

* js : Javascript code needed for, especially, HTML5 doc production.

Editing or removing these files may result in errors in HELambdaP
operations.

These files are overwritten every time HELambdaP is (re)initialized.

See the file COPYING in folder for licensing information.
")


;;; write-configuration-readme --

(defun write-configuration-readme ()
  (let* ((hlp-timestamp-pname
          (merge-pathnames (pathname "TIMESTAMP")
                           *helambdap-source-location*))

         (timestamp (if (probe-file hlp-timestamp-pname)
                        (with-open-file (ts hlp-timestamp-pname
                                            :direction :input)
                          (read-line ts))
                        (let ((dt (multiple-value-list
                                   (get-decoded-time)))
                              )
                          (format nil "~D~2,'0D~2,'0D" ; " (TZ: ~D, DLS: ~A)"
                                  (sixth dt)
                                  (fifth dt)
                                  (fourth dt)
                                  ;; (ninth dt)
                                  ;; (eight dt)
                                  ))
                        ))
         (conf-readme-pathname
          (make-pathname :name "README"
                         :type "md"
                         :defaults *helambdap-data-folder*))
         )
    (with-open-file (r conf-readme-pathname
                       :direction :output
                       :if-does-not-exist :create
                       :if-exists :supersede)
      (format r *configuration-readme* timestamp))
    t
    ))



;;;; ensure-configuration-files

(defun ensure-configuration-files ()
  (let ((hlp-css-pname
         (merge-pathnames (pathname *helambdap-css-filename*)
                          *helambdap-source-location*))
        (hlp5-css-pname
         (merge-pathnames (pathname *helambdap5-css-filename*)
                          *helambdap-source-location*))

        (hlp-js-pname
         (merge-pathnames (pathname *helambdap-js-relative-pathname*)
                          *helambdap-source-location*))
        )
    (unless (probe-file hlp-css-pname)
      (error "HELambdaP: CSS file \"~A\" does not exist."
             hlp-css-pname))

    (unless (probe-file hlp5-css-pname)
      (error "HELambdaP: CSS5 file \"~A\" does not exist."
             hlp5-css-pname))

    (unless (probe-file hlp-js-pname)
      (error "HELambdaP: JS file \"~A\" does not exist."
             hlp-js-pname))

    (cl-fad:copy-file hlp-css-pname *helambdap-css-pathname*
                      :overwrite t)

    (cl-fad:copy-file hlp5-css-pname *helambdap5-css-pathname*
                      :overwrite t)

    (ensure-directories-exist *helambdap-js-data-folder*)

    (cl-fad:copy-file hlp-js-pname *helambdap-js-pathname*
                      :overwrite t)

    (write-configuration-readme)

    t
    ))


;;; helambdap-print-configuration

(defun helambdap-print-configuration (&optional (where *standard-output*))
  "A function that prints out the value of most configuration paramenters.

Arguments and Values:

WHERE : a destination stream to be used by FORMAT.
"
  (format where
          "~%~
           HELambdaP main configuration parameters have the following values:~2%~
           ~:{~A : ~S~%~}~%"
          (mapcar (lambda (p) (list p (symbol-value p)))
                  *helambdap-configuration-parameters*)))


;;;; The setup.
;;;; Maybe add something to edit the CL init file as Quicklisp does.

(eval-when (:load-toplevel :execute)
  (let ((hlploc (or #+asdf
                    (and (asdf:find-system "helambdap" nil)
                         (asdf:system-source-directory "helambdap"))

                    #+mk-defsystem
                    (and (mk:find-system "helambdap" :load-or-nil)
                         (mk:system-definition-pathname "helambdap"))

                    #-(or asdf mk-defsystem)
                    *load-pathname*
                    ))
        )
    (setf *helambdap-source-location*
          (make-pathname :name nil
                         :type nil
                         :defaults hlploc))

    (ensure-configuration-files)

    (start-debug-logging :prefix "HLP: ")

    (pushnew :helambdap *features*)
    :helambdap
    ))

;;;; end of file -- setup.lisp --

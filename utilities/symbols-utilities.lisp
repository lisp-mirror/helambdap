;;;; -*- Mode: Lisp -*-

;;;; symbols-utilities.lisp --
;;;; A few functional utilities (to avoid dependencies).
;;;;
;;;; See file COPYING for copyright and licensing information.

(in-package "HELAMBDAP")

(defun external-symbol-p (s &optional (sp (symbol-package s)))
  (declare (type symbol s)
           (type (or null package) sp))
  (and sp
       (eq :external
           (nth-value 1
                      (find-symbol (symbol-name s) sp)))))
                                   

(defun inherited-symbol-p (s &optional (sp (symbol-package s)))
  (declare (type symbol s)
           (type (or null package) sp))
  (and sp
       (eq :inherited
           (nth-value 1
                      (find-symbol (symbol-name s) sp)))))


(defun internal-symbol-p (s &optional (sp (symbol-package s)))
  (declare (type symbol s)
           (type (or null package) sp))
  (and sp
       (eq :internal
           (nth-value 1
                      (find-symbol (symbol-name s) sp)))))


(defun uninterned-symbol-p (s)
  (declare (type symbol s))
  (null (symbol-package s)))


(defun symbol-status-in-package (sd pd &aux (p (find-package pd)))
  (declare (type (or string symbol) sd)
           (type (or string package) pd))

  (multiple-value-bind (s status)
      (find-symbol (string sd) p)
    (cond (status ; Symbol is "accessible" from P.
           (let ((sp (symbol-package s)))
             (if (null sp)
                 ;; Intersting case.  A "homeless" symbol, accessible
                 ;; from package PD
                 (values p status nil nil)
                 (if (eq sp p)
                     (values p status sp status)

                     ;; This is the interesing case.
                     (multiple-value-bind (s-in-sp s-status-in-sp)
                         (find-symbol (string sd) sp)
                       (declare (ignore s-in-sp))
                       (let ((s-p-vs-sp
                              (ecase status
                                (:internal :internal)
                                (:inherited :inherited)
                                (:external (ecase s-status-in-sp
                                             ((:inherited :internal)
                                              ;; Should never be :inherited.
                                              :imported-exported)
                                             (:external
                                              (if (find s (package-shadowing-symbols p))
                                                  :external
                                                  :re-exported))
                                             ))))
                             )
                         (values p s-p-vs-sp sp s-status-in-sp)))
                     ))))
          ((symbolp sd) ; Symbol is not "accessible" from P.
           (let ((sp (symbol-package sd)))
             (if (null sp)
                 ;; Again, a "homeless" symbol.
                 (values p status nil nil)
                 (multiple-value-bind (s-in-sp s-status-in-sp)
                     (find-symbol (string sd) sp)
                   (declare (ignore s-in-sp))
                   (values p status sp s-status-in-sp)))))
          (t ; Not a symbol of a string was passed. Signal a TYPE-ERROR.
           (error 'type-error
                  :datum sd
                  :expected-type '(or symbol string))))
    ))


(defun assimilate-package (from-package to-package)
  (declare (type package from-package to-package))
  ;; (assert (packagep from-package))
  ;; (assert (packagep to-package))

  ;; Assimilating a package evetually destroys FROM-PACKAGE.  Should
  ;; call this function BORG-PACKAGE.
  
  ;; NB. The assumption is that TO-PACKAGE is "empty"
  ;; (save - maybe - for using "CL").

  ;; 0 - Make TO-PACKAGE use what FROM-PACKAGE uses.

  (use-package (package-use-list from-package) to-package)
  
  ;; 1 - Import symbols from FROM-PACKAGE to TO-PACKAGE.
  ;;     After this step all symbols from FROM-PACKAGE will be
  ;;     accessible via TO-PACKAGE.

  (do-symbols (s from-package)
    (when (or (uninterned-symbol-p s)
              (eq (symbol-package s) from-package))
      (import s to-package)))

  ;; 2 - Now re-export form TO-PACKAGE those symbols that are external
  ;;     in FROM-PACKAGE.

  (do-external-symbols (s from-package)
    (export (find-symbol (string s) to-package) to-package))

  ;; 3 - Next we unintern the symbols from FROM-PACKAGE; the effect is
  ;;     to have the symbols from FROM-PACKAGE to be "homeless".
  ;;     Of course, this disrupts FROM-PACKAGE.

  (do-symbols (s from-package)
    (unintern s from-package))

  ;; 4 - We now re-import the symbols in TO-PACKAGE to establish their
  ;;     new package.

  (do-symbols (s to-package)
    (when (null (symbol-package s))
      (import s to-package)))

  ;; 6 - FROM-PACKAGE has been assimilated.

  (delete-package from-package)
  to-package
  )
    
;;;; end of file -- symbols-utilities.lisp --

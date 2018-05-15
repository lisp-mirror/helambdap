;;;; -*- Mode: Lisp -*-

;;;; lispworks.lisp --

;;;; XHTMLambda Lispworks dependent code.

(editor:setup-indent "with-html-syntax" 1 4 6)


;;;; HELambdaP Lispworks dependent code.

(in-package "HLP")

(define-condition unknown-package-lw (unknown-package
                                      conditions:package-not-found)
  ()
  )


(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf *unknown-package-condition-class* 'unknown-package-lw))


(defmethod make-unknown-package-condition
           ((upc-class (eql 'unknown-package-lw)))
  (make-instance 'unknown-package-lw))


(defmethod make-unknown-package-condition
           ((pnf conditions:package-not-found))
  (change-class pnf
                'unknown-package-lw
                :name (package-error-package pnf)))

;;;; end of file -- lispworks.lisp

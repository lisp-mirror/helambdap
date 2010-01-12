;;;; -*- Mode: Lisp -*-

;;;; naming.lisp --

(in-package "HELAMBDAP")

(defstruct (naming (:type list)
                   :named
                   (:conc-name name-)
                   (:constructor nil))
  (kind 'function :read-only t)
  (id nil :type symbol :read-only t)
  (nesting-path () :type list :read-only t)
  )

(deftype naming () '(or symbol (satisfies naming-p)))


(defun make-naming (kind name &optional nesting-path)
  (list 'naming kind name nesting-path))


(defun make-naming* (kind name &rest nesting-path)
  (make-naming kind name nesting-path))


;;;; end of file -- naming.lisp --

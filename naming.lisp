;;;; -*- Mode: Lisp -*-

;;;; naming.lisp --

(in-package "HELAMBDAP")

(defstruct (naming (:constructor %make-naming))
  (kind 'function :read-only t)
  (id nil :type (or symbol list) :read-only t)
  (nesting-path () :type list :read-only t)
  )


(defun make-naming (kind name &optional nesting-path)
  (%make-naming :kind kind :id name :nesting-path nesting-path))


(defun make-naming* (kind name &rest nesting-path)
  (%make-naming :kind kind :id name :nesting-path nesting-path))


;;;; end of file -- naming.lisp --

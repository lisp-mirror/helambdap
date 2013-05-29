;;;; -*- Mode: Lisp -*-

;;;; naming.lisp --
;;;;
;;;; See file COPYING for copyright and licensing information.

(in-package "HELAMBDAP")

(defstruct (naming (:constructor %make-naming))
  "The Naming Structure Class."
  (kind 'function :read-only t)
  (id nil :type (or symbol list) :read-only t)
  (nesting-path () :type list :read-only t)
  )


(defun make-naming (kind name &optional nesting-path)
  "Constructs a NAMING."
  (%make-naming :kind kind :id name :nesting-path nesting-path))


(defun make-naming* (kind name &rest nesting-path)
  "Constructs a NAMING.

RESTING-PATH is passed as a &rest parameter."
  (%make-naming :kind kind :id name :nesting-path nesting-path))


;;;; end of file -- naming.lisp --

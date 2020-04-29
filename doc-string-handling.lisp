;;;; -*- Mode: Lisp -*-

;;;; doc-string-handling.lisp --
;;;; Functions to "parse" doc strings.
;;;;
;;;; See file COPYING for copyright and licensing information.

(in-package "HELAMBDAP")


;;;;===========================================================================
;;;; Protocol.


;;;;===========================================================================
;;;; Implementation.

;;;---------------------------------------------------------------------------
;;; Doc bits HTML production.

;;; Usual SBCL appeasement.

#-sbcl
(defconstant +lambda-list-kwds+
  '(&optional &rest &key &allow-other-keys &whole &environment &aux))

#+sbcl
(defparameter +lambda-list-kwds+
  '(&optional &rest &key &allow-other-keys &whole &environment &aux))


(defun arg-name (arg)
  (if (consp arg)
      (let ((a1 (first arg)))
        ;; Is it a KWD spec?
        (if (consp a1)
            (first a1) ; The "visible" bit.
            a1
            ))
      arg))


(defun produce-doc-bit-title-name (doc-bit)
  (let* ((db-name (doc-bit-name doc-bit)) ; This can be a (SETF ID).
         (dbi (doc-bit-identifier doc-bit))
         (name (format nil "~(~A~)" db-name))
         (kind-tag (doc-bit-kind-tag doc-bit))
         (qualifier (if (symbolp dbi)
                        (if (external-symbol-p dbi)
                            ""
                            "Internal")
                        ""))
         )
    (<:h1 ()
          (<:i () qualifier kind-tag)
          (<:strong () name)
          )))


(defun paragraphize-doc-string (s)
  (loop for par in (split-at-tex-paragraphs s)
        when (string/= "" par)
        collect (<:p () par)))


(defgeneric process-doc-string (s input-syntax output-format
                                  &optional
                                  args-n-values-p
                                  lambda-list
                                  result-p
                                  returns-decl
                                  type-decls
                                  skip-description-header-p
                                  )
  (:method ((s null) input-syntax output-format
            &optional
            args-n-values-p
            lambda-list
            result-p
            returns-decl
            type-decls
            skip-description-header-p
            )
   (declare (ignorable input-syntax output-format)
            (ignore args-n-values-p
                    lambda-list
                    result-p
                    returns-decl
                    type-decls
                    skip-description-header-p
                    ))
   )
  (:documentation
   "Processes a 'doc string'.

The processing is done (or rather a best effort is made to parse)
according to a give 'input-syntax' and the result is built in a
given 'output-format'."))


(defun parse-doc-hyperspec-style (s)
  (declare (type string s))
  (let* ((syntax-header "Syntax:")
         (shl (length syntax-header))
         (args-n-values "Arguments and Values:")
         (anvl (length args-n-values))
         (description "Description:")
         (dl (length description))
         (examples "Examples:")
         (el (length examples))
         (affected-by "Affected By:")
         (abl (length affected-by))
         (see-also "See Also:")
         (sal (length see-also))
         (side-effects "Side Effects:")
         (sel (length side-effects))
         (notes "Notes:")
         (nl (length notes))
         (excepts "Exceptional Situations:")
         (exl (length excepts))
         (pars (split-at-tex-paragraphs s))
         )
    ;; Assumes that the doc string follows the Hyperspec sectioning
    ;; conventions. However, it ignores the 'Syntax:' section and it
    ;; assumes that - at a minimum - everything is 'Description:'.

    (loop with state = 'description
          
          for p in pars
          for pl = (length p)
          
          ;; do (format t ">>> ~A ~S~%" state p)
          
          if (string= p syntax-header :end1 (min pl shl))
          do (setf state 'syntax-header)
          else if (string= p args-n-values :end1 (min pl anvl))
          do (setf state 'args-n-values)
          else if (string= p description :end1 (min pl dl))
          do (setf state 'description)
          else if (string= p examples :end1 (min pl el))
          do (setf state 'examples)
          else if (string= p affected-by :end1 (min pl abl))
          do (setf state 'affected-by)
          else if (string= p see-also :end1 (min pl sal))
          do (setf state 'see-also)
          else if (string= p notes :end1 (min pl nl))
          do (setf state 'notes)
          else if (string= p excepts :end1 (min pl exl))
          do (setf state 'excepts)
          else if (string= p side-effects :end1 (min pl sel))
          do (setf state 'side-effects)
          
          else
          
          if (eq state 'syntax-header)
          collect p into syntax-pars
          
          else if (eq state 'args-n-values)
          collect p into args-n-values-pars
          
          else if (eq state 'description)
          collect p into description-pars
          
          else if (eq state 'examples)
          collect p into examples-pars
          
          else if (eq state 'affected-by)
          collect p into affected-by-pars
          
          else if (eq state 'see-also)
          collect p into see-also-pars
          
          else if (eq state 'notes)
          collect p into notes-pars

          else if (eq state 'excepts)
          collect p into excepts-pars

          else if (eq state 'side-effects)
          collect p into side-effects-pars

          ;; else collect p into description-pars ; ? Should I leave this?
          end

          finally
          (return (values syntax-pars
                          (split-all-lines args-n-values-pars)
                          (mapcar #'sanitize-string-for-html description-pars)
                          examples-pars
                          affected-by-pars
                          see-also-pars
                          notes-pars
                          excepts-pars
                          side-effects-pars
                          ))
          )))


(defun process-arg-n-value-pars (anv-pars)
  (let ((descrs ()))
    (dolist (p (split-all-lines anv-pars) (nreverse descrs))
      (unless (string= "" p)
        (let ((sep-pos (or (search "---" p :test #'char=)
                           (search " : " p :test #'char=))))
          (if sep-pos
              (let ((arg (string-right-trim '(#\Space #\Tab)
                                            (subseq p 0 sep-pos)))
                    (arg-desc (string-left-trim '(#\Space #\Tab)
                                                (subseq p (+ sep-pos 3))))
                    )
                (push (<:li (:style "list-style-type: none")
                            (<:i () (<:code () arg)) " : " arg-desc)
                      descrs))
              (push (<:p () p) descrs))
          )))))





(defgeneric render-syntax-section (format doc-bit
                                          &optional lambda-list values))


(defun bypass-pprint (s e &optional (colon-p t) at-sign-p)
  (declare (type stream s)
           (ignore colon-p at-sign-p))
  (let ((*print-pretty* nil))
    (format s "~A" e)))


(defun process-returns-declaration (returns)
  "Munging of RETURNS declaration."
  (declare (type list returns)
           (returns ("A list of (X)HTMLambda 'li' elements." list))
           ;; Money where mouth is!!!
           )
  
  (loop with rl = (< 1 (list-length returns))
        for r in returns
        for r-i from 0
        for is-full-syntax = (and (listp r) (stringp (first r)))
        
        for doc-string = (when is-full-syntax (first r))
        
        for type = (if is-full-syntax (second r) r)

        for name = (if (and is-full-syntax (third r))
                       (third r)
                       (format nil "result~:[~;-~D~]" rl r-i))
        collect (<:li (:style "list-style-type: none")
                      (<:i () (<:code () (string name)))
                      " : "
                      (if doc-string
                          doc-string
                          (format nil "a ~A." type)))
        into r-elements
        finally (if r-elements
                    (return r-elements)
                    (return (list (<:li (:style "list-style-type: none")
                                        (<:i () (<:code () "result"))
                                        " : a T."))))
        ))




;;;;===========================================================================
;;;; Utilities.

        
;;;; end of file -- doc-string-handling.lisp --

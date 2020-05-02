;;;; -*- Mode: Lisp -*-

;;;; extract-doc.lisp --
;;;;
;;;; See file COPYING for copyright and licensing information.

(in-package "HELAMBDAP")


;;;; Protocol.
;;;; =========


;;; extract-documentation --

(defgeneric extract-documentation (where-from)
  (:documentation
   "Extracts the documentation from a source WHERE-FROM."))


;;; extract-form-documentation --

(defgeneric extract-form-documentation (form-kind form)
  (:documentation
   "Extracts the documentation from a form tagged with a specific kind."))


;;; extract-named-form-documentation --

(defgeneric extract-named-form-documentation (form-kind name form)
  (:documentation
   "Extracts the documentation from a 'named' form.

A 'named' form has the following structure: (<name> . <forms>).
This generic function is useful to add functionality in the presence
of symbols for which the package may not be present at runtime.

This function is called by the more general EXTRACT-FORM-DOCUMENTATION
method on (FORM-KIND SYMBOL).  The call has the form:

    (extract-named-form-documentation s (intern s \"HELAMBDAP\") form)

In this way it is possible to write special methods to handle
\"unpackaged\" symbols.
")
  (:method ((form-kind symbol) name form)
   (declare (ignorable name form))
   nil)
  )


;;;; Package handling --

(defparameter *current-package* (find-package "COMMON-LISP-USER")
  "The variable holding the 'current' package.

Processing an IN-PACKAGE form sets this variable (if the package is
known) subsequent READs by the documentation extraction machinery
are done with *PACKAGE* bound to *CURRENT-PACKAGE*.")


;;; read-form --
;;; The core reader for CL source code.
;;; This function is necessarily kludgy because CL implementations
;;; don't agree what to signal when they read something that is
;;; kosher, like a symbol in an unknown package.  The standard reader
;;; will continue reading without trying to recover, yielding, in some
;;; implementations, "right parenthesis read" error, of course, non
;;; standard.
;;;
;;; One solution would be to use a full blown re-implementation
;;; of the reader, like Eclector
;;; https://github.com/s-expressionists/Eclector
;;; but FTTB I will postpone that.

(defun read-form (forms-stream &optional (eof (gensym "FORMS-STREAM-EOF-")))
  (handler-case
      (let* ((*package* *current-package*))
        (read forms-stream nil eof nil))

    (simple-error (e)
      (debugmsg *hlp-dbg-reader* "HLP form reader: "
                "trying to read a form caused errors.
                 ~?
                 The result will be NIL, hence the form will be ignored.~2%"
                (simple-condition-format-control e)
                (simple-condition-format-arguments e))
      nil)

    (error (e)
      ;; (describe e)
      ;; (finish-output)
      (debugmsg *hlp-dbg-reader* "HLP form reader: "
                "trying to read a form caused errors; most likely a missing package.
                 The error is ~S.
                 The result will be NIL, hence the form will be ignored.~2%"
                e)
      nil)))


;;; extract-documentation --
;;; ------------------------

(defmethod extract-documentation ((forms-stream stream))
  (let ((saved-package *package*)
        (*current-package* *package*)
        )
    (unwind-protect
        (loop with eof = (gensym "FORMS-STREAM-EOF-")
              for form = (read-form forms-stream eof)
              for form-doc = (form-documentation form)

              #|
              when (and (not (eq form eof))
                        (or (eq form :sink)
                            (and (consp form)
                                 (eq (first form) 'write-doctype))))
              do (break "Unexplicable on LW! ~S" form)
              end
              |#

              while (not (eq form eof))
                when form-doc
                  if (consp form-doc)
                    nconc (delete nil form-doc) into doc-bits
                  else
                    collect form-doc into doc-bits
                  end

              finally (return doc-bits))
      (setf *package* saved-package))))


(defmethod extract-documentation ((file pathname))
  (with-open-file (in file
                      :direction :input
                      :if-does-not-exist :error)
    (let ((file-doc-bits (extract-documentation in)))
      (dolist (fdb file-doc-bits file-doc-bits)
        (setf (doc-bit-location fdb) file)))))


(defmethod extract-documentation ((filename string))
  (extract-documentation (pathname filename)))


(defmethod extract-documentation :around ((forms-stream stream))
  (let ((doc-bits (delete nil (call-next-method))))
    (unless *everything*
      (when *only-documented*
        (setf doc-bits
              (delete-if (complement #'doc-bit-doc-string) doc-bits)))

      (when *only-exported*
        (setf doc-bits
              (delete-if (lambda (db &aux (dbn (doc-bit-identifier db)))
                           (and (symbolp dbn) (not (external-symbol-p dbn))))
                         doc-bits)))
      )
    doc-bits))


;;; Form handling.
;;; ==============

(defun form-documentation (form)
  (when (consp form)
    (extract-form-documentation (first form) form)))


(defun is-declaration (f)
  (and (consp f) (eq (first f) 'declare)))


(defun extricate-doc-string (forms)
  "Given a list of FORMS finds a doc-string according to CL rules.

Notes:

Cfr. ANSI 3.4.11 Syntactic Interaction of Documentation Strings and
Declarations."
  (loop for (doc-or-decl . more-forms) on forms
        while (or (stringp doc-or-decl) (is-declaration doc-or-decl))
        when (and (stringp doc-or-decl) more-forms)
        return doc-or-decl))


(defun collect-declarations (forms)
  "Collects a list of declarations from a form list according to CL rules.

Notes:

Cfr. ANSI 3.4.11 Syntactic Interaction of Documentation Strings and
Declarations."
  ;; Not really really really right, but good enough FTTB.
  (loop for f in forms
        while (or (stringp f) (is-declaration f))
        when (is-declaration f) collect f))


;;; Documentation per form.
;;; =======================

(defmacro define-documentation-extractor (spec &body forms)
  "Defines a specialized procedure to extract a doc string from a definition."
  `(defmethod extract-form-documentation ((_%FK%_ (eql ',(first spec)))
                                          (_%FORM%_ cons))
     ;; I know I should gensym these...
     (destructuring-bind ,spec
         _%FORM%_
       (declare (ignore ,(first spec)))
       ,.forms)))
       

;;; extract-form-documentation --
;;; -----------------------------

;;; Kitchen sink methods.

(defmethod extract-form-documentation ((fk symbol) (form cons))
  ;; (format t "~%>>> EFD: ~S ~S~2%" fk form)
  (let ((doc-bit
         (extract-named-form-documentation fk
                                           (intern (string fk) "HELAMBDAP")
                                           form))
        )
    (unless doc-bit
      (warnmsg t "HLP EFD: "
               "Operator ~A not handled." fk))
    doc-bit))


(defmethod extract-form-documentation ((fk cons) (form cons))
  (warnmsg t "HLP EFD: "
           "trying to extract documentation from a form without a SYMBOL header.~:
            ~S."
           fk
           )
  nil)


;;; Specialized methods.

;;; DEFTYPE

(defmethod extract-form-documentation ((fk (eql 'deftype)) (form cons))
  (destructuring-bind (deftype name ll &rest forms)
      form
    (declare (ignore deftype))
    (make-deftype-doc-bit :name name
                          :kind 'type
                          :lambda-list ll
                          :doc-string (extricate-doc-string forms))))


(defmethod extract-form-documentation ((fk (eql 'defun)) (form cons))
  (destructuring-bind (defun name ll &rest forms)
      form
    (declare (ignore defun))
    (let* ((decls (mapcan #'rest (collect-declarations forms)))
           (values-decl (find 'returns decls :key #'first))
           (type-decls (remove 'type decls :key #'first :test (complement #'eq)))
           (ftype-decls (remove 'ftype decls :key #'first :test (complement #'eq)))
           )
      (declare (ignorable type-decls ftype-decls))
      (make-function-doc-bit :name name
                             :kind 'function
                             :lambda-list ll
                             :values (rest values-decl)
                             :type-declarations type-decls
                             :ftype-declarations ftype-decls
                             :doc-string (extricate-doc-string forms)))))


;;; DEFMACRO

(defmethod extract-form-documentation ((fk (eql 'defmacro)) (form cons))
  (destructuring-bind (defmacro name ll &rest forms)
      form
    (declare (ignore defmacro))
    (make-macro-doc-bit :name name
                        :kind 'macro ; This breaks the rule.  There is
                                     ; no MACRO in the
                                     ; CL:DOCUMENTATION reference.
                        :lambda-list ll
                        :doc-string (extricate-doc-string forms))))


;;; DEFINE-COMPILER-MACRO

(defmethod extract-form-documentation ((fk (eql 'define-compiler-macro)) (form cons))
  (destructuring-bind (define-compiler-macro name ll &rest forms)
      form
    (declare (ignore define-compiler-macro))
    (make-compiler-macro-doc-bit :name name
                                 :kind 'compiler-macro
                                 :lambda-list ll
                                 :doc-string (extricate-doc-string forms))))


;;; DEFINE-SETF-EXPANDER

(defmethod extract-form-documentation ((fk (eql 'define-setf-expander))
                                       (form cons))
  (destructuring-bind (define-setf-expander name ll &rest forms)
      form
    (declare (ignore define-setf-expander))
    (make-setf-expander-doc-bit :name name
                                :kind 'setf
                                :lambda-list ll
                                :doc-string (extricate-doc-string forms))))


;;; DEFCLASS

(defmethod extract-form-documentation ((fk (eql 'defclass))
                                       (form cons))
  (destructuring-bind (defclass name supers slots &rest options)
      form
    (declare (ignore defclass))
    (make-class-doc-bit :name name
                        :kind 'type
                        :superclasses supers
                        :slots slots
                        :doc-string (second
                                     (find :documentation options
                                           :key #'first)))))


;;; DEFINE-CONDITION

(defmethod extract-form-documentation ((fk (eql 'define-condition))
                                       (form cons))
  (destructuring-bind (define-condition name supers slots &rest options)
      form
    (declare (ignore define-condition slots))
    (make-condition-doc-bit :name name
                            :kind 'type
                            :superclasses supers
                            :doc-string (second (find :documentation options
                                                      :key #'first)))))


;;; DEFGENERIC

(defmethod extract-form-documentation ((fk (eql 'defgeneric)) (form cons))
  (destructuring-bind (defgeneric name ll &rest options-and-methods)
      form
    (declare (ignore defgeneric))
    (let* ((decls (collect-declarations options-and-methods))
           ;; This is essentially wrong.  It should return NIL (almost) always.
           (values-decl (find 'returns (mapcan #'rest decls) :key #'first))
           (doc-string (second (find :documentation options-and-methods
                                     :key #'first)))
           (methods (mapcan (lambda (dg-form)
                              (when (eq (first dg-form) :method)
                                (list dg-form)))
                            options-and-methods))
           (m-doc-bits
            (mapcar (lambda (m)
                      (let ((dm-form (list* 'defmethod name (rest m))))
                        (extract-form-documentation 'defmethod dm-form)))
                    methods))
           )
      (make-generic-function-doc-bit :name name
                                     :kind 'function
                                     :lambda-list ll
                                     :values (rest values-decl)
                                     :doc-string doc-string
                                     :methods (delete nil m-doc-bits)
                                     ))))


;;; DEFPACKAGE

(defmethod extract-form-documentation ((fk (eql 'defpackage)) (form cons))
  (destructuring-bind (defpackage name &rest options)
      form
    (declare (ignore defpackage))
    ;; We must ensure that a package always has a doc string.
    ;; We must also ensure that the USE-LIST and the NICKNAMES list
    ;; contain strings, not fuc$@!ng uninterned symbols!!!!  Same for
    ;; NAME.

    (make-package-doc-bit :name (string name)
                          :kind 'package

                          :use-list 
                          (mapcar #'string (rest (find :use options :key #'first)))
                          :nicknames
                          (mapcar #'string (rest (find :nicknames options :key #'first)))

                          :doc-string (or (second (find :documentation options
                                                        :key #'first))
                                          (format nil "The ~A Package." name)
                                          ))))


(defvar *try-to-ensure-packages* t
  "Controls whether the system should try to create the packages it encouters.

DEFPACKAGE and IN-PACKAGE forms will be evaluated if non-NIL (default
T). Only top-level occurrences of these forms are considered.")


#| Not fully working...
(defmethod extract-form-documentation :before ((fk (eql 'defpackage)) (form cons))
  (when (and *try-to-ensure-packages* (not (find-package (second form))))
    ;; Suppose we are seeing this DEFPACKAGE *after* an IN-PACKAGE
    ;; that caused HELambdaP to crate the package with a nickname in
    ;; the DEFPACKAGE.
    ;;
    ;; We need to ensure that no package name conflicts appear and
    ;; that symbols already defined get properly handled.
    ;;
    ;; The following is rather kludgy, but hey!!!!

    (handler-case
        (eval form)
      (package-error (pe)
        ;; Most likely a 'nickname' error.
        ;; Different implementations have different 'nickname' errors.
        (let* ((nicknames
                (mapcan #'rest (remove :nicknames (cddr form)
                                       :key #'first
                                       :test-not #'eq)))
               (pkgs (delete-duplicates (mapcar #'find-package nicknames)
                                        :test #'eq))
               (n-pkgs (list-length pkgs))
               )
          (cond ((zerop n-pkgs)
                 ;; Infer that the error was something not 'nickname'
                 ;; related: just re-signal.
                 (signal pe))
                ((and (= n-pkgs 1)
                      (string-not-equal (package-name (first pkgs))
                                        (second form)))
                 ;; Maybe there is a bona-fide 'nickname' error.
                 ;; Allow the user to continue...
                 (cerror "Go ahead and fix the packages (the ~
                          defpackage form will have precedence and the nicknamed ~
                          package will be deleted)."
                         "HELambdaP found a defpackage form with one ~
                          of its nicknames naming the ~S package; ~
                          the defpackage wants to define a package named ~A."
                         (package-name (first pkgs))
                         (second form)
                         )
                 ;; Now the tricky part...
                 
                 ;; 1 - make the defpackage package.
                 (let ((pkg-defpkg (make-package (second form))))
                   ;; 2 - move the symbols from the 'nickname' pkg to
                   ;;     the defpkg package.
                   (loop for s being the present-symbols of (first pkgs)
                         for ns = (intern (string s) pkg-defpkg)
                         do (setf (symbol-plist ns) (symbol-plist s)
                                  (symbol-value ns) (symbol-value s)
                                  (symbol-function ns) (symbol-function s)
                                  ))
                   ;; 3 - delete the 'nicknamed' package.
                   (delete-package (first pkgs))
                   pkg-defpkg
                   ))
                ((and (= n-pkgs 1)
                      (string-equal (package-name (first pkgs))
                                    (second form)))
                 ;; Different kind of error: resignal.
                 (signal pe))
                (t
                 ;; Catch all.
                 (signal pe))
                )
          )))))
|#


(defmethod extract-form-documentation
           :before ((fk (eql 'defpackage)) (form cons))
  "This :before method takes care of ensuring that the defpackage is
actually evaluated while avoing problems with package synonyms.  This
method may signal a continuable error, that a user may decide s/he has
to handle in a particular way; the continuable error is generated when
there exist a package named by one of the defpackage form nicknames."

  (when (and *try-to-ensure-packages* (not (find-package (second form))))
    ;; Suppose we are seeing this DEFPACKAGE *after* an IN-PACKAGE
    ;; that caused HELambdaP to crate the package with a nickname in
    ;; the DEFPACKAGE.
    ;;
    ;; We need to ensure that no package name conflicts appear and
    ;; that symbols already defined get properly handled.
    ;;
    ;; The following is rather kludgy, but hey!!!!

    ;; We need to check whether a nickname already names a package.
    (let* ((nicknames
            (mapcan #'rest (remove :nicknames (cddr form)
                                   :key #'first
                                   :test-not #'eq)))
           (pkgs (delete nil
                         (delete-duplicates (mapcar #'find-package nicknames)
                                            :test #'eq)))
           (n-pkgs (list-length pkgs))
           )
      (cond ((zerop n-pkgs)
             ;; All "good nicknames"; just eval the form.
             (eval form))

            ((= n-pkgs 1)
             (cond ((string-equal (package-name (first pkgs))
                                  (string (second form)))
                    ;; No nickname will be generated; just eval the form.
                    (eval form))

                   ((string-not-equal (package-name (first pkgs))
                                      (string (second form)))
                    ;; This will generate a 'nickname error' (which different
                    ;; implementations signal differently).
                    (cerror "Go ahead and fix the packages; the 
                             'defpackage' form will have precedence and the nicknamed 
                             package will be assimilated and deleted."
                            "HELambdaP found a defpackage form with one 
                             of its nicknames naming the ~S package; 
                             the defpackage wants to define a package named ~A."
                            (package-name (first pkgs))
                            (second form)
                            )
                    ;; Now the tricky part...

                    ;; 1 - make the defpackage package.
                    (let ((pkg-defpkg (make-package (second form))))
                      ;; 2 - Assimilate the 'nickname' pkg to
                      ;;     the defpkg package.
                      (assimilate-package (first pkgs) pkg-defpkg)

                      ;; 3 - Eval the full defpackage form (maybe generating
                      ;; warnings, but who cares).
                      (eval form)
                     ))
                   ))
            ((> n-pkgs 1)
             ;; This is a show stopper.  Basically  we have
             ;; encountered two or more different packages named by
             ;; nicknames of the DEFPACKAGE form at hand.
             ;; This may have happened because of different IN-PACKAGE
             ;; forms in different files containing code of the
             ;; package.
             ;; There is no way to resolve this issue without doing
             ;; extra messy work looking at the different internal,
             ;; inherited and external lists of symbols from the
             ;; differen symbols.
             ;; FTTB I punt.
             
             (error
              "HELambdaP found a defpackage form with some of its nicknames naming 
               the ~{~S~#[~;, and ~:;, ~]~} package~P; 
               the DEFPACKAGE form wants to define a package named ~A.
               
               HELambdaP cannot fix this yet, but you probaly can just by using just 
               one package name or just one of its nicknames form the DEFPACKAGE 
               form."
              (mapcar #'package-name pkgs)
              (list-length pkgs)
              (string (second form)))
             )
            (t
             ;; Should never get here.
             ;; Catch all; which will probably make demons fly out of
             ;; your nose.  However, in that case, you are probably
             ;; better off cleaning up your code.
             (cerror
              "Continue evaluating the DEFPACKAGE form."
              "Something strange happened while dealing with the defpackage form for 
               ~S, with nickname~P ~{~S~#[~;, and ~:;, ~]~}."
              (list-length pkgs)
              (mapcar #'package-name pkgs))
             (eval form))
            )
      )))


;;; IN-PACKAGE

(defmethod extract-form-documentation
           :before ((fk (eql 'in-package)) (form cons))
  (when *try-to-ensure-packages*
    (let* ((pkg-name (second form))
           (pkg (find-package pkg-name))
           )
      (unless pkg
        ;; The next form can create 'nickname conflicts' with full
        ;; DEFPACKAGE forms encountered 'later' by HELambdaP.
        (make-package pkg-name)))))


(defmethod extract-form-documentation ((fk (eql 'in-package)) (form cons))
  nil)


(defmethod extract-form-documentation :after ((fk (eql 'in-package)) (form cons))
  (let ((pkg (find-package (second form))))
    (when pkg
      (setf *current-package* pkg))))


;;; USE-PACKAGE

(defmethod extract-form-documentation
           :before ((fk (eql 'use-package)) (form cons))
  (when *try-to-ensure-packages*
    (destructuring-bind (use-pkg-fn
                         package-use-list
                         &optional (package *package*))
        form
      (declare (ignore use-pkg-fn))
      (unless (listp package-use-list)
        (setq package-use-list (list package-use-list)))
      
      (let ((pkg-name (and package
                           (if (packagep package)
                               (package-name package)
                               package) ; Let's just hope it works FTTB.
                           ))
            (pkg (find-package package))
            )

        ;; In the rest we may not have all the packages available.
        ;; So we make an educated guess about the "use package list"
        ;; members.

        (let ((pkg-use-list
               (remove-if (lambda (p)
                            (not (find-package p)))
                          package-use-list)))

          ;; An empty package use list should be ok.

          (if pkg
              ;; We actually run the USE-PACKAGE.
              (use-package pkg-use-list pkg)
            
              ;; The next form can create 'nickname conflicts' with full
              ;; DEFPACKAGE forms encountered 'later' by HELambdaP.
              (make-package pkg-name :use pkg-use-list))))
      )))


(defmethod extract-form-documentation ((fk (eql 'use-package)) (form cons))
  nil)


;;; DEFMETHOD

(defmethod extract-form-documentation ((fk (eql 'defmethod)) (form cons))
  (destructuring-bind (defmethod name &rest rest-method)
      form
    (declare (ignore defmethod))
    (let ((arglist-pos (position-if #'consp rest-method)))
      (make-method-doc-bit :name name
                           :kind 'method ; Incorrect.
                           :qualifiers (subseq rest-method 0 arglist-pos)
                           :lambda-list (nth arglist-pos rest-method)
                           :doc-string (extricate-doc-string
                                        (subseq rest-method (1+ arglist-pos)))))
    ))


;;; DEFSTRUCT

(defmethod extract-form-documentation ((fk (eql 'defstruct)) (form cons))
  (destructuring-bind (defstruct name-and-options &rest doc-string-and-slots)
      form
    (declare (ignore defstruct))
    (let* ((name-is-symbol (symbolp name-and-options))
           (name (if name-is-symbol
                     name-and-options
                     (first name-and-options)))
           (options (unless name-is-symbol
                      (rest name-and-options)))
           (doc-string-present (stringp (first doc-string-and-slots)))
           (doc-string (when doc-string-present (first doc-string-and-slots)))
           (slots (if doc-string-present
                      (rest doc-string-and-slots)
                      doc-string-and-slots))

           ;; Options unpacking.
           (default-conc-name (format nil "~A-" name))
           (conc-name (if name-is-symbol
                          (intern default-conc-name (symbol-package name))
                          (loop for opt in options
                                if (eq opt :conc-name)
                                return nil
                                else if (and (consp opt)
                                             (eq (first opt) :conc-name))
                                return (second opt)
                                finally
                                (return (intern default-conc-name (symbol-package name)))
                                )))
           (default-constructor-name (format nil "~A-~A" 'make name))
           )
      (labels ((extract-slot-names (slots)
                 (mapcar (lambda (slot-spec)
                           (typecase slot-spec
                             (symbol slot-spec)
                             (list (first slot-spec))))
                         slots)
                 )

               (extract-slot-types (slots)
                 (mapcar (lambda (slot-spec)
                           (typecase slot-spec
                             (symbol t)
                             (list (getf (cddr slot-spec) :type t))))
                         slots))

               (build-doc-for-slots-fns (slots)
                 (loop for s in slots
                       if (symbolp s)
                       nconc (build-doc-for-single-slot-fns s nil t)
                       else
                       nconc
                       (destructuring-bind (sn vf
                                               &key
                                               read-only
                                               (type t)
                                               &allow-other-keys)
                           s
                         (declare (ignore vf))
                         (build-doc-for-single-slot-fns sn read-only type))))

               (build-doc-for-single-slot-fns (sn read-only type
                                                  &aux
                                                  (fns ())
                                                  (accessor-fn-name
                                                   (if conc-name
                                                       (intern (format nil "~A~A"
                                                                       conc-name
                                                                       sn)
                                                               (symbol-package sn))
                                                       sn))
                                                  )
                 (push (make-function-doc-bit
                        :name accessor-fn-name
                        :kind 'function
                        :lambda-list (list 'object)
                        :values (list type)
                        :doc-string
                        (format nil
                                "Accessor for the~:[~; read-only~] slot ~A of an object of type ~A.
                                     
                                 Arguments and Values:
                                     
                                 OBJECT : a ~A
                                 result : a ~A"
                                read-only
                                sn
                                name
                                name
                                type
                                )
                        )
                       fns)
                 (unless read-only
                   (push (make-function-doc-bit
                          :name `(setf ,accessor-fn-name)
                          :kind 'function
                          :lambda-list (list 'v 'object)
                          :values (list type)
                          :doc-string
                          (format nil "Setter for the slot ~A of an object of type ~A.
                                     
                                     Arguments and Values:
                                     
                                     OBJECT : a ~A
                                     result : a ~A"
                                  sn
                                  name
                                  name
                                  type)
                          )
                         fns)
                   )
                 fns
                 )

               (build-default-constructor-doc-bit ()
                 (let ((sns (extract-slot-names slots)))
                   (make-function-doc-bit
                    :name (intern default-constructor-name
                                  (symbol-package name))
                    :kind 'function
                    :lambda-list (if sns
                                     (cons '&key
                                           (extract-slot-names slots))
                                     ()
                                     )
                    :type-declarations (mapcan (lambda (slot type)
                                                 (when type
                                                   `((type ,type ,slot))))
                                               (extract-slot-names slots)
                                               (extract-slot-types slots))
                    :values (list name)
                    :doc-string
                    (format nil "A constructor for the structure ~A." name)
                    )))

               (build-doc-for-constructors (opts)
                 (let ((constructor-opts
                        (loop for opt in opts
                              when (typecase opt
                                     (symbol (eq :constructor opt))
                                     (list (eq :constructor (first opt))))
                              collect (if (symbolp opt) (list opt) opt)))
                       )
                   (cond ((null constructor-opts)
                          (list (build-default-constructor-doc-bit)))
                         ((not (find '(:constructor nil) constructor-opts :test #'equal))
                          (loop for cons-opt in constructor-opts
                                collect
                                (destructuring-bind (cons-kwd
                                                     &optional
                                                     cons-name
                                                     boa-ll)
                                    cons-opt
                                  (declare (ignore cons-kwd))
                                  (if (null cons-name) ; constructor
                                                       ; opt is (:constructor)
                                      (build-default-constructor-doc-bit)
                                      (make-function-doc-bit
                                       :name cons-name
                                       :kind 'function
                                       :lambda-list (cond (boa-ll boa-ll)
                                                          (slots (cons '&key
                                                                       (extract-slot-names slots))))
                                       :type-declarations (mapcan (lambda (slot type)
                                                                    (when type
                                                                      `((type ,type ,slot))))
                                                                  (extract-slot-names slots)
                                                                  (extract-slot-types slots))
                                       :values (list name)
                                       :doc-string
                                       (format nil
                                               "A constructor for the 
                                                structure ~A." name)
                                       ))
                                  )))
                         (t ())
                         )))
               )

        (append
         (list
          (make-struct-doc-bit :name name
                               :kind 'structure
                               :doc-string doc-string
                               :include (when (consp name-and-options)
                                          (second (find :include (remove-if #'symbolp name-and-options)
                                                        :key #'first)))
                               :slots slots
                               ))
         (build-doc-for-slots-fns slots)
         (build-doc-for-constructors options)
         )))))


;;; EVAL-WHEN

(defmethod extract-form-documentation ((fk (eql 'eval-when)) (form cons))
  (destructuring-bind (eval-when times &rest forms)
      form
    (declare (ignore eval-when times))
    (delete nil (mapcar #'form-documentation forms))))


;;; PROGN

(defmethod extract-form-documentation ((fk (eql 'progn)) (form cons))
  (destructuring-bind (progn &rest forms)
      form
    (declare (ignore progn))
    (delete nil (mapcar #'form-documentation forms))))


;;; DEFPARAMETER, DEFVAR, DEFCONSTANT

(defun extract-symbol-form-documentation (form)
  (fourth form))


(defmethod extract-form-documentation ((fk (eql 'defparameter)) (form cons))
  (let ((doc (extract-symbol-form-documentation form)))
    (make-parameter-doc-bit :name (second form)
                            :kind 'variable
                            :doc-string doc)))


(defmethod extract-form-documentation ((fk (eql 'defvar)) (form cons))
  (let ((doc (extract-symbol-form-documentation form)))
    (make-variable-doc-bit :name (second form)
                           :kind 'variable
                           :doc-string doc)))


(defmethod extract-form-documentation ((fk (eql 'defconstant)) (form cons))
  (let ((doc (extract-symbol-form-documentation form)))
    (make-constant-doc-bit :name (second form)
                           :initial-value (third form)
                           :kind 'constant
                           :doc-string doc)))


;;; DECLAIM

(define-documentation-extractor (declaim &rest forms)
  (declare (ignore forms))
  nil
  )


;;; PROCLAIM

(define-documentation-extractor (proclaim &rest forms)
  (declare (ignore forms))
  nil
  )


;;; LET

(define-documentation-extractor (let bindings &rest forms) ; Hmmmmm!
  ;; LET is a 'special operator'; it should work, but...
  (declare (ignore bindings))
  (delete nil (mapcar #'form-documentation forms))
  )


;;; LET*

(define-documentation-extractor (let* bindings &rest forms) ; Hmmmmm!
  ;; LET is a 'special operator'; it should work, but...
  (declare (ignore bindings))
  (mapcar #'form-documentation forms)
  )


;;; IMPORT, EXPORT, SHADOW, SHADOWING-IMPORT

(define-documentation-extractor (import symbols &optional pkg force)
  (declare (ignore symbols pkg force))
  nil
  )


(define-documentation-extractor (export symbols &optional pkg)
  (declare (ignore symbols pkg))
  nil
  )


(define-documentation-extractor (shadow symbols &optional pkg)
  (declare (ignore symbols pkg))
  nil
  )


(define-documentation-extractor (shadowing-import symbols &optional pkg)
  (declare (ignore symbols pkg))
  nil
  )


;;; SETQ, SETF, PSETF

(define-documentation-extractor (setq &rest pairs)
  (declare (ignore pairs))
  nil
  )


(define-documentation-extractor (setf &rest pairs)
  (declare (ignore pairs))
  nil
  )


(define-documentation-extractor (psetf &rest pairs)
  (declare (ignore pairs))
  nil
  )


;;; PUSH, PUSHNEW

(define-documentation-extractor (push obj place)
  (declare (ignore obj place))
  nil
  )


(define-documentation-extractor (pushnew obj place &rest keys)
  (declare (ignore obj place keys))
  nil
  )


;;; DEFSETF

(define-documentation-extractor (defsetf access-fn &rest form)
  (if (symbolp (first form))
      (let ((doc (second form)))
        (make-setf-expander-doc-bit :name access-fn
				    :kind 'setf
				    :doc-string doc))
      (let ((doc (extricate-doc-string (nthcdr 2 form)))
            (ll (first form))
            )
        (make-setf-expander-doc-bit :name access-fn
				    :kind 'setf
				    :lambda-list ll
				    :doc-string doc))))


;;; DEFINE-MODIFY-MACRO

(define-documentation-extractor (define-modify-macro name ll function
                                  &optional doc-string)
  (declare (ignore ll function))
  (make-doc-bit :name name
                :kind 'function
                :doc-string doc-string))


;;; DEFINE-SYMBOL-MACRO

(define-documentation-extractor (define-symbol-macro name expansion
                                  &optional doc-string)
  (make-symbol-macro-doc-bit :name name
                             :kind 'symbol-macro
                             :expansion expansion
                             :doc-string doc-string))


;;; DEFINE-METHOD-COMBINATION

(define-documentation-extractor (define-method-combination name &rest rest-dmc)
  (let ((doc (if (keywordp (first rest-dmc)) ; Short form.
                 (getf rest-dmc :documentation)
                 (let ((options-and-forms (copy-list (nthcdr 2 rest-dmc)))) ; Paranoid!
                   (when (eq :arguments (first options-and-forms))
                     (pop options-and-forms))
                   (when (eq :generic-function (first options-and-forms))
                     (pop options-and-forms))
                   (extricate-doc-string options-and-forms))))
        )
    (make-doc-bit :name name
                  :kind 'method-combination
                  :doc-string doc)))


;;; MK-DEFSYSTEM

#+mk-defsystem
(define-documentation-extractor (mk:defsystem name &rest rest-system)
  ;; We must ensure that a system always has a doc string.
  (make-mk-system-doc-bit :name name
                          :kind 'mk::system
                          :depends-on (getf rest-system :depends-on)
                          :doc-string (or (second
                                           (member :documentation rest-system
                                                   :test #'eq))
                                          (format nil "The ~A System." name))
                                          ))


;;; ASDF

#+asdf
(define-documentation-extractor (asdf:defsystem name &rest rest-system)
  (make-asdf-system-doc-bit :name name
                            :kind 'asdf:system
                            :depends-on (getf rest-system :depends-on)
                            :doc-string (or (second
                                             (member :description rest-system
                                                     :test #'eq))
                                            (format nil "The ~A System." name))
                            ))


;;; LW:DEFSYSTEM

#+lispworks
(define-documentation-extractor (lw:defsystem name options &rest keys)
  (declare (ignore keys))
  (make-lw-system-doc-bit :name name
                          :kind 'scm:scm-system
                          :doc-string (getf options :documentation "")))


;;; More kitchen sink.
;;; ------------------
;;; The method below is a catch all that fixes all problems with NIL
;;; being generated by "empty" extractors.

(defmethod extract-form-documentation :around ((fk symbol) form)
  ;; (declare (ignorable form))
  (let ((r (call-next-method fk form)))
    (etypecase r
      (doc-bit r)
      (null nil)
      (list (delete nil r)))))


;;; HELambdaP special extractors.
;;; -----------------------------
;;; HELambdaP defines a few macros (like...
;;; DEFINE-DOCUMENTATION-EXTRACTOR)  of the form DEF*.
;;; They do not carry doc strings (maybe they should) and are used
;;; only internally.  The following extractors simply ignore them.

(define-documentation-extractor (define-documentation-extractor
                                    name &body forms)
  (declare (ignore name forms))
  (warnmsg *hlp-dbg-warn* "HLP EDF: "
           "Operator DEFINE-DOCUMENTATION-EXTRACTOR not handled.")
           
  nil)


(define-documentation-extractor (define-doc-format
                                    name tag key
                                    &key
                                    derives-from
                                    documentation)
  ;; Come back later for this.
  (declare (ignore name tag key
                   derives-from
                   documentation))
  (warnmsg *hlp-dbg-warn* "HLP EDF: "
           "Operator DEFINE-DOC-FORMAT not handled.")
           
  nil)


(define-documentation-extractor (def-doc-element-class
                                 name
                                 superclasses
                                 pattern
                                 &optional slots
                                 &rest options)
  (declare (ignore name
                   superclasses
                   pattern
                   slots
                   options))
  (warnmsg *hlp-dbg-warn* "HLP EDF: "
           "Operator DEF-DOC-ELEMENT-CLASS not handled.")
  nil)


(define-documentation-extractor (def-doc-bit
                                    name
                                    include
                                    tag
                                  &body
                                  slots)
  (declare (ignore name include tag slots))
  (warnmsg *hlp-dbg-warn* "HLP EDF: "
           "Operator DEF-DOC-BIT not handled.")
  nil)


;;;;===========================================================================
;;;; Utilities.

(defun flatten-if (p l)
  (cond ((null l) nil)
        ((funcall p (first l))
         (cons (flatten-if p (first l))
               (flatten-if p (rest l))))
        (t
         (cons (first l)
               (flatten-if p (rest l))))
        ))


;;;; end of file -- extract-doc.lisp --

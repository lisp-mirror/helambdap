;;;; -*- Mode: Lisp -*-

;;;; doc-elements.lisp --

(in-package "HELAMBDAP")


;;;;---------------------------------------------------------------------------
;;;; "Document" doc elements; patterned after DocBook.

;;; Note.
;;; It may be worth to bite the bullet ad start defining a DTD-like or a
;;; XSD-like set of macros, but WTH!
;;;

;;; doc-element --

(defclass doc-element ()
  ((content :initarg :content
            :reader doc-element-content
            )
   (pattern :reader doc-element-pattern
            :allocation :class)
   )
  (:default-initargs :content ())
  )

(defgeneric is-doc-element (x)
  (:method ((x doc-element)) t)
  (:method ((x t)) nil))


(defclass component-mixin ()
  ((part-of :initarg :part-of :accessor component-part-of))
  (:default-initargs :part-of nil))

(defgeneric is-component (x)
  (:method ((x component-mixin)) t)
  (:method ((x t)) nil))


(defclass navigational-component-mixin (component-mixin) ())

(defgeneric is-navigational-component (x)
  (:method ((x navigational-component-mixin)) t)
  (:method ((x t)) nil))


(defclass division-mixin (component-mixin) ())

(defgeneric is-division (x)
  (:method ((x division-mixin)) t)
  (:method ((x t)) nil))


(defclass sectioning-mixin (component-mixin) ())

(defgeneric is-sectioning (x)
  (:method ((x sectioning-mixin)) t)
  (:method ((x t)) nil))


(defclass inline-mixin (component-mixin) ())

(defgeneric is-inline (x)
  (:method ((x inline-mixin)) t)
  (:method ((x t)) nil))


(defclass block-mixin (component-mixin) ())

(defgeneric is-block (x)
  (:method ((x block-mixin)) t)
  (:method ((x t)) nil))



(defmacro def-doc-element-class (name
                                 superclasses
                                 pattern
                                 &optional slots
                                 &rest options) ; Fanculo fascist SBCL! This is RIGHT.
  `(progn
     (defclass ,name ,superclasses
       ,(cons `(pattern :allocation :class
                        :initform ',pattern)
              slots)
       ,@options)

     (defgeneric ,(intern (format nil "~A-~A" 'is name) (symbol-package name))
                (x)
       (:method ((x ,name)) t)
       (:method ((x t)) nil))

     (find-class ',name nil))
  )


#+lispworks
(editor:setup-indent "def-doc-element-class" 2 4 4) ; Just for indentation!


;;; We'll see later how to "interpret a pattern" and have appropriate
;;; functions defined. Of course I could use CL-UNIFICATION, or maybe
;;; not :)


(deftype pcdata () 'string)


;;; Set.

(shadow '(cl:set))

(def-doc-element-class set (doc-element)
    (seq (opt (seq title
                   (opt subtitle)
                   (opt titleabbrev)))
         (opt setinfo)
         (opt toc)
         (+ (alt set book))
         (opt setindex))
    )


(def-doc-element-class book (doc-element)
    (seq (opt (seq title
                   (opt subtitle)
                   (opt titleabbrev)))
         (opt bookinfo)
         (* (alt dedication
                 ;; toc
                 ;; lot
                 ;; glossary
                 ;; bibliography
                 ;; preface
                 chapter
                 ;; reference
                 part
                 article
                 ;; appendix
                 index
                 ;; setindex
                 ;; colophon
                 ))))


(def-doc-element-class title (doc-element)
    (* (alt pcdata #| all the rest here |# )))

(def-doc-element-class subtitle (doc-element)
    (* (alt pcdata #| all the rest here |# )))

(def-doc-element-class titleabbrev (doc-element)
    (* (alt pcdata #| all the rest here |# )))


(def-doc-element-class part (doc-element division-mixin)
    (seq (opt beginpage)
         (opt partinfo)
         (seq title (opt subtitle) (opt titleabbrev))
         (opt partintro)
         (+ (alt appendix chapter article #| all the rest here |# ))))


(def-doc-element-class chapter (doc-element component-mixin)
    (seq (opt beginpage)
         (opt chapterinfo)
         (seq title (opt subtitle) (opt titleabbrev))
         (* (alt toc lot index glossary bibliography))
         (opt tocchap)
         (alt (seq (+ #| stuff |#)
               
                   (alt (* sect1)
                        (* refentry)
                        (* simplesect)
                        (* section))
                   )
              (alt (+ sect1)
                   (+ refentry)
                   (+ simplesect)
                   (+ section))
              )
         (* (alt toc lot index glossary bibliography))
         ))


(def-doc-element-class article (doc-element component-mixin)
    (seq (opt beginpage)
         (opt chapterinfo)
         (seq title (opt subtitle) (opt titleabbrev))
         (* (alt toc lot index glossary bibliography))
         (opt tocchap)
         (alt (seq (+ #| stuff |#)
               
                   (alt (* sect1)
                        (* refentry)
                        (* simplesect)
                        (* section))
                   )
              (alt (+ sect1)
                   (+ refentry)
                   (+ simplesect)
                   (+ section))
              )
         (* (alt toc lot index glossary bibliography))
         ))


(def-doc-element-class sect1 (doc-element section-mixin)
    (seq (opt sect1info)
         (seq title (opt subtitle) (opt titleabbrev))
         (* (alt toc lot index glossary bibliography))
         (alt (seq (+ (alt para simpara #| stuff |#))
                   (alt (* refentry)
                        (* sect2)
                        (* simplesect)))
              (alt (+ refentry)
                   (+ sect2)
                   (+ simplesect)
                   ))
         (* (alt toc lot index glossary bibliography))
         ))

(def-doc-element-class sect2 (doc-element section-mixin)
    (seq (opt sect2info)
         (seq title (opt subtitle) (opt titleabbrev))
         (* (alt toc lot index glossary bibliography))
         (alt (seq (+ (alt para simpara #| stuff |#))
                   (alt (* refentry)
                        (* sect3)
                        (* simplesect)))
              (alt (+ refentry)
                   (+ sect3)
                   (+ simplesect)
                   ))
         (* (alt toc lot index glossary bibliography))
         ))


(def-doc-element-class sect3 (doc-element section-mixin)
    (seq (opt sect3info)
         (seq title (opt subtitle) (opt titleabbrev))
         (* (alt toc lot index glossary bibliography))
         (alt (seq (+ (alt para simpara #| stuff |#))
                   (alt (* refentry)
                        (* sect4)
                        (* simplesect)))
              (alt (+ refentry)
                   (+ sect4)
                   (+ simplesect)
                   ))
         (* (alt toc lot index glossary bibliography))
         ))


(def-doc-element-class sect4 (doc-element section-mixin)
    (seq (opt sect4info)
         (seq title (opt subtitle) (opt titleabbrev))
         (* (alt toc lot index glossary bibliography))
         (alt (seq (+ (alt para simpara #| stuff |#))
                   (alt (* refentry)
                        (* sect5)
                        (* simplesect)))
              (alt (+ refentry)
                   (+ sect5)
                   (+ simplesect)
                   ))
         (* (alt toc lot index glossary bibliography))
         ))


(def-doc-element-class sect5 (doc-element section-mixin)
    (seq (opt sect5info)
         (seq title (opt subtitle) (opt titleabbrev))
         (* (alt toc lot index glossary bibliography))
         (alt (seq (+ (alt para simpara #| stuff |#))
                   (alt (* refentry)
                        (* simplesect)))
              (alt (+ refentry)
                   (+ simplesect)
                   ))
         (* (alt toc lot index glossary bibliography))
         ))


(def-doc-element-class section (doc-element section-mixin)
    (seq (opt sectioninfo)
         (seq title (opt subtitle) (opt titleabbrev))
         (* (alt toc lot index glossary bibliography))
         (alt (seq (+ (alt para simpara #| stuff |#))
                   (alt (* refentry)
                        (* section)
                        (* simplesect)))
              (alt (+ refentry)
                   (+ section)
                   (+ simplesect)
                   ))
         (* (alt toc lot index glossary bibliography))
         ))


(def-doc-element-class simplesect (doc-element section-mixin)
    (seq (seq title (opt subtitle) (opt titleabbrev))
         (+ (alt para simpara #| stuff |#))))



;;; Blocks

(def-doc-element-class para (doc-element block-mixin)
    (* (alt pcdata #| stuff |#)))

;;; end of file -- doc-elements.lisp --

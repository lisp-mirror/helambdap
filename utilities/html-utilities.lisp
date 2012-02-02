;;;; -*- Mode: Lisp -*-

;;;; html-utilities.lisp --
;;;; A few HTML utilities.
;;;;
;;;; Mostly simple parsing: I want to see if I can get away without
;;;; depending on a full blown XML/(X)HTML parser.
;;;;
;;;; See file COPYING for copyright and licensing information.

(in-package "HELAMBDAP")

(defun is-id-char (c)
  (or (alphanumericp c)
      (char= c #\-)
      (char= c #\:)))

(defun is-delimiter-char (c)
  (find c "<>&;=" :test #'char=))


(defparameter *token-collector*
  (make-array 80
              :fill-pointer 0
              :adjustable t
              :element-type 'character))


(defvar *eof* (gensym "EOF"))

(defun is-eof (x)
  (eq x *eof*))


(defun collect-id (html-stream)
  (let ((id *token-collector*))
    (setf (fill-pointer id) 0)
    (labels ((process-char (c)
               (cond ((is-eof c)
                      (copy-seq id))
                     ((is-id-char c)
                      (vector-push-extend c id)
                      (process-char (read-char html-stream nil *eof*)))
                     (t
                      (unread-char c html-stream)
                      (copy-seq id))))
             )
      (process-char (read-char html-stream nil *eof*)))))


(defun collect-string (html-stream &optional (string-delim #\"))
  (let ((id *token-collector*))
    (setf (fill-pointer id) 0)
    (labels ((process-char (c)
               (cond ((is-eof c)
                      (error 'end-of-file :stream html-stream))
                     ((char= c #\\)
                      (vector-push-extend c id)
                      (vector-push-extend (read-char html-stream nil *eof*) id)
                      (process-char (read-char html-stream nil *eof*)))
                     ((char= c string-delim)
                      (copy-seq id))
                     (t (vector-push-extend c id)
                        (process-char (read-char html-stream nil *eof*)))
                     ))

             (start ()
               (let ((quote-char (read-char html-stream nil *eof*)))
                 (if (char= quote-char string-delim)
                     (process-char (read-char html-stream nil *eof*))
                     (error "String parsing found a ~S instad of a ~C at the beginning."
                            quote-char
                            string-delim))))
             )
      (start))))



(defun collect-delim (html-stream)
  (let ((c (read-char html-stream nil *eof*)))
    (if (eq c *eof*)
        (error 'end-of-file :stream html-stream)
        (case c
          ((#\< #\> #\& #\; #\=) c)
          (t (unread-char c html-stream))))))



#|
(defun skip-whitespace (html-stream)
  (peek-char t html-stream)
  )
|#

(defun skip-whitespace (html-stream &optional (eof-error-p t) eof recursive)
  (look-at-char t html-stream eof-error-p eof recursive)
  )



(defun collect-attributes (html-stream &aux (attributes ()))
  (labels ((collect-attribute-val-pair ()
             (let* ((attr (progn
                            (skip-whitespace html-stream)
                            (collect-id html-stream)))
                    (equal-sign (progn
                                  (skip-whitespace html-stream)
                                  (collect-delim html-stream)))
                    )
               (if (char= #\= equal-sign)
                   (let ((val (progn
                                (skip-whitespace html-stream)
                                (collect-value html-stream))))
                     (push (cons attr val) attributes))
                   (push (list attr) attributes))
               
               (collect-more-attributes html-stream))
             )

           (collect-value (html-stream)
             (let ((att-val-quote (read-char html-stream nil *eof*)))
               (case att-val-quote
                 ((#\' #\")
                  (unread-char att-val-quote html-stream)
                  (collect-string html-stream att-val-quote))
                 (t (error "Incorrect quoting of value; first attribute character ~C."
                           att-val-quote))
                 )))

           (collect-more-attributes (html-stream)
             (let ((next-char (skip-whitespace html-stream)))
               (case next-char
                 (#\> (read-char html-stream nil *eof*)
                      (nreverse attributes))
                 (#\/ (read-char html-stream nil *eof*)
                      (let ((c (read-char html-stream nil *eof*)))
                        (if (eql c #\>) ; Not CHAR= as C could be *EOF*.
                            (nreverse attributes)
                            (error "Read ~C after a #\\/." c))))
                 (t (unread-char next-char html-stream)
                    (collect-attribute-val-pair)))
               ))
           )
    (collect-more-attributes html-stream)))



(defun parse-xml-like-data (xml-stream)
  (let ((prolog nil)
        (root nil)
        (misc nil)
        (document nil)
        )
    (labels ((start ()
               (skip-whitespace xml-stream)
               (setf prolog (prolog)
                     root (root)
                     misc (misc*))
               (setf document (list prolog root misc)))
           
             (prolog ()
               (parse-prolog xml-stream))

             (root ()
               (parse-root xml-stream))

             (misc* ()
               (parse-misc* xml-stream))
             )
      (start))))


(defun parse-prolog (xml-stream)
  (labels ((start ()
             (skip-whitespace xml-stream)
             (xml-pi)
             (misc*)
             (doctypedecl)
             )
           
           (xml-pi ()
             (let ((proc-instr (parse-pi xml-stream)))
               proc-instr))

           (misc* ()
             (parse-misc* xml-stream))

           (doctypedecl ()
             (parse-doctypedecl xml-stream))
           )
    (start)))


(defun read-char= (=c= &optional (xml-stream *standard-input*) (errorp t) (eof-error-p t) eof recursive)
  (let ((c (get-char xml-stream eof-error-p eof recursive))) ; Note the GET-CHAR.
    (cond ((and errorp (eq c eof)) ; ... and (NOT EOF-ERROR-P)
           nil)
          ((and errorp (char/= =c= c))
           (error "Parse error: seen ~C, expected ~C." c =c=))
          ((char/= =c= c)
           (unget-char c xml-stream)
           nil)
          (t c))))


(defun read-chars= (=s= &optional (xml-stream *standard-input*) (eof-error-p t) eof recursive)
  (declare (type string =s=))
  (loop for c across =s=
        for rc = (read-char= c xml-stream nil eof-error-p eof recursive)
        if rc
        collect rc into rcs
        else
        do (progn (unget-chars rcs xml-stream)
             (return-from read-chars= nil)))
  =s=)




(defun parse-pi (xml-stream)
  (let ((pi-target "")
        (pi-content *token-collector*)
        )
    (skip-whitespace xml-stream)
    (labels ((start ()
               (read-char= #\< xml-stream t)
               (read-char= #\? xml-stream t)
               (setf pi-target (collect-id xml-stream))
               (skip-whitespace xml-stream)
               (pi-rest))

             (pi-rest ()
               (setf (fill-pointer pi-content) 0)
               (loop for c = (read-char xml-stream)
                     if (char= c #\?)
                     do (let ((nc (peek-char nil xml-stream)))
                          (cond ((char= nc #\>)
                                 (read-char xml-stream)
                                 (loop-finish))
                                (t
                                 (vector-push-extend c pi-content))))
                     else
                     do (vector-push-extend c pi-content)
                     )
               (cons pi-target (copy-seq pi-content)))
             )
      (start))))


(defun parse-misc* (xml-stream)
  (let ((misc ()))
    (loop
     (skip-whitespace xml-stream nil)
     (if (read-char= #\< xml-stream t nil)
         (let ((c (get-char xml-stream)))
           (case c
             (#\? (unget-chars "<?" xml-stream)
                  (push (parse-pi xml-stream) misc))
             (#\! (let ((nc (look-at-char nil xml-stream)))
                    (case nc
                      (#\-
                       (unget-chars "<!" xml-stream)
                       (push (parse-comment xml-stream) misc))
                      ((#\D #\d)
                       (unget-chars (list #\< nc) xml-stream)
                       (push (parse-doctypedecl xml-stream) misc))
                      (t
                       (error "Seen '<!~C', expected either a comment or a doctype declaration."
                              nc)))))
             (t (unget-chars (list #\< c) xml-stream)
                (return-from parse-misc* (nreverse misc)))
             ))
         (return-from parse-misc* (nreverse misc))))))


(defun parse-comment (xml-stream)
  ;; Very hairy function...  careful with the state of the backlogged stream.
  (let ((comment *token-collector*))
    (setf (fill-pointer comment) 0)
    (labels ((start ()
               (skip-whitespace xml-stream)
               (when (read-chars= "<!--" xml-stream)
                 (collect-chars)
                 (cons 'comment (copy-seq comment))))

             (collect-chars ()
               (let ((c (get-char xml-stream)))
                 (if (char= c #\-)
                     (cond ((read-chars= "->" xml-stream)
                            (return-from collect-chars))
                           ((read-char= #\- xml-stream nil)
                            (get-char xml-stream) ; Consume the second
                                                  ; character read in
                                                  ; the previous clause.
                            (error "String '--' found in comment."))
                           (t (vector-push-extend c comment)
                              (vector-push-extend (get-char xml-stream) comment)))
                     (vector-push-extend c comment)))
               (collect-chars))
             )
      (start))))
      
                      
(defun parse-doctype (xml-stream)
  (skip-whitespace xml-stream nil)
  (read-chars= "<!DOCTYPE" xml-stream)
  (skip-whitespace xml-stream)
  (let ((id nil)
        (external-id nil)
        (int-subset nil)
        )
    (labels ((start ()
               (id)
               (rest-doctypedecl)
               (finish))

             (finish ()
               (list 'doctype id external-id int-subset))
             
             (id ()
               (setf id (collect-id xml-stream)))

             (rest-doctypedecl ()
               (skip-whitespace xml-stream)
               (cond ((read-char= #\[ xml-stream nil)
                      (unread-char #\[ xml-stream)
                      (int-subset)
                      (skip-whitespace xml-stream)
                      (read-char= #\> xml-stream))
                     ((read-char= #\> xml-stream nil))
                     (t
                      (external-id)
                      (int-subset)
                      (skip-whitespace xml-stream)
                      (read-char= #\> xml-stream))
                     ))

             (external-id ()
               (cond ((read-chars= "SYSTEM" xml-stream)
                      (skip-whitespace xml-stream)
                      (system-literal))
                     ((read-chars= "PUBLIC" xml-stream)
                      (skip-whitespace xml-stream)
                      (pubid-literal)
                      (skip-whitespace xml-stream)
                      (system-literal))))

             (system-literal ()
               (cond ((look-at-char #\" xml-stream)
                      (collect-string xml-stream #\"))
                     ((look-at-char #\' xml-stream)
                      (collect-string xml-stream #\'))
                     (t
                      (error "Expected a #\' or a \"; seen a ~C."
                             (get-char xml-stream)))))

             (pubid-literal ()
               (cond ((look-at-char #\" xml-stream)
                      (collect-string xml-stream #\"))
                     ((look-at-char #\' xml-stream)
                      (collect-string xml-stream #\'))
                     (t
                      (error "Expected a #\' or a \"; seen a ~C."
                             (get-char xml-stream)))))

             (int-subset ()
               (read-char #\[ xml-stream)
               (rest-int-subset))

             (rest-int-subset ()
               (skip-whitespace xml-stream)
               (let ((nc (get-char xml-stream)))
                 (cond ((char= nc #\%)
                        (collect-id xml-stream)
                        (rest-int-subset))
                       ((char= nc #\<)
                        (cond ((read-chars= "!ELEMENT" xml-stream)
                               (element-decl))
                              ((read-chars= "!ATTLIST" xml-stream)
                               (attlist-decl))
                              ((read-chars= "!ENTITY" xml-stream)
                               (entity-decl))
                              ((read-chars= "!NOTATION" xml-stream)
                               (notation-decl))
                              ((read-chars= "?" xml-stream)
                               (unget-chars "<?")
                               (parse-pi xml-stream))
                              ((read-chars= "!--" xml-stream)
                               (unget-chars "<!--" xml-stream)
                               (parse-comment xml-stream))
                              (t (error "Expected one of #\! or #\?, seen ~C."
                                        (get-char xml-stream))))
                        (rest-int-subset))
                       ((char= nc #\])
                        ))))

             (element-decl ()
               (skip-whitespace xml-stream)
               (collect-id xml-stream)
               (skip-whitespace xml-stream)
               (content-spec)
               (skip-whitespace xml-stream)
               (read-char= #\> xml-stream))

             )
      (start))))


(defun collect-html-a-links (html-stream)
  (let ((html-a-elements ()))
    (labels ((process-char (c)
               (if (eql c *eof*)
                   ;; (error 'end-of-file :stream html-stream)
                   (nreverse html-a-elements)
                   (case c
                     (#\< (tag-open))
                     (t (process-char (read-char html-stream nil *eof*))))))

             (start ()
               (skip-whitespace html-stream)
               (process-char (read-char html-stream nil *eof*)))

             (tag-open ()
               (let* ((tag (collect-tag))
                      (attributes (collect-attributes html-stream))
                      )
                 (when (string-equal "A" tag)
                   (push attributes html-a-elements))
                 (process-char (read-char html-stream nil *eof*))))

             (collect-tag ()
               (collect-id html-stream))
             )
      (start))))


;;;; end of file -- html-utilities.lisp --

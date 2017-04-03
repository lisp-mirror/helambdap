;;;; -*- Mode: Lisp -*-

;;;; html-source-handling.lisp --
;;;; Miscellanous functions to "parse" "HTML" files.
;;;;
;;;; See file COPYING for copyright and licensing information.

(in-package "HELAMBDAP")


;;;;===========================================================================
;;;; Utilities.

(defgeneric extract-sections (source format)
  (:documentation "Quick and dirty 'section' finding in (X)HTML(5) source.

The EXTRACT-SECTIONS function looks at an (X)HTML(5) source looking
for top level <H1>...</H1> 'sections'.  More specifically, it looks for
sectioning markup of the form:

     <H1><A name=\"Section Name\">....</A></H1>

The \"Section Name\" is what is eventually saved in the result, which
is then used to produce a file navigation bar.
"))


(defmethod extract-sections ((p pathname) format)
  (with-open-file (in p :direction :input)
    (extract-sections in format)))


(defmethod extract-sections ((s string) format)
  (with-input-from-string (in s)
    (extract-sections in format)))


(defmethod extract-sections ((in stream) (format (eql 'html)))
  ;; Poor man's parsing of HTML's files.
  ;; I just look for <H1>...</H1> sections.
  ;;
  ;; Each H1 section is supposed to look like:
  ;;
  ;;   <H1><A name=....>....</A></H1>
  ;;
  ;; The name and the actual section title are what is saved.

  (let ((section (make-array 256 ; This should be made easier!
                             :element-type 'character
                             :fill-pointer 0
                             :adjustable t
                             :initial-element (code-char 0)))
        (sections ())
        (collecting nil)
        )
    (declare (type (vector character) section)
             (type list sections)
             (type boolean collecting))

    (labels ((start ()
               (handler-case
                   (process-char (read-char in))
                 (end-of-file (eof)
                   (declare (ignore eof))
                   (finish))))

             (maybe-collect (c)
               (when collecting
                 (vector-push-extend c section)))

             (process-char (c)
               (maybe-collect c)
               (if (char= c #\<)
                   (process-< (read-char in))
                   (process-char (read-char in))
                   ))
        
             (process-< (c)
               (cond ((char-equal c #\H)
                      (process-h (read-char in)))
                     ((char= c #\/)
                      (maybe-collect #\/)
                      (process-/ (read-char in)))
                     (t
                      (process-char c))
                     ))

             (process-h (c)
               (if (char= c #\1)
                   (process-h1 (read-char in))
                   (process-char (read-char in))))

             (process-h1 (c)
               ;; (format t "HELAMBDA: Collecting H1 Section.~%")
               (setf (fill-pointer section) 0
                     collecting t)
               (vector-push-extend #\< section)
               (vector-push-extend #\H section)
               (vector-push-extend #\1 section)
               (process-char c))

             (process-/ (c)
               (if (char-equal #\h c)
                   (process-/h (read-char in))
                   (process-char c)))
              
             (process-/h (c)
               (when (char= #\1 c)
                 (vector-push-extend #\H section)
                 (vector-push-extend #\1 section)
                 (assert (char= (read-char in) #\>))
                 (vector-push-extend #\> section)
                 (push (copy-seq section) sections)
                 
                 ;; (format t "HELAMBDA: Collected  H1 Section ~S.~%" section)
               
                 (setf collecting nil))
               (process-char (read-char in))
               )

             (finish ()
               (nreverse sections))
             )
      (start)
      )))


(defun extract-section-name (sect)
  (declare (type string sect))
  (let ((name-search (or (search "<a name=" sect)
                         (search "<A name=" sect)
                         ))
        (end-char #\')
        )
    (if name-search
        (let* ((n-start (+ name-search (length "<a name=")))
               (n-start-plus (1+ n-start))
               )
          (declare (type fixnum n-start n-start-plus))
          (cond ((char= #\' (aref sect n-start))
                 (setf end-char #\'))
                ((char= #\" (aref sect n-start))
                 (setf end-char #\"))
                )
          (subseq sect
                  n-start-plus
                  (position end-char sect :start n-start-plus)))
        "")))


(defun extract-section-names (sects)
  (delete "" (mapcar #'extract-section-name sects) :test #'equal))
        
;;;; end of file -- doc-string-handling.lisp --

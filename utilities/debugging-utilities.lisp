;;;; -*- Mode: Lisp -*-

(in-package "HELAMBDAP")

;;;; This will evolve in the tool "Never Underestimate The Debugging
;;;; Power of Print" (NUTDPP).
;;;; For the time being, the facility is bare-bone and probably not so
;;;; well thought out.
;;;;
;;;; Always, for the time being this is used only within HELambdaP and
;;;; only with a very basic use of DEBUGMSG below.
;;;; Just to setup some basic functionality in the file setup (in the
;;;; main HELambdaP folder) I call:
;;;;
;;;;    (start-debug-logging :prefix "HLP: ")
;;;;
;;;; Then I use DEBUGMSG as needed.

(define-symbol-macro *debug-output* *debug-io*) ; It has to be so.

; (defvar *debug-output* *error-output*) ; For the time being; error
                                       ; otherwise because of variable
                                       ; redefinition as a symbol-macro.


;;; *debug-tag*
;;; Controls when and how to print debugging information.
;;; It can be a number of a SEXP (tested by EQUAL in any case).
;;; If a number it must be =< 42.
;;; A non positive number means "no printing".

(defparameter +max-debug-tag+ 42) ; All debugging messages are
                                  ; printed. It's a defparameter to
                                  ; appease SBCL.

(defparameter *current-debug-tag* +max-debug-tag+)


(defparameter *current-prefix* "NUTDPP: ")


(defvar *registered-debug-tags* ())


(defun add-debug-tag (tag)
  (pushnew tag *registered-debug-tags*
           :test #'equal)
  tag)


(defun delete-debug-tag (tag)
  (setq *registered-debug-tags*
        (delete tag *registered-debug-tags*
                :test #'equal))
  tag)


(defstruct debug-log-context
  (prefix *current-prefix* :type string)
  (current-tag *current-debug-tag*)
  (output *debug-output* :type stream))


(defstruct debug-log-channel
  (name :all :type symbol)
  (stack () :type list)
  (stream *debug-output* :type stream))


(defvar *debug-log-channels* (make-hash-table))


(defun open-log-channel (name)
  (setf (gethash name *debug-log-channels*)
        (make-debug-log-channel)))


(defun close-log-channel (name)
  (remhash name *debug-log-channels*))


(defun debugmsg (tag prefix format-control &rest format-arguments)
  "Prints/logs a message for debugging on *DEBUG-OUTPUT*.

TAG and PREFIX can be both T to simplify DEBUGMSG usage; in that case
defaults are used.

PREFIX, if not T, should be a STRING to be used as a prefix to the
message.
"
  (when (and tag
             (or (and (numberp tag)
                      (numberp *current-debug-tag*)
                      (plusp tag)
                      (or (<= +max-debug-tag+ tag)
                          (<= tag *current-debug-tag*)))
                 (eq tag t)
                 (equal tag *current-debug-tag*)
                 (find tag *registered-debug-tags* :test #'equal)))

    (fresh-line *debug-output*)
    (cond ((eq prefix t)
           (write-string *current-prefix* *debug-output*))
          ((stringp prefix)
           (write-string prefix *debug-output*))
          ((listp prefix)
           (format *debug-output* "~A: " prefix))
          (t ; Presumably NIL.
           (write-string "NUTPPD: " *debug-output*)))

    (apply #'format
           *debug-output*
           format-control
           format-arguments)

    (finish-output *debug-output*)
    ))


(defun warnmsg (tag prefix format-control &rest format-arguments)
  "Issues a warning under certain conditions.

TAG and PREFIX can be both T to simplify WARNMSG usage; in that case
defaults are used.

PREFIX, if not T, should be a STRING to be used as a prefix to the
message.
"
  (when (and tag
             (or (and (numberp tag)
                      (numberp *current-debug-tag*)
                      (plusp tag)
                      (or (<= +max-debug-tag+ tag)
                          (<= tag *current-debug-tag*)))
                 (eq tag t)
                 (equal tag *current-debug-tag*)
                 (find tag *registered-debug-tags* :test #'equal)))

    (warn (with-output-to-string (warn-msg)
            (cond ((eq prefix t)
                   (write-string *current-prefix* warn-msg))
                  ((stringp prefix)
                   (write-string prefix warn-msg))
                  ((listp prefix)
                   (format warn-msg "~A: " prefix))
                  (t ; Presumably NIL.
                   (write-string "NUTPPD: " warn-msg)))

            (apply #'format
                   warn-msg
                   format-control
                   format-arguments)
            (finish-output warn-msg)
            )))
  )


(defmacro with-debug-settings ((&key (prefix *current-prefix*)
                                     (tag *current-debug-tag*)
                                     (output *debug-output*)
                                     )
                               &body forms)
  `(let ((*current-prefix* ,prefix)
         (*current-debug-tag* ,tag)
         (*debug-output* ,output)
         )
     ,@forms))


(defmacro with-debug-context (dbg-ctx &body forms)
  `(when ,dbg-ctx
     (let ((*current-prefix* ,(debug-log-context-prefix dbg-ctx))
           (*current-debug-tag* ,(debug-log-context-current-tag dbg-ctx))
           (*debug-output* (debug-log-context-output dbg-ctx))
           )
       ,@forms)))


(defparameter *debug-log-context* (make-debug-log-context))

(defparameter *%%debug-logging-stack%%* ; DO NOT MESS WITH THIS!
  (list *debug-log-context*))


(defun start-debug-logging (&key (prefix *current-prefix*)
                                 (tag *current-debug-tag*)
                                 (output *debug-output*)
                                 (channel :all)
                                 )
  (declare (ignore channel)) ; Later.
  (let ((ndc (make-debug-log-context :prefix prefix
                                     :current-tag tag
                                     :output output)))
    (push ndc *%%debug-logging-stack%%*)
    (setf *current-prefix* prefix
          *current-debug-tag* tag
          *debug-output* output)
    ndc
    ))


(defun end-debugging-logging (&key (channel :all))
  (declare (ignore channel)) ; Later.
  (pop *%%debug-logging-stack%%*)
  (let ((dlc (first *%%debug-logging-stack%%*)))
    (when dlc ; It should never be NIL, but...
      (setf *current-prefix* (debug-log-context-prefix dlc)
            *current-debug-tag* (debug-log-context-current-tag dlc)
            *debug-output* (debug-log-context-output dlc)))
    dlc))

;;;; end of file -- debugging-utilities.lisp

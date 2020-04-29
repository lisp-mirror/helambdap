;;;; -*- Mode: Lisp -*-

(in-package "HELAMBDAP")

;;;; This will evolve in the tool "Never Underestimate The Debugging
;;;; Power of Print" (NUTDPP).

; (define-symbol-macro *debug-output* *debug-io*) ; It has to be so.

(defvar *debug-output* *error-output*) ; For the time being; error
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
  (stack () :type list))


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
                          (<= *current-debug-tag* tag)))
                 (eq tag t)
                 (equal tag *current-debug-tag*)
                 (find tag *registered-debug-tags* :test #'equal)))

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

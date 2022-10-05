;;; Copyright (c) 2022 Max-Gerd Retzlaff <mgr@matroid.org>, Datagraph GmbH.
;;; Distributed under the terms of the GNU General Public License, Version 2.0,
;;; see file LICENSE in the top level directory of this repository.

(cl:in-package :libndbapi)

(cl:defvar *ndbapi-verbose* cl:t)

(cl:defun debug (object foreign-pointer is-null-pinter cl:&optional (verbose *ndbapi-verbose*))
  (cl:when verbose
    (cl:format cl:*trace-output* "~&Destructor called for ~a object: ~8,'0x (~:[do free~;do NOT free~])"
               object
               foreign-pointer
               is-null-pinter)))


(cffi:define-foreign-type ndb-type ()
  ((garbage-collect :reader garbage-collect :initform cl:nil :initarg :garbage-collect))
  (:actual-type :pointer)
  (:simple-parser ndb-type))

(cl:defclass ndb ()
  ((foreign-pointer :reader foreign-pointer :initarg :foreign-pointer)))

(cl:defmethod cffi:translate-to-foreign ((lisp-object ndb) (foreign-type ndb-type))
  (foreign-pointer lisp-object))

(cl:defmethod cffi:translate-from-foreign (foreign-pointer (foreign-type ndb-type))
  (cl:let ((lisp-object (cl:make-instance 'ndb :foreign-pointer foreign-pointer)))
    (cl:when (garbage-collect foreign-type)
      (sb-ext:finalize lisp-object (cl:lambda ()
                                     (cl:let ((is-null-pointer (cffi:null-pointer-p foreign-pointer)))
                                       (debug 'ndb foreign-pointer is-null-pointer)
                                       (cl:unless is-null-pointer
                                         (delete-ndb foreign-pointer)
                                         ;; to be extra careful:
                                         (cl:setf foreign-pointer (cffi:null-pointer)))))))
    lisp-object))

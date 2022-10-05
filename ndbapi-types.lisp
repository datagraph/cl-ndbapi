;;; Copyright (c) 2022 Max-Gerd Retzlaff <mgr@matroid.org>, Datagraph GmbH.
;;; Distributed under the terms of the GNU General Public License, Version 2.0,
;;; see file LICENSE in the top level directory of this repository.

(cl:in-package :libndbapi)

(cl:defvar *ndbapi-verbose* cl:t)

(cl:defun debug (object foreign-pointer cl:&optional (verbose *ndbapi-verbose*))
  (cl:when verbose
    (cl:format cl:*trace-output* "~&Destructor called for ~a object: ~8,'0x" object foreign-pointer)))


(cffi:define-foreign-type ndb-type ()
  ((garbage-collect :reader garbage-collect :initform cl:nil :initarg :garbage-collect))
  (:actual-type :pointer)
  (:simple-parser %make-ndb-type))

(cl:defclass ndb ()
  ((foreign-pointer :reader foreign-pointer :initarg :foreign-pointer)))

(cl:defmethod cffi:translate-to-foreign ((lisp-object ndb) (foreign-type ndb-type))
  (foreign-pointer lisp-object))

(cl:defmethod cffi:translate-from-foreign (foreign-pointer (foreign-type ndb-type))
  (cl:let ((lisp-object (cl:make-instance 'ndb :foreign-pointer foreign-pointer)))
    (cl:when (garbage-collect foreign-type)
      (sb-ext:finalize lisp-object (cl:lambda ()
                              (debug 'ndb foreign-pointer)
                              (delete-ndb foreign-pointer))))
    lisp-object))

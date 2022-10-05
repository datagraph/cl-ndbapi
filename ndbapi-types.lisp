;;; Copyright (c) 2022 Max-Gerd Retzlaff <mgr@matroid.org>, Datagraph GmbH.
;;; Distributed under the terms of the GNU General Public License, Version 2.0,
;;; see file LICENSE in the top level directory of this repository.

(cl:in-package :libndbapi)

(cl:defvar *ndbapi-verbose* cl:t)

(cffi:define-foreign-type ndb-type ()
  ((garbage-collect :reader garbage-collect :initform cl:nil :initarg :garbage-collect))
  (:actual-type :pointer)
  (:simple-parser %make-ndb-type))

(cl:defclass ndb ()
  ((foreign-pointer :reader foreign-pointer :initarg :foreign-pointer)))

(cl:defmethod cffi:translate-to-foreign ((lisp-object ndb) (foreign-type ndb-type))
  (foreign-pointer lisp-object))

(cl:defmethod cffi:translate-from-foreign (foreign-pointer (foreign-type ndb-type))
  (cl:let ((wrap  (cl:make-instance 'ndb :foreign-pointer foreign-pointer)))
    (cl:when (garbage-collect foreign-type)
      (sb-ext:finalize wrap (cl:lambda ()
                              (cl:when *ndbapi-verbose*
                                (cl:format cl:*trace-output* "~&NDB destructor called for: ~8,'0x" foreign-pointer))
                              (delete-ndb foreign-pointer))))
    wrap))

(cffi:defcfun ("_wrap_new_Ndb__SWIG_1" #.(swig-lispify "new_Ndb1_wrap" 'function)) (%make-ndb-type :garbage-collect t)
  (ndb_cluster_connection :pointer)
  (aCatalogName :string))

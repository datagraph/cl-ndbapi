;;; Copyright (c) 2022 Max-Gerd Retzlaff <mgr@matroid.org>, Datagraph GmbH.
;;; Distributed under the terms of the GNU General Public License, Version 2.0,
;;; see file LICENSE in the top level directory of this repository.

(cl:in-package :libndbapi)

(cl:defun debug-free-connection (connection)
  (cl:when *ndbapi-verbose*
    (cl:format cl:*trace-output* "~&Freing connection ~a: ~8,'0x"
               connection
               (foreign-pointer connection))
    (cl:force-output cl:*trace-output*)))

(cl:defun libndbapi::new-ndb/swig-1 (connection database-name)
  (cl:let ((ndb (libndbapi::new-ndb/swig-1% connection database-name)))
    ;; SBCL's implementation for finalize says: Multiple finalizers are invoked in the order added.
    (sb-ext:finalize ndb (cl:lambda ()
                           ;; HACK: this reference to connection keeps the instance from being GC'ed
                           (debug-free-connection connection)))
    ndb))

(cl:defun libndbapi::new-ndb/swig-0 (connection database-name schema-name)
  (cl:let ((ndb (libndbapi::new-ndb/swig-0% connection database-name schema-name)))
    ;; SBCL's implementation for finalize says: Multiple finalizers are invoked in the order added.
    (sb-ext:finalize ndb (cl:lambda ()
                           ;; HACK: this reference to connection keeps the instance from being GC'ed
                           (debug-free-connection connection)))
    ndb))

(cl:defun libndbapi::new-ndb/swig-2 (connection)
  (cl:let ((ndb (libndbapi::new-ndb/swig-2% connection)))
    ;; SBCL's implementation for finalize says: Multiple finalizers are invoked in the order added.
    (sb-ext:finalize ndb (cl:lambda ()
                           ;; HACK: this reference to connection keeps the instance from being GC'ed
                           (debug-free-connection connection)))
    ndb))

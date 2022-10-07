;;; Copyright (c) 2022 Max-Gerd Retzlaff <mgr@matroid.org>, Datagraph GmbH.
;;; Distributed under the terms of the GNU General Public License, Version 2.0,
;;; see file LICENSE in the top level directory of this repository.

(cl:in-package :libndbapi)

(cl:defun debug-free-thing (type instance)
  (cl:when *ndbapi-verbose*
    (cl:format cl:*trace-output* "~&Remove reference to ~a: ~a (~8,'0x)"
               type
               instance
               (cl:cond
                 ((cl:typep instance 'garbage-collected-class)
                  (foreign-pointer instance))
                 ((cl:typep instance 'ndb-init)
                  (cl:if (initialized instance) :initialized :uninitialized))
                 (cl:t :unknown)))
    (cl:force-output cl:*trace-output*)))

(cl:defun finalize-thing (object type instance)
  ;; SBCL's implementation for finalize says: Multiple finalizers are invoked in the order added.
  (sb-ext:finalize object (cl:lambda ()
                            ;; HACK: this reference to instance keeps the instance from being GC'ed
                            (debug-free-thing type instance))))


(cl:defun libndbapi::new-ndb/swig-1 (cluster-connection database-name)
  (cl:let ((ndb (libndbapi::new-ndb/swig-1% cluster-connection database-name)))
    (finalize-thing ndb :cluster-connection cluster-connection)
    ndb))

(cl:defun libndbapi::new-ndb/swig-0 (cluster-connection database-name schema-name)
  (cl:let ((ndb (libndbapi::new-ndb/swig-0% cluster-connection database-name schema-name)))
    (finalize-thing ndb :cluster-connection cluster-connection)
    ndb))

(cl:defun libndbapi::new-ndb/swig-2 (cluster-connection)
  (cl:let ((ndb (libndbapi::new-ndb/swig-2% cluster-connection)))
    (finalize-thing ndb :cluster-connection cluster-connection)
    ndb))


;; add ndb-init wrapper as first argument to keep reference in finalizer for the ndb-cluster-connection created
(cl:defun libndbapi::new-ndb-cluster-connection/swig-0 (ndb-init connectstring)
  (cl:let ((cluster-connection (libndbapi::new-ndb-cluster-connection/swig-0% connectstring)))
    (finalize-thing cluster-connection :ndb-init ndb-init)
    cluster-connection))

(cl:defun libndbapi::new-ndb-cluster-connection/swig-1 (ndb-init)
  (cl:let ((cluster-connection (libndbapi::new-ndb-cluster-connection/swig-1%)))
    (finalize-thing cluster-connection :ndb-init ndb-init)
    cluster-connection))

(cl:defun libndbapi::new-ndb-cluster-connection/swig-2 (ndb-init connectstring force_api_nodeid)
  (cl:let ((cluster-connection (libndbapi::new-ndb-cluster-connection/swig-2% connectstring force_api_nodeid)))
    (finalize-thing cluster-connection :ndb-init ndb-init)
    cluster-connection))

(cl:defun libndbapi::new-ndb-cluster-connection/swig-3 (ndb-init connectstring main_connection)
  (cl:let ((cluster-connection (libndbapi::new-ndb-cluster-connection/swig-3% connectstring main_connection)))
    (finalize-thing cluster-connection :ndb-init ndb-init)
    cluster-connection))

(cl:defun libndbapi::new-ndb-cluster-connection/swig-4 (ndb-init connectstring main_connection force_api_nodeid)
  (cl:let ((cluster-connection (libndbapi::new-ndb-cluster-connection/swig-4% connectstring main_connection force_api_nodeid)))
    (finalize-thing cluster-connection :ndb-init ndb-init)
    cluster-connection))

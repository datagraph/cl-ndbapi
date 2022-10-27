;;; Copyright (c) 2022 Max-Gerd Retzlaff <mgr@matroid.org>, Datagraph GmbH.
;;; Distributed under the terms of the GNU General Public License, Version 2.0,
;;; see file LICENSE in the top level directory of this repository.

(cl:in-package :ndbapi.types)

(defun debug-free-thing (type instance)
  (when *ndbapi-verbose*
    (format *trace-output* "~&Remove reference to ~a: ~a (~8,'0x)"
               type
               instance
               (cond
                 ((typep instance 'garbage-collected-class)
                  (foreign-pointer instance))
                 ((typep instance 'ndbapi.ffi::ndb-init)
                  (if (initialized instance) :initialized :uninitialized))
                 (t :unknown)))
    (force-output *trace-output*)))

(defun finalize-thing (object type instance)
  ;; SBCL's implementation for finalize says: Multiple finalizers are invoked in the order added.
  (sb-ext:finalize object (lambda ()
                            ;; HACK: this reference to instance keeps the instance from being GC'ed
                            (debug-free-thing type instance))))

(defmacro make-finalize-wrapper (name (&rest params) &key (first-param (first params) first-param-p))
  (let ((hidden-name (find-symbol (format nil "~a%" name) :ndbapi.ffi))
        (type (intern (symbol-name first-param) :keyword)))
    `(defun ,name ,(if first-param-p (cons first-param params) params)
       (let ((object (,hidden-name ,@params)))
         (finalize-thing object ,type ,first-param)
         object))))

(make-finalize-wrapper ndbapi.ffi::new-ndb/swig-0 (cluster-connection database-name schema-name))
(make-finalize-wrapper ndbapi.ffi::new-ndb/swig-1 (cluster-connection database-name))
(make-finalize-wrapper ndbapi.ffi::new-ndb/swig-2 (cluster-connection))

;; add ndb-init wrapper as first argument to keep reference in finalizer for the ndb-cluster-connection created
(make-finalize-wrapper ndbapi.ffi::new-ndb-cluster-connection/swig-0 (connect-string) :first-param ndb-init)
(make-finalize-wrapper ndbapi.ffi::new-ndb-cluster-connection/swig-1 () :first-param ndb-init)
(make-finalize-wrapper ndbapi.ffi::new-ndb-cluster-connection/swig-2 (connect-string force-api-nodeid)
                       :first-param ndb-init)
(make-finalize-wrapper ndbapi.ffi::new-ndb-cluster-connection/swig-3 (connect-string main-connection)
                       :first-param ndb-init)
(make-finalize-wrapper ndbapi.ffi::new-ndb-cluster-connection/swig-4 (connect-string main-connection force-api-nodeid)
                       :first-param ndb-init)

;; add ndb-init wrapper as first argument to keep reference in finalizer for the ndb-interpreted-code created
(make-finalize-wrapper ndbapi.ffi::new-ndb-interpreted-code/swig-0 (table buffer buffer-word-size)
                       :first-param ndb-init)
(make-finalize-wrapper ndbapi.ffi::new-ndb-interpreted-code/swig-1 (table buffer)
                       :first-param ndb-init)
(make-finalize-wrapper ndbapi.ffi::new-ndb-interpreted-code/swig-2 (table)
                       :first-param ndb-init)
(make-finalize-wrapper ndbapi.ffi::new-ndb-interpreted-code/swig-3 ()
                       :first-param ndb-init)
(make-finalize-wrapper ndbapi.ffi::new-ndb-interpreted-code/swig-4 (arg0 buffer buffer-word-size)
                       :first-param ndb-init)
(make-finalize-wrapper ndbapi.ffi::new-ndb-interpreted-code/swig-5 (arg0 buffer)
                       :first-param ndb-init)
(make-finalize-wrapper ndbapi.ffi::new-ndb-interpreted-code/swig-6 (arg0)
                       :first-param ndb-init)

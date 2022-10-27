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

(defmacro make-finalize-wrapper (name type instance &rest arguments)
  (let ((hidden-name (find-symbol (format nil "~a%" name) :ndbapi.ffi)))
    `(defun ,name ,arguments
       (let ((object (,hidden-name ,@arguments)))
         (finalize-thing object ,type (or ,instance ,(first arguments)))
         object))))

(make-finalize-wrapper ndbapi.ffi::new-ndb/swig-1 :cluster-connection nil cluster-connection database-name)

(defmacro make-finalize-wrapper (name instance &rest arguments)
  (let ((hidden-name (find-symbol (format nil "~a%" name) :ndbapi.ffi))
        (type (intern (symbol-name (first arguments)) :keyword)))
    `(defun ,name ,arguments
       (let ((object (,hidden-name ,@arguments)))
         (finalize-thing object ,type (or ,instance ,(first arguments)))
         object))))

(defmacro make-finalize-wrapper (name (&rest params) &optional first-param)
  (let ((hidden-name (find-symbol (format nil "~a%" name) :ndbapi.ffi))
        (type (intern (symbol-name (or first-param (first params))) :keyword)))
    `(defun ,name ,(if first-param (cons first-param params) params)
       (let ((object (,hidden-name ,@params)))
         (finalize-thing object ,type ,(or first-param (first params)))
         object))))

(make-finalize-wrapper ndbapi.ffi::new-ndb/swig-1 (cluster-connection database-name))
(make-finalize-wrapper ndbapi.ffi::new-ndb-cluster-connection/swig-0 (connect-string) ndb-init)

(defmacro make-finalize-wrapper (name (&rest params) &key (first-param (first params) first-param-p))
  (let ((hidden-name (find-symbol (format nil "~a%" name) :ndbapi.ffi))
        (type (intern (symbol-name first-param) :keyword)))
    `(defun ,name ,(if first-param-p (cons first-param params) params)
       (let ((object (,hidden-name ,@params)))
         (finalize-thing object ,type ,first-param)
         object))))

(make-finalize-wrapper ndbapi.ffi::new-ndb/swig-1 (cluster-connection database-name))
(make-finalize-wrapper ndbapi.ffi::new-ndb-cluster-connection/swig-0 (connect-string) :first-param ndb-init)

(defun ndbapi.ffi::new-ndb/swig-1 (cluster-connection database-name)
  (let ((ndb (ndbapi.ffi::new-ndb/swig-1% cluster-connection database-name)))
    (finalize-thing ndb :cluster-connection cluster-connection)
    ndb))

(defun ndbapi.ffi::new-ndb/swig-0 (cluster-connection database-name schema-name)
  (let ((ndb (ndbapi.ffi::new-ndb/swig-0% cluster-connection database-name schema-name)))
    (finalize-thing ndb :cluster-connection cluster-connection)
    ndb))

(defun ndbapi.ffi::new-ndb/swig-2 (cluster-connection)
  (let ((ndb (ndbapi.ffi::new-ndb/swig-2% cluster-connection)))
    (finalize-thing ndb :cluster-connection cluster-connection)
    ndb))


;; add ndb-init wrapper as first argument to keep reference in finalizer for the ndb-cluster-connection created
(defun ndbapi.ffi::new-ndb-cluster-connection/swig-0 (ndb-init connectstring)
  (let ((cluster-connection (ndbapi.ffi::new-ndb-cluster-connection/swig-0% connectstring)))
    (finalize-thing cluster-connection :ndb-init ndb-init)
    cluster-connection))

(defun ndbapi.ffi::new-ndb-cluster-connection/swig-1 (ndb-init)
  (let ((cluster-connection (ndbapi.ffi::new-ndb-cluster-connection/swig-1%)))
    (finalize-thing cluster-connection :ndb-init ndb-init)
    cluster-connection))

(defun ndbapi.ffi::new-ndb-cluster-connection/swig-2 (ndb-init connectstring force_api_nodeid)
  (let ((cluster-connection (ndbapi.ffi::new-ndb-cluster-connection/swig-2% connectstring force_api_nodeid)))
    (finalize-thing cluster-connection :ndb-init ndb-init)
    cluster-connection))

(defun ndbapi.ffi::new-ndb-cluster-connection/swig-3 (ndb-init connectstring main_connection)
  (let ((cluster-connection (ndbapi.ffi::new-ndb-cluster-connection/swig-3% connectstring main_connection)))
    (finalize-thing cluster-connection :ndb-init ndb-init)
    cluster-connection))

(defun ndbapi.ffi::new-ndb-cluster-connection/swig-4 (ndb-init connectstring main_connection force_api_nodeid)
  (let ((cluster-connection (ndbapi.ffi::new-ndb-cluster-connection/swig-4% connectstring main_connection force_api_nodeid)))
    (finalize-thing cluster-connection :ndb-init ndb-init)
    cluster-connection))


;; add ndb-init wrapper as first argument to keep reference in finalizer for the ndb-interpreted-code created
(defun ndbapi.ffi::new-ndb-interpreted-code/swig-0 (ndb-init table buffer buffer-word-size)
  (let ((thing (ndbapi.ffi::new-ndb-interpreted-code/swig-0% table buffer buffer-word-size)))
    (finalize-thing thing :ndb-init ndb-init)
    thing))

;;; Copyright (c) 2022 Max-Gerd Retzlaff <mgr@matroid.org>, Datagraph GmbH.
;;; Distributed under the terms of the GNU General Public License, Version 2.0,
;;; see file LICENSE in the top level directory of this repository.

(in-package :ndbapi.implementation)

;; errors

(defun %get-ndb-error (object &optional (getter #'ndbapi.ffi::ndb-get-ndb-error/swig-0))
  (let ((pointer (if (typep  object 'ndbapi.types::garbage-collected-class)
                     (ndbapi.types::foreign-pointer object)
                     object)))
    (cffi:mem-aref (funcall getter pointer)
                   '(:struct ndbapi.ffi::ndberror-struct))))

(defun error-string (error-plist)
  (format nil "Error with code ~a: ~a"
          (getf error-plist :code)
          (getf error-plist :message)))

(defun get-ndb-error (object &optional (getter #'ndbapi.ffi::ndb-get-ndb-error/swig-0))
  (error-string (%get-ndb-error object getter)))

;; test :ndbapi.types for validity

(defun valid-object-p (object)
  (let ((pointer (if (typep object 'ndbapi.types::garbage-collected-class)
                     (ndbapi.types::foreign-pointer object)
                     object)))
    (and pointer
         (not (cffi:null-pointer-p pointer)))))

;; interface

#|
(setf (fdefinition 'ndb-init) #'ndbapi.ffi::ndb-init%)
(setf (fdefinition 'new-ndb-cluster-connection) #'ndbapi.ffi::new-ndb-cluster-connection/swig-0)
(setf (fdefinition 'ndb-cluster-connection-connect) #'ndbapi.ffi::ndb-cluster-connection-connect/swig-0)
(setf (fdefinition 'ndb-cluster-connection-wait-until-ready) #'ndbapi.ffi::ndb-cluster-connection-wait-until-ready/swig-0)
(setf (fdefinition 'new-ndb) #'ndbapi.ffi::new-ndb/swig-1)
(setf (fdefinition 'ndb-init-ndb) #'ndbapi.ffi::ndb-init/swig-1) ;; renamed to avoid conflict
(setf (fdefinition 'ndb-get-ndb-error) #'ndbapi.ffi::ndb-get-ndb-error/swig-0)
(setf (fdefinition 'ndb-start-transaction) #'ndbapi.ffi::ndb-start-transaction/swig-3)
(setf (fdefinition 'ndb-get-dictionary) #'ndbapi.ffi::ndb-get-dictionary%)
(setf (fdefinition 'dictionary-get-table) #'ndbapi.ffi::dictionary-get-table/swig-0)
(setf (fdefinition 'dictionary-get-index) #'ndbapi.ffi::dictionary-get-index/swig-0)
(setf (fdefinition 'index-get-default-record) #'ndbapi.ffi::index-get-default-record%)
(setf (fdefinition 'table-get-default-record) #'ndbapi.ffi::table-get-default-record%)
(setf (fdefinition 'ndb-transaction-scan-index) #'ndbapi.ffi::ndb-transaction-scan-index/swig-5)
(setf (fdefinition 'ndb-index-scan-operation-set-bound) #'ndbapi.ffi::ndb-index-scan-operation-set-bound/swig-6)
(setf (fdefinition 'ndb-transaction-execute) #'ndbapi.ffi::ndb-transaction-execute/swig-5)
(setf (fdefinition 'ndb-scan-operation-next-result) #'ndbapi.ffi::ndb-scan-operation-next-result/swig-3)
(setf (fdefinition 'ndb-scan-operation-close) #'ndbapi.ffi::ndb-scan-operation-close/swig-1)
|#

#+(or)
(defun new-ndb-cluster-connection (ndb-init connectstring)
  (let ((value (ndbapi.ffi::new-ndb-cluster-connection/swig-0 ndb-init connectstring)))
    (assert (ndbapi:valid-object-p value)
            ()
            "Create new ndb-cluster-connection object failed")
    value))

#+(or)
(defun ndb-cluster-connection-connect (self no_retries retry_delay_in_seconds verbose)
  (let ((value (ndbapi.ffi::ndb-cluster-connection-connect/swig-0 self no_retries retry_delay_in_seconds verbose)))
    (assert (zerop value)
            ()
            "Cluster management server was not ready within 30 secs")
    value))

(defmacro make-interface-function (name call test datum &rest arguments)
  `(defun ,name ,(cdr call)
     (let ((value ,call))
       (assert (funcall ,test value)
               ()
               ,datum
               ,@arguments)
    value)))

(make-interface-function ndb-init
                         (ndbapi.ffi::ndb-init%)
                         #'ndbapi:initialized
                         "ndb-init failed")

(make-interface-function new-ndb-cluster-connection
                         (ndbapi.ffi::new-ndb-cluster-connection/swig-0 ndb-init connectstring)
                         #'ndbapi:valid-object-p
                         "Create new ndb-cluster-connection object failed")

(make-interface-function ndb-cluster-connection-connect
                         (ndbapi.ffi::ndb-cluster-connection-connect/swig-0 self no-retries retry-delay-in-seconds verbose)
                         #'zerop
                         "Cluster management server was not ready within 30 secs")

(make-interface-function ndb-cluster-connection-wait-until-ready
                         (ndbapi.ffi::ndb-cluster-connection-wait-until-ready/swig-0 self timeout-for-first-alive timeout-after-first-alive)
                         #'zerop
                         "Cluster was not ready within 30 secs.")

(make-interface-function new-ndb
                         (ndbapi.ffi::new-ndb/swig-1 cluster-connection database-name)
                         #'ndbapi:valid-object-p
                         "Create new NDB object failed")

(make-interface-function ndb-init-ndb ;; renamed to avoid conflict
                         (ndbapi.ffi::ndb-init/swig-1 ndb)
                         #'zerop
                         "Ndb.init() failed: ~a"
                         (ndbapi:get-ndb-error ndb #'ndbapi:ndb-get-ndb-error))

(make-interface-function ndb-get-ndb-error
                         (ndbapi.ffi::ndb-get-ndb-error/swig-0 ndb)
                         (lambda (value) ;; no error handling
                           (declare (ignore value))
                           t)
                         nil)

(make-interface-function ndb-start-transaction
                         (ndbapi.ffi::ndb-start-transaction/swig-3 ndb)
                         #'ndbapi:valid-object-p
                         "start-transaction() failed: ~a"
                         (ndbapi:get-ndb-error ndb))

(make-interface-function ndb-get-dictionary
                         (ndbapi.ffi::ndb-get-dictionary% ndb)
                         #'ndbapi:valid-object-p
                         "get-dictionary() failed: ~a"
                         (ndbapi:get-ndb-error ndb))

(make-interface-function dictionary-get-table
                         (ndbapi.ffi::dictionary-get-table/swig-0 dictionary name)
                         #'ndbapi:valid-object-p
                         "get-table() failed: ~a"
                         (ndbapi:get-ndb-error dictionary #'ndbapi:dictionary-get-ndb-error))

(make-interface-function dictionary-get-index
                         (ndbapi.ffi::dictionary-get-index/swig-0 dictionary index-name table-name)
                         #'ndbapi:valid-object-p
                         "get-index() failed: ~a"
                         (ndbapi:get-ndb-error dictionary #'ndbapi:dictionary-get-ndb-error))

(make-interface-function index-get-default-record
                         (ndbapi.ffi::index-get-default-record% index)
                         #'ndbapi:valid-object-p
                         "get-default-record() of index ~a failed"
                         (ndbapi:index-get-name index))

(make-interface-function table-get-default-record
                         (ndbapi.ffi::table-get-default-record% table)
                         #'ndbapi:valid-object-p
                         "get-default-record() of table ~a failed"
                         (ndbapi:table-get-name table))

(make-interface-function ndb-transaction-scan-index
                         (ndbapi.ffi::ndb-transaction-scan-index/swig-5 transaction key-record result-record)
                         #'ndbapi:valid-object-p
                         "transaction-scan-index() failed: ~a"
                         (ndbapi:get-ndb-error transaction #'ndbapi:ndb-transaction-get-ndb-error))

(make-interface-function ndb-index-scan-operation-set-bound
                         (ndbapi.ffi::ndb-index-scan-operation-set-bound/swig-6 index-scan key-record bound)
                         #'zerop
                         "set-bound() failed: ~a"
                         (ndbapi:get-ndb-error (ndbapi:ndb-scan-operation-get-ndb-transaction index-scan) #'ndbapi:ndb-transaction-get-ndb-error))

(make-interface-function ndb-transaction-execute
                         (ndbapi.ffi::ndb-transaction-execute/swig-5 transaction exec-type)
                         #'zerop
                         "transactino-execute() failed: ~a"
                         (ndbapi:get-ndb-error transaction #'ndbapi:ndb-transaction-get-ndb-error))

(make-interface-function ndb-scan-operation-next-result
                         (ndbapi.ffi::ndb-scan-operation-next-result/swig-3 scan out-row-ptr fetch-allowed force-send)
                         (lambda (rc) (>= rc 0)) ;; only -1 indicates error, 0, 1, and 2 are valid and indicate different situations
                         "scan-operation-next-result() failed: ~a"
                         (ndbapi:get-ndb-error (ndbapi:ndb-scan-operation-get-ndb-transaction scan) #'ndbapi:ndb-transaction-get-ndb-error))

(make-interface-function ndb-scan-operation-close
                         (ndbapi.ffi::ndb-scan-operation-close/swig-1 scan force-send) ;; returns void
                         (lambda (value) ;; no error handling
                           (declare (ignore value))
                           t)
                         nil)

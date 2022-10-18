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

(setf (fdefinition 'new-ndb-cluster-connection) #'ndbapi.ffi::new-ndb-cluster-connection/swig-0)
(setf (fdefinition 'ndb-cluster-connection-connect) #'ndbapi.ffi::ndb-cluster-connection-connect/swig-0)
(setf (fdefinition 'ndb-cluster-connection-wait-until-ready) #'ndbapi.ffi::ndb-cluster-connection-wait-until-ready/swig-0)
(setf (fdefinition 'new-ndb) #'ndbapi.ffi::new-ndb/swig-1)
(setf (fdefinition 'ndb-init-ndb) #'ndbapi.ffi::ndb-init/swig-1) ;; renamed to avoid conflict
(setf (fdefinition 'ndb-get-ndb-error) #'ndbapi.ffi::ndb-get-ndb-error/swig-0)
(setf (fdefinition 'ndb-start-transaction) #'ndbapi.ffi::ndb-start-transaction/swig-3)
(setf (fdefinition 'dictionary-get-table) #'ndbapi.ffi::dictionary-get-table/swig-0)
(setf (fdefinition 'dictionary-get-index) #'ndbapi.ffi::dictionary-get-index/swig-0)
(setf (fdefinition 'ndb-transaction-scan-index) #'ndbapi.ffi::ndb-transaction-scan-index/swig-5)
(setf (fdefinition 'ndb-index-scan-operation-set-bound) #'ndbapi.ffi::ndb-index-scan-operation-set-bound/swig-6)
(setf (fdefinition 'ndb-transaction-execute) #'ndbapi.ffi::ndb-transaction-execute/swig-5)
(setf (fdefinition 'ndb-scan-operation-next-result) #'ndbapi.ffi::ndb-scan-operation-next-result/swig-3)
(setf (fdefinition 'ndb-scan-operation-close) #'ndbapi.ffi::ndb-scan-operation-close/swig-1)

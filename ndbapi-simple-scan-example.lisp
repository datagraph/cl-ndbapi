;;; Copyright (c) 2022 Max-Gerd Retzlaff <mgr@matroid.org>, Datagraph GmbH.
;;; Distributed under the terms of the GNU General Public License, Version 2.0,
;;; see file LICENSE in the top level directory of this repository.

(in-package :cl-user)

;; translation of the example ndbapi_simple_scan that I created
;; on the basis of my ndbapi_simple_scan example (in c++)

(asdf:oos 'asdf:load-op :libndbapi)

(defvar *ndb*)

(defun %get-ndb-error (object &optional (getter #'libndbapi::ndb-get-ndb-error/swig-0))
  (cffi:mem-aref (funcall getter object)
                 '(:struct libndbapi::ndberror-struct)))

(defun error-string (error-plist)
  (format nil "Error with code ~a: ~a"
          (getf error-plist 'libndbapi::code)
          (getf error-plist 'libndbapi::message)))

(defun get-ndb-error (object &optional (getter #'libndbapi::ndb-get-ndb-error/swig-0))
  (error-string (%get-ndb-error object getter)))


(assert (zerop (libndbapi::ndb-init/swig-0))
        ()
        "ndb-init failed")

(defparameter *conn* (libndbapi::new-ndb-cluster-connection/swig-0 "nl3:1186,nl3:1187"))

(assert (zerop (libndbapi::ndb-cluster-connection-connect/swig-0 *conn*
                                                                 4 ;; retries
                                                                 5 ;; delay between retries
                                                                 1 ;; verbose
                                                                 ))
        ()
        "Cluster management server was not ready within 30 secs")

(assert (zerop (libndbapi::ndb-cluster-connection-wait-until-ready/swig-0 *conn* 30 0))
        ()
        "Cluster was not ready within 30 secs.")

(defparameter *database-name* "mgr")
(defparameter *table-name* "test")

(defparameter *ndb* (libndbapi::new-ndb/swig-1 *conn* *database-name*))
(assert (not (cffi:null-pointer-p (libndbapi::foreign-pointer *ndb*)))
        ()
        "Create new NDB object failed")

(assert (zerop (libndbapi::ndb-init/swig-1 (libndbapi::foreign-pointer *ndb*)))
        ()
        "Ndb.init() failed: ~a"
        (get-ndb-error (libndbapi::foreign-pointer *ndb*) #'libndbapi::ndb-get-ndb-error/swig-0))

(defparameter *transaction* (libndbapi::ndb-start-transaction/swig-3 (libndbapi::foreign-pointer *ndb*)))
(assert (not (cffi:null-pointer-p *transaction*))
        ()
        "start-transaction() failed: ~a"
        (get-ndb-error (libndbapi::foreign-pointer *ndb*)))

(defparameter *dict* (libndbapi::ndb-get-dictionary (libndbapi::foreign-pointer *ndb*)))
(assert (not (cffi:null-pointer-p *dict*))
        ()
        "get-dictionary() failed: ~a"
        (get-ndb-error (libndbapi::foreign-pointer *ndb*)))

(defparameter *test-table* (libndbapi::dictionary-get-table/swig-0 *dict* *table-name*))
(assert (not (cffi:null-pointer-p *test-table*))
        ()
        "get-table() failed: ~a"
        (get-ndb-error *dict* #'libndbapi::dictionary-get-ndb-error))

(defparameter *index-name* "gspo")

(defparameter *index* (libndbapi::dictionary-get-index/swig-0 *dict*
                                                              *index-name*
                                                              (libndbapi::table-get-name *test-table*)))
(assert (not (cffi:null-pointer-p *index*))
        ()
        "get-index() failed: ~a"
        (get-ndb-error *dict* #'libndbapi::dictionary-get-ndb-error))

(defparameter *index-default-record* (libndbapi::index-get-default-record *index*))
(assert (not (cffi:null-pointer-p *index-default-record*))
        ()
        "get-default-record() of index ~a failed"
        *index-name*)

(defparameter *test-table-default-record* (libndbapi::table-get-default-record *test-table*))
(assert (not (cffi:null-pointer-p *test-table-default-record*))
        ()
        "get-default-record() of table ~a failed"
        *table-name*)

(defparameter *scan* (libndbapi::ndb-transaction-scan-index/swig-5 *transaction*
                                                                   *INDEX-DEFAULT-RECORD*
                                                                   *test-table-default-record*))
(assert (not (cffi:null-pointer-p *scan*))
        ()
        "transaction-scan-index() failed: ~a"
        (get-ndb-error *transaction* #'libndbapi::ndb-transaction-get-ndb-error))

(assert (zerop (libndbapi::ndb-transaction-execute/swig-5 *transaction* :+NO-COMMIT+))
        ()
        "transactino-execute() failed: ~a"
        (get-ndb-error *transaction* #'libndbapi::ndb-transaction-get-ndb-error))

;;   // Check rc anyway

(defparameter *true-val* (cffi::foreign-alloc :int :initial-element 1))
;; probably rather: (cffi:make-pointer 1)
(defparameter *false-val* (cffi::foreign-alloc :int :initial-element 0))
;; probably rather: (cffi:null-pointer)

(defparameter *row-data* (cffi:foreign-alloc :pointer))

(loop for rc = (libndbapi::ndb-scan-operation-next-result/swig-3 *scan* *row-data* *true-val* *false-val*)
      for j from 0
      while (zerop rc)
      do (format t "~&row ~5d: " j)
         (dotimes (i 4)
           (format t "~12d" (cffi::mem-aref (cffi:mem-aref *row-data* :pointer) :uint32 i))
           (when (< i 3)
             (format t ", ")))
      finally (assert (= rc 1)
                      ()
                      "scan-operation-next-result() failed: ~a"
                      (get-ndb-error *transaction* #'libndbapi::ndb-transaction-get-ndb-error)))

;;   if (rc != 1)  APIERROR(myTransaction->getNdbError());

(cffi:foreign-free *row-data*)
(setf *row-data* nil)

(libndbapi::ndb-scan-operation-close/swig-1 *scan* *true-val*) ;; no value
(libndbapi::ndb-end 0) ;; no value

(setf *ndb* nil)
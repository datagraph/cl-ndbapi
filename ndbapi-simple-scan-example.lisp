;;; Copyright (c) 2022 Max-Gerd Retzlaff <mgr@matroid.org>, Datagraph GmbH.
;;; Distributed under the terms of the GNU General Public License, Version 2.0,
;;; see file LICENSE in the top level directory of this repository.

(in-package :cl-user)

;; translation of the example ndbapi_simple_scan that I created
;; on the basis of my ndbapi_simple_scan example (in c++)

#+(or)
(asdf:oos 'asdf:load-op :libndbapi)

(defvar *ndb*)

(defun %get-ndb-error (object &optional (getter #'libndbapi::ndb-get-ndb-error/swig-0))
  (let ((pointer (if (typep  object 'libndbapi::garbage-collected-class)
                     (libndbapi::foreign-pointer object)
                     object)))
    (cffi:mem-aref (funcall getter pointer)
                   '(:struct libndbapi::ndberror-struct))))

(defun error-string (error-plist)
  (format nil "Error with code ~a: ~a"
          (getf error-plist 'libndbapi::code)
          (getf error-plist 'libndbapi::message)))

(defun get-ndb-error (object &optional (getter #'libndbapi::ndb-get-ndb-error/swig-0))
  (error-string (%get-ndb-error object getter)))

(defun valid-p (object)
  (let ((pointer (if (typep  object 'libndbapi::garbage-collected-class)
                     (libndbapi::foreign-pointer object)
                     object)))
    (not (cffi:null-pointer-p pointer))))

(defvar *ndb-initialized* nil)
(unless *ndb-initialized*
  (assert (zerop (libndbapi::ndb-init/swig-0))
          ()
          "ndb-init failed")
  (setf *ndb-initialized* t))

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
(assert (valid-p *ndb*)
        ()
        "Create new NDB object failed")

(assert (zerop (libndbapi::ndb-init/swig-1 *ndb*))
        ()
        "Ndb.init() failed: ~a"
        (get-ndb-error *ndb* #'libndbapi::ndb-get-ndb-error/swig-0))

(defparameter *transaction* (libndbapi::ndb-start-transaction/swig-3 *ndb*))
(assert (valid-p *transaction*)
        ()
        "start-transaction() failed: ~a"
        (get-ndb-error *ndb*))

(defparameter *dict* (libndbapi::ndb-get-dictionary *ndb*))
(assert (valid-p *dict*)
        ()
        "get-dictionary() failed: ~a"
        (get-ndb-error *ndb*))

(defparameter *test-table* (libndbapi::dictionary-get-table/swig-0 *dict* *table-name*))
(assert (valid-p *test-table*)
        ()
        "get-table() failed: ~a"
        (get-ndb-error *dict* #'libndbapi::dictionary-get-ndb-error))

(defparameter *index-name* "gspo")

(defparameter *index* (libndbapi::dictionary-get-index/swig-0 *dict*
                                                              *index-name*
                                                              (libndbapi::table-get-name *test-table*)))
(assert (valid-p *index*)
        ()
        "get-index() failed: ~a"
        (get-ndb-error *dict* #'libndbapi::dictionary-get-ndb-error))

(defparameter *index-default-record* (libndbapi::index-get-default-record *index*))
(assert (valid-p *index-default-record*)
        ()
        "get-default-record() of index ~a failed"
        *index-name*)

(defparameter *test-table-default-record* (libndbapi::table-get-default-record *test-table*))
(assert (valid-p *test-table-default-record*)
        ()
        "get-default-record() of table ~a failed"
        *table-name*)

(defparameter *scan* (libndbapi::ndb-transaction-scan-index/swig-5 *transaction*
                                                                   *INDEX-DEFAULT-RECORD*
                                                                   *test-table-default-record*))
(assert (valid-p *scan*)
        ()
        "transaction-scan-index() failed: ~a"
        (get-ndb-error *transaction* #'libndbapi::ndb-transaction-get-ndb-error))

(libndbapi::with-foreign-struct (low (list 'libndbapi::s 662743 'libndbapi::p 2000000) '(:struct libndbapi::tuple))
  (libndbapi::with-foreign-struct (high (list 'libndbapi::s 662743 'libndbapi::p 2200000) '(:struct libndbapi::tuple))
    (libndbapi::with-foreign-struct (bound (list 'libndbapi::low-key low
                                                 'libndbapi::low-key-count libndbapi::+tuple-count+
                                                 'libndbapi::low-inclusive t
                                                 'libndbapi::high-key high
                                                 'libndbapi::high-key-count libndbapi::+tuple-count+
                                                 'libndbapi::high-inclusive t
                                                 'libndbapi::range-no 0)
                                           '(:struct libndbapi::index-bound))
      ;;(cffi:foreign-slot-value bound '(:struct libndbapi::index-bound) 'libndbapi::low-inclusive)

      (assert (zerop (libndbapi::ndb-index-scan-operation-set-bound/swig-6 *scan* *index-default-record* bound))
        ()
        "set-bound() failed: ~a"
        (get-ndb-error *transaction* #'libndbapi::ndb-transaction-get-ndb-error))

      (assert (zerop (libndbapi::ndb-transaction-execute/swig-5 *transaction* :+NO-COMMIT+))
              ()
              "transactino-execute() failed: ~a"
              (get-ndb-error *transaction* #'libndbapi::ndb-transaction-get-ndb-error)))))


;;   // Check rc anyway

(defparameter *row-data* (cffi:foreign-alloc :pointer))

(loop for rc = (libndbapi::ndb-scan-operation-next-result/swig-3 *scan* *row-data* t nil)
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

(cffi:foreign-free *row-data*)
(setf *row-data* nil)

(libndbapi::ndb-scan-operation-close/swig-1 *scan* t) ;; no value

(setf *ndb* nil)
(setf *conn* nil)

#+(or) ;; ndb-end only at the very end when all objects are freed by GC
(setf *ndb-initialized* (libndbapi::ndb-end 0)) ;; no value

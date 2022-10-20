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

(defmacro make-interface-function (name call &optional test datum &rest arguments)
  `(defun ,name ,(cdr call)
     ,(if test
          `(let ((value ,call))
             (assert (funcall ,test value)
                     ()
                     ,datum
                     ,@arguments)
             value)
          call)))

(make-interface-function ndb-init
                         (ndbapi.ffi::ndb-init%)
                         #'ndbapi.types:initialized
                         "ndb-init failed")

(make-interface-function new-ndb-cluster-connection
                         (ndbapi.ffi::new-ndb-cluster-connection/swig-0 ndb-init connectstring)
                         #'valid-object-p
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
                         #'valid-object-p
                         "Create new NDB object failed")

(make-interface-function ndb-get-ndb-error
                         ;; no error handling
                         (ndbapi.ffi::ndb-get-ndb-error/swig-0 ndb))

(make-interface-function ndb-init-ndb ;; renamed to avoid conflict
                         (ndbapi.ffi::ndb-init/swig-1 ndb)
                         #'zerop
                         "Ndb.init() failed: ~a"
                         (get-ndb-error ndb #'ndb-get-ndb-error))

(make-interface-function ndb-start-transaction
                         (ndbapi.ffi::ndb-start-transaction/swig-3 ndb)
                         #'valid-object-p
                         "start-transaction() failed: ~a"
                         (get-ndb-error ndb))

(make-interface-function ndb-get-dictionary
                         (ndbapi.ffi::ndb-get-dictionary% ndb)
                         #'valid-object-p
                         "get-dictionary() failed: ~a"
                         (get-ndb-error ndb))

(make-interface-function dictionary-get-table
                         (ndbapi.ffi::dictionary-get-table/swig-0 dictionary name)
                         #'valid-object-p
                         "get-table() failed: ~a"
                         (get-ndb-error dictionary #'ndbapi.ffi:dictionary-get-ndb-error))

(make-interface-function dictionary-get-index
                         (ndbapi.ffi::dictionary-get-index/swig-0 dictionary index-name table-name)
                         #'valid-object-p
                         "get-index() failed: ~a"
                         (get-ndb-error dictionary #'ndbapi.ffi:dictionary-get-ndb-error))

(make-interface-function index-get-default-record
                         (ndbapi.ffi::index-get-default-record% index)
                         #'valid-object-p
                         "get-default-record() of index ~a failed"
                         (ndbapi.ffi:index-get-name index))

(make-interface-function table-get-default-record
                         (ndbapi.ffi::table-get-default-record% table)
                         #'valid-object-p
                         "get-default-record() of table ~a failed"
                         (ndbapi.ffi:table-get-name table))

(make-interface-function ndb-transaction-scan-index
                         (ndbapi.ffi::ndb-transaction-scan-index/swig-5 transaction key-record result-record)
                         #'valid-object-p
                         "transaction-scan-index() failed: ~a"
                         (get-ndb-error transaction #'ndbapi.ffi:ndb-transaction-get-ndb-error))

(make-interface-function ndb-index-scan-operation-set-bound
                         (ndbapi.ffi::ndb-index-scan-operation-set-bound/swig-6 index-scan key-record bound)
                         #'zerop
                         "set-bound() failed: ~a"
                         (get-ndb-error (ndbapi.ffi:ndb-scan-operation-get-ndb-transaction index-scan) #'ndbapi.ffi:ndb-transaction-get-ndb-error))

;; cannot use in our usage pattern as it leads to:
;; ndb-index-scan-operation-read-tuples() failed: Error with code 4284: Cannot mix NdbRecAttr and NdbRecord methods in one operation
#+(or)
(make-interface-function ndb-index-scan-operation-read-tuples
                         (ndbapi.ffi::ndb-index-scan-operation-read-tuples/swig-2 index-scan lock-mode scan-flags)
                         #'zerop
                         "transaction-scan-index() failed: ~a"
                         (get-ndb-error (ndbapi.ffi:ndb-scan-operation-get-ndb-transaction index-scan) #'ndbapi.ffi:ndb-transaction-get-ndb-error))

(make-interface-function ndb-transaction-execute
                         (ndbapi.ffi::ndb-transaction-execute/swig-5 transaction exec-type)
                         #'zerop
                         "transactino-execute() failed: ~a"
                         (get-ndb-error transaction #'ndbapi.ffi:ndb-transaction-get-ndb-error))

(make-interface-function ndb-scan-operation-next-result
                         (ndbapi.ffi::ndb-scan-operation-next-result/swig-3 scan out-row-ptr fetch-allowed force-send)
                         (lambda (rc) (>= rc 0)) ;; only -1 indicates error, 0, 1, and 2 are valid and indicate different situations
                         "scan-operation-next-result() failed: ~a"
                         (get-ndb-error (ndbapi.ffi:ndb-scan-operation-get-ndb-transaction scan) #'ndbapi.ffi:ndb-transaction-get-ndb-error))

(make-interface-function ndb-scan-operation-close
                         ;; returns void
                         (ndbapi.ffi::ndb-scan-operation-close/swig-1 scan force-send))

;; with- macros

(defmacro with-ndb-init ((var &rest args) &body body)
  (let ((op (gensym "OP-")))
    `(flet ((,op (,var) ,@body))
       (declare (dynamic-extent #',op))
       (call-with-ndb-init #',op ,@args))))

(defun call-with-ndb-init (op &rest args)
  (declare (dynamic-extent args))
  (let ((value (apply #'ndb-init args)))
    (unwind-protect (funcall op value)
      ;; explicit free of ndb-init possible but also not that important.
      ;; (freeing of ndb-init not that important as it does not bind any remote resources)
      ;; freeing the ndb-init will call ndb-end.
      ;; update: ndb-end has some internal counting (by counter ndb_init_called in ndb_end_interal)
      ;;   so it is okay to call ndb-init multiple times, and ndb-end as well. Only the very last
      ;;   ndb-end call, that reduces ndb_init_called to 0, actually cleans up.
      ;; if you free it, you should make a new object in each of your tasks,
      ;; as only that prevents that there ndb is still initialized as long as you use it.
      (ndbapi.types:free-foreign-object value))))

(defmacro with-ndb-cluster-connection ((var &rest args) &body body)
  (let ((op (gensym "OP-")))
    `(flet ((,op (,var) ,@body))
       (declare (dynamic-extent #',op))
       (call-with-ndb-cluster-connection #',op ,@args))))

(defun call-with-ndb-cluster-connection (op &rest args)
  (declare (dynamic-extent args))
  (let ((value (apply #'new-ndb-cluster-connection args)))
    (unwind-protect (funcall op value)
      (ndbapi.types:free-foreign-object value))))

(defmacro with-ndb ((var &rest args) &body body)
  (let ((op (gensym "OP-")))
    `(flet ((,op (,var) ,@body))
       (declare (dynamic-extent #',op))
       (call-with-ndb #',op ,@args))))

(defun call-with-ndb (op &rest args)
  (declare (dynamic-extent args))
  (let ((value (apply #'new-ndb args)))
    (unwind-protect (funcall op value)
      (ndbapi.types:free-foreign-object value))))

(defmacro with-ndb-transaction ((var &rest args) &body body)
  (let ((op (gensym "OP-")))
    `(flet ((,op (,var) ,@body))
       (declare (dynamic-extent #',op))
       (call-with-ndb-transaction #',op ,@args))))

(defun call-with-ndb-transaction (op &rest args)
  (declare (dynamic-extent args))
  (let ((value (apply #'ndb-start-transaction args)))
    (unwind-protect (funcall op value)
      ;; returns no value
      (ndbapi.ffi:ndb-close-transaction (ndbapi.ffi:ndb-transaction-get-ndb value) value))))

(defmacro with-ndb-transaction-scan-index ((var (&rest open-args) (&rest close-args)) &body body)
  (let ((op (gensym "OP-")))
    `(flet ((,op (,var) ,@body))
       (declare (dynamic-extent #',op))
       (call-with-ndb-transaction-scan-index #',op
                                             :open-args (list ,@open-args)
                                             :close-args (list ,@close-args)))))

(defun call-with-ndb-transaction-scan-index (op &key open-args close-args)
  (declare (dynamic-extent open-args close-args))
  (let ((value (apply #'ndb-transaction-scan-index open-args)))
    (unwind-protect (funcall op value)
      ;; returns no value
      (apply #'ndb-scan-operation-close value close-args))))

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
  (let ((translated-call (if (find '&rest call)
                             (cons 'apply (cons `(symbol-function ',(car call)) (cdr (remove '&rest call))))
                             call)))
    `(defun ,name ,(cdr call)
       ,(if test
            `(let ((value ,translated-call))
               (assert (funcall ,test value)
                       ()
                       ,datum
                       ,@arguments)
               value)
            translated-call))))

(make-interface-function ndb-begin ;; rename ndb-init to ndb-begin to avoid conflict
                                   ;; also this fits to the complimentary function ndb-end
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

(make-interface-function ndb-init ;; other ndb-init already renamed to ndb-begin to avoid conflict
                         (ndbapi.ffi.o::ndb-init ndb)
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
                          ;; all variants have the named three parameters
                         (ndbapi.ffi.o::ndb-transaction-scan-index transaction key-record result-record &rest args)
                         #'valid-object-p
                         "transaction-scan-index() failed: ~a"
                         (get-ndb-error transaction #'ndbapi.ffi:ndb-transaction-get-ndb-error))

(make-interface-function ndb-index-scan-operation-set-bound
                         (ndbapi.ffi::ndb-index-scan-operation-set-bound/swig-6 index-scan key-record bound)
                         #'zerop
                         "set-bound() failed: ~a"
                         (get-ndb-error (ndbapi.ffi:ndb-scan-operation-get-ndb-transaction index-scan) #'ndbapi.ffi:ndb-transaction-get-ndb-error))

(make-interface-function ndb-index-scan-operation-set-bound/recattr
                         (ndbapi.ffi::ndb-index-scan-operation-set-bound/swig-1 index-scan attr type value)
                         #'zerop
                         "set-bound() failed: ~a"
                         (get-ndb-error (ndbapi.ffi:ndb-scan-operation-get-ndb-transaction index-scan) #'ndbapi.ffi:ndb-transaction-get-ndb-error))

(make-interface-function ndb-transaction-get-ndb-index-scan-operation
                         (ndbapi.ffi::ndb-transaction-get-ndb-index-scan-operation/swig-2 transaction an-index)
                         #'valid-object-p
                         "transaction-get-ndb-index-scan-operation() failed: ~a"
                         (get-ndb-error transaction #'ndbapi.ffi:ndb-transaction-get-ndb-error))

(make-interface-function ndb-index-scan-operation-read-tuples
                         (ndbapi.ffi::ndb-index-scan-operation-read-tuples/swig-2 index-scan lock-mode scan-flags)
                         #'zerop
                         "transaction-scan-index() failed: ~a"
                         (get-ndb-error (ndbapi.ffi:ndb-scan-operation-get-ndb-transaction index-scan) #'ndbapi.ffi:ndb-transaction-get-ndb-error))

(make-interface-function ndb-transaction-execute
                         (ndbapi.ffi::ndb-transaction-execute/swig-5 transaction exec-type)
                         #'zerop
                         "transaction-execute() failed: ~a"
                         (get-ndb-error transaction #'ndbapi.ffi:ndb-transaction-get-ndb-error))

(make-interface-function ndb-scan-operation-next-result
                         (ndbapi.ffi.o::ndb-scan-operation-next-result scan &rest args) ;; out-row-ptr fetch-allowed force-send
                         (lambda (rc) (>= rc 0)) ;; only -1 indicates error, 0, 1, and 2 are valid and indicate different situations
                         "scan-operation-next-result() failed: ~a"
                         (get-ndb-error (ndbapi.ffi:ndb-scan-operation-get-ndb-transaction scan) #'ndbapi.ffi:ndb-transaction-get-ndb-error))

(make-interface-function ndb-scan-operation-close
                         ;; returns void
                         (ndbapi.ffi.o::ndb-scan-operation-close scan &rest args))

(make-interface-function new-ndb-interpreted-code
                         (ndbapi.ffi::new-ndb-interpreted-code/swig-0 ndb-init table buffer buffer-word-size)
                         #'valid-object-p
                         "Create new ndb-interpreted-code object failed")

(make-interface-function ndb-interpreted-code-interpret-exit-last-row
                         (ndbapi.ffi::ndb-interpreted-code-interpret-exit-last-row code)
                         #'zerop
                         "interpret-exit-last-row() failed: ~a"
                         (get-ndb-error code  #'ndbapi.ffi:ndb-interpreted-code-get-ndb-error))

(make-interface-function ndb-interpreted-code-finalise
                         (ndbapi.ffi::ndb-interpreted-code-finalise code)
                         #'zerop
                         "finalize() failed: ~a"
                         (get-ndb-error code  #'ndbapi.ffi:ndb-interpreted-code-get-ndb-error))

(make-interface-function ndb-scan-operation-set-interpreted-code
                         (ndbapi.ffi::ndb-scan-operation-set-interpreted-code scan code)
                         #'zerop
                         "ndb-scan-operation-set-interpreted-code() failed: ~a"
                         (get-ndb-error (ndbapi.ffi:ndb-scan-operation-get-ndb-transaction scan) #'ndbapi.ffi:ndb-transaction-get-ndb-error))


(make-interface-function ndb-operation-get-value
                         ;;(ndbapi.ffi::ndb-operation-get-value/swig-2 ndb-operation an-attr-id a-value)
                         (ndbapi.ffi::ndb-operation-get-value/swig-4 ndb-operation a-column a-value)
                         #'valid-object-p
                         "ndb-scan-operation-set-interpreted-code() failed: ~a"
                         (get-ndb-error (ndbapi.ffi:ndb-operation-get-ndb-transaction ndb-operation) #'ndbapi.ffi:ndb-transaction-get-ndb-error))


;; low-level free

(setf (fdefinition 'ndb-free-object) (fdefinition 'ndbapi.types:free-foreign-object))

;; with- macros

(defmacro with-ndb-init ((var &rest args) &body body)
  (let ((op (gensym "OP-")))
    `(flet ((,op (,var) ,@body))
       (declare (dynamic-extent #',op))
       (call-with-ndb-init #',op ,@args))))

(defun call-with-ndb-init (op &rest args)
  (declare (dynamic-extent args))
  (let ((value (apply #'ndb-begin args)))
    (unwind-protect (funcall op value)
      ;; explicit free of ndb-init possible but also not that important.
      ;; (freeing of ndb-init not that important as it does not bind any remote resources)
      ;; freeing the ndb-init will call ndb-end.
      ;; update: ndb-end has some internal counting (by counter ndb_init_called in ndb_end_interal)
      ;;   so it is okay to call ndb-init multiple times, and ndb-end as well. Only the very last
      ;;   ndb-end call, that reduces ndb_init_called to 0, actually cleans up.
      ;; if you free it, you should make a new object in each of your tasks,
      ;; as only that prevents that there ndb is still initialized as long as you use it.
      (ndb-free-object value))))

(defmacro with-ndb-cluster-connection ((var &rest args) &body body)
  (let ((op (gensym "OP-")))
    `(flet ((,op (,var) ,@body))
       (declare (dynamic-extent #',op))
       (call-with-ndb-cluster-connection #',op ,@args))))

(defun call-with-ndb-cluster-connection (op &rest args)
  (declare (dynamic-extent args))
  (let ((value (apply #'new-ndb-cluster-connection args)))
    (unwind-protect (funcall op value)
      (ndb-free-object value))))

(defmacro with-ndb ((var &rest args) &body body)
  (let ((op (gensym "OP-")))
    `(flet ((,op (,var) ,@body))
       (declare (dynamic-extent #',op))
       (call-with-ndb #',op ,@args))))

(defun call-with-ndb (op &rest args)
  (declare (dynamic-extent args))
  (let ((value (apply #'new-ndb args)))
    (unwind-protect (funcall op value)
      (ndb-free-object value))))

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

(defmacro with-ndb-transaction-get-ndb-index-scan-operation ((var (&rest open-args) (&rest close-args)) &body body)
  (let ((op (gensym "OP-")))
    `(flet ((,op (,var) ,@body))
       (declare (dynamic-extent #',op))
       (call-with-ndb-transaction-get-ndb-index-scan-operation #',op
                                                               :open-args (list ,@open-args)
                                                               :close-args (list ,@close-args)))))

(defun call-with-ndb-transaction-get-ndb-index-scan-operation (op &key open-args close-args)
  (declare (dynamic-extent open-args close-args))
  (let ((value (apply #'ndb-transaction-get-ndb-index-scan-operation open-args)))
    (unwind-protect (funcall op value)
      ;; returns no value
      (apply #'ndb-scan-operation-close value close-args))))

(defmacro with-ndb-interpreted-code ((var &rest args) &body body)
  (let ((op (gensym "OP-")))
    `(flet ((,op (,var) ,@body))
       (declare (dynamic-extent #',op))
       (call-with-ndb-interpreted-code #',op ,@args))))

(defun call-with-ndb-interpreted-code (op &rest args)
  (declare (dynamic-extent args))
  (let ((value (apply #'new-ndb-interpreted-code args)))
    (unwind-protect (funcall op value)
      (ndb-free-object value))))

;; pseudo columns

(defmacro make-enum-accessors (enum-name &rest fields)
  (let ((enum (find-symbol (format nil "~a" enum-name) :ndbapi.ffi)))
    `(progn
       ,@(loop for field in fields
               for lisp-name = (ndbapi.ffi::swig-lispify (symbol-name field) 'enumvalue :keyword)
               for keyword = (intern (format nil "+~a+" lisp-name) :keyword)
               for pc-keyword = (intern (format nil "+PC-~a+" lisp-name) :keyword)
               collect `(defun ,(intern (format nil "~a.~a" enum-name keyword)) ()
                          (cffi:foreign-enum-value ',enum ,pc-keyword))))))

(make-enum-accessors pseudo-columns
                     FRAGMENT
                     FRAGMENT_FIXED_MEMORY
                     FRAGMENT_VARSIZED_MEMORY
                     ROW_COUNT
                     COMMIT_COUNT
                     ROW_SIZE
                     RANGE_NO
                     DISK_REF
                     RECORDS_IN_RANGE
                     ROWID
                     ROW_GCI
                     ROW_GCI64nn
                     ROW_AUTHOR
                     ANY_VALUE
                     COPY_ROWID
                     LOCK_REF
                     OP_ID
                     OPTIMIZE
                     FRAGMENT_EXTENT_SPACE
                     FRAGMENT_FREE_EXTENT_SPACE)

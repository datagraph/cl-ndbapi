;;; Copyright (c) 2022 Max-Gerd Retzlaff <mgr@matroid.org>, Datagraph GmbH.
;;; Distributed under the terms of the GNU General Public License, Version 2.0,
;;; see file LICENSE in the top level directory of this repository.

(in-package :ndbapi.implementation)


;;; errors

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
  (let ((error-plist (%get-ndb-error object getter)))
    (values (error-string error-plist)
            error-plist)))

(defun explicitly-check-for-error (object &optional (getter #'ndbapi.ffi::ndb-get-ndb-error/swig-0))
  (let ((error-plist (%get-ndb-error object getter)))
    (assert (eq (getf error-plist :status) :+NDBERROR-ST-SUCCESS+)
            nil
            "explicitly-check-for-error() reports: ~a"
            (error-string error-plist))))

(defun explicitly-check-for-transaction-error (transaction)
  "The MySQL NDB Cluster API Developer Guide says on
NdbTransaction::execute() that it returns 0 on success and -1 on
failure. But it reports failure \"if and only if the transaction was
aborted\". That is, NdbTransaction::execute() returning 0 mains only
that the transaction was not aborted, some operations might still have
not been successful.

In such cases, the transaction's error information will be set and this
is for what this function checks.

See https://dev.mysql.com/doc/ndbapi/en/ndb-ndbtransaction.html#ndb-ndbtransaction-execute

Note: ndbapi:ndb-transaction-execute already does this extra call,
so you do not need to repeat it."
  (explicitly-check-for-error transaction #'ndbapi.ffi:ndb-transaction-get-ndb-error))

(defun check-for-tuple-not-found-error (transaction)
  (let ((error-plist (%get-ndb-error transaction #'ndbapi.ffi:ndb-transaction-get-ndb-error)))
    #+(or)
    (break "~a" (list (getf error-plist :code)
                      (getf error-plist :classification)
                      (getf error-plist :message) ;; "Tuple did not exist"
                      error-plist))
    (and ;; the documentation of ndb cluster on NdbTransaction::execute()
         ;; seems to imply that checking for:
         ;;   (myTransaction->getNdbError().classification == NdbError:NoDataFound)
         ;; is enough; see:
         ;;   https://dev.mysql.com/doc/ndbapi/en/ndb-ndbtransaction.html#ndb-ndbtransaction-execute
         (eq (getf error-plist :classification) :+NDBERROR-CL-NO-DATA-FOUND+)
         ;; but lets check the error :code as well to be extra sure:
         (eq (getf error-plist :code) 626))))

;;; test :ndbapi.types for validity

(defun valid-object-p (object)
  (let ((pointer (if (typep object 'ndbapi.types::garbage-collected-class)
                     (let ((valid-cons (ndbapi.types::valid-cons object)))
                       (when (and valid-cons
                                  (car valid-cons))
                         (ndbapi.types::foreign-pointer object)))
                     object)))
    (and pointer
         (not (cffi:null-pointer-p pointer)))))

(defun initialized-ndb-init-p (object)
  (and object
       (typep object 'ndbapi.ffi::ndb-init)
       (let ((valid-cons (ndbapi.types::valid-cons object)))
         (when (and valid-cons
                    (car valid-cons))
           (ndbapi.types::initialized object)))))

(defun valid-connection-p (object)
  (when (typep object 'ndbapi.ffi::ndb-cluster-connection)
    (valid-object-p object)))

(defun valid-ndb-p (object)
  (when (typep object 'ndbapi.ffi::ndb)
    (valid-object-p object)))

;;; interface

(defmacro make-interface-function (name call (&optional test datum &rest arguments) &body more-calls)
  (let ((translated-call (if (find '&rest call)
                             (cons 'apply (cons `(symbol-function ',(car call)) (cdr (remove '&rest call))))
                             call)))
    `(defun ,name ,(cdr call)
       ,`(let ((value ,translated-call))
           ,@(if test
                 `((assert (funcall ,test value)
                            ()
                            ,datum
                            ,@arguments)))
           ,@more-calls
           value))))

(make-interface-function ndb-begin% ;; rename ndb-init to ndb-begin to avoid conflict
                                    ;; also this fits to the complimentary function ndb-end
    (ndbapi.ffi::ndb-init%)
    (#'initialized-ndb-init-p
     "ndb-init failed"))

(make-interface-function new-ndb-cluster-connection
    (ndbapi.ffi::new-ndb-cluster-connection/swig-0 ndb-init connection-string) ;; no unique arity (extension of RonDB)
    (#'valid-object-p
     "Create new ndb-cluster-connection object failed"))

(defvar *default-connection-name* "cl-ndbapi" "default 2nd argument for ndb-cluster-connection-set-name")

(defun ndb-cluster-connection-set-name (cluster-connection &optional name) ;; do not use default value here as name might be nil explicitly
  (ndbapi.ffi::ndb-cluster-connection-set-name% cluster-connection (or name *default-connection-name*))
  ;; returns void -> no test
  )

(make-interface-function ndb-cluster-connection-connect ;; defaults are specified in C++: no-retries=30 retry-delay-in-seconds=1 verbose=0
    (ndbapi.ffi.o::ndb-cluster-connection-connect cluster-connection &rest args) ;; no-retries retry-delay-in-seconds verbose
    (#'zerop
     "Cluster management server was not ready within expected time"))

(make-interface-function ndb-cluster-connection-wait-until-ready%
    (ndbapi.ffi::ndb-cluster-connection-wait-until-ready/swig-0 cluster-connection timeout-for-first-alive timeout-after-first-alive)
    (#'zerop
     "Cluster was not ready within 30 secs."))

(defvar *timeout-for-first-alive* 30 "default for 2nd argument of ndb-cluster-connection-wait-until-ready ")
(defvar *timeout-after-first-alive* 0 "default for 3rd argument of ndb-cluster-connection-wait-until-ready ")

(defun ndb-cluster-connection-wait-until-ready (cluster-connection &optional timeout-for-first-alive timeout-after-first-alive) ;; do not use default value here
  ;; returns void
  (ndb-cluster-connection-wait-until-ready% cluster-connection
                                            (or timeout-for-first-alive *timeout-for-first-alive*)
                                            (or timeout-after-first-alive *timeout-after-first-alive*)))

(make-interface-function new-ndb
    (ndbapi.ffi.o::new-ndb cluster-connection &rest args)
    (#'valid-object-p
     "Create new NDB object failed"))

(make-interface-function ndb-get-ndb-error
    (ndbapi.ffi::ndb-get-ndb-error/swig-0 ndb)
    ;; no error handling
    ())

(make-interface-function ndb-init ;; other ndb-init already renamed to ndb-begin to avoid conflict
    (ndbapi.ffi.o::ndb-init ndb &rest args)
    (#'zerop
     "Ndb.init() failed: ~a"
     (get-ndb-error ndb #'ndb-get-ndb-error)))

(make-interface-function ndb-start-transaction
    (ndbapi.ffi::ndb-start-transaction/swig-3 ndb)
    (#'valid-object-p
     "start-transaction() failed: ~a"
     (get-ndb-error ndb)))

(make-interface-function ndb-get-dictionary
    (ndbapi.ffi::ndb-get-dictionary% ndb)
    (#'valid-object-p
     "get-dictionary() failed: ~a"
     (get-ndb-error ndb)))

(make-interface-function dictionary-get-table
    (ndbapi.ffi::dictionary-get-table/swig-0 dictionary name)
    (#'valid-object-p
     "get-table() failed: ~a"
     (get-ndb-error dictionary #'ndbapi.ffi:dictionary-get-ndb-error)))

(make-interface-function dictionary-invalidate-table
    (ndbapi.ffi::dictionary-invalidate-table/swig-0 dictionary name)
    ;; returns void
    ())

(make-interface-function dictionary-remove-cached-table
    (ndbapi.ffi::dictionary-remove-cached-table/swig-0 dictionary name)
    ;; returns void
    ())

(make-interface-function dictionary-get-index
    (ndbapi.ffi::dictionary-get-index/swig-0 dictionary index-name table-name)
    (#'valid-object-p
     "get-index() failed: ~a"
     (get-ndb-error dictionary #'ndbapi.ffi:dictionary-get-ndb-error)))

(make-interface-function dictionary-invalidate-index
    (ndbapi.ffi::dictionary-invalidate-index/swig-0 dictionary index-name table-name)
    ;; returns void
    ())

(make-interface-function dictionary-remove-cached-index
    (ndbapi.ffi::dictionary-remove-cached-index/swig-0 dictionary index-name table-name)
    ;; returns void
    ())

(make-interface-function index-get-default-record
    (ndbapi.ffi::index-get-default-record% index)
    (#'valid-object-p
     "get-default-record() of index ~a failed"
     (ndbapi.ffi:index-get-name index)))

(make-interface-function table-get-default-record
    (ndbapi.ffi::table-get-default-record% table)
    (#'valid-object-p
     "get-default-record() of table ~a failed"
     (ndbapi.ffi:table-get-name table)))

(make-interface-function ndb-transaction-scan-index
    ;; all variants have the named three parameters
    (ndbapi.ffi.o::ndb-transaction-scan-index transaction key-record result-record &rest args)
    (#'valid-object-p
     "transaction-scan-index() failed: ~a"
     (get-ndb-error transaction #'ndbapi.ffi:ndb-transaction-get-ndb-error)))

(make-interface-function ndb-index-scan-operation-set-bound
    (ndbapi.ffi::ndb-index-scan-operation-set-bound/swig-6 index-scan key-record bound)
    (#'zerop
     "set-bound() failed: ~a"
     (get-ndb-error (ndbapi.ffi:ndb-scan-operation-get-ndb-transaction index-scan) #'ndbapi.ffi:ndb-transaction-get-ndb-error)))

(make-interface-function ndb-index-scan-operation-set-bound/recattr
    (ndbapi.ffi::ndb-index-scan-operation-set-bound/swig-1 index-scan attr type value)
    (#'zerop
     "set-bound() failed: ~a"
     (get-ndb-error (ndbapi.ffi:ndb-scan-operation-get-ndb-transaction index-scan) #'ndbapi.ffi:ndb-transaction-get-ndb-error)))

(make-interface-function ndb-transaction-get-ndb-index-scan-operation
    (ndbapi.ffi::ndb-transaction-get-ndb-index-scan-operation/swig-2 transaction an-index)
    (#'valid-object-p
     "transaction-get-ndb-index-scan-operation() failed: ~a"
     (get-ndb-error transaction #'ndbapi.ffi:ndb-transaction-get-ndb-error)))

(make-interface-function ndb-index-scan-operation-read-tuples
    (ndbapi.ffi::ndb-index-scan-operation-read-tuples/swig-2 index-scan lock-mode scan-flags)
    (#'zerop
     "transaction-scan-index() failed: ~a"
     (get-ndb-error (ndbapi.ffi:ndb-scan-operation-get-ndb-transaction index-scan) #'ndbapi.ffi:ndb-transaction-get-ndb-error)))

(make-interface-function ndb-transaction-scan-table
    ;; all variants have the named two parameters
    (ndbapi.ffi.o::ndb-transaction-scan-table transaction result-record &rest args)
    (#'valid-object-p
     "transaction-scan-table() failed: ~a"
     (get-ndb-error transaction #'ndbapi.ffi:ndb-transaction-get-ndb-error)))

(make-interface-function ndb-transaction-execute/no-explicit-check
    (ndbapi.ffi::ndb-transaction-execute/swig-5 transaction exec-type)
    (#'zerop
     "transaction-execute() failed: ~a"
     (get-ndb-error transaction #'ndbapi.ffi:ndb-transaction-get-ndb-error)))

(make-interface-function ndb-transaction-execute
    (ndbapi.ffi::ndb-transaction-execute/swig-5 transaction exec-type)
    (#'zerop
     "transaction-execute() failed: ~a"
     (get-ndb-error transaction #'ndbapi.ffi:ndb-transaction-get-ndb-error))
  ;; need to explicitly check that there were no errors in addition to the ZEROP assert above
  ;; see documentation string on explicitly-check-for-transaction-error for details
  (explicitly-check-for-transaction-error transaction))

(make-interface-function ndb-scan-operation-next-result
    (ndbapi.ffi.o::ndb-scan-operation-next-result scan &rest args) ;; out-row-ptr fetch-allowed force-send
    ((lambda (rc) (>= rc 0)) ;; only -1 indicates error, 0, 1, and 2 are valid and indicate different situations
     "scan-operation-next-result() failed: ~a"
     (get-ndb-error (ndbapi.ffi:ndb-scan-operation-get-ndb-transaction scan) #'ndbapi.ffi:ndb-transaction-get-ndb-error)))

(make-interface-function ndb-scan-operation-close
    (ndbapi.ffi.o::ndb-scan-operation-close scan &rest args)
    ;; returns void
    ())

(make-interface-function ndb-scan-operation-delete-current-tuple
    (ndbapi.ffi.o::ndb-scan-operation-delete-current-tuple scan &rest args) ;; out-row-ptr fetch-allowed force-send
    (#'valid-object-p
     ;; documentation indicates: (lambda (rc) (>= rc 0)) ;; only -1 indicates error, 0 means success
     ;; but it actually returns a "const NdbOperation*"
     "scan-operation-delete-current-tuple() failed: ~a"
     (get-ndb-error (ndbapi.ffi:ndb-scan-operation-get-ndb-transaction scan) #'ndbapi.ffi:ndb-transaction-get-ndb-error)))

(make-interface-function ndb-scan-operation-update-current-tuple
    (ndbapi.ffi.o::ndb-scan-operation-update-current-tuple scan &rest args) ;; out-row-ptr fetch-allowed force-send
    (#'valid-object-p
     "scan-operation-update-current-tuple() failed: ~a"
     (get-ndb-error (ndbapi.ffi:ndb-scan-operation-get-ndb-transaction scan) #'ndbapi.ffi:ndb-transaction-get-ndb-error)))

(make-interface-function new-ndb-interpreted-code
    (ndbapi.ffi::new-ndb-interpreted-code/swig-0 ndb-init table buffer buffer-word-size)
    (#'valid-object-p
     "Create new ndb-interpreted-code object failed"))

(make-interface-function ndb-interpreted-code-interpret-exit-last-row
    (ndbapi.ffi::ndb-interpreted-code-interpret-exit-last-row code)
    (#'zerop
     "interpret-exit-last-row() failed: ~a"
     (get-ndb-error code  #'ndbapi.ffi:ndb-interpreted-code-get-ndb-error)))

(make-interface-function ndb-interpreted-code-finalise
    (ndbapi.ffi::ndb-interpreted-code-finalise code)
    (#'zerop
     "finalize() failed: ~a"
     (get-ndb-error code  #'ndbapi.ffi:ndb-interpreted-code-get-ndb-error)))

(make-interface-function ndb-scan-operation-set-interpreted-code
    (ndbapi.ffi::ndb-scan-operation-set-interpreted-code scan code)
    (#'zerop
     "ndb-scan-operation-set-interpreted-code() failed: ~a"
     (get-ndb-error (ndbapi.ffi:ndb-scan-operation-get-ndb-transaction scan) #'ndbapi.ffi:ndb-transaction-get-ndb-error)))


(make-interface-function ndb-operation-get-value
    (ndbapi.ffi::ndb-operation-get-value/swig-4 ndb-operation a-column a-value)
    (#'valid-object-p
     "ndb-operation-get-value() failed: ~a"
     (get-ndb-error (ndbapi.ffi:ndb-operation-get-ndb-transaction ndb-operation) #'ndbapi.ffi:ndb-transaction-get-ndb-error)))

(make-interface-function ndb-transaction-update-tuple
    (ndbapi.ffi.o::ndb-transaction-update-tuple transaction key-record key-row attribute-record attribute-row &rest args)
    (#'valid-object-p
     "ndb-transaction-update-tuple() failed: ~a"
     ;; all examples do this:
     (get-ndb-error transaction #'ndbapi.ffi:ndb-transaction-get-ndb-error)
     ;; but the documentation seems to imply:
     ;;   (get-ndb-error value #'ndbapi.ffi::ndb-operation-get-ndb-error)
     ;; this, howewer, must be wrong as in this case an erroneous value,
     ;; even a null-pointer, would be used for error checking!
     ))

(make-interface-function ndb-transaction-insert-tuple
    ;; as an insert needs all fields for us, we never need the mask,
    ;; and to insert with the combined-row/record is easiest, so this variant is enough:
    (ndbapi.ffi::ndb-transaction-insert-tuple/swig-7 transaction combined-record combined-row)
    (#'valid-object-p
     "ndb-transaction-insert-tuple() failed: ~a"
     ;; all examples do this:
     (get-ndb-error transaction #'ndbapi.ffi:ndb-transaction-get-ndb-error)
     ;; but the documentation seems to imply:
     ;;   (get-ndb-error value #'ndbapi.ffi::ndb-operation-get-ndb-error)
     ;; this, howewer, must be wrong as in this case an erroneous value,
     ;; even a null-pointer, would be used for error checking!
     ))

(make-interface-function ndb-transaction-write-tuple
    (ndbapi.ffi.o::ndb-transaction-write-tuple transaction key-record key-row attribute-record attribute-row &rest args)
    (#'valid-object-p
     "ndb-transaction-write-tuple() failed: ~a"
     ;; all examples do this:
     (get-ndb-error transaction #'ndbapi.ffi:ndb-transaction-get-ndb-error)
     ;; but the documentation seems to imply:
     ;;   (get-ndb-error value #'ndbapi.ffi::ndb-operation-get-ndb-error)
     ;; this, howewer, must be wrong as in this case an erroneous value,
     ;; even a null-pointer, would be used for error checking!
     ))

(make-interface-function ndb-transaction-delete-tuple
    (ndbapi.ffi.o::ndb-transaction-delete-tuple transaction key-record key-row result-record &rest args)
    (#'valid-object-p
     "ndb-transaction-delete-tuple() failed: ~a"
     ;; all examples do this:
     (get-ndb-error transaction #'ndbapi.ffi:ndb-transaction-get-ndb-error)
     ;; but the documentation seems to imply:
     ;;   (get-ndb-error value #'ndbapi.ffi::ndb-operation-get-ndb-error)
     ;; this, howewer, must be wrong as in this case an erroneous value,
     ;; even a null-pointer, would be used for error checking!
     ))

(make-interface-function ndb-transaction-read-tuple
    (ndbapi.ffi.o::ndb-transaction-read-tuple transaction key-record key-row result-record result-row &rest args)
    (#'valid-object-p
     "ndb-transaction-read-tuple() failed: ~a"
     ;; all examples do this:
     (get-ndb-error transaction #'ndbapi.ffi:ndb-transaction-get-ndb-error)
     ;; but the documentation seems to imply:
     ;;   (get-ndb-error value #'ndbapi.ffi::ndb-operation-get-ndb-error)
     ;; this, howewer, must be wrong as in this case an erroneous value,
     ;; even a null-pointer, would be used for error checking!
     ))


;; begin of pseudo-columns

(make-interface-function column-fragment
    (ndbapi.ffi:column-fragment)
    (#'valid-object-p
     "column-fragment(): null pointer returned.
Note: The pseudo columns are only available in the context of a cluster connection,
as they are created as a side-effect of making the cluster connection."))

(make-interface-function column-fragment-fixed-memory
    (ndbapi.ffi:column-fragment-fixed-memory)
    (#'valid-object-p
     "column-fragment-fixed-memory(): null pointer returned.
Note: The pseudo columns are only available in the context of a cluster connection,
as they are created as a side-effect of making the cluster connection."))

(make-interface-function column-fragment-varsized-memory
    (ndbapi.ffi:column-fragment-varsized-memory)
    (#'valid-object-p
     "column-fragment-varsized-memory(): null pointer returned.
Note: The pseudo columns are only available in the context of a cluster connection,
as they are created as a side-effect of making the cluster connection."))

(make-interface-function column-row-count
    (ndbapi.ffi:column-row-count)
    (#'valid-object-p
     "column-row-count(): null pointer returned.
Note: The pseudo columns are only available in the context of a cluster connection,
as they are created as a side-effect of making the cluster connection."))

(make-interface-function column-commit-count
    (ndbapi.ffi:column-commit-count)
    (#'valid-object-p
     "column-commit-count(): null pointer returned.
Note: The pseudo columns are only available in the context of a cluster connection,
as they are created as a side-effect of making the cluster connection."))

(make-interface-function column-row-size
    (ndbapi.ffi:column-row-size)
    (#'valid-object-p
     "column-row-size(): null pointer returned.
Note: The pseudo columns are only available in the context of a cluster connection,
as they are created as a side-effect of making the cluster connection."))

(make-interface-function column-range-no
    (ndbapi.ffi:column-range-no)
    (#'valid-object-p
     "column-range-no(): null pointer returned.
Note: The pseudo columns are only available in the context of a cluster connection,
as they are created as a side-effect of making the cluster connection."))

(make-interface-function column-disk-ref
    (ndbapi.ffi:column-disk-ref)
    (#'valid-object-p
     "column-disk-ref(): null pointer returned.
Note: The pseudo columns are only available in the context of a cluster connection,
as they are created as a side-effect of making the cluster connection."))

(make-interface-function column-records-in-range
    (ndbapi.ffi:column-records-in-range)
    (#'valid-object-p
     "column-records-in-range(): null pointer returned.
Note: The pseudo columns are only available in the context of a cluster connection,
as they are created as a side-effect of making the cluster connection."))

(make-interface-function column-rowid
    (ndbapi.ffi:column-rowid)
    (#'valid-object-p
     "column-rowid(): null pointer returned.
Note: The pseudo columns are only available in the context of a cluster connection,
as they are created as a side-effect of making the cluster connection."))

(make-interface-function column-row-gci
    (ndbapi.ffi:column-row-gci)
    (#'valid-object-p
     "column-row-gci(): null pointer returned.
Note: The pseudo columns are only available in the context of a cluster connection,
as they are created as a side-effect of making the cluster connection."))

(make-interface-function column-row-gci-64
    (ndbapi.ffi:column-row-gci-64)
    (#'valid-object-p
     "column-row-gci-64(): null pointer returned.
Note: The pseudo columns are only available in the context of a cluster connection,
as they are created as a side-effect of making the cluster connection."))

(make-interface-function column-row-author
    (ndbapi.ffi:column-row-author)
    (#'valid-object-p
     "column-row-author(): null pointer returned.
Note: The pseudo columns are only available in the context of a cluster connection,
as they are created as a side-effect of making the cluster connection."))

(make-interface-function column-any-value
    (ndbapi.ffi:column-any-value)
    (#'valid-object-p
     "column-any-value(): null pointer returned.
Note: The pseudo columns are only available in the context of a cluster connection,
as they are created as a side-effect of making the cluster connection."))

(make-interface-function column-copy-rowid
    (ndbapi.ffi:column-copy-rowid)
    (#'valid-object-p
     "column-copy-rowid(): null pointer returned.
Note: The pseudo columns are only available in the context of a cluster connection,
as they are created as a side-effect of making the cluster connection."))

(make-interface-function column-lock-ref
    (ndbapi.ffi:column-lock-ref)
    (#'valid-object-p
     "column-lock-ref(): null pointer returned.
Note: The pseudo columns are only available in the context of a cluster connection,
as they are created as a side-effect of making the cluster connection."))

(make-interface-function column-op-id
    (ndbapi.ffi:column-op-id)
    (#'valid-object-p
     "column-op-id(): null pointer returned.
Note: The pseudo columns are only available in the context of a cluster connection,
as they are created as a side-effect of making the cluster connection."))

(make-interface-function column-optimize
    (ndbapi.ffi:column-optimize)
    (#'valid-object-p
     "column-optimize(): null pointer returned.
Note: The pseudo columns are only available in the context of a cluster connection,
as they are created as a side-effect of making the cluster connection."))

(make-interface-function column-fragment-extent-space
    (ndbapi.ffi:column-fragment-extent-space)
    (#'valid-object-p
     "column-fragment-extent-space(): null pointer returned.
Note: The pseudo columns are only available in the context of a cluster connection,
as they are created as a side-effect of making the cluster connection."))

(make-interface-function column-fragment-free-extent-space
    (ndbapi.ffi:column-fragment-free-extent-space)
    (#'valid-object-p
     "column-fragment-free-extent-space(): null pointer returned.
Note: The pseudo columns are only available in the context of a cluster connection,
as they are created as a side-effect of making the cluster connection."))

;; end of pseudo-columns


;;; low-level free

(setf (fdefinition 'ndb-free-object) (fdefinition 'ndbapi.types:free-foreign-object))


;;; with- macros

(defvar *ndb-init* nil)
(defvar *ndb-init-lock* (bt:make-lock "*ndb-init* lock"))

(defun ndb-begin ()
  "safely initializes NDB API (that is, only once)"
  (bt:with-lock-held (*ndb-init-lock*)
    (if (initialized-ndb-init-p *ndb-init*)
        *ndb-init*
        (progn
          (load-ndbapi)
          (setf *ndb-init* (ndb-begin%))))))

;; NDB-BEGIN and ENSURE-NDB-INIT are identical, as there really should be only one
;; ndb-init object for the whole application. The NDB API must to be initialized once only.
(setf (fdefinition 'ensure-ndb-init) (fdefinition 'ndb-begin))

(defun ndb-end ()
  "WARNING: only call when you are sure that the NDB API is not used anymore at all"
  (bt:with-lock-held (*ndb-init-lock*)
    ;; no need to check with INITIALIZED-NDB-INIT-P as NDB-FREE-OBJECT is safe to be called repeatedly
    (ndb-free-object *ndb-init*)))

(defmacro with-ndb-init ((&key there-is-only-one) &body body)
  "better just call ENSURE-NDB-INIT once and do not use WITH-NDB-INIT
If THERE-IS-ONLY-ONE is t, ndb-end is called at the end IFF with-ndb-init defined *ndb-init*"
  (let ((op (gensym "OP-")))
    `(flet ((,op () ,@body))
       (declare (dynamic-extent #',op))
       (call-with-ndb-init #',op :there-is-only-one ,there-is-only-one))))

(defun call-with-ndb-init (op &key there-is-only-one)
  ;;(print 'ndb-begin *trace-output*) (time)
  (if (initialized-ndb-init-p *ndb-init*)
      (funcall op)
      (progn
        (ndb-begin)
        (unwind-protect (funcall op)
          ;; explicit free of ndb-init object IFF we have introduced it,
          ;; that is *ndb-init* was previously unbound or not initialized.
          ;; Still this is quite unsafe. E.g., when two threads interleave with-ndb-init,
          ;; and the second thread uses the ndb-init of the first thread.
          ;;
          ;; Better use WITH-NDB-INIT just in simple example with one thread only,
          ;; and use ensure-ndb-init in all other cases. Call NDB-END then
          ;; at a time when the NDB API is not used at all, e.g. at program exit.
          ;;
          ;; Update: now you have to call as (with-ndb-init (:there-is-only-one t) ...)
          ;;   to get this behavior at all.
          ;;(print 'free/ndb-begin *trace-output*) (time)
          (when there-is-only-one
            #+ndbapi-verbose
            (when ndbapi.types:*ndbapi-verbose*
              (format *trace-output* "~&WITH-NDB-INIT: force freeing of *ndb-init* as :THERE-IS-ONLY-ONE ~a"
                      there-is-only-one))
            (ndb-end))))))

(defmacro with-ndb-cluster-connection ((var (connection-string) ;; do not support more args for now,
                                                                ;;; as new-ndb-cluster-connection has
                                                                ;;; no unique arity (extension of RonDB)
                                        &key name
                                             connect-args
                                             wait-until-ready-args
                                             (connect-and-wait-p t))
                                       &body body)
  "If VAR is bound, it will be reused;
(This only works for dynamic (special) variables, not for lexical ones.
 If the variable is just lexical, a new connection will be made.)
It is suggested to use the special variable ndbapi:*connection* in most cases.

Also sets up the cluster connection with a name
(using *default-connection-name* when not explicitly specified),
and also calling connect and wait-until-ready
unless CONNECT-AND-WAIT-P is explicitly set to NIL"
  (let ((op (gensym "OP-")))
    `(flet ((,op (,var) ,@body))
       (declare (dynamic-extent #',op))
       (call-with-ndb-cluster-connection #',op
                                         (when (boundp ',var) (symbol-value ',var))
                                         (list ,connection-string)
                                         ,name
                                         (list ,@connect-args)
                                         (list ,@wait-until-ready-args)
                                         ,connect-and-wait-p))))

(defun call-with-ndb-cluster-connection (op value args &optional name connect-args wait-until-ready-args (connect-and-wait-p t))
  (declare (dynamic-extent value args name connect-args wait-until-ready-args connect-and-wait-p))
  ;;(print 'new-ndb-cluster-connection *trace-output*) (time)
  (ensure-ndb-init)
  (if (valid-connection-p value)
      (funcall op value)
      (let ((value (apply #'new-ndb-cluster-connection *ndb-init* args)))
        (unwind-protect
             (progn
               (ndb-cluster-connection-set-name value name)
               (when connect-and-wait-p
                 (apply #'ndb-cluster-connection-connect value connect-args)
                 (apply #'ndb-cluster-connection-wait-until-ready value wait-until-ready-args))
               (funcall op value))
          ;;(print 'free/new-ndb-cluster-connection *trace-output*) (time)
          (ndb-free-object value)))))


;;; simple connection interace - begin

(defun connect (connection-string &key name
                                       connect-args
                                       wait-until-ready-args
                                       connection)
  "create new cluster connection and initialize it with name, connect and wait-until-ready;
optionally you can specify an existing connection with the keyword argument :connection,
if it refers to an existing and valid connection, it will be passed-through."
  (ensure-ndb-init)
  (if (ndbapi.i::valid-connection-p connection)
      connection
      (let ((value (new-ndb-cluster-connection *ndb-init* connection-string)))
        (ndb-cluster-connection-set-name value name)
        (apply #'ndb-cluster-connection-connect value connect-args)
        (apply #'ndb-cluster-connection-wait-until-ready value wait-until-ready-args)
        value)))

(defun disconnect (connection)
  "it is safe to be called repeatedly"
  (ndb-free-object connection))

(defvar *connection* nil)
(defvar *connection-lock* (bt:make-lock "*connection* lock"))

;; DISCONNECT doesn't hold the lock as it only invalidates the connection object,
;; but does not change the value of the *connection* variable.

(defun ensure-connection (connection-string &rest args
                                            &key name
                                                 connect-args
                                                 wait-until-ready-args)
  "safely initializes NDB API (that is, only once)
Works only with one connection in ndbapi:*connection* in total,
if you need more then one, use ndbapi:connect, which can also
pass-through an existing connection with the keyword argument :connection."
  (declare (ignorable name connect-args wait-until-ready-args))
  (bt:with-lock-held (*connection-lock*)
    (if (ndbapi.i::valid-connection-p *connection*)
        *connection*
        (setf *connection* (apply #'connect connection-string args)))))

(setf (macro-function 'with-connection)
      (macro-function 'with-ndb-cluster-connection))

;;; simple connection interace - end

(defvar *ndb* nil)

(defmacro with-ndb ((var (cluster-connection &rest more-new-ndb-args) &key max-no-of-transactions (ndb-init-p t)) &body body)
  "If VAR is bound, it will be reused;
(This only works for dynamic (special) variables, not for lexical ones.
 If the variable is just lexical, a new connection will be made.)
It is suggested to use the special variable ndbapi:*transaction* in most cases."
  (let ((op (gensym "OP-")))
    `(flet ((,op (,var) ,@body))
       (declare (dynamic-extent #',op))
       (call-with-ndb #',op
                      (when (boundp ',var) (symbol-value ',var))
                      ,cluster-connection
                      (list ,@more-new-ndb-args) ;; need to convert (foo), a call form, to (list foo), a list form
                      ,max-no-of-transactions
                      ,ndb-init-p))))

(defun call-with-ndb (op value cluster-connection &optional more-new-ndb-args max-no-of-transactions (ndb-init-p t))
  (declare (dynamic-extent value cluster-connection more-new-ndb-args max-no-of-transactions ndb-init-p))
  ;;(print 'new-ndb *trace-output*) (time)
  (if (valid-ndb-p value)
      (funcall op value)
      (let ((value (apply #'new-ndb cluster-connection more-new-ndb-args)))
        (unwind-protect
             (progn
               (when ndb-init-p
                 (apply #'ndb-init value (when max-no-of-transactions
                                           (list max-no-of-transactions))))
               (funcall op value))
          ;;(print 'free/new-ndb *trace-output*) (time)
          (ndb-free-object value)))))

(defvar *transaction* nil)

(defmacro with-ndb-transaction ((var &rest args) &body body)
  "If VAR is bound, it will be reused;
(This only works for dynamic (special) variables, not for lexical ones.
 If the variable is just lexical, a new connection will be made.)
It is suggested to use the special variable ndbapi:*transaction* in most cases."
  (let ((op (gensym "OP-")))
    `(flet ((,op (,var) ,@body))
       (declare (dynamic-extent #',op))
       (call-with-ndb-transaction #',op (when (boundp ',var) (symbol-value ',var)) ,@args))))

(defun call-with-ndb-transaction (op value &rest args)
  (declare (dynamic-extent args))
  (if (valid-object-p value)
      (funcall op value)
      (let ((value (apply #'ndb-start-transaction args)))
        (unwind-protect
             (funcall op value)
          ;; returns no value
          (ndbapi.ffi:ndb-close-transaction (ndbapi.ffi:ndb-transaction-get-ndb value) value)))))

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

(defmacro with-ndb-transaction-scan-table ((var (&rest open-args) (&rest close-args)) &body body)
  (let ((op (gensym "OP-")))
    `(flet ((,op (,var) ,@body))
       (declare (dynamic-extent #',op))
       (call-with-ndb-transaction-scan-table #',op
                                             :open-args (list ,@open-args)
                                             :close-args (list ,@close-args)))))

(defun call-with-ndb-transaction-scan-table (op &key open-args close-args)
  (declare (dynamic-extent open-args close-args))
  (let ((value (apply #'ndb-transaction-scan-table open-args)))
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
  (let ((value (apply #'new-ndb-interpreted-code *ndb-init* args)))
    (unwind-protect (funcall op value)
      (ndb-free-object value))))


;;; more advanced commands

(defun get-index-names (database-name table-name)
  (with-ndb (ndb (*connection* database-name))
    (ndbapi.types::with-foreign-struct (list (list :count 0) '(:struct ndbapi.ffi::list))

      (ndbapi.ffi::dictionary-list-indexes/swig-1 (ndb-get-dictionary ndb) list table-name)

      (loop with count = (cffi:foreign-slot-value list '(:struct ndbapi.ffi::list) :count)
            with elements = (cffi:foreign-slot-value list '(:struct ndbapi.ffi::list) :elements)
            with field = :name
            for i below count
            for element = (cffi:mem-aptr elements '(:struct ndbapi.ffi::element) i)
            collect (cffi:foreign-slot-value element '(:struct ndbapi.ffi::element) field)))))

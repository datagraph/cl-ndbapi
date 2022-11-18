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
  (error-string (%get-ndb-error object getter)))


;;; test :ndbapi.types for validity

(defun valid-object-p (object)
  (let ((pointer (if (typep object 'ndbapi.types::garbage-collected-class)
                     (ndbapi.types::foreign-pointer object)
                     object)))
    (and pointer
         (not (cffi:null-pointer-p pointer)))))

(defun initialized-ndb-init-p (object)
  (and object
       (typep object 'ndbapi.ffi::ndb-init)
       (ndbapi.types::initialized object)))


;;; interface

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

(make-interface-function ndb-begin% ;; rename ndb-init to ndb-begin to avoid conflict
                                    ;; also this fits to the complimentary function ndb-end
                         (ndbapi.ffi::ndb-init%)
                         #'initialized-ndb-init-p
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
                         (ndbapi.ffi::ndb-operation-get-value/swig-4 ndb-operation a-column a-value)
                         #'valid-object-p
                         "ndb-scan-operation-set-interpreted-code() failed: ~a"
                         (get-ndb-error (ndbapi.ffi:ndb-operation-get-ndb-transaction ndb-operation) #'ndbapi.ffi:ndb-transaction-get-ndb-error))

;; begin of pseudo-columns

(make-interface-function column-fragment
                         (ndbapi.ffi:column-fragment)
                         #'valid-object-p
                         "column-fragment(): null pointer returned.
Note: The pseudo columns are only available in the context of a cluster connection,
as they are created as a side-effect of making the cluster connection.")

(make-interface-function column-fragment-fixed-memory
                         (ndbapi.ffi:column-fragment-fixed-memory)
                         #'valid-object-p
                         "column-fragment-fixed-memory(): null pointer returned.
Note: The pseudo columns are only available in the context of a cluster connection,
as they are created as a side-effect of making the cluster connection.")

(make-interface-function column-fragment-varsized-memory
                         (ndbapi.ffi:column-fragment-varsized-memory)
                         #'valid-object-p
                         "column-fragment-varsized-memory(): null pointer returned.
Note: The pseudo columns are only available in the context of a cluster connection,
as they are created as a side-effect of making the cluster connection.")

(make-interface-function column-row-count
                         (ndbapi.ffi:column-row-count)
                         #'valid-object-p
                         "column-row-count(): null pointer returned.
Note: The pseudo columns are only available in the context of a cluster connection,
as they are created as a side-effect of making the cluster connection.")

(make-interface-function column-commit-count
                         (ndbapi.ffi:column-commit-count)
                         #'valid-object-p
                         "column-commit-count(): null pointer returned.
Note: The pseudo columns are only available in the context of a cluster connection,
as they are created as a side-effect of making the cluster connection.")

(make-interface-function column-row-size
                         (ndbapi.ffi:column-row-size)
                         #'valid-object-p
                         "column-row-size(): null pointer returned.
Note: The pseudo columns are only available in the context of a cluster connection,
as they are created as a side-effect of making the cluster connection.")

(make-interface-function column-range-no
                         (ndbapi.ffi:column-range-no)
                         #'valid-object-p
                         "column-range-no(): null pointer returned.
Note: The pseudo columns are only available in the context of a cluster connection,
as they are created as a side-effect of making the cluster connection.")

(make-interface-function column-disk-ref
                         (ndbapi.ffi:column-disk-ref)
                         #'valid-object-p
                         "column-disk-ref(): null pointer returned.
Note: The pseudo columns are only available in the context of a cluster connection,
as they are created as a side-effect of making the cluster connection.")

(make-interface-function column-records-in-range
                         (ndbapi.ffi:column-records-in-range)
                         #'valid-object-p
                         "column-records-in-range(): null pointer returned.
Note: The pseudo columns are only available in the context of a cluster connection,
as they are created as a side-effect of making the cluster connection.")

(make-interface-function column-rowid
                         (ndbapi.ffi:column-rowid)
                         #'valid-object-p
                         "column-rowid(): null pointer returned.
Note: The pseudo columns are only available in the context of a cluster connection,
as they are created as a side-effect of making the cluster connection.")

(make-interface-function column-row-gci
                         (ndbapi.ffi:column-row-gci)
                         #'valid-object-p
                         "column-row-gci(): null pointer returned.
Note: The pseudo columns are only available in the context of a cluster connection,
as they are created as a side-effect of making the cluster connection.")

(make-interface-function column-row-gci-64
                         (ndbapi.ffi:column-row-gci-64)
                         #'valid-object-p
                         "column-row-gci-64(): null pointer returned.
Note: The pseudo columns are only available in the context of a cluster connection,
as they are created as a side-effect of making the cluster connection.")

(make-interface-function column-row-author
                         (ndbapi.ffi:column-row-author)
                         #'valid-object-p
                         "column-row-author(): null pointer returned.
Note: The pseudo columns are only available in the context of a cluster connection,
as they are created as a side-effect of making the cluster connection.")

(make-interface-function column-any-value
                         (ndbapi.ffi:column-any-value)
                         #'valid-object-p
                         "column-any-value(): null pointer returned.
Note: The pseudo columns are only available in the context of a cluster connection,
as they are created as a side-effect of making the cluster connection.")

(make-interface-function column-copy-rowid
                         (ndbapi.ffi:column-copy-rowid)
                         #'valid-object-p
                         "column-copy-rowid(): null pointer returned.
Note: The pseudo columns are only available in the context of a cluster connection,
as they are created as a side-effect of making the cluster connection.")

(make-interface-function column-lock-ref
                         (ndbapi.ffi:column-lock-ref)
                         #'valid-object-p
                         "column-lock-ref(): null pointer returned.
Note: The pseudo columns are only available in the context of a cluster connection,
as they are created as a side-effect of making the cluster connection.")

(make-interface-function column-op-id
                         (ndbapi.ffi:column-op-id)
                         #'valid-object-p
                         "column-op-id(): null pointer returned.
Note: The pseudo columns are only available in the context of a cluster connection,
as they are created as a side-effect of making the cluster connection.")

(make-interface-function column-optimize
                         (ndbapi.ffi:column-optimize)
                         #'valid-object-p
                         "column-optimize(): null pointer returned.
Note: The pseudo columns are only available in the context of a cluster connection,
as they are created as a side-effect of making the cluster connection.")

(make-interface-function column-fragment-extent-space
                         (ndbapi.ffi:column-fragment-extent-space)
                         #'valid-object-p
                         "column-fragment-extent-space(): null pointer returned.
Note: The pseudo columns are only available in the context of a cluster connection,
as they are created as a side-effect of making the cluster connection.")

(make-interface-function column-fragment-free-extent-space
                         (ndbapi.ffi:column-fragment-free-extent-space)
                         #'valid-object-p
                         "column-fragment-free-extent-space(): null pointer returned.
Note: The pseudo columns are only available in the context of a cluster connection,
as they are created as a side-effect of making the cluster connection.")

;; end of pseudo-columns


;;; low-level free

(setf (fdefinition 'ndb-free-object) (fdefinition 'ndbapi.types:free-foreign-object))


;;; with- macros

(defvar *ndb-init* nil)

(defun ndb-begin ()
  "safely initializes NDB API (that is, only once)"
  (if (initialized-ndb-init-p *ndb-init*)
      *ndb-init*
      (setf *ndb-init* (ndb-begin%))))

(setf (fdefinition 'ensure-ndb-init) (fdefinition 'ndb-begin))

(defun ndb-end ()
  "WARNING: only call when you are sure that the NDB API is not used anymore at all"
  (when (initialized-ndb-init-p *ndb-init*)
    (ndb-free-object *ndb-init*)))

(defmacro with-ndb-init (() &body body)
  (let ((op (gensym "OP-")))
    `(flet ((,op () ,@body))
       (declare (dynamic-extent #',op))
       (call-with-ndb-init #',op))))

(defun call-with-ndb-init (op)
  ;;(print 'ndb-begin *trace-output*) (time)
  (if (initialized-ndb-init-p *ndb-init*)
      (funcall op)
      (progn
        (setf *ndb-init* (ndb-begin%))
        (unwind-protect (funcall op)
          ;; explicit free of ndb-init possible (WRONG!) but also not that important.
          ;; (freeing of ndb-init not that important as it does not bind any remote resources)
          ;; freeing the ndb-init will call ndb-end.
          ;; update: ndb-end has some internal counting (by counter ndb_init_called in ndb_end_interal)
          ;;   so it is okay to call ndb-init multiple times, and ndb-end as well. Only the very last
          ;;   ndb-end call, that reduces ndb_init_called to 0, actually cleans up.
          ;; if you free it, you should make a new object in each of your tasks,
          ;; as only that prevents that there ndb is still initialized as long as you use it.
          ;; update 2: THIS IS WRONG! I did not read the code in
          ;;   rondb/storage/ndb/src/common/util/ndb_init.cpp carefully enough. The counting
          ;;   in ndb_init_internal serves a different purpose, it will not help here. Calling
          ;;   ndb_init() multiple times will only initialize once. Calling ndb_end() will
          ;;   multiple times will also only deinitialize once but on the first call! There
          ;;   is not nested couting. So it is not allowed to call ndb_end while other ndb
          ;;   objects, cluster connecttion etc. are still used!
          ;;(print 'free/ndb-begin *trace-output*) (time)
          (ndb-end)))))

(defmacro with-ndb-cluster-connection ((var ndb-init connection-string
                                        &key name
                                             connect-args
                                             wait-until-ready-args)
                                       &body body)
  (let ((op (gensym "OP-")))
    `(flet ((,op (,var) ,@body))
       (declare (dynamic-extent #',op))
       (call-with-ndb-cluster-connection #',op
                                         (list ,ndb-init ,connection-string)
                                         ,name
                                         (list ,@connect-args)
                                         (list ,@wait-until-ready-args)))))

(defun call-with-ndb-cluster-connection (op args name connect-args wait-until-ready-args)
  (declare (dynamic-extent args))
  ;;(print 'new-ndb-cluster-connection *trace-output*) (time)
  (let ((value (apply #'new-ndb-cluster-connection args)))
    (unwind-protect
         (progn
           (when name
             (ndbapi.ffi::ndb-cluster-connection-set-name value name))
           (when connect-args
             (apply #'ndbapi:ndb-cluster-connection-connect value connect-args))
           (when wait-until-ready-args
             (apply #'ndbapi:ndb-cluster-connection-wait-until-ready value wait-until-ready-args))
           (funcall op value))
      ;;(print 'free/new-ndb-cluster-connection *trace-output*) (time)
      (ndb-free-object value))))


;;; connection object - begin

(defclass connection ()
  ((ndb-init :initarg :ndb-init :reader connection-ndb-init)
   (cluster-connection :initarg :cluster-connection :reader connection-cluster-connection)
   (open-p :initform t :accessor connection-open-p)))

(defvar *connection* nil)

(defun cluster-connect (ndb-init connection-string &key name
                                                        connect-args
                                                        wait-until-ready-args)
  (let ((value (new-ndb-cluster-connection ndb-init connection-string)))
    (when name
      (ndbapi.ffi::ndb-cluster-connection-set-name value name))
    (when connect-args
      (apply #'ndbapi:ndb-cluster-connection-connect value connect-args))
    (when wait-until-ready-args
      (apply #'ndbapi:ndb-cluster-connection-wait-until-ready value wait-until-ready-args))
    value))

(defun connect (connection-string &key name
                                       connect-args
                                       wait-until-ready-args)
  (let* ((ndb-init (ndb-begin)))
    (make-instance 'connection
                   :ndb-init ndb-init
                   :cluster-connection (cluster-connect ndb-init connection-string
                                                        :name name
                                                        :connect-args connect-args
                                                        :wait-until-ready-args wait-until-ready-args))))

(defun disconnect (connection)
  (when (connection-open-p connection)
    (prog1
        (ndb-free-object (connection-cluster-connection connection))
      (ndb-free-object (connection-ndb-init connection))
      (setf (connection-open-p connection) nil))))

;;; connection object - end

(defmacro with-ndb ((var &rest args) &body body)
  (let ((op (gensym "OP-")))
    `(flet ((,op (,var) ,@body))
       (declare (dynamic-extent #',op))
       (call-with-ndb #',op ,@args))))

(defun call-with-ndb (op &rest args)
  (declare (dynamic-extent args))
  ;;(print 'new-ndb *trace-output*) (time)
  (let ((value (apply #'new-ndb args)))
    (unwind-protect (funcall op value)
      ;;;;(print 'free/new-ndb *trace-output*) (time)
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

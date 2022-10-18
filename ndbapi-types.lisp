;;; Copyright (c) 2022 Max-Gerd Retzlaff <mgr@matroid.org>, Datagraph GmbH.
;;; Distributed under the terms of the GNU General Public License, Version 2.0,
;;; see file LICENSE in the top level directory of this repository.

(in-package :ndbapi.types)

(defvar *ndbapi-verbose* t)

(defun debug-print (object foreign-pointer do-free &optional (verbose *ndbapi-verbose*))
  (when verbose
    (format *trace-output* "~&Destructor called for ~a object: ~8,'0x (~:[do NOT free~;do free~])"
               object
               foreign-pointer
               do-free)
    (force-output *trace-output*)))

(cffi:define-foreign-type garbage-collected-type ()
  ((garbage-collect :reader garbage-collect :initform nil :initarg :garbage-collect)
   (lisp-class :allocation :class :reader lisp-class))
  (:actual-type :pointer))


(defclass garbage-collected-class ()
  ((foreign-pointer :reader foreign-pointer :initarg :foreign-pointer)
   (valid-cons :reader valid-cons :initarg :valid-cons)))


(defgeneric delete-foreign-object (class pointer))

(defmethod delete-foreign-object (class pointer)
  (error "Do not how to delete ~a object on pointer: ~8,'0x" class pointer))

(defmethod delete-foreign-object :before (class pointer)
  (when *ndbapi-verbose*
    (format *trace-output* "~&Calling DELETE for ~a object on pointer: ~8,'0x"
               class
               pointer)
    (force-output *trace-output*)))

(defmethod cffi:translate-to-foreign ((lisp-object garbage-collected-class)
                                      (foreign-type garbage-collected-type))
  (when *ndbapi-verbose*
    (format *trace-output* "~&translate ~a to ~a"
               (class-name (class-of lisp-object))
               (class-name (class-of foreign-type))))
  (foreign-pointer lisp-object))

(defgeneric delete-foreign-object% (class pointer valid-cons))

(defmethod free-foreign-object% ((class t) foreign-pointer valid-cons)
  (let ((do-free (and (not (cffi:null-pointer-p foreign-pointer))
                      (car valid-cons))))
    (debug-print class foreign-pointer do-free)
    (values (when do-free
                 (setf (car valid-cons) nil) ;; prevent double-free
                 (delete-foreign-object class foreign-pointer))
            do-free)))

(defgeneric free-foreign-object (object))

(defmethod free-foreign-object ((object garbage-collected-class))
  (multiple-value-bind (first-value do-free)
      (free-foreign-object% (class-name (class-of object))
                            (foreign-pointer object)
                            (valid-cons object))
    (when do-free
      (setf (slot-value object 'foreign-pointer) (cffi:null-pointer)))
    first-value))

(defmethod cffi:translate-from-foreign (foreign-pointer (foreign-type garbage-collected-type))
  (let* ((class (lisp-class foreign-type))
         (valid-cons (list t)) ;; hack: store validity of pointer in a cons cell
                               ;; that can be accessed from the finalizer
         (lisp-object (make-instance class :foreign-pointer foreign-pointer
                                           :valid-cons valid-cons)))
    (when *ndbapi-verbose*
      (format *trace-output* "~&translate ~a to ~a: ~8,'0x"
                 (class-name (class-of foreign-type))
                 class
                 foreign-pointer))
    (when (garbage-collect foreign-type)
      (sb-ext:finalize lisp-object (lambda () (free-foreign-object% class foreign-pointer valid-cons))))
    lisp-object))

;; macro

(defmacro make-concrete-foreign-type% (type class delete-fn)
  `(progn
     (cffi:define-foreign-type ,type (garbage-collected-type)
       ((lisp-class :allocation :class :initform ',class))
       (:simple-parser ,type))

     (defclass ,class (garbage-collected-class)
       ())

     (defmethod delete-foreign-object ((class (eql ',class)) pointer)
       (when *ndbapi-verbose*
         (format *trace-output* "~&Calling ~a" ',delete-fn)
         (force-output *trace-output*))
       (,delete-fn pointer))))

;; (make-concrete-foreign-type% ndb-type ndb delete-ndb)

(defmacro make-concrete-foreign-type (name)
  (let ((type (intern (concatenate 'string  (symbol-name name) "-" "TYPE") :ndbapi.ffi))
        (class name)
        (delete (intern (concatenate 'string "DELETE" "-" (symbol-name name)) :ndbapi.ffi)))
    `(make-concrete-foreign-type% ,type ,class ,delete)))

;; (make-concrete-foreign-type ndb)
;; ndb

#|
(cffi:define-foreign-type ndb-type (garbage-collected-type)
  ((lisp-class :initform 'ndb :allocation :class))
  (:simple-parser ndb-type))

(defclass ndb (garbage-collected-class)
  ())

(defmethod delete-foreign-object ((class (eql 'ndb)) pointer)
  (delete-ndb pointer))
|#

;; example

#|
(ndbapi.ffi::new-ndb/swig-1 *conn* *database-name*)
translate NDB-TYPE to NDB
#<NDBAPI.FFI::NDB {1006CE1393}>

(ndbapi.ffi::foreign-pointer *)
#.(SB-SYS:INT-SAP #X7FD1CC0011E0)

(cffi:convert-to-foreign ** 'ndbapi.ffi::NDB-TYPE)
translate NDB to NDB-TYPE
#.(SB-SYS:INT-SAP #X7FD1CC0011E0)

Destructor called for NDB object: #.(SB-SYS:INT-SAP #X7FD1CC0011E0) (do free)
calling DELETE for NDB object on pointer: #.(SB-SYS:INT-SAP #X7FD1CC0011E0)
|#

;; grep new_ ndbapi.lisp |awk '{print $4}'|sed 's/\/.*//g'|sed 's/"new_//g'| sed 's/"//g' |sed 's/_/-/g' | sort|uniq

(make-concrete-foreign-type #.(ndbapi.ffi::swig-lispify "Column" 'class :ndbapi.ffi))
(make-concrete-foreign-type #.(ndbapi.ffi::swig-lispify "Datafile" 'class :ndbapi.ffi))
(make-concrete-foreign-type #.(ndbapi.ffi::swig-lispify "Event" 'class :ndbapi.ffi))
(make-concrete-foreign-type #.(ndbapi.ffi::swig-lispify "ForeignKey" 'class :ndbapi.ffi))
(make-concrete-foreign-type #.(ndbapi.ffi::swig-lispify "HashMap" 'class :ndbapi.ffi))
(make-concrete-foreign-type #.(ndbapi.ffi::swig-lispify "Index" 'class :ndbapi.ffi))
(make-concrete-foreign-type #.(ndbapi.ffi::swig-lispify "LogfileGroup" 'class :ndbapi.ffi))
(make-concrete-foreign-type #.(ndbapi.ffi::swig-lispify "Ndb" 'class :ndbapi.ffi))
(make-concrete-foreign-type #.(ndbapi.ffi::swig-lispify "Ndb_cluster_connection" 'class :ndbapi.ffi))
(make-concrete-foreign-type #.(ndbapi.ffi::swig-lispify "Ndb_cluster_connection_node_iter" 'class :ndbapi.ffi))
(make-concrete-foreign-type #.(ndbapi.ffi::swig-lispify "NdbDataPrintFormat" 'class :ndbapi.ffi))
(make-concrete-foreign-type #.(ndbapi.ffi::swig-lispify "NdbDictionary" 'class :ndbapi.ffi))
(make-concrete-foreign-type #.(ndbapi.ffi::swig-lispify "NdbIndexStat" 'class :ndbapi.ffi))
(make-concrete-foreign-type #.(ndbapi.ffi::swig-lispify "NdbInterpretedCode" 'class :ndbapi.ffi))
(make-concrete-foreign-type #.(ndbapi.ffi::swig-lispify "NdbReceiver" 'class :ndbapi.ffi))
(make-concrete-foreign-type #.(ndbapi.ffi::swig-lispify "NdbRecordPrintFormat" 'class :ndbapi.ffi))
(make-concrete-foreign-type #.(ndbapi.ffi::swig-lispify "NdbScanFilter" 'class :ndbapi.ffi))
(make-concrete-foreign-type #.(ndbapi.ffi::swig-lispify "ObjectId" 'class :ndbapi.ffi))
(make-concrete-foreign-type #.(ndbapi.ffi::swig-lispify "OptimizeIndexHandle" 'class :ndbapi.ffi))
(make-concrete-foreign-type #.(ndbapi.ffi::swig-lispify "OptimizeTableHandle" 'class :ndbapi.ffi))
(make-concrete-foreign-type #.(ndbapi.ffi::swig-lispify "Table" 'class :ndbapi.ffi))
(make-concrete-foreign-type #.(ndbapi.ffi::swig-lispify "Tablespace" 'class :ndbapi.ffi))
(make-concrete-foreign-type #.(ndbapi.ffi::swig-lispify "Undofile" 'class :ndbapi.ffi))

;; special translation for ndb-end, which is similar but does not return an object
;; as a pointer but ndb_init and ndb_end are just c functions. ndb_init returns nothing
;; and ndb_init either 0 (= success) or 1 (= failed).

(cffi:define-foreign-type ndbapi.ffi::ndb-init-type (garbage-collected-type)
  ((lisp-class :allocation :class :initform 'ndbapi.ffi::ndb-init))
  (:actual-type :int)
  (:simple-parser ndbapi.ffi::ndb-init-type))

(defclass ndbapi.ffi::ndb-init ()
  ((initialized :reader initialized :initarg :initialized)
   (valid-cons :reader valid-cons :initarg :valid-cons)))

(defmethod delete-foreign-object ((class (eql 'ndbapi.ffi::ndb-init)) initialized)
  (declare (ignore initialized))
  (when *ndbapi-verbose*
    (format *trace-output* "~&Calling ~a" 'ndbapi.ffi::ndb-end)
    (force-output *trace-output*))
  (ndbapi.ffi:ndb-end 0))

(defmethod free-foreign-object% ((class (eql 'ndbapi.ffi::ndb-init)) initialized valid-cons)
  (let ((do-free (and initialized
                      (car valid-cons))))
    (debug-print class initialized do-free)
    (values (when do-free
                 (setf (car valid-cons) nil) ;; prevent double-free
                 (delete-foreign-object class initialized))
            do-free)))

(defmethod free-foreign-object ((object ndbapi.ffi::ndb-init))
  (multiple-value-bind (first-value do-free)
      (free-foreign-object% (class-name (class-of object))
                            (initialized object)
                            (valid-cons object))
    (when do-free
      (setf (slot-value object 'initialized) nil))
    first-value))

(defmethod cffi:translate-to-foreign ((lisp-object ndbapi.ffi::ndb-init)
                                      (foreign-type ndbapi.ffi::ndb-init-type))
  (when *ndbapi-verbose*
    (format *trace-output* "~&translate ~a to ~a"
               (class-name (class-of lisp-object))
               (class-name (class-of foreign-type))))
  ;; translate boolean to exit code
  (if (initialized lisp-object)
      0
      1))

(defmethod cffi:translate-from-foreign (exit-code (foreign-type ndbapi.ffi::ndb-init-type))
  (let* ((class (lisp-class foreign-type))
         (initialized (zerop exit-code)) ;; translate exit code to boolean
         (valid-cons (list t)) ;; hack: store validity of pointer in a cons cell
                               ;; that can be accessed from the finalizer
         (lisp-object (make-instance class :initialized initialized
                                           :valid-cons valid-cons)))
    (when *ndbapi-verbose*
      (format *trace-output* "~&translate ~a to ~a: ~a"
                 (class-name (class-of foreign-type))
                 class
                 initialized))
    (when (garbage-collect foreign-type)
      (sb-ext:finalize lisp-object (lambda () (free-foreign-object% class initialized valid-cons))))
    lisp-object))

;; foreign structs

(defmacro with-foreign-struct ((var initform type &optional alloc-params) &body body)
  `(let ((,var (cffi:convert-to-foreign ,initform ,type)))
     (unwind-protect
          (progn ,@body)
       (cffi:free-converted-object ,var ,type ,alloc-params))))

#+(or)
(ndbapi.types::with-foreign-struct (low (list :s 32 :p 42) '(:struct ndb.quads::tuple))
  (list
   (cffi::mem-aref low :uint32 0)
   (cffi::mem-aref low :uint32 1)))

;;; Copyright (c) 2022 Max-Gerd Retzlaff <mgr@matroid.org>, Datagraph GmbH.
;;; Distributed under the terms of the GNU General Public License, Version 2.0,
;;; see file LICENSE in the top level directory of this repository.

(cl:in-package :libndbapi)

(cl:defvar *ndbapi-verbose* cl:t)

(cl:defun debug (object foreign-pointer is-null-pinter cl:&optional (verbose *ndbapi-verbose*))
  (cl:when verbose
    (cl:format cl:*trace-output* "~&Destructor called for ~a object: ~8,'0x (~:[do free~;do NOT free~])"
               object
               foreign-pointer
               is-null-pinter)
    (cl:force-output cl:*trace-output*)))

(cffi:define-foreign-type garbage-collected-type ()
  ((garbage-collect :reader garbage-collect :initform cl:nil :initarg :garbage-collect)
   (lisp-class :allocation :class :reader lisp-class))
  (:actual-type :pointer))


(cl:defclass garbage-collected-class ()
  ((foreign-pointer :reader foreign-pointer :initarg :foreign-pointer)))


(cl:defgeneric delete-foreign-object (class pointer))

(cl:defmethod delete-foreign-object (class pointer)
  (cl:error "Do not how to delete ~a object on pointer: ~8,'0x" class pointer))

(cl:defmethod delete-foreign-object :before (class pointer)
  (cl:when *ndbapi-verbose*
    (cl:format cl:*trace-output* "~&calling DELETE for ~a object on pointer: ~8,'0x"
               class
               pointer)
    (cl:force-output cl:*trace-output*)))

(cl:defmethod cffi:translate-to-foreign ((lisp-object garbage-collected-class)
                                         (foreign-type garbage-collected-type))
  (cl:when *ndbapi-verbose*
    (cl:format cl:*trace-output* "~&translate ~a to ~a"
               (cl:class-name (cl:class-of lisp-object))
               (cl:class-name (cl:class-of foreign-type))))
  (foreign-pointer lisp-object))

(cl:defmethod cffi:translate-from-foreign (foreign-pointer (foreign-type garbage-collected-type))
  (cl:let* ((class (lisp-class foreign-type))
            (lisp-object (cl:make-instance class :foreign-pointer foreign-pointer)))
    (cl:when *ndbapi-verbose*
      (cl:format cl:*trace-output* "~&translate ~a to ~a"
                 (cl:class-name (cl:class-of foreign-type))
                 class))
    (cl:when (garbage-collect foreign-type)
      (sb-ext:finalize lisp-object (cl:lambda ()
                                     (cl:let ((is-null-pointer (cffi:null-pointer-p foreign-pointer)))
                                       (debug class foreign-pointer is-null-pointer)
                                       (cl:unless is-null-pointer
                                         (delete-foreign-object class foreign-pointer)
                                         ;; to be extra careful:
                                         (cl:setf foreign-pointer (cffi:null-pointer)))))))
    lisp-object))

;; macro

(cl:defmacro make-concrete-foreign-type% (type class delete-fn)
  `(cl:progn
     (cffi:define-foreign-type ,type (garbage-collected-type)
       ((lisp-class :initform ',class :allocation :class))
       (:simple-parser ,type))

     (cl:defclass ,class (garbage-collected-class)
       ())

     (cl:defmethod delete-foreign-object ((class (cl:eql ',class)) pointer)
       (,delete-fn pointer))))

;; (make-concrete-foreign-type% ndb-type ndb delete-ndb)

(cl:defmacro make-concrete-foreign-type (name)
  (cl:let ((type (cl:intern (cl:concatenate 'cl:string  (cl:symbol-name name) "-" "TYPE")))
           (class name)
           (delete (cl:intern (cl:concatenate 'cl:string "DELETE" "-" (cl:symbol-name name)))))
    `(make-concrete-foreign-type% ,type ,class ,delete)))

;; (make-concrete-foreign-type ndb)
;; ndb

#|
(cffi:define-foreign-type ndb-type (garbage-collected-type)
  ((lisp-class :initform 'ndb :allocation :class))
  (:simple-parser ndb-type))

(cl:defclass ndb (garbage-collected-class)
  ())

(cl:defmethod delete-foreign-object ((class (cl:eql 'ndb)) pointer)
  (delete-ndb pointer))
|#

;; example

#|
(libndbapi::new-ndb/swig-1 *conn* *database-name*)
translate NDB-TYPE to NDB
#<LIBNDBAPI::NDB {1006CE1393}>

(libndbapi::foreign-pointer *)
#.(SB-SYS:INT-SAP #X7FD1CC0011E0)

(cffi:convert-to-foreign ** 'libndbapi::NDB-TYPE)
translate NDB to NDB-TYPE
#.(SB-SYS:INT-SAP #X7FD1CC0011E0)

Destructor called for NDB object: #.(SB-SYS:INT-SAP #X7FD1CC0011E0) (do free)
calling DELETE for NDB object on pointer: #.(SB-SYS:INT-SAP #X7FD1CC0011E0)
|#

;; grep new_ ndbapi.lisp |awk '{print $4}'|sed 's/\/.*//g'|sed 's/"new_//g'| sed 's/"//g' |sed 's/_/-/g' | sort|uniq

(make-concrete-foreign-type #.(libndbapi::swig-lispify "Column" 'class))
(make-concrete-foreign-type #.(libndbapi::swig-lispify "Datafile" 'class))
(make-concrete-foreign-type #.(libndbapi::swig-lispify "Event" 'class))
(make-concrete-foreign-type #.(libndbapi::swig-lispify "ForeignKey" 'class))
(make-concrete-foreign-type #.(libndbapi::swig-lispify "HashMap" 'class))
(make-concrete-foreign-type #.(libndbapi::swig-lispify "Index" 'class))
(make-concrete-foreign-type #.(libndbapi::swig-lispify "LogfileGroup" 'class))
(make-concrete-foreign-type #.(libndbapi::swig-lispify "Ndb" 'class))
(make-concrete-foreign-type #.(libndbapi::swig-lispify "Ndb_cluster_connection" 'class))
(make-concrete-foreign-type #.(libndbapi::swig-lispify "Ndb_cluster_connection_node_iter" 'class))
(make-concrete-foreign-type #.(libndbapi::swig-lispify "NdbDataPrintFormat" 'class))
(make-concrete-foreign-type #.(libndbapi::swig-lispify "NdbDictionary" 'class))
(make-concrete-foreign-type #.(libndbapi::swig-lispify "NdbIndexStat" 'class))
(make-concrete-foreign-type #.(libndbapi::swig-lispify "NdbInterpretedCode" 'class))
(make-concrete-foreign-type #.(libndbapi::swig-lispify "NdbReceiver" 'class))
(make-concrete-foreign-type #.(libndbapi::swig-lispify "NdbRecordPrintFormat" 'class))
(make-concrete-foreign-type #.(libndbapi::swig-lispify "NdbScanFilter" 'class))
(make-concrete-foreign-type #.(libndbapi::swig-lispify "ObjectId" 'class))
(make-concrete-foreign-type #.(libndbapi::swig-lispify "OptimizeIndexHandle" 'class))
(make-concrete-foreign-type #.(libndbapi::swig-lispify "OptimizeTableHandle" 'class))
(make-concrete-foreign-type #.(libndbapi::swig-lispify "Table" 'class))
(make-concrete-foreign-type #.(libndbapi::swig-lispify "Tablespace" 'class))
(make-concrete-foreign-type #.(libndbapi::swig-lispify "Undofile" 'class))

;; special translation for ndb-end

(cffi:define-foreign-type ndb-init-type ()
  ((garbage-collect :reader garbage-collect :initform cl:nil :initarg :garbage-collect)
   (lisp-class :allocation :class :reader lisp-class :initform 'ndb-init))
  (:actual-type :int))

(cl:defclass ndb-init ()
  ((initialized :reader initialized :initarg :initialized)))

(cl:defmethod cffi:translate-to-foreign ((lisp-object ndb-init)
                                         (foreign-type ndb-init-type))
  (cl:when *ndbapi-verbose*
    (cl:format cl:*trace-output* "~&translate ~a to ~a"
               (cl:class-name (cl:class-of lisp-object))
               (cl:class-name (cl:class-of foreign-type))))
  (initialized lisp-object))

(cl:defmethod cffi:translate-from-foreign (exit-code (foreign-type ndb-init-type))
  (cl:let* ((class (lisp-class foreign-type))
            (initialized (cl:zerop exit-code))
            (lisp-object (cl:make-instance class :initialized initialized)))
    (cl:when *ndbapi-verbose*
      (cl:format cl:*trace-output* "~&translate ~a to ~a: ~a"
                 (cl:class-name (cl:class-of foreign-type))
                 class
                 initialized))
    (cl:when (garbage-collect foreign-type)
      (sb-ext:finalize lisp-object (cl:lambda ()
                                     (debug class exit-code initialized)
                                     (cl:when initialized
                                       (cl:when *ndbapi-verbose*
                                         (cl:format cl:*trace-output* "~&call NDB-END")
                                         (cl:force-output cl:*trace-output*))
                                       (ndb-end 0)))))
    lisp-object))

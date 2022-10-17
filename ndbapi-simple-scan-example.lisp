;;; Copyright (c) 2022 Max-Gerd Retzlaff <mgr@matroid.org>, Datagraph GmbH.
;;; Distributed under the terms of the GNU General Public License, Version 2.0,
;;; see file LICENSE in the top level directory of this repository.

(in-package :ndb.simple-scan)

;; translation of the example ndbapi_simple_scan that I created
;; on the basis of my ndbapi_simple_scan example (in c++)

#+(or)
(asdf:oos 'asdf:load-op :ndbapi)

(defvar *ndb*)

#|
 create table as:
   create table test
          (s int unsigned not null, p int unsigned not null,
           o int unsigned not null, g int unsigned not null,
           index gspo (g,s,p,o), index gpos (g,p,o,s), index gosp (g,o,s,p),
           index spog (s,p,o,g), index posg (p,o,s,g), index ospg (o,s,p,g));

 load data with:
    load data infile '/path/to/data.tsv' into table test;
|#

(defvar *ndb-init* nil)
(unless *ndb-init*
  (let ((ndb-init (ndbapi.ffi::ndb-init)))
    (assert (ndbapi.types::initialized ndb-init)
            ()
            "ndb-init failed")
    (setf *ndb-init* ndb-init)))

(defparameter *conn* (ndbapi.ffi::new-ndb-cluster-connection/swig-0 *ndb-init* "nl3:1186,nl3:1187"))

(assert (zerop (ndbapi.ffi::ndb-cluster-connection-connect/swig-0 *conn*
                                                                 4 ;; retries
                                                                 5 ;; delay between retries
                                                                 1 ;; verbose
                                                                 ))
        ()
        "Cluster management server was not ready within 30 secs")

(assert (zerop (ndbapi.ffi::ndb-cluster-connection-wait-until-ready/swig-0 *conn* 30 0))
        ()
        "Cluster was not ready within 30 secs.")

(defparameter *database-name* "mgr")
(defparameter *table-name* "test")

(defparameter *ndb* (ndbapi.ffi::new-ndb/swig-1 *conn* *database-name*))
(assert (ndbapi:valid-object-p *ndb*)
        ()
        "Create new NDB object failed")

(assert (zerop (ndbapi.ffi::ndb-init/swig-1 *ndb*))
        ()
        "Ndb.init() failed: ~a"
        (ndbapi:get-ndb-error *ndb* #'ndbapi.ffi::ndb-get-ndb-error/swig-0))

(defparameter *transaction* (ndbapi.ffi::ndb-start-transaction/swig-3 *ndb*))
(assert (ndbapi:valid-object-p *transaction*)
        ()
        "start-transaction() failed: ~a"
        (ndbapi:get-ndb-error *ndb*))

(defparameter *dict* (ndbapi.ffi::ndb-get-dictionary *ndb*))
(assert (ndbapi:valid-object-p *dict*)
        ()
        "get-dictionary() failed: ~a"
        (ndbapi:get-ndb-error *ndb*))

(defparameter *test-table* (ndbapi.ffi::dictionary-get-table/swig-0 *dict* *table-name*))
(assert (ndbapi:valid-object-p *test-table*)
        ()
        "get-table() failed: ~a"
        (ndbapi:get-ndb-error *dict* #'ndbapi.ffi::dictionary-get-ndb-error))

(defparameter *index-name* "gspo")

(defparameter *index* (ndbapi.ffi::dictionary-get-index/swig-0 *dict*
                                                              *index-name*
                                                              (ndbapi.ffi::table-get-name *test-table*)))
(assert (ndbapi:valid-object-p *index*)
        ()
        "get-index() failed: ~a"
        (ndbapi:get-ndb-error *dict* #'ndbapi.ffi::dictionary-get-ndb-error))

(defparameter *index-default-record* (ndbapi.ffi::index-get-default-record *index*))
(assert (ndbapi:valid-object-p *index-default-record*)
        ()
        "get-default-record() of index ~a failed"
        *index-name*)

(defparameter *test-table-default-record* (ndbapi.ffi::table-get-default-record *test-table*))
(assert (ndbapi:valid-object-p *test-table-default-record*)
        ()
        "get-default-record() of table ~a failed"
        *table-name*)

(defparameter *scan* (ndbapi.ffi::ndb-transaction-scan-index/swig-5 *transaction*
                                                                   *INDEX-DEFAULT-RECORD*
                                                                   *test-table-default-record*))
(assert (ndbapi:valid-object-p *scan*)
        ()
        "transaction-scan-index() failed: ~a"
        (ndbapi:get-ndb-error *transaction* #'ndbapi.ffi::ndb-transaction-get-ndb-error))

#+nil
(ndbapi.types::with-foreign-struct (low (list :s 662743 :p 2000000) '(:struct ndb.quads::tuple))
  (ndbapi.types::with-foreign-struct (high (list :s 662743 :p 2200000) '(:struct ndb.quads::tuple))
    (ndbapi.types::with-foreign-struct (bound (list :low-key low
                                                 :low-key-count ndb.quads::+tuple-count+
                                                 :low-inclusive t
                                                 :high-key high
                                                 :high-key-count ndb.quads::+tuple-count+
                                                 :high-inclusive t
                                                 :range-no 0)
                                           '(:struct ndbapi.ffi::index-bound))
      ;;(cffi:foreign-slot-value bound '(:struct ndbapi.ffi::index-bound) :low-inclusive)

      (assert (zerop (ndbapi.ffi::ndb-index-scan-operation-set-bound/swig-6 *scan* *index-default-record* bound))
        ()
        "set-bound() failed: ~a"
        (ndbapi:get-ndb-error *transaction* #'ndbapi.ffi::ndb-transaction-get-ndb-error))

      (assert (zerop (ndbapi.ffi::ndb-transaction-execute/swig-5 *transaction* :+NO-COMMIT+))
              ()
              "transactino-execute() failed: ~a"
              (ndbapi:get-ndb-error *transaction* #'ndbapi.ffi::ndb-transaction-get-ndb-error)))))

(ndbapi.types::with-foreign-struct (low (list :s 1106 :p 1105 :o 1105 :g 638)
                                     '(:struct ndb.quads::quad))
  (ndbapi.types::with-foreign-struct (high (list :s 1109 :p 1105 :o 1106 :g 1108)
                                     '(:struct ndb.quads::quad))
    (ndbapi.types::with-foreign-struct (bound (list :low-key low
                                                 :low-key-count ndb.quads::+quad-count+
                                                 :low-inclusive t
                                                 :high-key high
                                                 :high-key-count ndb.quads::+quad-count+
                                                 :high-inclusive t
                                                 :range-no 0)
                                           '(:struct ndbapi.ffi::index-bound))
      ;;(cffi:foreign-slot-value bound '(:struct ndbapi.ffi::index-bound) :low-inclusive)

      (assert (zerop (ndbapi.ffi::ndb-index-scan-operation-set-bound/swig-6 *scan* *index-default-record* bound))
        ()
        "set-bound() failed: ~a"
        (ndbapi:get-ndb-error *transaction* #'ndbapi.ffi::ndb-transaction-get-ndb-error))

      (assert (zerop (ndbapi.ffi::ndb-transaction-execute/swig-5 *transaction* :+NO-COMMIT+))
              ()
              "transactino-execute() failed: ~a"
              (ndbapi:get-ndb-error *transaction* #'ndbapi.ffi::ndb-transaction-get-ndb-error)))))



;;   // Check rc anyway

(defparameter *row-data* (cffi:foreign-alloc :pointer))

(loop for rc = (ndbapi.ffi::ndb-scan-operation-next-result/swig-3 *scan* *row-data* t nil)
      for j from 0
      while (zerop rc)
      for row = (cffi:convert-from-foreign (cffi:mem-aref *row-data* :pointer) '(:struct ndb.quads::quad))
      do (format t "~&row ~5d: ~{~12d~^, ~}" j (ndb.quads::quad-to-list row))
      finally (assert (= rc 1)
                      ()
                      "scan-operation-next-result() failed: ~a"
                      (ndbapi:get-ndb-error *transaction* #'ndbapi.ffi::ndb-transaction-get-ndb-error)))

(cffi:foreign-free *row-data*)
(setf *row-data* nil)

(ndbapi.ffi::ndb-scan-operation-close/swig-1 *scan* t) ;; no value

;; explit freeing (in correct order!)
(ndbapi.types::free-foreign-object *ndb*)
(ndbapi.types::free-foreign-object *conn*)
;; explicit free of *ndb-init* not implemented yet
;; (and not that important as it does not bind any remote resources)

(setf *ndb* nil)
(setf *conn* nil)

;; only call ndb-end at the very end when all objects are freed by GC
;; update: ndb-end has some internal counting (by counter ndb_init_called in ndb_end_interal)
;;   so it is okay to call ndb-init multiple times, and ndb-end as well. Only the very last
;;   end-end call, that reduces ndb_init_called to 0, actually cleans up.
(setf *ndb-init* nil)

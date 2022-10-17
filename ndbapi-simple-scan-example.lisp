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
  (let ((ndb-init (ndbapi:ndb-init)))
    (assert (ndbapi:initialized ndb-init)
            ()
            "ndb-init failed")
    (setf *ndb-init* ndb-init)))

(defparameter *conn* (ndbapi:new-ndb-cluster-connection *ndb-init* "nl3:1186,nl3:1187"))

(assert (zerop (ndbapi:ndb-cluster-connection-connect *conn*
                                                      4 ;; retries
                                                      5 ;; delay between retries
                                                      1 ;; verbose
                                                      ))
        ()
        "Cluster management server was not ready within 30 secs")

(assert (zerop (ndbapi:ndb-cluster-connection-wait-until-ready *conn* 30 0))
        ()
        "Cluster was not ready within 30 secs.")

(defparameter *database-name* "mgr")
(defparameter *table-name* "test")

(defparameter *ndb* (ndbapi:new-ndb *conn* *database-name*))
(assert (ndbapi:valid-object-p *ndb*)
        ()
        "Create new NDB object failed")

(assert (zerop (ndbapi:ndb-init-ndb *ndb*))
        ()
        "Ndb.init() failed: ~a"
        (ndbapi:get-ndb-error *ndb* #'ndbapi:ndb-get-ndb-error))

(defparameter *transaction* (ndbapi:ndb-start-transaction *ndb*))
(assert (ndbapi:valid-object-p *transaction*)
        ()
        "start-transaction() failed: ~a"
        (ndbapi:get-ndb-error *ndb*))

(defparameter *dict* (ndbapi:ndb-get-dictionary *ndb*))
(assert (ndbapi:valid-object-p *dict*)
        ()
        "get-dictionary() failed: ~a"
        (ndbapi:get-ndb-error *ndb*))

(defparameter *test-table* (ndbapi:dictionary-get-table *dict* *table-name*))
(assert (ndbapi:valid-object-p *test-table*)
        ()
        "get-table() failed: ~a"
        (ndbapi:get-ndb-error *dict* #'ndbapi:dictionary-get-ndb-error))

(defparameter *index-name* "gspo")

(defparameter *index* (ndbapi:dictionary-get-index *dict*
                                                   *index-name*
                                                   (ndbapi:table-get-name *test-table*)))
(assert (ndbapi:valid-object-p *index*)
        ()
        "get-index() failed: ~a"
        (ndbapi:get-ndb-error *dict* #'ndbapi:dictionary-get-ndb-error))

(defparameter *index-default-record* (ndbapi:index-get-default-record *index*))
(assert (ndbapi:valid-object-p *index-default-record*)
        ()
        "get-default-record() of index ~a failed"
        *index-name*)

(defparameter *test-table-default-record* (ndbapi:table-get-default-record *test-table*))
(assert (ndbapi:valid-object-p *test-table-default-record*)
        ()
        "get-default-record() of table ~a failed"
        *table-name*)

(defparameter *scan* (ndbapi:ndb-transaction-scan-index *transaction*
                                                        *INDEX-DEFAULT-RECORD*
                                                        *test-table-default-record*))
(assert (ndbapi:valid-object-p *scan*)
        ()
        "transaction-scan-index() failed: ~a"
        (ndbapi:get-ndb-error *transaction* #'ndbapi:ndb-transaction-get-ndb-error))

#+nil
(ndb.quads:with-foreign-quad (low (list :s 662743 :p 2000000))
  (ndb.quads:with-foreign-quad (high (list :s 662743 :p 2200000))
    (ndbapi:with-foreign-struct (bound (list :low-key low
                                                 :low-key-count ndb.quads:+tuple-count+
                                                 :low-inclusive t
                                                 :high-key high
                                                 :high-key-count ndb.quads:+tuple-count+
                                                 :high-inclusive t
                                                 :range-no 0)
                                           '(:struct ndbapi:index-bound))
      ;;(cffi:foreign-slot-value bound '(:struct ndbapi:index-bound) :low-inclusive)

      (assert (zerop (ndbapi:ndb-index-scan-operation-set-bound *scan* *index-default-record* bound))
        ()
        "set-bound() failed: ~a"
        (ndbapi:get-ndb-error *transaction* #'ndbapi:ndb-transaction-get-ndb-error))

      (assert (zerop (ndbapi:ndb-transaction-execute *transaction* :+NO-COMMIT+))
              ()
              "transactino-execute() failed: ~a"
              (ndbapi:get-ndb-error *transaction* #'ndbapi:ndb-transaction-get-ndb-error)))))

(ndb.quads:with-foreign-quad (low (ndb.quads:list-to-quad* 1106 1105 1105 638))
  (ndb.quads:with-foreign-quad (high (ndb.quads:list-to-quad* 1109 1105 1106 1108))
    (ndbapi:with-foreign-struct (bound (list :low-key low
                                             :low-key-count ndb.quads:+quad-count+
                                             :low-inclusive t
                                             :high-key high
                                             :high-key-count ndb.quads:+quad-count+
                                             :high-inclusive t
                                             :range-no 0)
                                       '(:struct ndbapi:index-bound))
      ;;(cffi:foreign-slot-value bound '(:struct ndbapi:index-bound) :low-inclusive)

      (assert (zerop (ndbapi:ndb-index-scan-operation-set-bound *scan* *index-default-record* bound))
        ()
        "set-bound() failed: ~a"
        (ndbapi:get-ndb-error *transaction* #'ndbapi:ndb-transaction-get-ndb-error))

      (assert (zerop (ndbapi:ndb-transaction-execute *transaction* :+NO-COMMIT+))
              ()
              "transactino-execute() failed: ~a"
              (ndbapi:get-ndb-error *transaction* #'ndbapi:ndb-transaction-get-ndb-error)))))

;;   // Check rc anyway

(cffi:with-foreign-pointer (row-data 1)
  (loop for rc = (ndbapi:ndb-scan-operation-next-result *scan* row-data t nil)
        for j from 0
        while (zerop rc)
        for row = (ndb.quads:convert-foreign-quad (cffi:mem-aref row-data :pointer))
        do (format t "~&row ~5d: ~{~12d~^, ~}" j (ndb.quads:quad-to-list row))
        finally (assert (= rc 1)
                        ()
                        "scan-operation-next-result() failed: ~a"
                        (ndbapi:get-ndb-error *transaction* #'ndbapi:ndb-transaction-get-ndb-error))))

(ndbapi:ndb-scan-operation-close *scan* t) ;; no value

;; explit freeing (in correct order!)
(ndbapi:free-foreign-object *ndb*)
(ndbapi:free-foreign-object *conn*)
;; explicit free of *ndb-init* not implemented yet
;; (and not that important as it does not bind any remote resources)

(setf *ndb* nil)
(setf *conn* nil)

;; only call ndb-end at the very end when all objects are freed by GC
;; update: ndb-end has some internal counting (by counter ndb_init_called in ndb_end_interal)
;;   so it is okay to call ndb-init multiple times, and ndb-end as well. Only the very last
;;   end-end call, that reduces ndb_init_called to 0, actually cleans up.
(setf *ndb-init* nil)

;;; Copyright (c) 2022 Max-Gerd Retzlaff <mgr@matroid.org>, Datagraph GmbH.
;;; Distributed under the terms of the GNU General Public License, Version 2.0,
;;; see file LICENSE in the top level directory of this repository.

(in-package :ndb.scan-count)

;; Warning: WIP not complete
;; based on the NDB cluster command ndb_select_count - Print Row Counts for NDB Tables
;; and its implementation in rondb/storage/ndb/tools/select_count.cpp

#+(or)
(asdf:oos 'asdf:load-op :ndbapi)

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

(defun scan-count/recattr (&key connection-string database-name table-name index-name
                                low (low-inclusive t)
                                high (high-inclusive t)
                                debug)
  (ndbapi:with-ndb-init (ndb-init)
    (ndbapi:with-ndb-cluster-connection (cluster-connection ndb-init connection-string)
      (ndbapi.ffi::ndb-cluster-connection-set-name cluster-connection "ndbapi-simple-scan")
      (ndbapi:ndb-cluster-connection-connect cluster-connection
                                             ;; retries:
                                             4
                                             ;; delay between retries:
                                             5
                                             ;; verbose:
                                             1)
      (ndbapi:ndb-cluster-connection-wait-until-ready cluster-connection
                                                      ;; timeout for first alive:
                                                      30
                                                      ;; timeout after first alive:
                                                      0)
      (ndbapi:with-ndb (ndb cluster-connection database-name)
        (ndbapi:ndb-init ndb)

        (let ((code-words 1))
          (cffi:with-foreign-pointer (code-space (* code-words (cffi:foreign-type-size :unsigned-int)))
            (ndbapi:with-ndb-interpreted-code (code ndb-init (cffi:null-pointer) code-space code-words)
              (ndbapi:ndb-interpreted-code-interpret-exit-last-row code)
              (ndbapi:ndb-interpreted-code-finalise code)

              (ndbapi:with-ndb-transaction (transaction ndb)
                (let* ((dict (ndbapi:ndb-get-dictionary ndb))
                       (table (ndbapi:dictionary-get-table dict table-name))
                       (index (ndbapi:dictionary-get-index dict
                                                           index-name
                                                           (ndbapi:table-get-name table)))
                       (scan-flags '(:+SF-MULTI-RANGE+)))
                  (ndbapi:with-ndb-transaction-get-ndb-index-scan-operation (scan (transaction index)
                                                                             ())
                    (ndbapi:ndb-index-scan-operation-read-tuples scan :+LM-READ+ scan-flags)
                    (ndbapi:ndb-scan-operation-set-interpreted-code scan code)

                    (cffi:with-foreign-object (records-in-range-ptr :uint32 4)
                      ;; set bounds for scan
                      (ndb.quads:with-foreign-quad (low-quad (ndb.quads:list-to-quad low))
                        (ndb.quads:with-foreign-quad (high-quad (ndb.quads:list-to-quad high))
                          ;; set lower bound with old setBound api
                          (loop with max = (1- (length low))
                                for i from 0 to max
                                ;; for value in low
                                for value-ptr = (cffi:mem-aptr low-quad :unsigned-int i)
                                for attr = (subseq index-name i (1+ i))
                                for type = (if (< i max)
                                               ;; "all but possibly the last bound must be nonstrict"
                                               :+BOUND-LE+
                                               (if low-inclusive
                                                   :+BOUND-LE+
                                                   :+BOUND-LT+))
                                do #+(or)(print (list 'low i attr type (cffi:mem-ref value-ptr :unsigned-int)) t)
                                   (ndbapi:ndb-index-scan-operation-set-bound/recattr scan attr type value-ptr))
                          ;; set upper bound with old setBound api
                          (loop with max = (1- (length high))
                                for i from 0 to max
                                ;; for value in high
                                for value-ptr = (cffi:mem-aptr high-quad :unsigned-int i)
                                for attr = (subseq index-name i (1+ i))
                                for type = (if (< i max)
                                               ;; "all but possibly the last bound must be nonstrict"
                                               :+BOUND-GE+
                                               (if high-inclusive
                                                   :+BOUND-GE+
                                                   :+BOUND-GT+))
                                do #+(or)(print (list 'high i attr type (cffi:mem-ref value-ptr :unsigned-int)) t)
                                   (ndbapi:ndb-index-scan-operation-set-bound/recattr scan attr type value-ptr))

                          ;; configure to get row count
                          (ndbapi:ndb-operation-get-value scan (ndbapi:column-records-in-range) records-in-range-ptr)

                          (ndbapi:ndb-transaction-execute transaction :+NO-COMMIT+)))

                      ;; // Check rc anyway

                      ;; do scan and print
                      (when debug
                        (format t "~&table: ~a" table-name))
                      (let ((total-row-count 0))
                        (loop for rc = (ndbapi:ndb-scan-operation-next-result scan t)
                              for j from 0
                              while (zerop rc)
                              for range-count = (cffi:mem-aref records-in-range-ptr :uint32 1)
                              do (when debug
                                   (let ((partition-count (cffi:mem-aref records-in-range-ptr :uint32 0))
                                         (before-count (cffi:mem-aref records-in-range-ptr :uint32 2))
                                         (after-count (cffi:mem-aref records-in-range-ptr :uint32 3)))
                                     (format t "~&~t#partition: ~8d~t#range: ~8d~t#before: ~8d~t#after: ~8d"
                                               partition-count range-count before-count after-count)))
                                 (incf total-row-count range-count)
                              finally (assert (= rc 1)
                                              ()
                                              "scan-operation-next-result() failed: ~a"
                                              (ndbapi:get-ndb-error transaction #'ndbapi:ndb-transaction-get-ndb-error)))
                        total-row-count))))))))))))

#+(or)
(let ((args (list :connection-string "nl3:1186,nl3:1187"
                  :database-name "mgr"
                  :table-name "test"
                  :index-name "gpos")))
  (list
   (apply #'ndb.simple-scan:simple-scan args)
   (apply #'ndb.scan-count:scan-count :debug t args)))
           
#+(or)
(let ((args (list :connection-string "nl3:1186,nl3:1187"
                  :database-name "mgr"
                  :table-name "test"
                  :index-name "gpos"
                  :low (list 1106 1105 1105 638) :low-inclusive t
                  :high (list 1109 1105 1106 1108) :high-inclusive t)))
  (list
   (apply #'ndb.simple-scan:simple-scan args)
   (apply #'ndb.scan-count:scan-count :debug t args)))

#+(or)
(let ((args (list :connection-string "nl3:1186,nl3:1187"
                  :database-name "mgr"
                  :table-name "test"
                  :index-name "gosp"
                  :low (list 662743 2000000) :low-inclusive t
                  :high (list 662743 2200000) :high-inclusive t)))
  (list
   (apply #'ndb.simple-scan:simple-scan args)
   (apply #'ndb.scan-count:scan-count :debug t args)))

#+(or)
(let ((args (list :connection-string "nl3:1186,nl3:1187"
                  :database-name "mgr"
                  :table-name "test"
                  :index-name "gpos"
                  :low (list 1109 1106 1137) :low-inclusive t
                  :high (list 2108202 1106 603481) :high-inclusive t)))
  (list
   (apply #'ndb.simple-scan:simple-scan args)
   (apply #'ndb.scan-count:scan-count :debug t args)))

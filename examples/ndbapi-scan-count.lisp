;;; Copyright (c) 2022 Max-Gerd Retzlaff <mgr@matroid.org>, Datagraph GmbH.
;;; Distributed under the terms of the GNU General Public License, Version 2.0,
;;; see file LICENSE in the top level directory of this repository.

(in-package :ndb.scan-count)

;; based on the NDB cluster command ndb_select_count - Print Row Counts for NDB Tables
;; and its implementation in rondb/storage/ndb/tools/select_count.cpp
;; but modified to the second value column records-in-range, which is the number
;; of rows in range, instead of colum row-count, as we want to do counts of
;; bounded scans not full table counts

;; what it does is:
;; 1. set up a small interpreted code so that for our scanning operation
;;    each each fragment (or partition) returns only one row (interpret-exit-last-row)
;; 2. in the scan options specify an extra get value for pseudo column records-in-range
;;    and give it space for the for uint32 values it will return as app-storage
;; 3. in the scan index call: specify a result-mask mask of 0 as :unsigned-char
;;    so that actually no columns of the underlying table will be extracted
;;    (Note: I hope that actually means that only the index is used and the
;;     underlying table are not accessed at all. That would be good.)
;; 4. ndbapi:ndb-scan-operation-next-result will have one round per partition
;;    now and each round will have the four values of records-in-range set;
;;    the second value will hold the "number of rows in the range"
;;    (We still have to call the three arguments version of
;;     NdbScanOperation::nextResult() with a pointer for the row-data
;;     as the second value, as the two argument versions are part of the
;;     recattr API and using those will lead to error: "Error with code 4284:
;;     Cannot mix NdbRecAttr and NdbRecord methods in one operation".
;;     So we have to specify the row-data pointer, but as we have configured
;;     0 as the result-mask no rows will actually get retrieved.)
;; 5. we get the number of rows in our range from each partition,
;;    summing them up will result in the total matching columns
;;
;; Warning: This gives only an estimate of the counts!
;; See rondb/storage/ndb/src/kernel/blocks/dbtux/DbtuxStat.cpp
;; explaining records-in-range as:
;;
;;  // RECORDS_IN_RANGE
;;
;;  /*
;;   * Estimate entries in range.  Scan is at first entry.  Search for last
;;   * entry i.e. start of descending scan.  Use the 2 positions to estimate
;;   * entries before and after the range.  Finally get entries in range by
;;   * subtracting from total.  Errors come from imperfectly balanced tree
;;   * and from uncommitted entries which differ only in tuple version.
;;   *
;;   * Returns 4 Uint32 values: 0) total entries 1) in range 2) before range
;;   * 3) after range.  1-3) are estimates and need not add up to 0).
;;   */
;;
;; Copyright of this extract of 12 lines of the RonDB source code
;; in the file rondb/storage/ndb/src/kernel/blocks/dbtux/DbtuxStat.cpp:
;;   Copyright (c) 2005, 2019, Oracle and/or its affiliates. All rights reserved.
;;
;; An exact count could be achieved by not using the interpreted code,
;; still extract no colums and just add up 1 per round of
;; ndbapi:ndb-scan-operation-next-result. Without interpret-exit-last-row
;; next-result will not be called once per partition but once per column.
;; This is actually implemented in simple-scan.lisp when calling
;; simple-scan:simple-scan with :just-count t in the argument list.

#+(or)
(asdf:oos 'asdf:load-op :ndbapi)

#|
 create table as:
   create table test
          (g int unsigned not null, s int unsigned not null,
           p int unsigned not null, o int unsigned not null,
           index gspo (g,s,p,o), index gpos (g,p,o,s), index gosp (g,o,s,p),
           index spog (s,p,o,g), index posg (p,o,s,g), index ospg (o,s,p,g),
           unique (g,s,p,o));

 load data with:
    load data infile '/path/to/data.tsv' into table test;
|#

(defun scan-count (&key connection-string database-name table-name index-name
                        bound-specs ;; list of specs: (&key low high (low-inclusive t) (high-inclusive t))
                        debug
                        (lock-mode :+LM-READ+) ;; might switch to: :+LM-COMMITTED-READ+ or other modes
                        )
  "WARNING: scan-count just gives an estimation of the matching rows.
If you need an exact count call simple-scan instead with :just-count t"
  (ndbapi:ensure-ndb-init)
  (ndbapi:with-ndb-cluster-connection (ndbapi:*connection*
                                       (connection-string)
                                       :name "ndbapi-scan-count"
                                       ;; :connect-args (;; retries:
                                       ;;                4
                                       ;;                ;; delay between retries:
                                       ;;                5
                                       ;;                ;; verbose:
                                       ;;                1)
                                       ;; :wait-until-ready-args (;; timeout for first alive:
                                       ;;                         30
                                       ;;                         ;; timeout after first alive:
                                       ;;                         0)
                                       )
    (ndbapi:with-ndb (ndb (ndbapi:*connection* database-name))

      (let ((code-words 1))
        (cffi:with-foreign-pointer (code-space (* code-words (cffi:foreign-type-size :unsigned-int)))
          (ndbapi.ic:with-code (code (cffi:null-pointer) code-space code-words)
            (ndbapi.ic:interpret-exit-last-row code)
            (ndbapi.ic:finalise code)

            (ndbapi:with-ndb-transaction (transaction ndb)
              (let* ((dict (ndbapi:ndb-get-dictionary ndb))
                     (table (ndbapi:dictionary-get-table dict table-name))
                     (index (ndbapi:dictionary-get-index dict
                                                         index-name
                                                         (ndbapi:table-get-name table)))
                     (index-default-record (ndbapi:index-get-default-record index))
                     (table-default-record (ndbapi:table-get-default-record table))
                     (scan-flags '(:+SF-MULTI-RANGE+)))

                (cffi:with-foreign-object (records-in-range-ptr :uint32 4)
                  (ndbapi:with-foreign-struct (extra-get-values (list :column (ndbapi:column-records-in-range)
                                                                      :app-storage records-in-range-ptr)
                                                                '(:struct ndbapi:get-value-spec))

                    (ndbapi:with-foreign-struct (scan-options (list :options-present '(:+SO-SCANFLAGS+
                                                                                       :+SO-GETVALUE+
                                                                                       :+SO-INTERPRETED+)
                                                                    :scan-flags scan-flags
                                                                    :extra-get-values extra-get-values
                                                                    :num-extra-get-values 1
                                                                    :interpreted-code (ndbapi:foreign-pointer code))
                                                              '(:struct ndbapi:scan-options))
                      ;;(break "~a" (cffi:convert-from-foreign scan-options '(:struct ndbapi:scan-options)))
                      (cffi:with-foreign-object (result-mask :unsigned-char)
                        (setf (cffi:mem-ref result-mask :unsigned-char) #b00000000)

                        (ndbapi:with-ndb-transaction-scan-index (scan (transaction
                                                                          index-default-record
                                                                          table-default-record
                                                                          lock-mode
                                                                          result-mask
                                                                          (cffi:null-pointer)
                                                                          scan-options
                                                                          (cffi:foreign-type-size
                                                                           '(:struct ndbapi:scan-options)))
                                                                 (t))

                          ;; set bounds for scan
                          (let ((bounds-count (length bound-specs)))
                            (cffi:with-foreign-objects ((low-quads '(:struct ndb.quads:quad) bounds-count)
                                                        (high-quads '(:struct ndb.quads:quad) bounds-count)
                                                        (bounds '(:struct ndbapi:index-bound) bounds-count))
                              (loop for bound-spec in bound-specs
                                    for i from 0
                                    do (destructuring-bind (&key low high (low-inclusive t) (high-inclusive t))
                                           bound-spec
                                         (setf (cffi:mem-aref low-quads '(:struct ndb.quads:quad) i) (ndb.quads:list-to-quad low)
                                               (cffi:mem-aref high-quads '(:struct ndb.quads:quad) i) (ndb.quads:list-to-quad high)
                                               (cffi:mem-aref bounds '(:struct ndbapi:index-bound) i)
                                               (list :low-key (cffi:mem-aptr low-quads '(:struct ndb.quads:quad) i)
                                                     :low-key-count (length low)
                                                     :low-inclusive low-inclusive
                                                     :high-key (cffi:mem-aptr high-quads '(:struct ndb.quads:quad) i)
                                                     :high-key-count (length high)
                                                     :high-inclusive high-inclusive
                                                     :range-no i))
                                         (let ((bound (cffi:mem-aptr bounds '(:struct ndbapi:index-bound) i)))
                                           (ndbapi:ndb-index-scan-operation-set-bound scan index-default-record bound))))
                              ;; execute transaction
                              (ndbapi:ndb-transaction-execute transaction :+NO-COMMIT+)))

                          ;; explicitly check for errors
                          ;; as there might still be errors even though the execute call itself was successful
                          ;; Update: this is done by ndbapi:ndb-transaction-execute already now (20221213 mgr).
                          ;;   (ndbapi:explicitly-check-for-transaction-error transaction)

                          ;; do scan and print
                          (when debug
                            (format t "~&table: ~a" table-name))
                          (let ((total-row-count 0))
                            (cffi:with-foreign-object (row-data :pointer)
                              (loop for rc = (ndbapi:ndb-scan-operation-next-result scan row-data t nil)
                                    for j from 0
                                    while (zerop rc)
                                    for range-count = (cffi:mem-aref records-in-range-ptr :uint32 1)
                                    ;;for row = (ndb.quads:convert-foreign-quad (cffi:mem-aref row-data :pointer))
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
                                                    (ndbapi:get-ndb-error transaction #'ndbapi:ndb-transaction-get-ndb-error))))
                            total-row-count))))))))))))))

#+(or)
(let ((args (list :connection-string "localhost:1186,localhost:1187"
                  :database-name "mgr"
                  :table-name "test"
                  :index-name "gspo")))
  (list
   (apply #'ndb.simple-scan:simple-scan args)
   (apply #'ndb.simple-scan:simple-scan :just-count t args)
   (apply #'ndb.scan-count:scan-count :debug t args)))

#+(or)
(let ((args (list :connection-string "localhost:1186,localhost:1187"
                  :database-name "mgr"
                  :table-name "test"
                  :index-name "ospg"
                  :bound-specs '((:low  (1106 1105 638 1105) :low-inclusive t
                                  :high (1109 1106 1108 1105) :high-inclusive t)))))
  (list
   (apply #'ndb.simple-scan:simple-scan args)
   (apply #'ndb.simple-scan:simple-scan :just-count t args)
   (apply #'ndb.scan-count:scan-count :debug t args)))

#+(or)
(let ((args (list :connection-string "localhost:1186,localhost:1187"
                  :database-name "mgr"
                  :table-name "test"
                  :index-name "gpos"
                  :bound-specs '((:low  (1105 2000000) :low-inclusive t
                                  :high (1105 2200000) :high-inclusive t)))))
  (list
   (apply #'ndb.simple-scan:simple-scan args)
   (apply #'ndb.simple-scan:simple-scan :just-count t args)
   (apply #'ndb.scan-count:scan-count :debug t args)))

#+(or)
(let ((args (list :connection-string "localhost:1186,localhost:1187"
                  :database-name "mgr"
                  :table-name "test"
                  :index-name "ospg"
                  :bound-specs '((:low (1109 1106 1137) :low-inclusive t
                                  :high (2108202 1106 603481) :high-inclusive t)))))
  (list
   (apply #'ndb.simple-scan:simple-scan args)
   (apply #'ndb.simple-scan:simple-scan :just-count t args)
   (apply #'ndb.scan-count:scan-count :debug t args)))

#+(or)
(let ((args (list :connection-string "localhost:1186,localhost:1187"
                  :database-name "mgr"
                  :table-name "test"
                  :index-name "ospg"
                  :bound-specs '((:low (1106 1105 638 1105) :low-inclusive t
                                  :high (1109 1106 1108 1105 ) :high-inclusive t)
                                 (:low  (600000) :low-inclusive nil
                                  :high (662743) :high-inclusive t)))))
  (list
   (apply #'ndb.simple-scan:simple-scan args)
   (apply #'ndb.simple-scan:simple-scan :just-count t args)
   (apply #'ndb.scan-count:scan-count :debug t args)))

;;; Copyright (c) 2022 Max-Gerd Retzlaff <mgr@matroid.org>, Datagraph GmbH.
;;; Distributed under the terms of the GNU General Public License, Version 2.0,
;;; see file LICENSE in the top level directory of this repository.

(in-package :ndb.simple-scan)

;; translation of the example ndbapi_simple_scan that I created
;; on the basis of my ndbapi_simple_scan example (in c++)

#+(or)
(asdf:oos 'asdf:load-op :ndbapi)

#|
 create table as:
   create table test
          (s int unsigned not null, p int unsigned not null,
           o int unsigned not null, g int unsigned not null,
           index gspo (g,s,p,o), index gpos (g,p,o,s), index gosp (g,o,s,p),
           index spog (s,p,o,g), index posg (p,o,s,g), index ospg (o,s,p,g),
           unique (s,p,o,g));

 load data with:
    load data infile '/path/to/data.tsv' into table test;
|#

(defun simple-scan (&key connection-string database-name table-name index-name
                         low (low-inclusive t)
                         high (high-inclusive t)
                         just-count)
  (ndbapi:ensure-ndb-init)
  (ndbapi:with-ndb-cluster-connection (ndbapi:*connection* (connection-string)
                                                           :name "ndbapi-scan-count")
    (ndbapi:with-ndb (ndb (ndbapi:*connection* database-name))

      (cffi:with-foreign-object (result-mask :unsigned-char)
        (setf (cffi:mem-ref result-mask :unsigned-char) #b00000000)

        (ndbapi:with-ndb-transaction (transaction ndb)
          (let* ((dict (ndbapi:ndb-get-dictionary ndb))
                 (table (ndbapi:dictionary-get-table dict table-name))
                 (index (ndbapi:dictionary-get-index dict
                                                     index-name
                                                     (ndbapi:table-get-name table)))
                 (index-default-record (ndbapi:index-get-default-record index))
                 (table-default-record (ndbapi:table-get-default-record table))
                 (scan-flags (remove nil (list (unless just-count
                                                 :+SF-ORDER-BY+)
                                               :+SF-MULTI-RANGE+))))
            (ndbapi:with-foreign-struct (scan-options (list :options-present :+SO-SCANFLAGS+
                                                            :scan-flags scan-flags)
                                                      '(:struct ndbapi:scan-options))
              ;;(break "~a" (cffi:convert-from-foreign scan-options '(:struct ndbapi:scan-options)))
              (ndbapi:with-ndb-transaction-scan-index (scan (transaction
                                                                index-default-record
                                                                table-default-record
                                                                :+LM-READ+
                                                                (if just-count
                                                                    result-mask
                                                                    (cffi:null-pointer))
                                                                (cffi:null-pointer)
                                                                scan-options
                                                                (cffi:foreign-type-size
                                                                 '(:struct ndbapi:scan-options)))
                                                       (t))

                ;; set bounds for scan
                (ndb.quads:with-foreign-quad (low-quad (ndb.quads:list-to-quad low))
                  (ndb.quads:with-foreign-quad (high-quad (ndb.quads:list-to-quad high))
                    (ndbapi:with-foreign-struct (bound (list :low-key low-quad
                                                             :low-key-count (length low)
                                                             :low-inclusive low-inclusive
                                                             :high-key high-quad
                                                             :high-key-count (length high)
                                                             :high-inclusive high-inclusive
                                                             :range-no 0)
                                                       '(:struct ndbapi:index-bound))
                      (ndbapi:ndb-index-scan-operation-set-bound scan index-default-record bound)
                      (ndbapi:ndb-transaction-execute transaction :+NO-COMMIT+))))

                ;; // Check rc anyway

                ;; do scan and print
                (unless just-count
                  (format t "~&table: ~a" table-name)
                  (format t "~&columns:   ~{~12@a~^, ~}" (list :subject :predicate :object :graph)))
                (let ((total-row-count 0))
                  (cffi:with-foreign-object (row-data :pointer)
                    (loop for rc = (ndbapi:ndb-scan-operation-next-result scan row-data t nil)
                          for j from 0
                          while (zerop rc)
                          do (unless just-count
                               (format t "~&row ~5d: ~{~12d~^, ~}" j
                                       (ndb.quads:quad-to-list
                                        (ndb.quads:convert-foreign-quad (cffi:mem-aref row-data :pointer)))))
                             (incf total-row-count)
                          finally (assert (= rc 1)
                                          ()
                                          "scan-operation-next-result() failed: ~a"
                                          (ndbapi:get-ndb-error transaction #'ndbapi:ndb-transaction-get-ndb-error))))
                  total-row-count)))))))))

#+(or)
(ndb.simple-scan:simple-scan :connection-string "nl3:1186,nl3:1187"
                             :database-name "mgr"
                             :table-name "test"
                             :index-name "gspo")
           
#+(or)
(ndb.simple-scan:simple-scan :connection-string "nl3:1186,nl3:1187"
                             :database-name "mgr"
                             :table-name "test"
                             :index-name "gspo"
                             :low (list 1106 1105 1105 638) :low-inclusive t
                             :high (list 1109 1105 1106 1108) :high-inclusive t)
#+(or)
(ndb.simple-scan:simple-scan :connection-string "nl3:1186,nl3:1187"
                             :database-name "mgr"
                             :table-name "test"
                             :index-name "gspo"
                             :low (list 662743 2000000) :low-inclusive t
                             :high (list 662743 2200000) :high-inclusive t)

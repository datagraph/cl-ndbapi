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
           index spog (s,p,o,g), index posg (p,o,s,g), index ospg (o,s,p,g));

 load data with:
    load data infile '/path/to/data.tsv' into table test;
|#

(defun simple-scan (&key connection-string database-name table-name index-name)
  (ndbapi:with-ndb-init (ndb-init)
    (ndbapi:with-ndb-cluster-connection (cluster-connection ndb-init connection-string)
      (ndbapi:ndb-cluster-connection-connect cluster-connection
                                             ;; retries:
                                             4
                                             ;; delay between retries:
                                             5
                                             ;; verbose:
                                             1)
      (ndbapi:ndb-cluster-connection-wait-until-ready cluster-connection 30 0)
      (unwind-protect
           (progn
             (ndbapi:with-ndb (ndb cluster-connection database-name)
               (ndbapi:ndb-init-ndb ndb)

               (ndbapi:with-ndb-transaction (transaction ndb)
                 (let* ((dict (ndbapi:ndb-get-dictionary ndb))
                        (table (ndbapi:dictionary-get-table dict table-name))
                        (index (ndbapi:dictionary-get-index dict
                                                            index-name
                                                            (ndbapi:table-get-name table)))
                        (index-default-record (ndbapi:index-get-default-record index))
                        (table-default-record (ndbapi:table-get-default-record table)))
                   (ndbapi:with-ndb-transaction-scan-index (scan transaction
                                                                 index-default-record
                                                                 table-default-record)
                                                           (t)

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

                           (ndbapi:ndb-index-scan-operation-set-bound scan index-default-record bound)

                           (ndbapi:ndb-transaction-execute transaction :+NO-COMMIT+))))

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

                           (ndbapi:ndb-index-scan-operation-set-bound scan index-default-record bound)
                           (ndbapi:ndb-transaction-execute transaction :+NO-COMMIT+))))

                     ;;   // Check rc anyway

                     (format t "~&table: ~a" table-name)
                     (format t "~&columns:   ~{~12@a~^, ~}" (list :subject :predicate :object :graph))
                     (cffi:with-foreign-pointer (row-data 1)
                       (loop for rc = (ndbapi:ndb-scan-operation-next-result scan row-data t nil)
                             for j from 0
                             while (zerop rc)
                             for row = (ndb.quads:convert-foreign-quad (cffi:mem-aref row-data :pointer))
                             do (format t "~&row ~5d: ~{~12d~^, ~}" j (ndb.quads:quad-to-list row))
                             finally (assert (= rc 1)
                                             ()
                                             "scan-operation-next-result() failed: ~a"
                                             (ndbapi:get-ndb-error transaction #'ndbapi:ndb-transaction-get-ndb-error)))))))))))))

#+(or)
(ndb.simple-scan::simple-scan :connection-string "nl3:1186,nl3:1187"
                              :database-name "mgr"
                              :table-name "test"
                              :index-name "gspo")
           

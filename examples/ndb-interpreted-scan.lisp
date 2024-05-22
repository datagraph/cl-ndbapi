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
   create table quads
          (g int unsigned not null, s int unsigned not null,
           p int unsigned not null, o int unsigned not null,
           visibility varbinary(28000) NOT NULL,
           primary key (g,s,p,o), index gpos (g,p,o,s), index gosp (g,o,s,p),
           index spog (s,p,o,g), index posg (p,o,s,g), index ospg (o,s,p,g));

or via graph store protocol to spocq:
  curl --ipv4 --http1.1 -k -X POST \
     -H Content-Type:application/json -H Accept:application/n-quads \
     --data-binary @- \
     -u :${ADMIN_TOKEN} \
     http://localhost:8101/system/accounts/mgr/repositories <<EOF
  {"repository": {"name": "scantest", "class": "rondb-revisioned-repository" } }
  EOF

 echo '{"repository": {"name": "scantest", "class": "rondb-revisioned-repository" }}' | \
   curl --ipv4 --http1.1 -k -X POST \
        -H Content-Type:application/json -H Accept:application/n-quads \
        --data-binary @- \
        -u :${ADMIN_TOKEN} \
        http://localhost:8101/system/accounts/mgr/repositories

  echo '<http://example.com/default-subject> <http://example.com/default-predicate> "default object" .' | \
    curl --ipv4 -X PUT \
          -H "Content-Type: application/n-quads" \
          --data-binary @- \
          -u :${TOKEN} \
          "http://localhost:8101/mgr/scantest/service"

  echo '<http://example.com/default-subject> <http://example.com/default-predicate> "named object" <http://dydra.com/named-graph> .' | \
    curl --ipv4 -X PUT \
          -H "Content-Type: application/n-quads" \
          --data-binary @- \
          -u :${TOKEN} \
          "http://localhost:8101/mgr/scantest/service"

  echo '<http://example.com/default-subject> <http://example.com/special-predicate> "another named object" <http://dydra.com/named-graph> .' | \
    curl --ipv4 -X PUT \
          -H "Content-Type: application/n-quads" \
          --data-binary @- \
          -u :${TOKEN} \
          "http://localhost:8101/mgr/scantest/service"

  echo '<http://example.com/default-subject> <http://example.com/default-predicate> "second object" .' | \
    curl --ipv4 -X POST \
          -H "Content-Type: application/n-quads" \
          --data-binary @- \
          -u :${TOKEN} \
          "http://localhost:8101/mgr/scantest/service"

|#

#+(or)
(ndb.simple-scan::interpreted-scan :connection-string "localhost"
                                   :database-name "mgr°scantest"
                                   :table-name "quads"
                                   :index-name "PRIMARY" )

#|
table: quads
columns:          GRAPH,      SUBJECT,    PREDICATE,       OBJECT
row     0:    745897444,    745897440,    745897441,    745897443,            8, #(4 0 0 0 5 0 0 0)
row     1:    745897444,    745897440,    745897445,    745897446,            4, #(5 0 0 0)
row     2:   4294967295,    745897440,    745897441,    745897442,           16, #(2 0 0 0 3 0 0 0 3 0 0 0 4 0 0 0)
row     3:   4294967295,    745897440,    745897441,    745897447,            4, #(6 0 0 0)
4
|#

(defun interpreted-scan (&key connection-string database-name table-name index-name
                         bound-specs ;; list of specs: (&key low high (low-inclusive t) (high-inclusive t))
                         just-count)
  (ndbapi:ensure-ndb-init)
  (ndbapi:with-ndb-cluster-connection (ndbapi:*connection* (connection-string)
                                                           :name "ndbapi-scan-count")
    (ndbapi:with-ndb (ndb (ndbapi:*connection* database-name))

      (ndbapi:with-ndb-transaction (transaction ndb)
        
        (cffi:with-foreign-objects ((result-mask :unsigned-char))
          (setf (cffi:mem-ref result-mask :unsigned-char) #b00000000)

          (let* ((dict (ndbapi:ndb-get-dictionary ndb))
                 (table (ndbapi:dictionary-get-table dict table-name))
                 (column (ndbapi.implementation::table-get-column table "o"))
                 ;;(attr-id (ndbapi.ffi::column-get-attr-id column))
                 (code-words 8)
                 (reg-1 1)
                 (reg-2 2))
            (cffi:with-foreign-pointer (code-space (* code-words (cffi:foreign-type-size :unsigned-int)))
              (ndbapi:with-ndb-interpreted-code (code table code-space code-words)
                (ndbapi.implementation::ndb-interpreted-code-load-const-u32 code reg-2 745897446)
                (ndbapi.implementation::ndb-interpreted-code-read-attr code reg-1 column)
                (ndbapi.implementation::ndb-interpreted-code-branch-eq code reg-1 reg-2 0)
                (ndbapi.implementation::ndb-interpreted-code-interpret-exit-nok code)
                (ndbapi.implementation::ndb-interpreted-code-def-label code 0)
                (ndbapi.implementation::ndb-interpreted-code-interpret-exit-ok code)
                (ndbapi:ndb-interpreted-code-finalise code)

                (let* ((index (ndbapi:dictionary-get-index dict
                                                           index-name
                                                           (ndbapi:table-get-name table)))
                       (index-default-record (ndbapi:index-get-default-record index))
                       (table-default-record (ndbapi:table-get-default-record table))
                       (scan-flags (remove nil (list (unless just-count
                                                       :+SF-ORDER-BY+)
                                                     :+SF-MULTI-RANGE+))))
                  (ndbapi:with-foreign-struct (scan-options (list :options-present '(:+SO-SCANFLAGS+
                                                                                     :+SO-INTERPRETED+)
                                                                  :scan-flags scan-flags
                                                                  :interpreted-code (ndbapi:foreign-pointer code))
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
                      (unless just-count
                        (format t "~&table: ~a" table-name)
                        (format t "~&columns:   ~{~12@a~^, ~}" (list :graph :subject :predicate :object)))
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
                        total-row-count))))))))))))

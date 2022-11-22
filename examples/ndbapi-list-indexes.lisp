;;; Copyright (c) 2022 Max-Gerd Retzlaff <mgr@matroid.org>, Datagraph GmbH.
;;; Distributed under the terms of the GNU General Public License, Version 2.0,
;;; see file LICENSE in the top level directory of this repository.

(in-package :ndb.list-indexes)

#|

Given the database as defined in the beginning commet of the
file ndbapi-simple-scan.lisp, here is a example session:

CL-USER> (ndb.list-indexes:list-indexes :connection-string "localhost:1186,localhost:1187" :database-name "mgr" :table-name "test")
((:ID 26 :NAME "spog" :SCHEMA "def" :DATABASE "sys" :TYPE :+OTY-ORDERED-INDEX+)
 (:ID 25 :NAME "gosp" :SCHEMA "def" :DATABASE "sys" :TYPE :+OTY-ORDERED-INDEX+)
 (:ID 23 :NAME "gspo" :SCHEMA "def" :DATABASE "sys" :TYPE :+OTY-ORDERED-INDEX+)
 (:ID 22 :NAME "s"    :SCHEMA "def" :DATABASE "sys" :TYPE :+OTY-ORDERED-INDEX+)
 (:ID 24 :NAME "gpos" :SCHEMA "def" :DATABASE "sys" :TYPE :+OTY-ORDERED-INDEX+)
 (:ID 27 :NAME "posg" :SCHEMA "def" :DATABASE "sys" :TYPE :+OTY-ORDERED-INDEX+)
 (:ID 28 :NAME "ospg" :SCHEMA "def" :DATABASE "sys" :TYPE :+OTY-ORDERED-INDEX+))

The type of the index with name "s" is a surprise.
I suspected it should return :+UniqueHashIndex+.

But Mysql also reports it as such:

mysql> show indexes from test where Key_name = "s";
+-------+------------+----------+--------------+-------------+-----------+-------------+----------+--------+------+------------+---------+---------------+---------+------------+
| Table | Non_unique | Key_name | Seq_in_index | Column_name | Collation | Cardinality | Sub_part | Packed | Null | Index_type | Comment | Index_comment | Visible | Expression |
+-------+------------+----------+--------------+-------------+-----------+-------------+----------+--------+------+------------+---------+---------------+---------+------------+
| test  |          0 | s        |            1 | s           | A         |        NULL |     NULL |   NULL |      | BTREE      |         |               | YES     | NULL       |
| test  |          0 | s        |            2 | p           | A         |        NULL |     NULL |   NULL |      | BTREE      |         |               | YES     | NULL       |
| test  |          0 | s        |            3 | o           | A         |        NULL |     NULL |   NULL |      | BTREE      |         |               | YES     | NULL       |
| test  |          0 | s        |            4 | g           | A         |          10 |     NULL |   NULL |      | BTREE      |         |               | YES     | NULL       |
+-------+------------+----------+--------------+-------------+-----------+-------------+----------+--------+------+------------+---------+---------------+---------+------------+
4 rows in set (0.01 sec)

|#

(defun list-indexes (&key connection-string database-name table-name)
  (ndbapi:ensure-ndb-init)
  (ndbapi:with-ndb-cluster-connection (ndbapi:*connection* (connection-string)
                                       :name "ndbapi-list-indexes")
    (ndbapi:with-ndb (ndb (ndbapi:*connection* database-name))

      (ndbapi.types::with-foreign-struct (list (list :count 0) '(:struct ndbapi.ffi::list))

        (ndbapi.ffi::dictionary-list-indexes/swig-1 (ndbapi::ndb-get-dictionary ndb) list table-name)

        (loop with count = (cffi:foreign-slot-value list '(:struct ndbapi.ffi::list) :count)
              with elements = (cffi:foreign-slot-value list '(:struct ndbapi.ffi::list) :elements)
              for i below count
              for element = (cffi:mem-aptr elements '(:struct ndbapi.ffi::element) i)
              collect (loop for field in '(:id :name :schema :database :type)
                            nconc (list field (cffi:foreign-slot-value element '(:struct ndbapi.ffi::element) field))))))))

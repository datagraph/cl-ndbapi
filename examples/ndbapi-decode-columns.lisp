;;; Copyright (c) 2022 Max-Gerd Retzlaff <mgr@matroid.org>, Datagraph GmbH.
;;; Distributed under the terms of the GNU General Public License, Version 2.0,
;;; see file LICENSE in the top level directory of this repository.

(in-package :ndb.decode-columns)

(defparameter *connection-string* "localhost:1186,localhost:1187"
  "default rondb connection-string")

(defparameter *connection-name* "decode columns"
  "default name for rondb connection")

(defun get-column-info (&key (connection-string *connection-string*)
                             (connection-name *connection-name*)
                             database-name
                             (table-name "quads"))
  "This function just uses the Column class of the ndbapi to gather information on the column,
see: https://dev.mysql.com/doc/ndbapi/en/ndb-column.html"
  (ndbapi:with-connection (ndbapi:*connection* (connection-string)
                           :name connection-name)
    (ndbapi:with-ndb (ndb (ndbapi:*connection* database-name))

      (cffi:with-foreign-object (result-mask :unsigned-char)
        (setf (cffi:mem-ref result-mask :unsigned-char) #b00000000)

        (ndbapi:with-ndb-transaction (transaction ndb)
          (declare (ignorable transaction))
          (let* ((dict (ndbapi:ndb-get-dictionary ndb))
                 (table (ndbapi:dictionary-get-table dict table-name))
                 (table-default-record (ndbapi:table-get-default-record table))
                 (columns (ndbapi.ffi::table-get-no-of-columns table)))
            (declare (ignorable table-default-record))

            (loop for c from 0 below columns
                  for column = (ndbapi.ffi::table-get-column/swig-1 table c)
                  collect (list c
                                :name (ndbapi.ffi::column-get-name column)
                                :array-type (cffi:foreign-enum-keyword 'ndbapi.ffi::array-type
                                                                       (cffi::pointer-address
                                                                        (ndbapi.ffi::column-get-array-type column)))
                                :size (ndbapi.ffi::column-get-size column)
                                :length (ndbapi.ffi::column-get-length column)
                                :size-in-bytes (ndbapi.ffi::column-get-size-in-bytes column)
                                :column-type (cffi:foreign-enum-keyword 'ndbapi.ffi::column-ctype
                                                                        (cffi::pointer-address
                                                                         (ndbapi.ffi::column-get-type column)))
                                :nullable (ndbapi.ffi::column-get-nullable column)
                                :attr-id (ndbapi.ffi::column-get-attr-id column)))))))))
#|
Note: See beginning of last comment in this file on how to create database ron019.

(ndb.decode-columns:get-column-info :database-name "ron019")
=>
((0 :NAME "g" :ARRAY-TYPE :+ARRAY-TYPE-FIXED+ :SIZE 4 :LENGTH 1 :SIZE-IN-BYTES 4 :COLUMN-TYPE :+UNSIGNED+ :NULLABLE NIL :ATTR-ID 0)
 (1 :NAME "s" :ARRAY-TYPE :+ARRAY-TYPE-FIXED+ :SIZE 4 :LENGTH 1 :SIZE-IN-BYTES 4 :COLUMN-TYPE :+UNSIGNED+ :NULLABLE NIL :ATTR-ID 1)
 (2 :NAME "p" :ARRAY-TYPE :+ARRAY-TYPE-FIXED+ :SIZE 4 :LENGTH 1 :SIZE-IN-BYTES 4 :COLUMN-TYPE :+UNSIGNED+ :NULLABLE NIL :ATTR-ID 2)
 (3 :NAME "o" :ARRAY-TYPE :+ARRAY-TYPE-FIXED+ :SIZE 4 :LENGTH 1 :SIZE-IN-BYTES 4 :COLUMN-TYPE :+UNSIGNED+ :NULLABLE NIL :ATTR-ID 3)
 (4 :NAME "visibility" :ARRAY-TYPE :+ARRAY-TYPE-MEDIUM-VAR+ :SIZE 1 :LENGTH 28000 :SIZE-IN-BYTES 28002 :COLUMN-TYPE :+LONGVARBINARY+ :NULLABLE T :ATTR-ID 4))
|#

(defun get-attribute-info (&key (connection-string *connection-string*)
                                (connection-name *connection-name*)
                                database-name
                                (table-name "quads"))
  "This function just uses the NdbDictionary class of the ndbapi
to gather information on the table's default-record,
see: https://dev.mysql.com/doc/ndbapi/en/ndb-ndbdictionary.html"
  (ndbapi:with-connection (ndbapi:*connection* (connection-string)
                           :name connection-name)
    (ndbapi:with-ndb (ndb (ndbapi:*connection* database-name))

      (cffi:with-foreign-object (result-mask :unsigned-char)
        (setf (cffi:mem-ref result-mask :unsigned-char) #b00000000)

        (ndbapi:with-ndb-transaction (transaction ndb)
          (declare (ignorable transaction))
          (let* ((dict (ndbapi:ndb-get-dictionary ndb))
                 (table (ndbapi:dictionary-get-table dict table-name))
                 (table-default-record (ndbapi:table-get-default-record table))
                 (columns (ndbapi.ffi::table-get-no-of-columns table)))
            (declare (ignorable columns))

            (let* ((attr-ids
                     (cffi:with-foreign-object (id :uint32)
                       (when (ndbapi.ffi::ndb-dictionary-get-first-attr-id table-default-record id)
                         (cons (cffi:mem-ref id :uint32)
                               (loop while (ndbapi.ffi::ndb-dictionary-get-next-attr-id table-default-record id)
                                     collect (cffi:mem-ref id :uint32))))))
                   (null-bit-offsets
                     (cffi:with-foreign-objects ((bytes :uint32)
                                                 (bit :uint32))
                       (loop for id in attr-ids
                             when (ndbapi.ffi::ndb-dictionary-get-null-bit-offset table-default-record id bytes bit)
                               collect (list id :present t
                                                :nullbit-byte-offset (cffi:mem-ref bytes :uint32)
                                                :nullbit-bit-in-byte (cffi:mem-ref bit :uint32))
                             else
                               collect (list id :present nil))
                       ))
                   (offsets
                     (cffi:with-foreign-objects ((offset :uint32))
                       (loop for id in attr-ids
                             when (ndbapi.ffi::ndb-dictionary-get-offset table-default-record id offset)
                               collect (list id :offset (cffi:mem-ref offset :uint32)))
                       )))
              (list :row-length (ndbapi.ffi::ndb-dictionary-get-record-row-length table-default-record)
                    :attr-ids attr-ids
                    :null-bit-offsets null-bit-offsets
                    :offsets offsets))))))))

#|
Note: See beginning of last comment in this file on how to create database ron019.

(ndb.decode-columns:get-attribute-info :database-name "ron019")
=>
(:ROW-LENGTH 28019
 :ATTR-IDS (0 1 2 3 4)
 :NULL-BIT-OFFSETS
   ((0 :PRESENT T :NULLBIT-BYTE-OFFSET 0 :NULLBIT-BIT-IN-BYTE 0)
    (1 :PRESENT T :NULLBIT-BYTE-OFFSET 0 :NULLBIT-BIT-IN-BYTE 0)
    (2 :PRESENT T :NULLBIT-BYTE-OFFSET 0 :NULLBIT-BIT-IN-BYTE 0)
    (3 :PRESENT T :NULLBIT-BYTE-OFFSET 0 :NULLBIT-BIT-IN-BYTE 0)
    (4 :PRESENT T :NULLBIT-BYTE-OFFSET 0 :NULLBIT-BIT-IN-BYTE 0))
 :OFFSETS
   ((0 :OFFSET 1) (1 :OFFSET 5) (2 :OFFSET 9) (3 :OFFSET 13) (4 :OFFSET 17)))
|#

(defun get-column-and-attribute-info (&key (connection-string *connection-string*)
                                           (connection-name *connection-name*)
                                           database-name
                                           (table-name "quads"))
  "This function combines the functions get-column-info and get-attribute-info,
and thus uses the ndbapi classes Column and NdbDictionary to collect
information on each column together with its offset in the table record
and its bit in the \"nullbyte\" if applicable.

It gives enough information to know how to decode the data that
the outRow pointer of NdbScanOperation::nextResult() points to,
see: https://dev.mysql.com/doc/ndbapi/en/ndb-ndbscanoperation.html#ndb-ndbscanoperation-nextresult
also in more complicated cases, that, is with NULL(able) columns.

All ndbapi examples of rondb / ndb cluster uses NOT NULL columns,
(either directly with NOT NULL or via PRIMARY KEY),
see https://dev.mysql.com/doc/ndbapi/en/ndb-examples.html"
  (ndbapi:with-connection (ndbapi:*connection* (connection-string)
                           :name connection-name)
    (ndbapi:with-ndb (ndb (ndbapi:*connection* database-name))

      (cffi:with-foreign-object (result-mask :unsigned-char)
        (setf (cffi:mem-ref result-mask :unsigned-char) #b00000000)

        (ndbapi:with-ndb-transaction (transaction ndb)
          (declare (ignorable transaction))
          (let* ((dict (ndbapi:ndb-get-dictionary ndb))
                 (table (ndbapi:dictionary-get-table dict table-name))
                 (table-default-record (ndbapi:table-get-default-record table))
                 (columns (ndbapi.ffi::table-get-no-of-columns table)))

            (list :row-length
                  (ndbapi.ffi::ndb-dictionary-get-record-row-length table-default-record)
                  :columns
                  (loop for c from 0 below columns
                        for column = (ndbapi.ffi::table-get-column/swig-1 table c)
                        collect (let ((attr-id (ndbapi.ffi::column-get-attr-id column)))
                                  (list c
                                        :name (ndbapi.ffi::column-get-name column)
                                        :column-type (cffi:foreign-enum-keyword 'ndbapi.ffi::column-ctype
                                                                                (cffi::pointer-address
                                                                                 (ndbapi.ffi::column-get-type column)))
                                        :array-type (cffi:foreign-enum-keyword 'ndbapi.ffi::array-type
                                                                               (cffi::pointer-address
                                                                                (ndbapi.ffi::column-get-array-type column)))
                                        :length (ndbapi.ffi::column-get-length column)
                                        :size (ndbapi.ffi::column-get-size column)
                                        :size-in-bytes (ndbapi.ffi::column-get-size-in-bytes column)
                                        :offset (cffi:with-foreign-objects ((offset :uint32))
                                                  (if (ndbapi.ffi::ndb-dictionary-get-offset table-default-record attr-id offset)
                                                      (cffi:mem-ref offset :uint32)
                                                      :error-attribute-not-found))
                                        :attr-id attr-id
                                        :nullable (when (ndbapi.ffi::column-get-nullable column)
                                                    (cffi:with-foreign-objects ((bytes :uint32)
                                                                                (bit :uint32))
                                                      (if (ndbapi.ffi::ndb-dictionary-get-null-bit-offset table-default-record attr-id bytes bit)
                                                          (list :nullbit-byte-offset (cffi:mem-ref bytes :uint32)
                                                                :nullbit-bit-in-byte (cffi:mem-ref bit :uint32))
                                                          :error-attribute-not-present)))))))))))))

#|

1.
   create database ron018;
   use ron018;
   create table quads
          (g int unsigned not null, s int unsigned not null,
           p int unsigned not null, o int unsigned not null,
           index gspo (g,s,p,o), index gpos (g,p,o,s), index gosp (g,o,s,p),
           index spog (s,p,o,g), index posg (p,o,s,g), index ospg (o,s,p,g),
           unique (g,s,p,o));

(ndb.decode-columns:get-column-and-attribute-info :database-name "ron018")
=>
(:ROW-LENGTH 16
 :COLUMNS ((0 :NAME "g" :COLUMN-TYPE :+UNSIGNED+
              :ARRAY-TYPE :+ARRAY-TYPE-FIXED+ :LENGTH 1 :SIZE 4 :SIZE-IN-BYTES 4 :OFFSET 0
              :ATTR-ID 0 :NULLABLE NIL)
           (1 :NAME "s" :COLUMN-TYPE :+UNSIGNED+
              :ARRAY-TYPE :+ARRAY-TYPE-FIXED+ :LENGTH 1 :SIZE 4 :SIZE-IN-BYTES 4 :OFFSET 4
              :ATTR-ID 1 :NULLABLE NIL)
           (2 :NAME "p" :COLUMN-TYPE :+UNSIGNED+
              :ARRAY-TYPE :+ARRAY-TYPE-FIXED+ :LENGTH 1 :SIZE 4 :SIZE-IN-BYTES 4 :OFFSET 8
              :ATTR-ID 2 :NULLABLE NIL)
           (3 :NAME "o" :COLUMN-TYPE :+UNSIGNED+
              :ARRAY-TYPE :+ARRAY-TYPE-FIXED+ :LENGTH 1 :SIZE 4 :SIZE-IN-BYTES 4 :OFFSET 12
              :ATTR-ID 3 :NULLABLE NIL)))


2.
   create database ron019;
   use ron019;
   create table quads
          (g int unsigned not null, s int unsigned not null,
           p int unsigned not null, o int unsigned not null,
           visibility varbinary(28000),
           index gspo (g,s,p,o), index gpos (g,p,o,s), index gosp (g,o,s,p),
           index spog (s,p,o,g), index posg (p,o,s,g), index ospg (o,s,p,g),
           unique (g,s,p,o));

(ndb.decode-columns:get-column-and-attribute-info :database-name "ron019")
=>
(:ROW-LENGTH 28019
 :COLUMNS ((0 :NAME "g" :COLUMN-TYPE :+UNSIGNED+
              :ARRAY-TYPE :+ARRAY-TYPE-FIXED+ :LENGTH 1 :SIZE 4 :SIZE-IN-BYTES 4 :OFFSET 1
              :ATTR-ID 0 :NULLABLE NIL)
           (1 :NAME "s" :COLUMN-TYPE :+UNSIGNED+
              :ARRAY-TYPE :+ARRAY-TYPE-FIXED+ :LENGTH 1 :SIZE 4 :SIZE-IN-BYTES 4 :OFFSET 5
              :ATTR-ID 1 :NULLABLE NIL)
           (2 :NAME "p" :COLUMN-TYPE :+UNSIGNED+
              :ARRAY-TYPE :+ARRAY-TYPE-FIXED+ :LENGTH 1 :SIZE 4 :SIZE-IN-BYTES 4 :OFFSET 9
              :ATTR-ID 2 :NULLABLE NIL)
           (3 :NAME "o" :COLUMN-TYPE :+UNSIGNED+
              :ARRAY-TYPE :+ARRAY-TYPE-FIXED+ :LENGTH 1 :SIZE 4 :SIZE-IN-BYTES 4 :OFFSET 13
              :ATTR-ID 3 :NULLABLE NIL)
           (4 :NAME "visibility" :COLUMN-TYPE :+LONGVARBINARY+
              :ARRAY-TYPE :+ARRAY-TYPE-MEDIUM-VAR+ :LENGTH 28000 :SIZE 1 :SIZE-IN-BYTES 28002 :OFFSET 17
              :ATTR-ID 4 :NULLABLE (:NULLBIT-BYTE-OFFSET 0 :NULLBIT-BIT-IN-BYTE 0))))


3.
   create database ron020;
   use ron020;
   create table quads
          (g int unsigned not null, s int unsigned not null,
           p int unsigned not null, o int unsigned not null,
           visibility varbinary(28000) not null,
           index gspo (g,s,p,o), index gpos (g,p,o,s), index gosp (g,o,s,p),
           index spog (s,p,o,g), index posg (p,o,s,g), index ospg (o,s,p,g),
           unique (g,s,p,o));

(ndb.decode-columns:get-column-and-attribute-info :database-name "ron020")
=>
(:ROW-LENGTH 28018
 :COLUMNS ((0 :NAME "g" :COLUMN-TYPE :+UNSIGNED+
              :ARRAY-TYPE :+ARRAY-TYPE-FIXED+ :LENGTH 1 :SIZE 4 :SIZE-IN-BYTES 4 :OFFSET 0
              :ATTR-ID 0 :NULLABLE NIL)
           (1 :NAME "s" :COLUMN-TYPE :+UNSIGNED+
              :ARRAY-TYPE :+ARRAY-TYPE-FIXED+ :LENGTH 1 :SIZE 4 :SIZE-IN-BYTES 4 :OFFSET 4
              :ATTR-ID 1 :NULLABLE NIL)
           (2 :NAME "p" :COLUMN-TYPE :+UNSIGNED+
              :ARRAY-TYPE :+ARRAY-TYPE-FIXED+ :LENGTH 1 :SIZE 4 :SIZE-IN-BYTES 4 :OFFSET 8
              :ATTR-ID 2 :NULLABLE NIL)
           (3 :NAME "o" :COLUMN-TYPE :+UNSIGNED+
              :ARRAY-TYPE :+ARRAY-TYPE-FIXED+ :LENGTH 1 :SIZE 4 :SIZE-IN-BYTES 4 :OFFSET 12
              :ATTR-ID 3 :NULLABLE NIL)
           (4 :NAME "visibility" :COLUMN-TYPE :+LONGVARBINARY+
              :ARRAY-TYPE :+ARRAY-TYPE-MEDIUM-VAR+ :LENGTH 28000 :SIZE 1 :SIZE-IN-BYTES 28002 :OFFSET 16
              :ATTR-ID 4 :NULLABLE NIL)))


4.
   create database ron021;
   use ron021;
   create table quads
          (g int unsigned not null, s int unsigned not null,
           p int unsigned not null, o int unsigned not null,
           stupid int,
           visibility varbinary(28000),
           index gspo (g,s,p,o), index gpos (g,p,o,s), index gosp (g,o,s,p),
           index spog (s,p,o,g), index posg (p,o,s,g), index ospg (o,s,p,g),
           unique (g,s,p,o));

(ndb.decode-columns:get-column-and-attribute-info :database-name "ron021")
=>
(:ROW-LENGTH 28023
 :COLUMNS ((0 :NAME "g" :COLUMN-TYPE :+UNSIGNED+
              :ARRAY-TYPE :+ARRAY-TYPE-FIXED+ :LENGTH 1 :SIZE 4 :SIZE-IN-BYTES 4 :OFFSET 1
              :ATTR-ID 0 :NULLABLE NIL)
           (1 :NAME "s" :COLUMN-TYPE :+UNSIGNED+
              :ARRAY-TYPE :+ARRAY-TYPE-FIXED+ :LENGTH 1 :SIZE 4 :SIZE-IN-BYTES 4 :OFFSET 5
              :ATTR-ID 1 :NULLABLE NIL)
           (2 :NAME "p" :COLUMN-TYPE :+UNSIGNED+
              :ARRAY-TYPE :+ARRAY-TYPE-FIXED+ :LENGTH 1 :SIZE 4 :SIZE-IN-BYTES 4 :OFFSET 9
              :ATTR-ID 2 :NULLABLE NIL)
           (3 :NAME "o" :COLUMN-TYPE :+UNSIGNED+
              :ARRAY-TYPE :+ARRAY-TYPE-FIXED+ :LENGTH 1 :SIZE 4 :SIZE-IN-BYTES 4 :OFFSET 13
              :ATTR-ID 3 :NULLABLE NIL)
           (4 :NAME "stupid" :COLUMN-TYPE :+INT+
              :ARRAY-TYPE :+ARRAY-TYPE-FIXED+ :LENGTH 1 :SIZE 4 :SIZE-IN-BYTES 4 :OFFSET 17
              :ATTR-ID 4 :NULLABLE (:NULLBIT-BYTE-OFFSET 0 :NULLBIT-BIT-IN-BYTE 0))
           (5 :NAME "visibility" :COLUMN-TYPE :+LONGVARBINARY+
              :ARRAY-TYPE :+ARRAY-TYPE-MEDIUM-VAR+ :LENGTH 28000 :SIZE 1 :SIZE-IN-BYTES 28002 :OFFSET 21
              :ATTR-ID 5 :NULLABLE (:NULLBIT-BYTE-OFFSET 0 :NULLBIT-BIT-IN-BYTE 1))))


5.
   create database ron022;
   use ron022;
   create table quads
          (g int unsigned not null, s int unsigned not null,
           p int unsigned not null, o int unsigned not null,
           stupid int,
           visibility varbinary(28000) not null,
           another int,
           index gspo (g,s,p,o), index gpos (g,p,o,s), index gosp (g,o,s,p),
           index spog (s,p,o,g), index posg (p,o,s,g), index ospg (o,s,p,g),
           unique (g,s,p,o));

(ndb.decode-columns:get-column-and-attribute-info :database-name "ron022")
=>
(:ROW-LENGTH 28027
 :COLUMNS ((0 :NAME "g" :COLUMN-TYPE :+UNSIGNED+
              :ARRAY-TYPE :+ARRAY-TYPE-FIXED+ :LENGTH 1 :SIZE 4 :SIZE-IN-BYTES 4 :OFFSET 1
              :ATTR-ID 0 :NULLABLE NIL)
           (1 :NAME "s" :COLUMN-TYPE :+UNSIGNED+
              :ARRAY-TYPE :+ARRAY-TYPE-FIXED+ :LENGTH 1 :SIZE 4 :SIZE-IN-BYTES 4 :OFFSET 5
              :ATTR-ID 1 :NULLABLE NIL)
           (2 :NAME "p" :COLUMN-TYPE :+UNSIGNED+
              :ARRAY-TYPE :+ARRAY-TYPE-FIXED+ :LENGTH 1 :SIZE 4 :SIZE-IN-BYTES 4 :OFFSET 9
              :ATTR-ID 2 :NULLABLE NIL)
           (3 :NAME "o" :COLUMN-TYPE :+UNSIGNED+
              :ARRAY-TYPE :+ARRAY-TYPE-FIXED+ :LENGTH 1 :SIZE 4 :SIZE-IN-BYTES 4 :OFFSET 13
              :ATTR-ID 3 :NULLABLE NIL)
           (4 :NAME "stupid" :COLUMN-TYPE :+INT+
              :ARRAY-TYPE :+ARRAY-TYPE-FIXED+ :LENGTH 1 :SIZE 4 :SIZE-IN-BYTES 4 :OFFSET 17
              :ATTR-ID 4 :NULLABLE
              (:NULLBIT-BYTE-OFFSET 0 :NULLBIT-BIT-IN-BYTE 0))
           (5 :NAME "visibility" :COLUMN-TYPE :+LONGVARBINARY+
              :ARRAY-TYPE :+ARRAY-TYPE-MEDIUM-VAR+ :LENGTH 28000 :SIZE 1 :SIZE-IN-BYTES 28002 :OFFSET 21
              :ATTR-ID 5 :NULLABLE NIL)
           (6 :NAME "another" :COLUMN-TYPE :+INT+
              :ARRAY-TYPE :+ARRAY-TYPE-FIXED+ :LENGTH 1 :SIZE 4 :SIZE-IN-BYTES 4 :OFFSET 28023
              :ATTR-ID 6 :NULLABLE (:NULLBIT-BYTE-OFFSET 0 :NULLBIT-BIT-IN-BYTE 1))))


6.
   create database ron023;
   use ron023;
   create table quads
          (g int unsigned not null, s int unsigned not null,
           p int unsigned not null, o int unsigned not null,
           stupid int,
           visibility varbinary(28000),
           another int,
           index gspo (g,s,p,o), index gpos (g,p,o,s), index gosp (g,o,s,p),
           index spog (s,p,o,g), index posg (p,o,s,g), index ospg (o,s,p,g),
           unique (g,s,p,o));

(ndb.decode-columns:get-column-and-attribute-info :database-name "ron023")
=>
(:ROW-LENGTH 28027
 :COLUMNS ((0 :NAME "g" :COLUMN-TYPE :+UNSIGNED+
              :ARRAY-TYPE :+ARRAY-TYPE-FIXED+ :LENGTH 1 :SIZE 4 :SIZE-IN-BYTES 4 :OFFSET 1
              :ATTR-ID 0 :NULLABLE NIL)
           (1 :NAME "s" :COLUMN-TYPE :+UNSIGNED+
              :ARRAY-TYPE :+ARRAY-TYPE-FIXED+ :LENGTH 1 :SIZE 4 :SIZE-IN-BYTES 4 :OFFSET 5
              :ATTR-ID 1 :NULLABLE NIL)
           (2 :NAME "p" :COLUMN-TYPE :+UNSIGNED+
              :ARRAY-TYPE :+ARRAY-TYPE-FIXED+ :LENGTH 1 :SIZE 4 :SIZE-IN-BYTES 4 :OFFSET 9
              :ATTR-ID 2 :NULLABLE NIL)
           (3 :NAME "o" :COLUMN-TYPE :+UNSIGNED+
              :ARRAY-TYPE :+ARRAY-TYPE-FIXED+ :LENGTH 1 :SIZE 4 :SIZE-IN-BYTES 4 :OFFSET 13
              :ATTR-ID 3 :NULLABLE NIL)
           (4 :NAME "stupid" :COLUMN-TYPE :+INT+
              :ARRAY-TYPE :+ARRAY-TYPE-FIXED+ :LENGTH 1 :SIZE 4 :SIZE-IN-BYTES 4 :OFFSET 17
              :ATTR-ID 4 :NULLABLE
              (:NULLBIT-BYTE-OFFSET 0 :NULLBIT-BIT-IN-BYTE 0))
           (5 :NAME "visibility" :COLUMN-TYPE :+LONGVARBINARY+
              :ARRAY-TYPE :+ARRAY-TYPE-MEDIUM-VAR+ :LENGTH 28000 :SIZE 1 :SIZE-IN-BYTES 28002 :OFFSET 21
              :ATTR-ID 5 :NULLABLE (:NULLBIT-BYTE-OFFSET 0 :NULLBIT-BIT-IN-BYTE 1))
           (6 :NAME "another" :COLUMN-TYPE :+INT+
              :ARRAY-TYPE :+ARRAY-TYPE-FIXED+ :LENGTH 1 :SIZE 4 :SIZE-IN-BYTES 4 :OFFSET 28023
              :ATTR-ID 6 :NULLABLE (:NULLBIT-BYTE-OFFSET 0 :NULLBIT-BIT-IN-BYTE 2))))

|#

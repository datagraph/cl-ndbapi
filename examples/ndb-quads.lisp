;;; Copyright (c) 2022 Max-Gerd Retzlaff <mgr@matroid.org>, Datagraph GmbH.
;;; Distributed under the terms of the GNU General Public License, Version 2.0,
;;; see file LICENSE in the top level directory of this repository.

(in-package :ndb.quads)

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

(cffi:defcstruct quad
  (:s :unsigned-int)
  (:p :unsigned-int)
  (:o :unsigned-int)
  (:g :unsigned-int))

(cffi:defcstruct triple
  (:s :unsigned-int)
  (:p :unsigned-int)
  (:o :unsigned-int))

(cffi:defcstruct tuple
  (:s :unsigned-int)
  (:p :unsigned-int))

(cffi:defcstruct single
  (:s :unsigned-int))

(defconstant +quad-size+ (cffi:foreign-type-size '(:struct ndb.quads::quad)))
(defconstant +triple-size+ (cffi:foreign-type-size '(:struct ndb.quads::triple)))
(defconstant +tuple-size+ (cffi:foreign-type-size '(:struct ndb.quads::tuple)))
(defconstant +single-size+ (cffi:foreign-type-size '(:struct ndb.quads::single)))

(defconstant +quad-count+ (/ +quad-size+ +single-size+))
(defconstant +triple-count+ (/ +triple-size+ +single-size+))
(defconstant +tuple-count+ (/ +tuple-size+ +single-size+))
(defconstant +single-count+ (/ +single-size+ +single-size+))

#|
(defparameter *test* (cffi:convert-to-foreign (list :s 32 :p 42) '(:struct ndb.quads::tuple)))
*TEST*
(cffi:foreign-slot-value *test* '(:struct ndb.quads::tuple) :s)
32
(cffi:foreign-slot-value *test* '(:struct ndb.quads::tuple) :p)
42
(cffi::mem-aref *test* :uint32 0)
32
(cffi::mem-aref *test* :uint32 1)
42
(cffi:convert-from-foreign *test* '(:struct ndb.quads::tuple))
(:P 42 :S 32)
(cffi::free-converted-object *test* '(:struct ndb.quads::tuple) t)
|#

#|
(ndbapi.types::with-foreign-struct (high (list :s 1109 :p 1105 :o 1106 :g 1108)
                                         '(:struct ndb.quads::quad))
  (cffi:convert-from-foreign high '(:struct ndb.quads::quad)))
(:G 1108 :O 1106 :P 1105 :S 1109)
|#

(defmacro with-foreign-quad ((var initform &optional alloc-params) &body body)
  `(ndbapi:with-foreign-struct (,var
                                ,initform
                                '(:struct ndb.quads:quad)
                                ,alloc-params)
    ,@body))

(defun list-to-quad (list)
  "take spog list and return as quad (given as property list)"
  (destructuring-bind (s p o g) list
    (list :s s :p p :o o :g g)))

(defun list-to-quad* (&rest list)
  (list-to-quad list))

(defun quad-to-list (quad)
  "deconstruct quad (given as property list) and return as spog list"
  (destructuring-bind (&key s p o g) quad
    (list s p o g)))

(defun quad-to-list* (&rest quad)
  (quad-to-list quad))

(defun convert-foreign-quad (sap)
  (cffi:convert-from-foreign sap '(:struct ndb.quads:quad)))

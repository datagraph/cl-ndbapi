;;; Copyright (c) 2022 Max-Gerd Retzlaff <mgr@matroid.org>, Datagraph GmbH.
;;; Distributed under the terms of the GNU General Public License, Version 2.0,
;;; see file LICENSE in the top level directory of this repository.

(cl:in-package :libndbapi)

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

(cl:defconstant +quad-size+ (cffi:foreign-type-size '(:struct libndbapi::quad)))
(cl:defconstant +triple-size+ (cffi:foreign-type-size '(:struct libndbapi::triple)))
(cl:defconstant +tuple-size+ (cffi:foreign-type-size '(:struct libndbapi::tuple)))
(cl:defconstant +single-size+ (cffi:foreign-type-size '(:struct libndbapi::single)))

(cl:defconstant +quad-count+ (cl:/ +quad-size+ +single-size+))
(cl:defconstant +triple-count+ (cl:/ +triple-size+ +single-size+))
(cl:defconstant +tuple-count+ (cl:/ +tuple-size+ +single-size+))
(cl:defconstant +single-count+ (cl:/ +single-size+ +single-size+))

#|
(defparameter *test* (cffi:convert-to-foreign (list 'libndbapi::s 32 'libndbapi::p 42) '(:struct libndbapi::tuple)))
*TEST*
(cffi:foreign-slot-value *test* '(:struct libndbapi::tuple) 'libndbapi::s)
32
(cffi:foreign-slot-value *test* '(:struct libndbapi::tuple) 'libndbapi::p)
42
(cffi::mem-aref *test* :uint32 0)
32
(cffi::mem-aref *test* :uint32 1)
42
(cffi:convert-from-foreign *test* '(:struct libndbapi::tuple))
(LIBNDBAPI::P 42 LIBNDBAPI::S 32)
(cffi::free-converted-object *test* '(:struct libndbapi::tuple) t)
|#

(cl:defmacro with-foreign-struct ((var initform type cl:&optional alloc-params) cl:&body body)
  `(cl:let ((,var (cffi:convert-to-foreign ,initform ,type)))
     (cl:unwind-protect
          (cl:progn ,@body)
       (cffi:free-converted-object ,var ,type ,alloc-params))))

#+(cl:or)
(libndbapi::with-foreign-struct (low (list 'libndbapi::s 32 'libndbapi::p 42) '(:struct libndbapi::tuple))
  (cl:list
   (cffi::mem-aref low :uint32 0)
   (cffi::mem-aref low :uint32 1)))

(cl:defun quad-to-list (quad)
  "deconstruct quad and return as spog list"
  (cl:destructuring-bind (cl:&key s p o g) quad
    (cl:list s p o g)))

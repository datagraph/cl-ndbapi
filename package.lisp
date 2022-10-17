;;; Copyright (c) 2022 Max-Gerd Retzlaff <mgr@matroid.org>, Datagraph GmbH.
;;; Distributed under the terms of the GNU General Public License, Version 2.0,
;;; see file LICENSE in the top level directory of this repository.

(in-package :cl-user)

(defpackage :ndbapi.types
  (:use :cl)
  (:documentation "The low-level C++ NDB API interface (for RonDB)."))

(defpackage :ndbapi.ffi
  (:use #| :cl |#)
  (:export :ndb-end)
  (:documentation "The low-level C++ NDB API interface (for RonDB)."))

(defpackage :ndbapi.implementation
  (:use :cl)
  (:nicknames :ndbapi.i)
  (:documentation "A higher level NDB API interface (for RonDB)."))

;; defpackage :ndbapi in file ndbapi-interface.lisp
;; as it needs the implementation files to be loaded
;; while those files need the other package definitions already...

(defpackage :ndb.quads
  (:use :cl)
  (:export :quad :triple :tuple :single
           :+quad-size+ :+tuple-size+ :+tuple-size+ :+single-size+
           :+quad-count+ :+triple-count+ :+tuple-count+ :+single-count+
           :quad-to-list ;; better simplify with more direct interface
           )
  (:documentation "Definitions to access quads in tables"))

(defpackage :ndb.simple-scan
  (:use :cl)
  (:documentation "Definitions to access quads in tables"))

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
  (:documentation "A higher level NDB API interface (for RonDB)."))

(defpackage :ndbapi
  (:use :cl)
  (:import-from :ndbapi.implementation
                :get-ndb-error
                :valid-object-p)
  (:export :get-ndb-error
           :valid-object-p)
  (:documentation "A higher level NDB API interface (for RonDB)."))

(defpackage :ndb.quads
  (:use :cl)
  (:documentation "Definitions to access quads in tables"))

(defpackage :ndb.simple-scan
  (:use :cl)
  (:documentation "Definitions to access quads in tables"))

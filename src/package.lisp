;;; Copyright (c) 2022 Max-Gerd Retzlaff <mgr@matroid.org>, Datagraph GmbH.
;;; Distributed under the terms of the GNU General Public License, Version 2.0,
;;; see file LICENSE in the top level directory of this repository.

(in-package :cl-user)

(defpackage :ndbapi.types
  (:use :cl)
  (:export :initialized
           :free-foreign-object
           :*ndbapi-verbose*)
  (:documentation "The low-level C++ NDB API interface (for RonDB)."))

(defpackage :ndbapi.ffi
  (:use #| :cl |#)
  (:export :ndb-end
           :dictionary-get-ndb-error
           :table-get-name
           :ndb-transaction-get-ndb-error
           :index-bound
           :index-get-name
           :ndb-scan-operation-get-ndb-transaction
           :ndb-operation-get-ndb-transaction
           :ndb-close-transaction
           :ndb-transaction-get-ndb
           :ndb-interpreted-code-get-ndb-error
           ;; pseudo columns
           :column-fragment :column-fragment-fixed-memory :column-fragment-varsized-memory
           :column-row-count :column-commit-count :column-row-size
           :column-range-no :column-disk-ref :column-records-in-range
           :column-rowid :column-row-gci :column-row-gci-64
           :column-row-author :column-any-value :column-copy-rowid
           :column-lock-ref :column-op-id :column-optimize
           :column-fragment-extent-space :column-fragment-free-extent-space)
  (:documentation "The low-level C++ NDB API interface (for RonDB)."))

(defpackage :ndbapi.ffi.overloading
  (:use :cl)
  (:import-from :ndbapi.types
                :*ndbapi-verbose*)
  (:nicknames :ndbapi.ffi.o))

(defpackage :ndbapi.implementation
  (:use :cl)
  (:nicknames :ndbapi.i)
  (:documentation "A higher level NDB API interface (for RonDB)."))

;; defpackage :ndbapi in file ndbapi-interface.lisp
;; as it needs the implementation files to be loaded
;; while those files need the other package definitions already...

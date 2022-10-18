;;; Copyright (c) 2022 Max-Gerd Retzlaff <mgr@matroid.org>, Datagraph GmbH.
;;; Distributed under the terms of the GNU General Public License, Version 2.0,
;;; see file LICENSE in the top level directory of this repository.

(in-package :cl-user)

(defpackage :ndbapi
  (:use :cl)
  (:import-from :ndbapi.implementation
                :*ndbapi-directory*
                :get-ndb-error
                :valid-object-p
                :new-ndb-cluster-connection
                :ndb-cluster-connection-connect
                :ndb-cluster-connection-wait-until-ready
                :new-ndb
                :ndb-init-ndb ;; renamed to avoid conflict
                :ndb-get-ndb-error
                :ndb-start-transaction
                :dictionary-get-table
                :dictionary-get-index
                :ndb-transaction-scan-index
                :ndb-index-scan-operation-set-bound
                :ndb-transaction-execute
                :ndb-scan-operation-next-result
                :ndb-scan-operation-close)
  (:import-from :ndbapi.ffi
                :ndb-init
                :ndb-get-dictionary
                :dictionary-get-ndb-error
                :table-get-name
                :index-get-default-record
                :table-get-default-record
                :ndb-transaction-get-ndb-error
                :index-bound)
  (:import-from :ndbapi.types
                :*ndbapi-verbose*
                :initialized
                :with-foreign-struct
                :free-foreign-object)
  (:export ;; from :ndbapi.implementation
           :*ndbapi-directory*
           :get-ndb-error
           :valid-object-p
           :new-ndb-cluster-connection
           :ndb-cluster-connection-connect
           :ndb-cluster-connection-wait-until-ready
           :new-ndb
           :ndb-init-ndb ;; renamed to avoid conflict
           :ndb-get-ndb-error
           :ndb-start-transaction
           :dictionary-get-table
           :dictionary-get-index
           :ndb-transaction-scan-index
           :ndb-index-scan-operation-set-bound
           :ndb-transaction-execute
           :ndb-scan-operation-next-result
           :ndb-scan-operation-close
           ;; from :ndbapi.ffi
           :ndb-init
           :ndb-get-dictionary
           :dictionary-get-ndb-error
           :table-get-name
           :index-get-default-record
           :table-get-default-record
           :ndb-transaction-get-ndb-error
           :index-bound
           ;; from :ndbapi.types
           :*ndbapi-verbose*
           :initialized
           :with-foreign-struct
           :free-foreign-object)
  (:documentation "A higher level NDB API interface (for RonDB)."))

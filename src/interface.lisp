;;; Copyright (c) 2022 Max-Gerd Retzlaff <mgr@matroid.org>, Datagraph GmbH.
;;; Distributed under the terms of the GNU General Public License, Version 2.0,
;;; see file LICENSE in the top level directory of this repository.

(in-package :cl-user)

(defpackage :ndbapi
  (:use :cl)
  (:import-from :ndbapi.implementation
                :get-ndb-error
                :valid-object-p
                :new-ndb-cluster-connection
                :ndb-cluster-connection-set-name
                :ndb-cluster-connection-connect
                :ndb-cluster-connection-wait-until-ready
                :new-ndb
                :ndb-init ;; other ndb-init renamed to :ndb-begin to avoid conflict
                :ndb-get-ndb-error
                :explicitly-check-for-error
                :explicitly-check-for-transaction-error
                :check-for-tuple-not-found-error
                :ndb-start-transaction
                :dictionary-get-table
                :dictionary-get-index
                :ndb-transaction-scan-index
                :ndb-index-scan-operation-set-bound
                :ndb-index-scan-operation-set-bound/recattr
                :ndb-transaction-get-ndb-index-scan-operation
                :ndb-index-scan-operation-read-tuples
                :ndb-transaction-execute
                :ndb-transaction-execute/no-explicit-check
                :ndb-scan-operation-next-result
                :ndb-scan-operation-close
                :new-ndb-interpreted-code
                :ndb-interpreted-code-interpret-exit-last-row
                :ndb-interpreted-code-finalise
                :ndb-scan-operation-set-interpreted-code
                :ndb-operation-get-value
                :ndb-begin ;; this ndb-init renamed to :ndb-begin to avoid conflict
                :initialized-ndb-init-p
                :ensure-ndb-init
                :*ndb-init*
                :ndb-end
                :ndb-get-dictionary
                :index-get-default-record
                :table-get-default-record
                :ndb-free-object
                :with-ndb-init
                :with-ndb-cluster-connection
                :with-ndb
                :*ndb*
                :with-ndb-transaction
                :*transaction*
                :with-ndb-transaction-scan-index
                :with-ndb-transaction-get-ndb-index-scan-operation
                :with-ndb-interpreted-code
                :ndb-transaction-update-tuple
                :ndb-transaction-insert-tuple
                :ndb-transaction-delete-tuple
                :ndb-transaction-read-tuple
                ;; pseudo columns
                :column-fragment :column-fragment-fixed-memory :column-fragment-varsized-memory
                :column-row-count :column-commit-count :column-row-size
                :column-range-no :column-disk-ref :column-records-in-range
                :column-rowid :column-row-gci :column-row-gci-64
                :column-row-author :column-any-value :column-copy-rowid
                :column-lock-ref :column-op-id :column-optimize
                :column-fragment-extent-space :column-fragment-free-extent-space
                ;; simple connection interace
                :*connection*
                :connect
                :disconnect
                :valid-connection-p
                :ensure-connection
                :with-connection
                ;; more advanced commands
                :get-index-names)
  (:import-from :ndbapi.ffi
                :dictionary-get-ndb-error
                :table-get-name
                :ndb-transaction-get-ndb-error
                :index-bound
                :index-get-name
                :ndb-scan-operation-get-ndb-transaction
                :ndb-operation-get-ndb-transaction
                :ndb-close-transaction
                :scan-options
                :get-value-spec)
  (:import-from :ndbapi.types
                :*ndbapi-verbose*
                :initialized
                :with-foreign-struct
                :free-foreign-object
                :foreign-pointer)
  (:export ;; from :ndbapi.implementation
           :get-ndb-error
           :valid-object-p
           :new-ndb-cluster-connection
           :ndb-cluster-connection-set-name
           :ndb-cluster-connection-connect
           :ndb-cluster-connection-wait-until-ready
           :new-ndb
           :ndb-init ;; renamed to avoid conflict
           :ndb-begin
           :initialized-ndb-init-p
           :ensure-ndb-init
           :*ndb-init*
           :ndb-end
           :ndb-get-ndb-error
           :explicitly-check-for-error
           :explicitly-check-for-transaction-error
           :check-for-tuple-not-found-error
           :ndb-start-transaction
           :dictionary-get-table
           :dictionary-get-index
           :ndb-transaction-scan-index
           :ndb-index-scan-operation-set-bound
           :ndb-index-scan-operation-set-bound/recattr
           :ndb-transaction-get-ndb-index-scan-operation
           :ndb-index-scan-operation-read-tuples
           :ndb-transaction-execute
           :ndb-transaction-execute/no-explicit-check
           :ndb-scan-operation-next-result
           :ndb-scan-operation-close
           :new-ndb-interpreted-code
           :ndb-interpreted-code-interpret-exit-last-row
           :ndb-interpreted-code-finalise
           :ndb-scan-operation-set-interpreted-code
           :ndb-operation-get-value
           :ndb-free-object
           :with-ndb-init
           :with-ndb-cluster-connection
           :with-ndb
           :*ndb*
           :with-ndb-transaction
           :*transaction*
           :with-ndb-transaction-scan-index
           :with-ndb-transaction-get-ndb-index-scan-operation
           :with-ndb-interpreted-code
           :ndb-transaction-update-tuple
           :ndb-transaction-insert-tuple
           :ndb-transaction-delete-tuple
           :ndb-transaction-read-tuple
           ;; pseudo columns
           :column-fragment :column-fragment-fixed-memory :column-fragment-varsized-memory
           :column-row-count :column-commit-count :column-row-size
           :column-range-no :column-disk-ref :column-records-in-range
           :column-rowid :column-row-gci :column-row-gci-64
           :column-row-author :column-any-value :column-copy-rowid
           :column-lock-ref :column-op-id :column-optimize
           :column-fragment-extent-space :column-fragment-free-extent-space
           ;; simple connection interace
           :*connection*
           :connect
           :disconnect
           :valid-connection-p
           :ensure-connection
           :with-connection
           ;; more advanced commands
           :get-index-names
           ;; from :ndbapi.ffi
           :ndb-get-dictionary
           :dictionary-get-ndb-error
           :table-get-name
           :index-get-default-record
           :table-get-default-record
           :ndb-transaction-get-ndb-error
           :index-bound
           :index-get-name
           :ndb-scan-operation-get-ndb-transaction
           :ndb-close-transaction
           :scan-options
           :get-value-spec
           ;; from :ndbapi.types
           :*ndbapi-verbose*
           :initialized
           :with-foreign-struct
           :free-foreign-object
           :foreign-pointer)
  (:documentation "A higher level NDB API interface (for RonDB)."))

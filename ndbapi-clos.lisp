;;; Copyright (c) 2022 Max-Gerd Retzlaff <mgr@matroid.org>, Datagraph GmbH.
;;; Distributed under the terms of the GNU General Public License, Version 2.0,
;;; see file LICENSE in the top level directory of this repository.

(cl:in-package :libndbapi)

(cl:defclass #.(swig-lispify "ndb-cluster-connection-node-iter" 'classname)()
  ((ff-pointer :reader ff-pointer)))

(cl:defmethod initialize-instance :after ((obj #.(swig-lispify "ndb-cluster-connection-node-iter" 'class)) &key)
  (setf (slot-value obj 'ff-pointer) (#.(swig-lispify "new_Ndb_cluster_connection_node_iter" 'function))))


(cl:defclass #.(swig-lispify "ndb-cluster-connection" 'classname)()
  ((ff-pointer :reader ff-pointer)))

(cl:defmethod initialize-instance :after ((obj #.(swig-lispify "ndb-cluster-connection" 'class)) &key (connectstring cl:string))
  (setf (slot-value obj 'ff-pointer) (#.(swig-lispify "new_Ndb_cluster_connection" 'function) connectstring)))

(cl:defmethod initialize-instance :after ((obj #.(swig-lispify "ndb-cluster-connection" 'class)) &key)
  (setf (slot-value obj 'ff-pointer) (#.(swig-lispify "new_Ndb_cluster_connection" 'function))))

(cl:defmethod initialize-instance :after ((obj #.(swig-lispify "ndb-cluster-connection" 'class)) &key (connectstring cl:string) (force_api_nodeid cl:integer))
  (setf (slot-value obj 'ff-pointer) (#.(swig-lispify "new_Ndb_cluster_connection" 'function) connectstring force_api_nodeid)))

(cl:defmethod initialize-instance :after ((obj #.(swig-lispify "ndb-cluster-connection" 'class)) &key (connectstring cl:string) (main_connection #.(swig-lispify "ndb-cluster-connection" 'classname)))
  (setf (slot-value obj 'ff-pointer) (#.(swig-lispify "new_Ndb_cluster_connection" 'function) connectstring (ff-pointer main_connection))))

(cl:defmethod initialize-instance :after ((obj #.(swig-lispify "ndb-cluster-connection" 'class)) &key (connectstring cl:string) (main_connection #.(swig-lispify "ndb-cluster-connection" 'classname)) (force_api_nodeid cl:integer))
  (setf (slot-value obj 'ff-pointer) (#.(swig-lispify "new_Ndb_cluster_connection" 'function) connectstring (ff-pointer main_connection) force_api_nodeid)))

(cl:defmethod #.(swig-lispify "set-data-node-neighbour" 'method) ((self #.(swig-lispify "ndb-cluster-connection" 'classname)) (neighbour_node cl:integer))
  (#.(swig-lispify "Ndb_cluster_connection_set_data_node_neighbour" 'function) (ff-pointer self) neighbour_node))

(cl:defmethod #.(swig-lispify "set-name" 'method) ((self #.(swig-lispify "ndb-cluster-connection" 'classname)) (name cl:string))
  (#.(swig-lispify "Ndb_cluster_connection_set_name" 'function) (ff-pointer self) name))

(cl:defmethod #.(swig-lispify "set-service-uri" 'method) ((self #.(swig-lispify "ndb-cluster-connection" 'classname)) (scheme cl:string) (host cl:string) (port cl:integer) (path cl:string))
  (#.(swig-lispify "Ndb_cluster_connection_set_service_uri" 'function) (ff-pointer self) scheme host port path))

(cl:defmethod #.(swig-lispify "set-timeout" 'method) ((self #.(swig-lispify "ndb-cluster-connection" 'classname)) (timeout_ms cl:integer))
  (#.(swig-lispify "Ndb_cluster_connection_set_timeout" 'function) (ff-pointer self) timeout_ms))

(cl:defmethod #.(swig-lispify "connect" 'method) ((self #.(swig-lispify "ndb-cluster-connection" 'classname)) (no_retries cl:integer) (retry_delay_in_seconds cl:integer) (verbose cl:integer))
  (#.(swig-lispify "Ndb_cluster_connection_connect" 'function) (ff-pointer self) no_retries retry_delay_in_seconds verbose))

(cl:defmethod #.(swig-lispify "connect" 'method) ((self #.(swig-lispify "ndb-cluster-connection" 'classname)) (no_retries cl:integer) (retry_delay_in_seconds cl:integer))
  (#.(swig-lispify "Ndb_cluster_connection_connect" 'function) (ff-pointer self) no_retries retry_delay_in_seconds))

(cl:defmethod #.(swig-lispify "connect" 'method) ((self #.(swig-lispify "ndb-cluster-connection" 'classname)) (no_retries cl:integer))
  (#.(swig-lispify "Ndb_cluster_connection_connect" 'function) (ff-pointer self) no_retries))

(cl:defmethod #.(swig-lispify "connect" 'method) ((self #.(swig-lispify "ndb-cluster-connection" 'classname)))
  (#.(swig-lispify "Ndb_cluster_connection_connect" 'function) (ff-pointer self)))

(cl:defmethod #.(swig-lispify "start-connect-thread" 'method) ((self #.(swig-lispify "ndb-cluster-connection" 'classname)) connect_callback)
  (#.(swig-lispify "Ndb_cluster_connection_start_connect_thread" 'function) (ff-pointer self) connect_callback))

(cl:defmethod #.(swig-lispify "start-connect-thread" 'method) ((self #.(swig-lispify "ndb-cluster-connection" 'classname)))
  (#.(swig-lispify "Ndb_cluster_connection_start_connect_thread" 'function) (ff-pointer self)))

(cl:defmethod #.(swig-lispify "wait-until-ready" 'method) ((self #.(swig-lispify "ndb-cluster-connection" 'classname)) (timeout_for_first_alive cl:integer) (timeout_after_first_alive cl:integer))
  (#.(swig-lispify "Ndb_cluster_connection_wait_until_ready" 'function) (ff-pointer self) timeout_for_first_alive timeout_after_first_alive))

(cl:defmethod #.(swig-lispify "lock-ndb-objects" 'method) ((self #.(swig-lispify "ndb-cluster-connection" 'classname)))
  (#.(swig-lispify "Ndb_cluster_connection_lock_ndb_objects" 'function) (ff-pointer self)))

(cl:defmethod #.(swig-lispify "unlock-ndb-objects" 'method) ((self #.(swig-lispify "ndb-cluster-connection" 'classname)))
  (#.(swig-lispify "Ndb_cluster_connection_unlock_ndb_objects" 'function) (ff-pointer self)))

(cl:defmethod #.(swig-lispify "get-next-ndb-object" 'method) ((self #.(swig-lispify "ndb-cluster-connection" 'classname)) p)
  (#.(swig-lispify "Ndb_cluster_connection_get_next_ndb_object" 'function) (ff-pointer self) p))

(cl:defmethod #.(swig-lispify "get-latest-error" 'method) ((self #.(swig-lispify "ndb-cluster-connection" 'classname)))
  (#.(swig-lispify "Ndb_cluster_connection_get_latest_error" 'function) (ff-pointer self)))

(cl:defmethod #.(swig-lispify "get-latest-error-msg" 'method) ((self #.(swig-lispify "ndb-cluster-connection" 'classname)))
  (#.(swig-lispify "Ndb_cluster_connection_get_latest_error_msg" 'function) (ff-pointer self)))

(cl:defmethod #.(swig-lispify "set-auto-reconnect" 'method) ((self #.(swig-lispify "ndb-cluster-connection" 'classname)) (value cl:integer))
  (#.(swig-lispify "Ndb_cluster_connection_set_auto_reconnect" 'function) (ff-pointer self) value))

(cl:defmethod #.(swig-lispify "get-auto-reconnect" 'method) ((self #.(swig-lispify "ndb-cluster-connection" 'classname)))
  (#.(swig-lispify "Ndb_cluster_connection_get_auto_reconnect" 'function) (ff-pointer self)))

(cl:defmethod #.(swig-lispify "get-system-name" 'method) ((self #.(swig-lispify "ndb-cluster-connection" 'classname)))
  (#.(swig-lispify "Ndb_cluster_connection_get_system_name" 'function) (ff-pointer self)))

(cl:defmethod #.(swig-lispify "collect-client-stats" 'method) ((self #.(swig-lispify "ndb-cluster-connection" 'classname)) statsArr (sz cl:integer))
  (#.(swig-lispify "Ndb_cluster_connection_collect_client_stats" 'function) (ff-pointer self) statsArr sz))

(cl:defmethod #.(swig-lispify "set-error-print" 'method) ((self #.(swig-lispify "ndb-cluster-connection" 'classname)) (val cl:t))
  (#.(swig-lispify "Ndb_cluster_connection_set_error_print" 'function) (ff-pointer self) val))

(cl:defmethod #.(swig-lispify "set-max-adaptive-send-time" 'method) ((self #.(swig-lispify "ndb-cluster-connection" 'classname)) (milliseconds cl:integer))
  (#.(swig-lispify "Ndb_cluster_connection_set_max_adaptive_send_time" 'function) (ff-pointer self) milliseconds))

(cl:defmethod #.(swig-lispify "get-max-adaptive-send-time" 'method) ((self #.(swig-lispify "ndb-cluster-connection" 'classname)))
  (#.(swig-lispify "Ndb_cluster_connection_get_max_adaptive_send_time" 'function) (ff-pointer self)))

(cl:defmethod #.(swig-lispify "set-num-recv-threads" 'method) ((self #.(swig-lispify "ndb-cluster-connection" 'classname)) (num_recv_threads cl:integer))
  (#.(swig-lispify "Ndb_cluster_connection_set_num_recv_threads" 'function) (ff-pointer self) num_recv_threads))

(cl:defmethod #.(swig-lispify "get-num-recv-threads" 'method) ((self #.(swig-lispify "ndb-cluster-connection" 'classname)))
  (#.(swig-lispify "Ndb_cluster_connection_get_num_recv_threads" 'function) (ff-pointer self)))

(cl:defmethod #.(swig-lispify "unset-recv-thread-cpu" 'method) ((self #.(swig-lispify "ndb-cluster-connection" 'classname)) (recv_thread_id cl:integer))
  (#.(swig-lispify "Ndb_cluster_connection_unset_recv_thread_cpu" 'function) (ff-pointer self) recv_thread_id))

(cl:defmethod #.(swig-lispify "set-recv-thread-cpu" 'method) ((self #.(swig-lispify "ndb-cluster-connection" 'classname)) (cpuid cl:integer))
  (#.(swig-lispify "Ndb_cluster_connection_set_recv_thread_cpu" 'function) (ff-pointer self) cpuid))

(cl:defmethod #.(swig-lispify "set-recv-thread-cpu" 'method) ((self #.(swig-lispify "ndb-cluster-connection" 'classname)) cpuid_array (array_len cl:integer) (recv_thread_id cl:integer))
  (#.(swig-lispify "Ndb_cluster_connection_set_recv_thread_cpu" 'function) (ff-pointer self) cpuid_array array_len recv_thread_id))

(cl:defmethod #.(swig-lispify "set-recv-thread-cpu" 'method) ((self #.(swig-lispify "ndb-cluster-connection" 'classname)) cpuid_array (array_len cl:integer))
  (#.(swig-lispify "Ndb_cluster_connection_set_recv_thread_cpu" 'function) (ff-pointer self) cpuid_array array_len))

(cl:defmethod #.(swig-lispify "set-recv-thread-activation-threshold" 'method) ((self #.(swig-lispify "ndb-cluster-connection" 'classname)) (threshold cl:integer))
  (#.(swig-lispify "Ndb_cluster_connection_set_recv_thread_activation_threshold" 'function) (ff-pointer self) threshold))

(cl:defmethod #.(swig-lispify "get-recv-thread-activation-threshold" 'method) ((self #.(swig-lispify "ndb-cluster-connection" 'classname)))
  (#.(swig-lispify "Ndb_cluster_connection_get_recv_thread_activation_threshold" 'function) (ff-pointer self)))

(cl:defmethod #.(swig-lispify "get-no-ready" 'method) ((self #.(swig-lispify "ndb-cluster-connection" 'classname)))
  (#.(swig-lispify "Ndb_cluster_connection_get_no_ready" 'function) (ff-pointer self)))

(cl:defmethod #.(swig-lispify "get-connectstring" 'method) ((self #.(swig-lispify "ndb-cluster-connection" 'classname)) (buf cl:string) (buf_sz cl:integer))
  (#.(swig-lispify "Ndb_cluster_connection_get_connectstring" 'function) (ff-pointer self) buf buf_sz))

(cl:defmethod #.(swig-lispify "get-connected-port" 'method) ((self #.(swig-lispify "ndb-cluster-connection" 'classname)))
  (#.(swig-lispify "Ndb_cluster_connection_get_connected_port" 'function) (ff-pointer self)))

(cl:defmethod #.(swig-lispify "get-connected-host" 'method) ((self #.(swig-lispify "ndb-cluster-connection" 'classname)))
  (#.(swig-lispify "Ndb_cluster_connection_get_connected_host" 'function) (ff-pointer self)))

(cl:defmethod #.(swig-lispify "set-optimized-node-selection" 'method) ((self #.(swig-lispify "ndb-cluster-connection" 'classname)) (val cl:integer))
  (#.(swig-lispify "Ndb_cluster_connection_set_optimized_node_selection" 'function) (ff-pointer self) val))

(cl:defmethod #.(swig-lispify "no-db-nodes" 'method) ((self #.(swig-lispify "ndb-cluster-connection" 'classname)))
  (#.(swig-lispify "Ndb_cluster_connection_no_db_nodes" 'function) (ff-pointer self)))

(cl:defmethod #.(swig-lispify "max-api-nodeid" 'method) ((self #.(swig-lispify "ndb-cluster-connection" 'classname)))
  (#.(swig-lispify "Ndb_cluster_connection_max_api_nodeid" 'function) (ff-pointer self)))

(cl:defmethod #.(swig-lispify "max-nodegroup" 'method) ((self #.(swig-lispify "ndb-cluster-connection" 'classname)))
  (#.(swig-lispify "Ndb_cluster_connection_max_nodegroup" 'function) (ff-pointer self)))

(cl:defmethod #.(swig-lispify "node-id" 'method) ((self #.(swig-lispify "ndb-cluster-connection" 'classname)))
  (#.(swig-lispify "Ndb_cluster_connection_node_id" 'function) (ff-pointer self)))

(cl:defmethod #.(swig-lispify "get-connect-count" 'method) ((self #.(swig-lispify "ndb-cluster-connection" 'classname)))
  (#.(swig-lispify "Ndb_cluster_connection_get_connect_count" 'function) (ff-pointer self)))

(cl:defmethod #.(swig-lispify "get-min-db-version" 'method) ((self #.(swig-lispify "ndb-cluster-connection" 'classname)))
  (#.(swig-lispify "Ndb_cluster_connection_get_min_db_version" 'function) (ff-pointer self)))

(cl:defmethod #.(swig-lispify "get-min-api-version" 'method) ((self #.(swig-lispify "ndb-cluster-connection" 'classname)))
  (#.(swig-lispify "Ndb_cluster_connection_get_min_api_version" 'function) (ff-pointer self)))

(cl:defmethod #.(swig-lispify "init-get-next-node" 'method) ((self #.(swig-lispify "ndb-cluster-connection" 'classname)) (iter #.(swig-lispify "ndb-cluster-connection-node-iter" 'classname)))
  (#.(swig-lispify "Ndb_cluster_connection_init_get_next_node" 'function) (ff-pointer self) iter))

(cl:defmethod #.(swig-lispify "get-next-node" 'method) ((self #.(swig-lispify "ndb-cluster-connection" 'classname)) (iter #.(swig-lispify "ndb-cluster-connection-node-iter" 'classname)))
  (#.(swig-lispify "Ndb_cluster_connection_get_next_node" 'function) (ff-pointer self) iter))

(cl:defmethod #.(swig-lispify "get-next-alive-node" 'method) ((self #.(swig-lispify "ndb-cluster-connection" 'classname)) (iter #.(swig-lispify "ndb-cluster-connection-node-iter" 'classname)))
  (#.(swig-lispify "Ndb_cluster_connection_get_next_alive_node" 'function) (ff-pointer self) iter))

(cl:defmethod #.(swig-lispify "get-active-ndb-objects" 'method) ((self #.(swig-lispify "ndb-cluster-connection" 'classname)))
  (#.(swig-lispify "Ndb_cluster_connection_get_active_ndb_objects" 'function) (ff-pointer self)))

(cl:defmethod #.(swig-lispify "get-latest-trans-gci" 'method) ((self #.(swig-lispify "ndb-cluster-connection" 'classname)))
  (#.(swig-lispify "Ndb_cluster_connection_get_latest_trans_gci" 'function) (ff-pointer self)))

(cl:defmethod #.(swig-lispify "create-ndb-wait-group" 'method) ((self #.(swig-lispify "ndb-cluster-connection" 'classname)) (size cl:integer))
  (#.(swig-lispify "Ndb_cluster_connection_create_ndb_wait_group" 'function) (ff-pointer self) size))

(cl:defmethod #.(swig-lispify "release-ndb-wait-group" 'method) ((self #.(swig-lispify "ndb-cluster-connection" 'classname)) arg1)
  (#.(swig-lispify "Ndb_cluster_connection_release_ndb_wait_group" 'function) (ff-pointer self) arg1))

(cl:defmethod #.(swig-lispify "wait-until-ready" 'method) ((self #.(swig-lispify "ndb-cluster-connection" 'classname)) nodes (cnt cl:integer) (timeout cl:integer))
  (#.(swig-lispify "Ndb_cluster_connection_wait_until_ready" 'function) (ff-pointer self) nodes cnt timeout))


(cl:defclass #.(swig-lispify "ndb-dictionary" 'classname)()
  ((ff-pointer :reader ff-pointer)))

(cl:defmethod initialize-instance :after ((obj #.(swig-lispify "ndb-dictionary" 'class)) &key)
  (setf (slot-value obj 'ff-pointer) (#.(swig-lispify "new_NdbDictionary" 'function))))


(cl:defclass #.(swig-lispify "object" 'classname)()
  ((ff-pointer :reader ff-pointer)))

(cl:defmethod #.(swig-lispify "get-object-status" 'method) ((self #.(swig-lispify "object" 'classname)))
  (#.(swig-lispify "Object_getObjectStatus" 'function) (ff-pointer self)))

(cl:defmethod #.(swig-lispify "get-object-version" 'method) ((self #.(swig-lispify "object" 'classname)))
  (#.(swig-lispify "Object_getObjectVersion" 'function) (ff-pointer self)))

(cl:defmethod #.(swig-lispify "get-object-id" 'method) ((self #.(swig-lispify "object" 'classname)))
  (#.(swig-lispify "Object_getObjectId" 'function) (ff-pointer self)))


(cl:defclass #.(swig-lispify "object-id" 'classname)(#.(swig-lispify "ndb-dictionary::object" 'classname))
  ((ff-pointer :reader ff-pointer)))

(cl:defmethod initialize-instance :after ((obj #.(swig-lispify "object-id" 'class)) &key)
  (setf (slot-value obj 'ff-pointer) (#.(swig-lispify "new_ObjectId" 'function))))

(cl:defmethod #.(swig-lispify "get-object-status" 'method) ((self #.(swig-lispify "object-id" 'classname)))
  (#.(swig-lispify "ObjectId_getObjectStatus" 'function) (ff-pointer self)))

(cl:defmethod #.(swig-lispify "get-object-version" 'method) ((self #.(swig-lispify "object-id" 'classname)))
  (#.(swig-lispify "ObjectId_getObjectVersion" 'function) (ff-pointer self)))

(cl:defmethod #.(swig-lispify "get-object-id" 'method) ((self #.(swig-lispify "object-id" 'classname)))
  (#.(swig-lispify "ObjectId_getObjectId" 'function) (ff-pointer self)))


(cl:defclass #.(swig-lispify "column" 'classname)()
  ((ff-pointer :reader ff-pointer)))

(cl:defmethod #.(swig-lispify "get-name" 'method) ((self #.(swig-lispify "column" 'classname)))
  (#.(swig-lispify "Column_getName" 'function) (ff-pointer self)))

(cl:defmethod #.(swig-lispify "get-nullable" 'method) ((self #.(swig-lispify "column" 'classname)))
  (#.(swig-lispify "Column_getNullable" 'function) (ff-pointer self)))

(cl:defmethod #.(swig-lispify "get-primary-key" 'method) ((self #.(swig-lispify "column" 'classname)))
  (#.(swig-lispify "Column_getPrimaryKey" 'function) (ff-pointer self)))

(cl:defmethod #.(swig-lispify "get-column-no" 'method) ((self #.(swig-lispify "column" 'classname)))
  (#.(swig-lispify "Column_getColumnNo" 'function) (ff-pointer self)))

(cl:defmethod #.(swig-lispify "get-attr-id" 'method) ((self #.(swig-lispify "column" 'classname)))
  (#.(swig-lispify "Column_getAttrId" 'function) (ff-pointer self)))

(cl:defmethod #.(swig-lispify "equal" 'method) ((self #.(swig-lispify "column" 'classname)) (column #.(swig-lispify "column" 'classname)))
  (#.(swig-lispify "Column_equal" 'function) (ff-pointer self) (ff-pointer column)))

(cl:defmethod #.(swig-lispify "get-type" 'method) ((self #.(swig-lispify "column" 'classname)))
  (#.(swig-lispify "Column_getType" 'function) (ff-pointer self)))

(cl:defmethod #.(swig-lispify "get-precision" 'method) ((self #.(swig-lispify "column" 'classname)))
  (#.(swig-lispify "Column_getPrecision" 'function) (ff-pointer self)))

(cl:defmethod #.(swig-lispify "get-scale" 'method) ((self #.(swig-lispify "column" 'classname)))
  (#.(swig-lispify "Column_getScale" 'function) (ff-pointer self)))

(cl:defmethod #.(swig-lispify "get-length" 'method) ((self #.(swig-lispify "column" 'classname)))
  (#.(swig-lispify "Column_getLength" 'function) (ff-pointer self)))

(cl:defmethod #.(swig-lispify "get-size-in-bytes-for-record" 'method) ((self #.(swig-lispify "column" 'classname)))
  (#.(swig-lispify "Column_getSizeInBytesForRecord" 'function) (ff-pointer self)))

(cl:defmethod #.(swig-lispify "get-charset" 'method) ((self #.(swig-lispify "column" 'classname)))
  (#.(swig-lispify "Column_getCharset" 'function) (ff-pointer self)))

(cl:defmethod #.(swig-lispify "get-charset-number" 'method) ((self #.(swig-lispify "column" 'classname)))
  (#.(swig-lispify "Column_getCharsetNumber" 'function) (ff-pointer self)))

(cl:defmethod #.(swig-lispify "get-inline-size" 'method) ((self #.(swig-lispify "column" 'classname)))
  (#.(swig-lispify "Column_getInlineSize" 'function) (ff-pointer self)))

(cl:defmethod #.(swig-lispify "get-part-size" 'method) ((self #.(swig-lispify "column" 'classname)))
  (#.(swig-lispify "Column_getPartSize" 'function) (ff-pointer self)))

(cl:defmethod #.(swig-lispify "get-stripe-size" 'method) ((self #.(swig-lispify "column" 'classname)))
  (#.(swig-lispify "Column_getStripeSize" 'function) (ff-pointer self)))

(cl:defmethod #.(swig-lispify "get-size" 'method) ((self #.(swig-lispify "column" 'classname)))
  (#.(swig-lispify "Column_getSize" 'function) (ff-pointer self)))

(cl:defmethod #.(swig-lispify "get-partition-key" 'method) ((self #.(swig-lispify "column" 'classname)))
  (#.(swig-lispify "Column_getPartitionKey" 'function) (ff-pointer self)))

(cl:defmethod #.(swig-lispify "get-distribution-key" 'method) ((self #.(swig-lispify "column" 'classname)))
  (#.(swig-lispify "Column_getDistributionKey" 'function) (ff-pointer self)))

(cl:defmethod #.(swig-lispify "get-array-type" 'method) ((self #.(swig-lispify "column" 'classname)))
  (#.(swig-lispify "Column_getArrayType" 'function) (ff-pointer self)))

(cl:defmethod #.(swig-lispify "get-storage-type" 'method) ((self #.(swig-lispify "column" 'classname)))
  (#.(swig-lispify "Column_getStorageType" 'function) (ff-pointer self)))

(cl:defmethod #.(swig-lispify "get-dynamic" 'method) ((self #.(swig-lispify "column" 'classname)))
  (#.(swig-lispify "Column_getDynamic" 'function) (ff-pointer self)))

(cl:defmethod #.(swig-lispify "get-index-sourced" 'method) ((self #.(swig-lispify "column" 'classname)))
  (#.(swig-lispify "Column_getIndexSourced" 'function) (ff-pointer self)))

(cl:defmethod initialize-instance :after ((obj #.(swig-lispify "column" 'class)) &key (name cl:string))
  (setf (slot-value obj 'ff-pointer) (#.(swig-lispify "new_Column" 'function) name)))

(cl:defmethod initialize-instance :after ((obj #.(swig-lispify "column" 'class)) &key)
  (setf (slot-value obj 'ff-pointer) (#.(swig-lispify "new_Column" 'function))))

(cl:defmethod initialize-instance :after ((obj #.(swig-lispify "column" 'class)) &key (column #.(swig-lispify "column" 'classname)))
  (setf (slot-value obj 'ff-pointer) (#.(swig-lispify "new_Column" 'function) (ff-pointer column))))

(cl:defmethod #.(swig-lispify "set-name" 'method) ((self #.(swig-lispify "column" 'classname)) (name cl:string))
  (#.(swig-lispify "Column_setName" 'function) (ff-pointer self) name))

(cl:defmethod #.(swig-lispify "set-nullable" 'method) ((self #.(swig-lispify "column" 'classname)) (arg1 cl:t))
  (#.(swig-lispify "Column_setNullable" 'function) (ff-pointer self) arg1))

(cl:defmethod #.(swig-lispify "set-primary-key" 'method) ((self #.(swig-lispify "column" 'classname)) (arg1 cl:t))
  (#.(swig-lispify "Column_setPrimaryKey" 'function) (ff-pointer self) arg1))

(cl:defmethod #.(swig-lispify "set-type" 'method) ((self #.(swig-lispify "column" 'classname)) (type cl:integer))
  (#.(swig-lispify "Column_setType" 'function) (ff-pointer self) type))

(cl:defmethod #.(swig-lispify "set-precision" 'method) ((self #.(swig-lispify "column" 'classname)) (arg1 cl:integer))
  (#.(swig-lispify "Column_setPrecision" 'function) (ff-pointer self) arg1))

(cl:defmethod #.(swig-lispify "set-scale" 'method) ((self #.(swig-lispify "column" 'classname)) (arg1 cl:integer))
  (#.(swig-lispify "Column_setScale" 'function) (ff-pointer self) arg1))

(cl:defmethod #.(swig-lispify "set-length" 'method) ((self #.(swig-lispify "column" 'classname)) (length cl:integer))
  (#.(swig-lispify "Column_setLength" 'function) (ff-pointer self) length))

(cl:defmethod #.(swig-lispify "set-charset" 'method) ((self #.(swig-lispify "column" 'classname)) cs)
  (#.(swig-lispify "Column_setCharset" 'function) (ff-pointer self) cs))

(cl:defmethod #.(swig-lispify "set-inline-size" 'method) ((self #.(swig-lispify "column" 'classname)) (size cl:integer))
  (#.(swig-lispify "Column_setInlineSize" 'function) (ff-pointer self) size))

(cl:defmethod #.(swig-lispify "set-part-size" 'method) ((self #.(swig-lispify "column" 'classname)) (size cl:integer))
  (#.(swig-lispify "Column_setPartSize" 'function) (ff-pointer self) size))

(cl:defmethod #.(swig-lispify "set-stripe-size" 'method) ((self #.(swig-lispify "column" 'classname)) (size cl:integer))
  (#.(swig-lispify "Column_setStripeSize" 'function) (ff-pointer self) size))

(cl:defmethod #.(swig-lispify "set-partition-key" 'method) ((self #.(swig-lispify "column" 'classname)) (enable cl:t))
  (#.(swig-lispify "Column_setPartitionKey" 'function) (ff-pointer self) enable))

(cl:defmethod #.(swig-lispify "set-distribution-key" 'method) ((self #.(swig-lispify "column" 'classname)) (enable cl:t))
  (#.(swig-lispify "Column_setDistributionKey" 'function) (ff-pointer self) enable))

(cl:defmethod #.(swig-lispify "set-array-type" 'method) ((self #.(swig-lispify "column" 'classname)) (type cl:integer))
  (#.(swig-lispify "Column_setArrayType" 'function) (ff-pointer self) type))

(cl:defmethod #.(swig-lispify "set-storage-type" 'method) ((self #.(swig-lispify "column" 'classname)) (type cl:integer))
  (#.(swig-lispify "Column_setStorageType" 'function) (ff-pointer self) type))

(cl:defmethod #.(swig-lispify "set-dynamic" 'method) ((self #.(swig-lispify "column" 'classname)) (arg1 cl:t))
  (#.(swig-lispify "Column_setDynamic" 'function) (ff-pointer self) arg1))

(cl:defmethod #.(swig-lispify "set-default-value" 'method) ((self #.(swig-lispify "column" 'classname)) (arg1 cl:string))
  (#.(swig-lispify "Column_setDefaultValue" 'function) (ff-pointer self) arg1))

(cl:defmethod #.(swig-lispify "set-default-value" 'method) ((self #.(swig-lispify "column" 'classname)) buf (len cl:integer))
  (#.(swig-lispify "Column_setDefaultValue" 'function) (ff-pointer self) buf len))

(cl:defmethod #.(swig-lispify "get-default-value" 'method) ((self #.(swig-lispify "column" 'classname)) len)
  (#.(swig-lispify "Column_getDefaultValue" 'function) (ff-pointer self) len))

(cl:defmethod #.(swig-lispify "get-default-value" 'method) ((self #.(swig-lispify "column" 'classname)))
  (#.(swig-lispify "Column_getDefaultValue" 'function) (ff-pointer self)))

(cl:defmethod #.(swig-lispify "get-blob-table" 'method) ((self #.(swig-lispify "column" 'classname)))
  (#.(swig-lispify "Column_getBlobTable" 'function) (ff-pointer self)))

(cl:defmethod #.(swig-lispify "set-auto-increment" 'method) ((self #.(swig-lispify "column" 'classname)) (arg1 cl:t))
  (#.(swig-lispify "Column_setAutoIncrement" 'function) (ff-pointer self) arg1))

(cl:defmethod #.(swig-lispify "get-auto-increment" 'method) ((self #.(swig-lispify "column" 'classname)))
  (#.(swig-lispify "Column_getAutoIncrement" 'function) (ff-pointer self)))

(cl:defmethod #.(swig-lispify "set-auto-increment-initial-value" 'method) ((self #.(swig-lispify "column" 'classname)) val)
  (#.(swig-lispify "Column_setAutoIncrementInitialValue" 'function) (ff-pointer self) val))

(cl:defmethod #.(swig-lispify "get-size-in-bytes" 'method) ((self #.(swig-lispify "column" 'classname)))
  (#.(swig-lispify "Column_getSizeInBytes" 'function) (ff-pointer self)))

(cl:defmethod #.(swig-lispify "get-blob-version" 'method) ((self #.(swig-lispify "column" 'classname)))
  (#.(swig-lispify "Column_getBlobVersion" 'function) (ff-pointer self)))

(cl:defmethod #.(swig-lispify "set-blob-version" 'method) ((self #.(swig-lispify "column" 'classname)) (blobVersion cl:integer))
  (#.(swig-lispify "Column_setBlobVersion" 'function) (ff-pointer self) blobVersion))

(cl:defmethod #.(swig-lispify "is-bindable" 'method) ((self #.(swig-lispify "column" 'classname)) (arg1 #.(swig-lispify "column" 'classname)))
  (#.(swig-lispify "Column_isBindable" 'function) (ff-pointer self) (ff-pointer arg1)))


(cl:defclass #.(swig-lispify "table" 'classname)(#.(swig-lispify "ndb-dictionary::-object" 'classname))
  ((ff-pointer :reader ff-pointer)))

(cl:defmethod #.(swig-lispify "get-name" 'method) ((self #.(swig-lispify "table" 'classname)))
  (#.(swig-lispify "Table_getName" 'function) (ff-pointer self)))

(cl:defmethod #.(swig-lispify "get-table-id" 'method) ((self #.(swig-lispify "table" 'classname)))
  (#.(swig-lispify "Table_getTableId" 'function) (ff-pointer self)))

(cl:defmethod #.(swig-lispify "get-column" 'method) ((self #.(swig-lispify "table" 'classname)) (name cl:string))
  (#.(swig-lispify "Table_getColumn" 'function) (ff-pointer self) name))

(cl:defmethod #.(swig-lispify "get-column" 'method) ((self #.(swig-lispify "table" 'classname)) (attributeId cl:integer))
  (#.(swig-lispify "Table_getColumn" 'function) (ff-pointer self) attributeId))

(cl:defmethod #.(swig-lispify "get-column" 'method) ((self #.(swig-lispify "table" 'classname)) (name cl:string))
  (#.(swig-lispify "Table_getColumn" 'function) (ff-pointer self) name))

(cl:defmethod #.(swig-lispify "get-column" 'method) ((self #.(swig-lispify "table" 'classname)) (attributeId cl:integer))
  (#.(swig-lispify "Table_getColumn" 'function) (ff-pointer self) attributeId))

(cl:defmethod #.(swig-lispify "get-logging" 'method) ((self #.(swig-lispify "table" 'classname)))
  (#.(swig-lispify "Table_getLogging" 'function) (ff-pointer self)))

(cl:defmethod #.(swig-lispify "get-fragment-type" 'method) ((self #.(swig-lispify "table" 'classname)))
  (#.(swig-lispify "Table_getFragmentType" 'function) (ff-pointer self)))

(cl:defmethod #.(swig-lispify "get-kvalue" 'method) ((self #.(swig-lispify "table" 'classname)))
  (#.(swig-lispify "Table_getKValue" 'function) (ff-pointer self)))

(cl:defmethod #.(swig-lispify "get-min-load-factor" 'method) ((self #.(swig-lispify "table" 'classname)))
  (#.(swig-lispify "Table_getMinLoadFactor" 'function) (ff-pointer self)))

(cl:defmethod #.(swig-lispify "get-max-load-factor" 'method) ((self #.(swig-lispify "table" 'classname)))
  (#.(swig-lispify "Table_getMaxLoadFactor" 'function) (ff-pointer self)))

(cl:defmethod #.(swig-lispify "get-no-of-columns" 'method) ((self #.(swig-lispify "table" 'classname)))
  (#.(swig-lispify "Table_getNoOfColumns" 'function) (ff-pointer self)))

(cl:defmethod #.(swig-lispify "get-no-of-auto-increment-columns" 'method) ((self #.(swig-lispify "table" 'classname)))
  (#.(swig-lispify "Table_getNoOfAutoIncrementColumns" 'function) (ff-pointer self)))

(cl:defmethod #.(swig-lispify "get-no-of-primary-keys" 'method) ((self #.(swig-lispify "table" 'classname)))
  (#.(swig-lispify "Table_getNoOfPrimaryKeys" 'function) (ff-pointer self)))

(cl:defmethod #.(swig-lispify "get-primary-key" 'method) ((self #.(swig-lispify "table" 'classname)) (no cl:integer))
  (#.(swig-lispify "Table_getPrimaryKey" 'function) (ff-pointer self) no))

(cl:defmethod #.(swig-lispify "equal" 'method) ((self #.(swig-lispify "table" 'classname)) (arg1 #.(swig-lispify "table" 'classname)))
  (#.(swig-lispify "Table_equal" 'function) (ff-pointer self) (ff-pointer arg1)))

(cl:defmethod #.(swig-lispify "get-frm-data" 'method) ((self #.(swig-lispify "table" 'classname)))
  (#.(swig-lispify "Table_getFrmData" 'function) (ff-pointer self)))

(cl:defmethod #.(swig-lispify "get-frm-length" 'method) ((self #.(swig-lispify "table" 'classname)))
  (#.(swig-lispify "Table_getFrmLength" 'function) (ff-pointer self)))

(cl:defmethod #.(swig-lispify "get-default-record" 'method) ((self #.(swig-lispify "table" 'classname)))
  (#.(swig-lispify "Table_getDefaultRecord" 'function) (ff-pointer self)))

(cl:defmethod initialize-instance :after ((obj #.(swig-lispify "table" 'class)) &key (name cl:string))
  (setf (slot-value obj 'ff-pointer) (#.(swig-lispify "new_Table" 'function) name)))

(cl:defmethod initialize-instance :after ((obj #.(swig-lispify "table" 'class)) &key)
  (setf (slot-value obj 'ff-pointer) (#.(swig-lispify "new_Table" 'function))))

(cl:defmethod initialize-instance :after ((obj #.(swig-lispify "table" 'class)) &key (table #.(swig-lispify "table" 'classname)))
  (setf (slot-value obj 'ff-pointer) (#.(swig-lispify "new_Table" 'function) (ff-pointer table))))

(cl:shadow "=")
(cl:defmethod #.(swig-lispify "=" 'method) ((self #.(swig-lispify "table" 'classname)) (table #.(swig-lispify "table" 'classname)))
  (#.(swig-lispify "Table___assign__" 'function) (ff-pointer self) (ff-pointer table)))

(cl:defmethod #.(swig-lispify "set-name" 'method) ((self #.(swig-lispify "table" 'classname)) (name cl:string))
  (#.(swig-lispify "Table_setName" 'function) (ff-pointer self) name))

(cl:defmethod #.(swig-lispify "add-column" 'method) ((self #.(swig-lispify "table" 'classname)) (arg1 #.(swig-lispify "column" 'classname)))
  (#.(swig-lispify "Table_addColumn" 'function) (ff-pointer self) arg1))

(cl:defmethod #.(swig-lispify "set-logging" 'method) ((self #.(swig-lispify "table" 'classname)) (arg1 cl:t))
  (#.(swig-lispify "Table_setLogging" 'function) (ff-pointer self) arg1))

(cl:defmethod #.(swig-lispify "set-linear-flag" 'method) ((self #.(swig-lispify "table" 'classname)) (flag cl:integer))
  (#.(swig-lispify "Table_setLinearFlag" 'function) (ff-pointer self) flag))

(cl:defmethod #.(swig-lispify "get-linear-flag" 'method) ((self #.(swig-lispify "table" 'classname)))
  (#.(swig-lispify "Table_getLinearFlag" 'function) (ff-pointer self)))

(cl:defmethod #.(swig-lispify "set-fragment-count" 'method) ((self #.(swig-lispify "table" 'classname)) (arg1 cl:integer))
  (#.(swig-lispify "Table_setFragmentCount" 'function) (ff-pointer self) arg1))

(cl:defmethod #.(swig-lispify "get-fragment-count" 'method) ((self #.(swig-lispify "table" 'classname)))
  (#.(swig-lispify "Table_getFragmentCount" 'function) (ff-pointer self)))

(cl:defmethod #.(swig-lispify "get-partition-count" 'method) ((self #.(swig-lispify "table" 'classname)))
  (#.(swig-lispify "Table_getPartitionCount" 'function) (ff-pointer self)))

(cl:defmethod #.(swig-lispify "set-partition-balance" 'method) ((self #.(swig-lispify "table" 'classname)) (arg1 cl:integer))
  (#.(swig-lispify "Table_setPartitionBalance" 'function) (ff-pointer self) arg1))

(cl:defmethod #.(swig-lispify "get-partition-balance" 'method) ((self #.(swig-lispify "table" 'classname)))
  (#.(swig-lispify "Table_getPartitionBalance" 'function) (ff-pointer self)))

(cl:defmethod #.(swig-lispify "get-partition-balance-string" 'method) ((self #.(swig-lispify "table" 'classname)))
  (#.(swig-lispify "Table_getPartitionBalanceString" 'function) (ff-pointer self)))

(cl:defmethod #.(swig-lispify "set-fragment-type" 'method) ((self #.(swig-lispify "table" 'classname)) (arg1 cl:integer))
  (#.(swig-lispify "Table_setFragmentType" 'function) (ff-pointer self) arg1))

(cl:defmethod #.(swig-lispify "set-kvalue" 'method) ((self #.(swig-lispify "table" 'classname)) (kValue cl:integer))
  (#.(swig-lispify "Table_setKValue" 'function) (ff-pointer self) kValue))

(cl:defmethod #.(swig-lispify "set-min-load-factor" 'method) ((self #.(swig-lispify "table" 'classname)) (arg1 cl:integer))
  (#.(swig-lispify "Table_setMinLoadFactor" 'function) (ff-pointer self) arg1))

(cl:defmethod #.(swig-lispify "set-max-load-factor" 'method) ((self #.(swig-lispify "table" 'classname)) (arg1 cl:integer))
  (#.(swig-lispify "Table_setMaxLoadFactor" 'function) (ff-pointer self) arg1))

(cl:defmethod #.(swig-lispify "set-tablespace-name" 'method) ((self #.(swig-lispify "table" 'classname)) (name cl:string))
  (#.(swig-lispify "Table_setTablespaceName" 'function) (ff-pointer self) name))

(cl:defmethod #.(swig-lispify "get-tablespace-name" 'method) ((self #.(swig-lispify "table" 'classname)))
  (#.(swig-lispify "Table_getTablespaceName" 'function) (ff-pointer self)))

(cl:defmethod #.(swig-lispify "set-tablespace" 'method) ((self #.(swig-lispify "table" 'classname)) arg1)
  (#.(swig-lispify "Table_setTablespace" 'function) (ff-pointer self) arg1))

(cl:defmethod #.(swig-lispify "get-tablespace" 'method) ((self #.(swig-lispify "table" 'classname)) id version)
  (#.(swig-lispify "Table_getTablespace" 'function) (ff-pointer self) id version))

(cl:defmethod #.(swig-lispify "get-tablespace" 'method) ((self #.(swig-lispify "table" 'classname)) id)
  (#.(swig-lispify "Table_getTablespace" 'function) (ff-pointer self) id))

(cl:defmethod #.(swig-lispify "get-tablespace" 'method) ((self #.(swig-lispify "table" 'classname)))
  (#.(swig-lispify "Table_getTablespace" 'function) (ff-pointer self)))

(cl:defmethod #.(swig-lispify "get-hash-map" 'method) ((self #.(swig-lispify "table" 'classname)) id version)
  (#.(swig-lispify "Table_getHashMap" 'function) (ff-pointer self) id version))

(cl:defmethod #.(swig-lispify "get-hash-map" 'method) ((self #.(swig-lispify "table" 'classname)) id)
  (#.(swig-lispify "Table_getHashMap" 'function) (ff-pointer self) id))

(cl:defmethod #.(swig-lispify "get-hash-map" 'method) ((self #.(swig-lispify "table" 'classname)))
  (#.(swig-lispify "Table_getHashMap" 'function) (ff-pointer self)))

(cl:defmethod #.(swig-lispify "set-hash-map" 'method) ((self #.(swig-lispify "table" 'classname)) arg1)
  (#.(swig-lispify "Table_setHashMap" 'function) (ff-pointer self) arg1))

(cl:defmethod #.(swig-lispify "get-object-status" 'method) ((self #.(swig-lispify "table" 'classname)))
  (#.(swig-lispify "Table_getObjectStatus" 'function) (ff-pointer self)))

(cl:defmethod #.(swig-lispify "set-status-invalid" 'method) ((self #.(swig-lispify "table" 'classname)))
  (#.(swig-lispify "Table_setStatusInvalid" 'function) (ff-pointer self)))

(cl:defmethod #.(swig-lispify "get-object-version" 'method) ((self #.(swig-lispify "table" 'classname)))
  (#.(swig-lispify "Table_getObjectVersion" 'function) (ff-pointer self)))

(cl:defmethod #.(swig-lispify "set-default-no-partitions-flag" 'method) ((self #.(swig-lispify "table" 'classname)) (indicator cl:integer))
  (#.(swig-lispify "Table_setDefaultNoPartitionsFlag" 'function) (ff-pointer self) indicator))

(cl:defmethod #.(swig-lispify "get-default-no-partitions-flag" 'method) ((self #.(swig-lispify "table" 'classname)))
  (#.(swig-lispify "Table_getDefaultNoPartitionsFlag" 'function) (ff-pointer self)))

(cl:defmethod #.(swig-lispify "get-object-id" 'method) ((self #.(swig-lispify "table" 'classname)))
  (#.(swig-lispify "Table_getObjectId" 'function) (ff-pointer self)))

(cl:defmethod #.(swig-lispify "set-frm" 'method) ((self #.(swig-lispify "table" 'classname)) data (len cl:integer))
  (#.(swig-lispify "Table_setFrm" 'function) (ff-pointer self) data len))

(cl:defmethod #.(swig-lispify "set-extra-metadata" 'method) ((self #.(swig-lispify "table" 'classname)) (version cl:integer) data (data_length cl:integer))
  (#.(swig-lispify "Table_setExtraMetadata" 'function) (ff-pointer self) version data data_length))

(cl:defmethod #.(swig-lispify "get-extra-metadata" 'method) ((self #.(swig-lispify "table" 'classname)) version data data_length)
  (#.(swig-lispify "Table_getExtraMetadata" 'function) (ff-pointer self) version data data_length))

(cl:defmethod #.(swig-lispify "set-fragment-data" 'method) ((self #.(swig-lispify "table" 'classname)) nodegroups (cnt cl:integer))
  (#.(swig-lispify "Table_setFragmentData" 'function) (ff-pointer self) nodegroups cnt))

(cl:defmethod #.(swig-lispify "get-fragment-data" 'method) ((self #.(swig-lispify "table" 'classname)))
  (#.(swig-lispify "Table_getFragmentData" 'function) (ff-pointer self)))

(cl:defmethod #.(swig-lispify "get-fragment-data-len" 'method) ((self #.(swig-lispify "table" 'classname)))
  (#.(swig-lispify "Table_getFragmentDataLen" 'function) (ff-pointer self)))

(cl:defmethod #.(swig-lispify "set-range-list-data" 'method) ((self #.(swig-lispify "table" 'classname)) data (cnt cl:integer))
  (#.(swig-lispify "Table_setRangeListData" 'function) (ff-pointer self) data cnt))

(cl:defmethod #.(swig-lispify "get-range-list-data" 'method) ((self #.(swig-lispify "table" 'classname)))
  (#.(swig-lispify "Table_getRangeListData" 'function) (ff-pointer self)))

(cl:defmethod #.(swig-lispify "get-range-list-data-len" 'method) ((self #.(swig-lispify "table" 'classname)))
  (#.(swig-lispify "Table_getRangeListDataLen" 'function) (ff-pointer self)))

(cl:defmethod #.(swig-lispify "get-fragment-nodes" 'method) ((self #.(swig-lispify "table" 'classname)) (fragmentId cl:integer) nodeIdArrayPtr (arraySize cl:integer))
  (#.(swig-lispify "Table_getFragmentNodes" 'function) (ff-pointer self) fragmentId nodeIdArrayPtr arraySize))

(cl:defmethod #.(swig-lispify "set-max-rows" 'method) ((self #.(swig-lispify "table" 'classname)) maxRows)
  (#.(swig-lispify "Table_setMaxRows" 'function) (ff-pointer self) maxRows))

(cl:defmethod #.(swig-lispify "get-max-rows" 'method) ((self #.(swig-lispify "table" 'classname)))
  (#.(swig-lispify "Table_getMaxRows" 'function) (ff-pointer self)))

(cl:defmethod #.(swig-lispify "set-min-rows" 'method) ((self #.(swig-lispify "table" 'classname)) minRows)
  (#.(swig-lispify "Table_setMinRows" 'function) (ff-pointer self) minRows))

(cl:defmethod #.(swig-lispify "get-min-rows" 'method) ((self #.(swig-lispify "table" 'classname)))
  (#.(swig-lispify "Table_getMinRows" 'function) (ff-pointer self)))

(cl:defmethod #.(swig-lispify "set-single-user-mode" 'method) ((self #.(swig-lispify "table" 'classname)) (arg1 cl:integer))
  (#.(swig-lispify "Table_setSingleUserMode" 'function) (ff-pointer self) arg1))

(cl:defmethod #.(swig-lispify "get-single-user-mode" 'method) ((self #.(swig-lispify "table" 'classname)))
  (#.(swig-lispify "Table_getSingleUserMode" 'function) (ff-pointer self)))

(cl:defmethod #.(swig-lispify "set-row-gciindicator" 'method) ((self #.(swig-lispify "table" 'classname)) (value cl:t))
  (#.(swig-lispify "Table_setRowGCIIndicator" 'function) (ff-pointer self) value))

(cl:defmethod #.(swig-lispify "get-row-gciindicator" 'method) ((self #.(swig-lispify "table" 'classname)))
  (#.(swig-lispify "Table_getRowGCIIndicator" 'function) (ff-pointer self)))

(cl:defmethod #.(swig-lispify "set-row-checksum-indicator" 'method) ((self #.(swig-lispify "table" 'classname)) (value cl:t))
  (#.(swig-lispify "Table_setRowChecksumIndicator" 'function) (ff-pointer self) value))

(cl:defmethod #.(swig-lispify "get-row-checksum-indicator" 'method) ((self #.(swig-lispify "table" 'classname)))
  (#.(swig-lispify "Table_getRowChecksumIndicator" 'function) (ff-pointer self)))

(cl:defmethod #.(swig-lispify "set-read-backup-flag" 'method) ((self #.(swig-lispify "table" 'classname)) (value cl:t))
  (#.(swig-lispify "Table_setReadBackupFlag" 'function) (ff-pointer self) value))

(cl:defmethod #.(swig-lispify "get-read-backup-flag" 'method) ((self #.(swig-lispify "table" 'classname)))
  (#.(swig-lispify "Table_getReadBackupFlag" 'function) (ff-pointer self)))

(cl:defmethod #.(swig-lispify "get-mysql-name" 'method) ((self #.(swig-lispify "table" 'classname)))
  (#.(swig-lispify "Table_getMysqlName" 'function) (ff-pointer self)))

(cl:defmethod #.(swig-lispify "set-stored-table" 'method) ((self #.(swig-lispify "table" 'classname)) (x cl:t))
  (#.(swig-lispify "Table_setStoredTable" 'function) (ff-pointer self) x))

(cl:defmethod #.(swig-lispify "get-stored-table" 'method) ((self #.(swig-lispify "table" 'classname)))
  (#.(swig-lispify "Table_getStoredTable" 'function) (ff-pointer self)))

(cl:defmethod #.(swig-lispify "get-row-size-in-bytes" 'method) ((self #.(swig-lispify "table" 'classname)))
  (#.(swig-lispify "Table_getRowSizeInBytes" 'function) (ff-pointer self)))

(cl:defmethod #.(swig-lispify "create-table-in-db" 'method) ((self #.(swig-lispify "table" 'classname)) arg1 (existingEqualIsOk cl:t))
  (#.(swig-lispify "Table_createTableInDb" 'function) (ff-pointer self) arg1 existingEqualIsOk))

(cl:defmethod #.(swig-lispify "create-table-in-db" 'method) ((self #.(swig-lispify "table" 'classname)) arg1)
  (#.(swig-lispify "Table_createTableInDb" 'function) (ff-pointer self) arg1))

(cl:defmethod #.(swig-lispify "get-replica-count" 'method) ((self #.(swig-lispify "table" 'classname)))
  (#.(swig-lispify "Table_getReplicaCount" 'function) (ff-pointer self)))

(cl:defmethod #.(swig-lispify "get-temporary" 'method) ((self #.(swig-lispify "table" 'classname)))
  (#.(swig-lispify "Table_getTemporary" 'function) (ff-pointer self)))

(cl:defmethod #.(swig-lispify "set-temporary" 'method) ((self #.(swig-lispify "table" 'classname)) (arg1 cl:t))
  (#.(swig-lispify "Table_setTemporary" 'function) (ff-pointer self) arg1))

(cl:defmethod #.(swig-lispify "get-force-var-part" 'method) ((self #.(swig-lispify "table" 'classname)))
  (#.(swig-lispify "Table_getForceVarPart" 'function) (ff-pointer self)))

(cl:defmethod #.(swig-lispify "set-force-var-part" 'method) ((self #.(swig-lispify "table" 'classname)) (arg1 cl:t))
  (#.(swig-lispify "Table_setForceVarPart" 'function) (ff-pointer self) arg1))

(cl:defmethod #.(swig-lispify "check-columns" 'method) ((self #.(swig-lispify "table" 'classname)) bitmap (len_in_bytes cl:integer))
  (#.(swig-lispify "Table_checkColumns" 'function) (ff-pointer self) bitmap len_in_bytes))

(cl:defmethod #.(swig-lispify "assign-obj-id" 'method) ((self #.(swig-lispify "table" 'classname)) (arg1 #.(swig-lispify "object-id" 'classname)))
  (#.(swig-lispify "Table_assignObjId" 'function) (ff-pointer self) arg1))

(cl:defmethod #.(swig-lispify "set-storage-type" 'method) ((self #.(swig-lispify "table" 'classname)) (arg1 cl:integer))
  (#.(swig-lispify "Table_setStorageType" 'function) (ff-pointer self) arg1))

(cl:defmethod #.(swig-lispify "get-storage-type" 'method) ((self #.(swig-lispify "table" 'classname)))
  (#.(swig-lispify "Table_getStorageType" 'function) (ff-pointer self)))

(cl:defmethod #.(swig-lispify "set-extra-row-gci-bits" 'method) ((self #.(swig-lispify "table" 'classname)) (arg1 cl:integer))
  (#.(swig-lispify "Table_setExtraRowGciBits" 'function) (ff-pointer self) arg1))

(cl:defmethod #.(swig-lispify "get-extra-row-gci-bits" 'method) ((self #.(swig-lispify "table" 'classname)))
  (#.(swig-lispify "Table_getExtraRowGciBits" 'function) (ff-pointer self)))

(cl:defmethod #.(swig-lispify "set-extra-row-author-bits" 'method) ((self #.(swig-lispify "table" 'classname)) (arg1 cl:integer))
  (#.(swig-lispify "Table_setExtraRowAuthorBits" 'function) (ff-pointer self) arg1))

(cl:defmethod #.(swig-lispify "get-extra-row-author-bits" 'method) ((self #.(swig-lispify "table" 'classname)))
  (#.(swig-lispify "Table_getExtraRowAuthorBits" 'function) (ff-pointer self)))

(cl:defmethod #.(swig-lispify "set-fully-replicated" 'method) ((self #.(swig-lispify "table" 'classname)) (val cl:t))
  (#.(swig-lispify "Table_setFullyReplicated" 'function) (ff-pointer self) val))

(cl:defmethod #.(swig-lispify "get-fully-replicated" 'method) ((self #.(swig-lispify "table" 'classname)))
  (#.(swig-lispify "Table_getFullyReplicated" 'function) (ff-pointer self)))

(cl:defmethod #.(swig-lispify "set-row-checksum" 'method) ((self #.(swig-lispify "table" 'classname)) (arg1 cl:integer))
  (#.(swig-lispify "Table_setRowChecksum" 'function) (ff-pointer self) arg1))

(cl:defmethod #.(swig-lispify "get-row-checksum" 'method) ((self #.(swig-lispify "table" 'classname)))
  (#.(swig-lispify "Table_getRowChecksum" 'function) (ff-pointer self)))

(cl:defmethod #.(swig-lispify "aggregate" 'method) ((self #.(swig-lispify "table" 'classname)) error)
  (#.(swig-lispify "Table_aggregate" 'function) (ff-pointer self) error))

(cl:defmethod #.(swig-lispify "validate" 'method) ((self #.(swig-lispify "table" 'classname)) error)
  (#.(swig-lispify "Table_validate" 'function) (ff-pointer self) error))

(cl:defmethod #.(swig-lispify "get-partition-id" 'method) ((self #.(swig-lispify "table" 'classname)) (hashvalue cl:integer))
  (#.(swig-lispify "Table_getPartitionId" 'function) (ff-pointer self) hashvalue))

(cl:defmethod #.(swig-lispify "has-default-values" 'method) ((self #.(swig-lispify "table" 'classname)))
  (#.(swig-lispify "Table_hasDefaultValues" 'function) (ff-pointer self)))


(cl:defclass #.(swig-lispify "index" 'classname)(#.(swig-lispify "ndb-dictionary::-object" 'classname))
  ((ff-pointer :reader ff-pointer)))

(cl:defmethod #.(swig-lispify "get-name" 'method) ((self #.(swig-lispify "index" 'classname)))
  (#.(swig-lispify "Index_getName" 'function) (ff-pointer self)))

(cl:defmethod #.(swig-lispify "get-table" 'method) ((self #.(swig-lispify "index" 'classname)))
  (#.(swig-lispify "Index_getTable" 'function) (ff-pointer self)))

(cl:defmethod #.(swig-lispify "get-no-of-columns" 'method) ((self #.(swig-lispify "index" 'classname)))
  (#.(swig-lispify "Index_getNoOfColumns" 'function) (ff-pointer self)))

(cl:defmethod #.(swig-lispify "get-no-of-index-columns" 'method) ((self #.(swig-lispify "index" 'classname)))
  (#.(swig-lispify "Index_getNoOfIndexColumns" 'function) (ff-pointer self)))

(cl:defmethod #.(swig-lispify "get-column" 'method) ((self #.(swig-lispify "index" 'classname)) (no cl:integer))
  (#.(swig-lispify "Index_getColumn" 'function) (ff-pointer self) no))

(cl:defmethod #.(swig-lispify "get-index-column" 'method) ((self #.(swig-lispify "index" 'classname)) (no cl:integer))
  (#.(swig-lispify "Index_getIndexColumn" 'function) (ff-pointer self) no))

(cl:defmethod #.(swig-lispify "get-type" 'method) ((self #.(swig-lispify "index" 'classname)))
  (#.(swig-lispify "Index_getType" 'function) (ff-pointer self)))

(cl:defmethod #.(swig-lispify "get-logging" 'method) ((self #.(swig-lispify "index" 'classname)))
  (#.(swig-lispify "Index_getLogging" 'function) (ff-pointer self)))

(cl:defmethod #.(swig-lispify "get-object-status" 'method) ((self #.(swig-lispify "index" 'classname)))
  (#.(swig-lispify "Index_getObjectStatus" 'function) (ff-pointer self)))

(cl:defmethod #.(swig-lispify "get-object-version" 'method) ((self #.(swig-lispify "index" 'classname)))
  (#.(swig-lispify "Index_getObjectVersion" 'function) (ff-pointer self)))

(cl:defmethod #.(swig-lispify "get-object-id" 'method) ((self #.(swig-lispify "index" 'classname)))
  (#.(swig-lispify "Index_getObjectId" 'function) (ff-pointer self)))

(cl:defmethod #.(swig-lispify "get-default-record" 'method) ((self #.(swig-lispify "index" 'classname)))
  (#.(swig-lispify "Index_getDefaultRecord" 'function) (ff-pointer self)))

(cl:defmethod initialize-instance :after ((obj #.(swig-lispify "index" 'class)) &key (name cl:string))
  (setf (slot-value obj 'ff-pointer) (#.(swig-lispify "new_Index" 'function) name)))

(cl:defmethod initialize-instance :after ((obj #.(swig-lispify "index" 'class)) &key)
  (setf (slot-value obj 'ff-pointer) (#.(swig-lispify "new_Index" 'function))))

(cl:defmethod #.(swig-lispify "set-name" 'method) ((self #.(swig-lispify "index" 'classname)) (name cl:string))
  (#.(swig-lispify "Index_setName" 'function) (ff-pointer self) name))

(cl:defmethod #.(swig-lispify "set-table" 'method) ((self #.(swig-lispify "index" 'classname)) (name cl:string))
  (#.(swig-lispify "Index_setTable" 'function) (ff-pointer self) name))

(cl:defmethod #.(swig-lispify "add-column" 'method) ((self #.(swig-lispify "index" 'classname)) (c #.(swig-lispify "column" 'classname)))
  (#.(swig-lispify "Index_addColumn" 'function) (ff-pointer self) c))

(cl:defmethod #.(swig-lispify "add-column-name" 'method) ((self #.(swig-lispify "index" 'classname)) (name cl:string))
  (#.(swig-lispify "Index_addColumnName" 'function) (ff-pointer self) name))

(cl:defmethod #.(swig-lispify "add-index-column" 'method) ((self #.(swig-lispify "index" 'classname)) (name cl:string))
  (#.(swig-lispify "Index_addIndexColumn" 'function) (ff-pointer self) name))

(cl:defmethod #.(swig-lispify "add-column-names" 'method) ((self #.(swig-lispify "index" 'classname)) (noOfNames cl:integer) names)
  (#.(swig-lispify "Index_addColumnNames" 'function) (ff-pointer self) noOfNames names))

(cl:defmethod #.(swig-lispify "add-index-columns" 'method) ((self #.(swig-lispify "index" 'classname)) (noOfNames cl:integer) names)
  (#.(swig-lispify "Index_addIndexColumns" 'function) (ff-pointer self) noOfNames names))

(cl:defmethod #.(swig-lispify "set-type" 'method) ((self #.(swig-lispify "index" 'classname)) (type cl:integer))
  (#.(swig-lispify "Index_setType" 'function) (ff-pointer self) type))

(cl:defmethod #.(swig-lispify "set-logging" 'method) ((self #.(swig-lispify "index" 'classname)) (enable cl:t))
  (#.(swig-lispify "Index_setLogging" 'function) (ff-pointer self) enable))

(cl:defmethod #.(swig-lispify "set-stored-index" 'method) ((self #.(swig-lispify "index" 'classname)) (x cl:t))
  (#.(swig-lispify "Index_setStoredIndex" 'function) (ff-pointer self) x))

(cl:defmethod #.(swig-lispify "get-stored-index" 'method) ((self #.(swig-lispify "index" 'classname)))
  (#.(swig-lispify "Index_getStoredIndex" 'function) (ff-pointer self)))

(cl:defmethod #.(swig-lispify "get-temporary" 'method) ((self #.(swig-lispify "index" 'classname)))
  (#.(swig-lispify "Index_getTemporary" 'function) (ff-pointer self)))

(cl:defmethod #.(swig-lispify "set-temporary" 'method) ((self #.(swig-lispify "index" 'classname)) (arg1 cl:t))
  (#.(swig-lispify "Index_setTemporary" 'function) (ff-pointer self) arg1))


(cl:defclass #.(swig-lispify "optimize-table-handle" 'classname)()
  ((ff-pointer :reader ff-pointer)))

(cl:defmethod initialize-instance :after ((obj #.(swig-lispify "optimize-table-handle" 'class)) &key)
  (setf (slot-value obj 'ff-pointer) (#.(swig-lispify "new_OptimizeTableHandle" 'function))))

(cl:defmethod #.(swig-lispify "next" 'method) ((self #.(swig-lispify "optimize-table-handle" 'classname)))
  (#.(swig-lispify "OptimizeTableHandle_next" 'function) (ff-pointer self)))

(cl:defmethod #.(swig-lispify "close" 'method) ((self #.(swig-lispify "optimize-table-handle" 'classname)))
  (#.(swig-lispify "OptimizeTableHandle_close" 'function) (ff-pointer self)))


(cl:defclass #.(swig-lispify "optimize-index-handle" 'classname)()
  ((ff-pointer :reader ff-pointer)))

(cl:defmethod initialize-instance :after ((obj #.(swig-lispify "optimize-index-handle" 'class)) &key)
  (setf (slot-value obj 'ff-pointer) (#.(swig-lispify "new_OptimizeIndexHandle" 'function))))

(cl:defmethod #.(swig-lispify "next" 'method) ((self #.(swig-lispify "optimize-index-handle" 'classname)))
  (#.(swig-lispify "OptimizeIndexHandle_next" 'function) (ff-pointer self)))

(cl:defmethod #.(swig-lispify "close" 'method) ((self #.(swig-lispify "optimize-index-handle" 'classname)))
  (#.(swig-lispify "OptimizeIndexHandle_close" 'function) (ff-pointer self)))


(cl:defclass #.(swig-lispify "event" 'classname)(#.(swig-lispify "ndb-dictionary::-object" 'classname))
  ((ff-pointer :reader ff-pointer)))

(cl:defmethod initialize-instance :after ((obj #.(swig-lispify "event" 'class)) &key (name cl:string))
  (setf (slot-value obj 'ff-pointer) (#.(swig-lispify "new_Event" 'function) name)))

(cl:defmethod initialize-instance :after ((obj #.(swig-lispify "event" 'class)) &key (name cl:string) (table #.(swig-lispify "table" 'classname)))
  (setf (slot-value obj 'ff-pointer) (#.(swig-lispify "new_Event" 'function) name table)))

(cl:defmethod #.(swig-lispify "set-name" 'method) ((self #.(swig-lispify "event" 'classname)) (name cl:string))
  (#.(swig-lispify "Event_setName" 'function) (ff-pointer self) name))

(cl:defmethod #.(swig-lispify "get-name" 'method) ((self #.(swig-lispify "event" 'classname)))
  (#.(swig-lispify "Event_getName" 'function) (ff-pointer self)))

(cl:defmethod #.(swig-lispify "get-table" 'method) ((self #.(swig-lispify "event" 'classname)))
  (#.(swig-lispify "Event_getTable" 'function) (ff-pointer self)))

(cl:defmethod #.(swig-lispify "set-table" 'method) ((self #.(swig-lispify "event" 'classname)) (table #.(swig-lispify "table" 'classname)))
  (#.(swig-lispify "Event_setTable" 'function) (ff-pointer self) table))

(cl:defmethod #.(swig-lispify "set-table" 'method) ((self #.(swig-lispify "event" 'classname)) (table #.(swig-lispify "table" 'classname)))
  (#.(swig-lispify "Event_setTable" 'function) (ff-pointer self) table))

(cl:defmethod #.(swig-lispify "set-table" 'method) ((self #.(swig-lispify "event" 'classname)) (tableName cl:string))
  (#.(swig-lispify "Event_setTable" 'function) (ff-pointer self) tableName))

(cl:defmethod #.(swig-lispify "get-table-name" 'method) ((self #.(swig-lispify "event" 'classname)))
  (#.(swig-lispify "Event_getTableName" 'function) (ff-pointer self)))

(cl:defmethod #.(swig-lispify "add-table-event" 'method) ((self #.(swig-lispify "event" 'classname)) (te cl:integer))
  (#.(swig-lispify "Event_addTableEvent" 'function) (ff-pointer self) te))

(cl:defmethod #.(swig-lispify "get-table-event" 'method) ((self #.(swig-lispify "event" 'classname)) (te cl:integer))
  (#.(swig-lispify "Event_getTableEvent" 'function) (ff-pointer self) te))

(cl:defmethod #.(swig-lispify "set-durability" 'method) ((self #.(swig-lispify "event" 'classname)) (arg1 cl:integer))
  (#.(swig-lispify "Event_setDurability" 'function) (ff-pointer self) arg1))

(cl:defmethod #.(swig-lispify "get-durability" 'method) ((self #.(swig-lispify "event" 'classname)))
  (#.(swig-lispify "Event_getDurability" 'function) (ff-pointer self)))

(cl:defmethod #.(swig-lispify "set-report" 'method) ((self #.(swig-lispify "event" 'classname)) (arg1 cl:integer))
  (#.(swig-lispify "Event_setReport" 'function) (ff-pointer self) arg1))

(cl:defmethod #.(swig-lispify "get-report" 'method) ((self #.(swig-lispify "event" 'classname)))
  (#.(swig-lispify "Event_getReport" 'function) (ff-pointer self)))

(cl:defmethod #.(swig-lispify "add-column" 'method) ((self #.(swig-lispify "event" 'classname)) (c #.(swig-lispify "column" 'classname)))
  (#.(swig-lispify "Event_addColumn" 'function) (ff-pointer self) c))

(cl:defmethod #.(swig-lispify "add-event-column" 'method) ((self #.(swig-lispify "event" 'classname)) (attrId cl:integer))
  (#.(swig-lispify "Event_addEventColumn" 'function) (ff-pointer self) attrId))

(cl:defmethod #.(swig-lispify "add-event-column" 'method) ((self #.(swig-lispify "event" 'classname)) (columnName cl:string))
  (#.(swig-lispify "Event_addEventColumn" 'function) (ff-pointer self) columnName))

(cl:defmethod #.(swig-lispify "add-event-columns" 'method) ((self #.(swig-lispify "event" 'classname)) (n cl:integer) columnNames)
  (#.(swig-lispify "Event_addEventColumns" 'function) (ff-pointer self) n columnNames))

(cl:defmethod #.(swig-lispify "get-no-of-event-columns" 'method) ((self #.(swig-lispify "event" 'classname)))
  (#.(swig-lispify "Event_getNoOfEventColumns" 'function) (ff-pointer self)))

(cl:defmethod #.(swig-lispify "get-event-column" 'method) ((self #.(swig-lispify "event" 'classname)) (no cl:integer))
  (#.(swig-lispify "Event_getEventColumn" 'function) (ff-pointer self) no))

(cl:defmethod #.(swig-lispify "merge-events" 'method) ((self #.(swig-lispify "event" 'classname)) (flag cl:t))
  (#.(swig-lispify "Event_mergeEvents" 'function) (ff-pointer self) flag))

(cl:defmethod #.(swig-lispify "get-object-status" 'method) ((self #.(swig-lispify "event" 'classname)))
  (#.(swig-lispify "Event_getObjectStatus" 'function) (ff-pointer self)))

(cl:defmethod #.(swig-lispify "get-object-version" 'method) ((self #.(swig-lispify "event" 'classname)))
  (#.(swig-lispify "Event_getObjectVersion" 'function) (ff-pointer self)))

(cl:defmethod #.(swig-lispify "get-object-id" 'method) ((self #.(swig-lispify "event" 'classname)))
  (#.(swig-lispify "Event_getObjectId" 'function) (ff-pointer self)))

(cl:defmethod #.(swig-lispify "print" 'method) ((self #.(swig-lispify "event" 'classname)))
  (#.(swig-lispify "Event_print" 'function) (ff-pointer self)))


(cl:defclass #.(swig-lispify "logfile-group" 'classname)(#.(swig-lispify "ndb-dictionary::-object" 'classname))
  ((ff-pointer :reader ff-pointer)))

(cl:defmethod initialize-instance :after ((obj #.(swig-lispify "logfile-group" 'class)) &key)
  (setf (slot-value obj 'ff-pointer) (#.(swig-lispify "new_LogfileGroup" 'function))))

(cl:defmethod initialize-instance :after ((obj #.(swig-lispify "logfile-group" 'class)) &key (arg0 #.(swig-lispify "logfile-group" 'classname)))
  (setf (slot-value obj 'ff-pointer) (#.(swig-lispify "new_LogfileGroup" 'function) (ff-pointer arg0))))

(cl:defmethod #.(swig-lispify "set-name" 'method) ((self #.(swig-lispify "logfile-group" 'classname)) (name cl:string))
  (#.(swig-lispify "LogfileGroup_setName" 'function) (ff-pointer self) name))

(cl:defmethod #.(swig-lispify "get-name" 'method) ((self #.(swig-lispify "logfile-group" 'classname)))
  (#.(swig-lispify "LogfileGroup_getName" 'function) (ff-pointer self)))

(cl:defmethod #.(swig-lispify "set-undo-buffer-size" 'method) ((self #.(swig-lispify "logfile-group" 'classname)) (sz cl:integer))
  (#.(swig-lispify "LogfileGroup_setUndoBufferSize" 'function) (ff-pointer self) sz))

(cl:defmethod #.(swig-lispify "get-undo-buffer-size" 'method) ((self #.(swig-lispify "logfile-group" 'classname)))
  (#.(swig-lispify "LogfileGroup_getUndoBufferSize" 'function) (ff-pointer self)))

(cl:defmethod #.(swig-lispify "set-auto-grow-specification" 'method) ((self #.(swig-lispify "logfile-group" 'classname)) arg1)
  (#.(swig-lispify "LogfileGroup_setAutoGrowSpecification" 'function) (ff-pointer self) arg1))

(cl:defmethod #.(swig-lispify "get-auto-grow-specification" 'method) ((self #.(swig-lispify "logfile-group" 'classname)))
  (#.(swig-lispify "LogfileGroup_getAutoGrowSpecification" 'function) (ff-pointer self)))

(cl:defmethod #.(swig-lispify "get-undo-free-words" 'method) ((self #.(swig-lispify "logfile-group" 'classname)))
  (#.(swig-lispify "LogfileGroup_getUndoFreeWords" 'function) (ff-pointer self)))

(cl:defmethod #.(swig-lispify "get-object-status" 'method) ((self #.(swig-lispify "logfile-group" 'classname)))
  (#.(swig-lispify "LogfileGroup_getObjectStatus" 'function) (ff-pointer self)))

(cl:defmethod #.(swig-lispify "get-object-version" 'method) ((self #.(swig-lispify "logfile-group" 'classname)))
  (#.(swig-lispify "LogfileGroup_getObjectVersion" 'function) (ff-pointer self)))

(cl:defmethod #.(swig-lispify "get-object-id" 'method) ((self #.(swig-lispify "logfile-group" 'classname)))
  (#.(swig-lispify "LogfileGroup_getObjectId" 'function) (ff-pointer self)))


(cl:defclass #.(swig-lispify "tablespace" 'classname)(#.(swig-lispify "ndb-dictionary::-object" 'classname))
  ((ff-pointer :reader ff-pointer)))

(cl:defmethod initialize-instance :after ((obj #.(swig-lispify "tablespace" 'class)) &key)
  (setf (slot-value obj 'ff-pointer) (#.(swig-lispify "new_Tablespace" 'function))))

(cl:defmethod initialize-instance :after ((obj #.(swig-lispify "tablespace" 'class)) &key (arg0 #.(swig-lispify "tablespace" 'classname)))
  (setf (slot-value obj 'ff-pointer) (#.(swig-lispify "new_Tablespace" 'function) (ff-pointer arg0))))

(cl:defmethod #.(swig-lispify "set-name" 'method) ((self #.(swig-lispify "tablespace" 'classname)) (name cl:string))
  (#.(swig-lispify "Tablespace_setName" 'function) (ff-pointer self) name))

(cl:defmethod #.(swig-lispify "get-name" 'method) ((self #.(swig-lispify "tablespace" 'classname)))
  (#.(swig-lispify "Tablespace_getName" 'function) (ff-pointer self)))

(cl:defmethod #.(swig-lispify "set-extent-size" 'method) ((self #.(swig-lispify "tablespace" 'classname)) (sz cl:integer))
  (#.(swig-lispify "Tablespace_setExtentSize" 'function) (ff-pointer self) sz))

(cl:defmethod #.(swig-lispify "get-extent-size" 'method) ((self #.(swig-lispify "tablespace" 'classname)))
  (#.(swig-lispify "Tablespace_getExtentSize" 'function) (ff-pointer self)))

(cl:defmethod #.(swig-lispify "set-auto-grow-specification" 'method) ((self #.(swig-lispify "tablespace" 'classname)) arg1)
  (#.(swig-lispify "Tablespace_setAutoGrowSpecification" 'function) (ff-pointer self) arg1))

(cl:defmethod #.(swig-lispify "get-auto-grow-specification" 'method) ((self #.(swig-lispify "tablespace" 'classname)))
  (#.(swig-lispify "Tablespace_getAutoGrowSpecification" 'function) (ff-pointer self)))

(cl:defmethod #.(swig-lispify "set-default-logfile-group" 'method) ((self #.(swig-lispify "tablespace" 'classname)) (name cl:string))
  (#.(swig-lispify "Tablespace_setDefaultLogfileGroup" 'function) (ff-pointer self) name))

(cl:defmethod #.(swig-lispify "set-default-logfile-group" 'method) ((self #.(swig-lispify "tablespace" 'classname)) (arg1 #.(swig-lispify "logfile-group" 'classname)))
  (#.(swig-lispify "Tablespace_setDefaultLogfileGroup" 'function) (ff-pointer self) arg1))

(cl:defmethod #.(swig-lispify "get-default-logfile-group" 'method) ((self #.(swig-lispify "tablespace" 'classname)))
  (#.(swig-lispify "Tablespace_getDefaultLogfileGroup" 'function) (ff-pointer self)))

(cl:defmethod #.(swig-lispify "get-default-logfile-group-id" 'method) ((self #.(swig-lispify "tablespace" 'classname)))
  (#.(swig-lispify "Tablespace_getDefaultLogfileGroupId" 'function) (ff-pointer self)))

(cl:defmethod #.(swig-lispify "get-object-status" 'method) ((self #.(swig-lispify "tablespace" 'classname)))
  (#.(swig-lispify "Tablespace_getObjectStatus" 'function) (ff-pointer self)))

(cl:defmethod #.(swig-lispify "get-object-version" 'method) ((self #.(swig-lispify "tablespace" 'classname)))
  (#.(swig-lispify "Tablespace_getObjectVersion" 'function) (ff-pointer self)))

(cl:defmethod #.(swig-lispify "get-object-id" 'method) ((self #.(swig-lispify "tablespace" 'classname)))
  (#.(swig-lispify "Tablespace_getObjectId" 'function) (ff-pointer self)))


(cl:defclass #.(swig-lispify "datafile" 'classname)(#.(swig-lispify "ndb-dictionary::-object" 'classname))
  ((ff-pointer :reader ff-pointer)))

(cl:defmethod initialize-instance :after ((obj #.(swig-lispify "datafile" 'class)) &key)
  (setf (slot-value obj 'ff-pointer) (#.(swig-lispify "new_Datafile" 'function))))

(cl:defmethod initialize-instance :after ((obj #.(swig-lispify "datafile" 'class)) &key (arg0 #.(swig-lispify "datafile" 'classname)))
  (setf (slot-value obj 'ff-pointer) (#.(swig-lispify "new_Datafile" 'function) (ff-pointer arg0))))

(cl:defmethod #.(swig-lispify "set-path" 'method) ((self #.(swig-lispify "datafile" 'classname)) (name cl:string))
  (#.(swig-lispify "Datafile_setPath" 'function) (ff-pointer self) name))

(cl:defmethod #.(swig-lispify "get-path" 'method) ((self #.(swig-lispify "datafile" 'classname)))
  (#.(swig-lispify "Datafile_getPath" 'function) (ff-pointer self)))

(cl:defmethod #.(swig-lispify "set-size" 'method) ((self #.(swig-lispify "datafile" 'classname)) arg1)
  (#.(swig-lispify "Datafile_setSize" 'function) (ff-pointer self) arg1))

(cl:defmethod #.(swig-lispify "get-size" 'method) ((self #.(swig-lispify "datafile" 'classname)))
  (#.(swig-lispify "Datafile_getSize" 'function) (ff-pointer self)))

(cl:defmethod #.(swig-lispify "get-free" 'method) ((self #.(swig-lispify "datafile" 'classname)))
  (#.(swig-lispify "Datafile_getFree" 'function) (ff-pointer self)))

(cl:defmethod #.(swig-lispify "set-tablespace" 'method) ((self #.(swig-lispify "datafile" 'classname)) (name cl:string))
  (#.(swig-lispify "Datafile_setTablespace" 'function) (ff-pointer self) name))

(cl:defmethod #.(swig-lispify "set-tablespace" 'method) ((self #.(swig-lispify "datafile" 'classname)) (arg1 #.(swig-lispify "tablespace" 'classname)))
  (#.(swig-lispify "Datafile_setTablespace" 'function) (ff-pointer self) arg1))

(cl:defmethod #.(swig-lispify "get-tablespace" 'method) ((self #.(swig-lispify "datafile" 'classname)))
  (#.(swig-lispify "Datafile_getTablespace" 'function) (ff-pointer self)))

(cl:defmethod #.(swig-lispify "get-tablespace-id" 'method) ((self #.(swig-lispify "datafile" 'classname)) (dst #.(swig-lispify "object-id" 'classname)))
  (#.(swig-lispify "Datafile_getTablespaceId" 'function) (ff-pointer self) dst))

(cl:defmethod #.(swig-lispify "get-object-status" 'method) ((self #.(swig-lispify "datafile" 'classname)))
  (#.(swig-lispify "Datafile_getObjectStatus" 'function) (ff-pointer self)))

(cl:defmethod #.(swig-lispify "get-object-version" 'method) ((self #.(swig-lispify "datafile" 'classname)))
  (#.(swig-lispify "Datafile_getObjectVersion" 'function) (ff-pointer self)))

(cl:defmethod #.(swig-lispify "get-object-id" 'method) ((self #.(swig-lispify "datafile" 'classname)))
  (#.(swig-lispify "Datafile_getObjectId" 'function) (ff-pointer self)))


(cl:defclass #.(swig-lispify "undofile" 'classname)(#.(swig-lispify "ndb-dictionary::-object" 'classname))
  ((ff-pointer :reader ff-pointer)))

(cl:defmethod initialize-instance :after ((obj #.(swig-lispify "undofile" 'class)) &key)
  (setf (slot-value obj 'ff-pointer) (#.(swig-lispify "new_Undofile" 'function))))

(cl:defmethod initialize-instance :after ((obj #.(swig-lispify "undofile" 'class)) &key (arg0 #.(swig-lispify "undofile" 'classname)))
  (setf (slot-value obj 'ff-pointer) (#.(swig-lispify "new_Undofile" 'function) (ff-pointer arg0))))

(cl:defmethod #.(swig-lispify "set-path" 'method) ((self #.(swig-lispify "undofile" 'classname)) (path cl:string))
  (#.(swig-lispify "Undofile_setPath" 'function) (ff-pointer self) path))

(cl:defmethod #.(swig-lispify "get-path" 'method) ((self #.(swig-lispify "undofile" 'classname)))
  (#.(swig-lispify "Undofile_getPath" 'function) (ff-pointer self)))

(cl:defmethod #.(swig-lispify "set-size" 'method) ((self #.(swig-lispify "undofile" 'classname)) arg1)
  (#.(swig-lispify "Undofile_setSize" 'function) (ff-pointer self) arg1))

(cl:defmethod #.(swig-lispify "get-size" 'method) ((self #.(swig-lispify "undofile" 'classname)))
  (#.(swig-lispify "Undofile_getSize" 'function) (ff-pointer self)))

(cl:defmethod #.(swig-lispify "set-logfile-group" 'method) ((self #.(swig-lispify "undofile" 'classname)) (name cl:string))
  (#.(swig-lispify "Undofile_setLogfileGroup" 'function) (ff-pointer self) name))

(cl:defmethod #.(swig-lispify "set-logfile-group" 'method) ((self #.(swig-lispify "undofile" 'classname)) (arg1 #.(swig-lispify "logfile-group" 'classname)))
  (#.(swig-lispify "Undofile_setLogfileGroup" 'function) (ff-pointer self) arg1))

(cl:defmethod #.(swig-lispify "get-logfile-group" 'method) ((self #.(swig-lispify "undofile" 'classname)))
  (#.(swig-lispify "Undofile_getLogfileGroup" 'function) (ff-pointer self)))

(cl:defmethod #.(swig-lispify "get-logfile-group-id" 'method) ((self #.(swig-lispify "undofile" 'classname)) (dst #.(swig-lispify "object-id" 'classname)))
  (#.(swig-lispify "Undofile_getLogfileGroupId" 'function) (ff-pointer self) dst))

(cl:defmethod #.(swig-lispify "get-object-status" 'method) ((self #.(swig-lispify "undofile" 'classname)))
  (#.(swig-lispify "Undofile_getObjectStatus" 'function) (ff-pointer self)))

(cl:defmethod #.(swig-lispify "get-object-version" 'method) ((self #.(swig-lispify "undofile" 'classname)))
  (#.(swig-lispify "Undofile_getObjectVersion" 'function) (ff-pointer self)))

(cl:defmethod #.(swig-lispify "get-object-id" 'method) ((self #.(swig-lispify "undofile" 'classname)))
  (#.(swig-lispify "Undofile_getObjectId" 'function) (ff-pointer self)))


(cl:defclass #.(swig-lispify "hash-map" 'classname)(#.(swig-lispify "ndb-dictionary::-object" 'classname))
  ((ff-pointer :reader ff-pointer)))

(cl:defmethod initialize-instance :after ((obj #.(swig-lispify "hash-map" 'class)) &key)
  (setf (slot-value obj 'ff-pointer) (#.(swig-lispify "new_HashMap" 'function))))

(cl:defmethod initialize-instance :after ((obj #.(swig-lispify "hash-map" 'class)) &key (arg0 #.(swig-lispify "hash-map" 'classname)))
  (setf (slot-value obj 'ff-pointer) (#.(swig-lispify "new_HashMap" 'function) (ff-pointer arg0))))

(cl:defmethod #.(swig-lispify "set-name" 'method) ((self #.(swig-lispify "hash-map" 'classname)) (arg1 cl:string))
  (#.(swig-lispify "HashMap_setName" 'function) (ff-pointer self) arg1))

(cl:defmethod #.(swig-lispify "get-name" 'method) ((self #.(swig-lispify "hash-map" 'classname)))
  (#.(swig-lispify "HashMap_getName" 'function) (ff-pointer self)))

(cl:defmethod #.(swig-lispify "set-map" 'method) ((self #.(swig-lispify "hash-map" 'classname)) values (len cl:integer))
  (#.(swig-lispify "HashMap_setMap" 'function) (ff-pointer self) values len))

(cl:defmethod #.(swig-lispify "get-map-len" 'method) ((self #.(swig-lispify "hash-map" 'classname)))
  (#.(swig-lispify "HashMap_getMapLen" 'function) (ff-pointer self)))

(cl:defmethod #.(swig-lispify "get-map-values" 'method) ((self #.(swig-lispify "hash-map" 'classname)) dst (len cl:integer))
  (#.(swig-lispify "HashMap_getMapValues" 'function) (ff-pointer self) dst len))

(cl:defmethod #.(swig-lispify "equal" 'method) ((self #.(swig-lispify "hash-map" 'classname)) (arg1 #.(swig-lispify "hash-map" 'classname)))
  (#.(swig-lispify "HashMap_equal" 'function) (ff-pointer self) (ff-pointer arg1)))

(cl:defmethod #.(swig-lispify "get-object-status" 'method) ((self #.(swig-lispify "hash-map" 'classname)))
  (#.(swig-lispify "HashMap_getObjectStatus" 'function) (ff-pointer self)))

(cl:defmethod #.(swig-lispify "get-object-version" 'method) ((self #.(swig-lispify "hash-map" 'classname)))
  (#.(swig-lispify "HashMap_getObjectVersion" 'function) (ff-pointer self)))

(cl:defmethod #.(swig-lispify "get-object-id" 'method) ((self #.(swig-lispify "hash-map" 'classname)))
  (#.(swig-lispify "HashMap_getObjectId" 'function) (ff-pointer self)))


(cl:defclass #.(swig-lispify "foreign-key" 'classname)(#.(swig-lispify "ndb-dictionary::-object" 'classname))
  ((ff-pointer :reader ff-pointer)))

(cl:defmethod initialize-instance :after ((obj #.(swig-lispify "foreign-key" 'class)) &key)
  (setf (slot-value obj 'ff-pointer) (#.(swig-lispify "new_ForeignKey" 'function))))

(cl:defmethod initialize-instance :after ((obj #.(swig-lispify "foreign-key" 'class)) &key (arg0 #.(swig-lispify "foreign-key" 'classname)))
  (setf (slot-value obj 'ff-pointer) (#.(swig-lispify "new_ForeignKey" 'function) (ff-pointer arg0))))

(cl:defmethod #.(swig-lispify "get-name" 'method) ((self #.(swig-lispify "foreign-key" 'classname)))
  (#.(swig-lispify "ForeignKey_getName" 'function) (ff-pointer self)))

(cl:defmethod #.(swig-lispify "get-parent-table" 'method) ((self #.(swig-lispify "foreign-key" 'classname)))
  (#.(swig-lispify "ForeignKey_getParentTable" 'function) (ff-pointer self)))

(cl:defmethod #.(swig-lispify "get-child-table" 'method) ((self #.(swig-lispify "foreign-key" 'classname)))
  (#.(swig-lispify "ForeignKey_getChildTable" 'function) (ff-pointer self)))

(cl:defmethod #.(swig-lispify "get-parent-column-count" 'method) ((self #.(swig-lispify "foreign-key" 'classname)))
  (#.(swig-lispify "ForeignKey_getParentColumnCount" 'function) (ff-pointer self)))

(cl:defmethod #.(swig-lispify "get-child-column-count" 'method) ((self #.(swig-lispify "foreign-key" 'classname)))
  (#.(swig-lispify "ForeignKey_getChildColumnCount" 'function) (ff-pointer self)))

(cl:defmethod #.(swig-lispify "get-parent-column-no" 'method) ((self #.(swig-lispify "foreign-key" 'classname)) (no cl:integer))
  (#.(swig-lispify "ForeignKey_getParentColumnNo" 'function) (ff-pointer self) no))

(cl:defmethod #.(swig-lispify "get-child-column-no" 'method) ((self #.(swig-lispify "foreign-key" 'classname)) (no cl:integer))
  (#.(swig-lispify "ForeignKey_getChildColumnNo" 'function) (ff-pointer self) no))

(cl:defmethod #.(swig-lispify "get-parent-index" 'method) ((self #.(swig-lispify "foreign-key" 'classname)))
  (#.(swig-lispify "ForeignKey_getParentIndex" 'function) (ff-pointer self)))

(cl:defmethod #.(swig-lispify "get-child-index" 'method) ((self #.(swig-lispify "foreign-key" 'classname)))
  (#.(swig-lispify "ForeignKey_getChildIndex" 'function) (ff-pointer self)))

(cl:defmethod #.(swig-lispify "get-on-update-action" 'method) ((self #.(swig-lispify "foreign-key" 'classname)))
  (#.(swig-lispify "ForeignKey_getOnUpdateAction" 'function) (ff-pointer self)))

(cl:defmethod #.(swig-lispify "get-on-delete-action" 'method) ((self #.(swig-lispify "foreign-key" 'classname)))
  (#.(swig-lispify "ForeignKey_getOnDeleteAction" 'function) (ff-pointer self)))

(cl:defmethod #.(swig-lispify "set-name" 'method) ((self #.(swig-lispify "foreign-key" 'classname)) (arg1 cl:string))
  (#.(swig-lispify "ForeignKey_setName" 'function) (ff-pointer self) arg1))

(cl:defmethod #.(swig-lispify "set-parent" 'method) ((self #.(swig-lispify "foreign-key" 'classname)) (arg1 #.(swig-lispify "table" 'classname)) (index #.(swig-lispify "index" 'classname)) cols)
  (#.(swig-lispify "ForeignKey_setParent" 'function) (ff-pointer self) arg1 index cols))

(cl:defmethod #.(swig-lispify "set-parent" 'method) ((self #.(swig-lispify "foreign-key" 'classname)) (arg1 #.(swig-lispify "table" 'classname)) (index #.(swig-lispify "index" 'classname)))
  (#.(swig-lispify "ForeignKey_setParent" 'function) (ff-pointer self) arg1 index))

(cl:defmethod #.(swig-lispify "set-parent" 'method) ((self #.(swig-lispify "foreign-key" 'classname)) (arg1 #.(swig-lispify "table" 'classname)))
  (#.(swig-lispify "ForeignKey_setParent" 'function) (ff-pointer self) arg1))

(cl:defmethod #.(swig-lispify "set-child" 'method) ((self #.(swig-lispify "foreign-key" 'classname)) (arg1 #.(swig-lispify "table" 'classname)) (index #.(swig-lispify "index" 'classname)) cols)
  (#.(swig-lispify "ForeignKey_setChild" 'function) (ff-pointer self) arg1 index cols))

(cl:defmethod #.(swig-lispify "set-child" 'method) ((self #.(swig-lispify "foreign-key" 'classname)) (arg1 #.(swig-lispify "table" 'classname)) (index #.(swig-lispify "index" 'classname)))
  (#.(swig-lispify "ForeignKey_setChild" 'function) (ff-pointer self) arg1 index))

(cl:defmethod #.(swig-lispify "set-child" 'method) ((self #.(swig-lispify "foreign-key" 'classname)) (arg1 #.(swig-lispify "table" 'classname)))
  (#.(swig-lispify "ForeignKey_setChild" 'function) (ff-pointer self) arg1))

(cl:defmethod #.(swig-lispify "set-on-update-action" 'method) ((self #.(swig-lispify "foreign-key" 'classname)) (arg1 cl:integer))
  (#.(swig-lispify "ForeignKey_setOnUpdateAction" 'function) (ff-pointer self) arg1))

(cl:defmethod #.(swig-lispify "set-on-delete-action" 'method) ((self #.(swig-lispify "foreign-key" 'classname)) (arg1 cl:integer))
  (#.(swig-lispify "ForeignKey_setOnDeleteAction" 'function) (ff-pointer self) arg1))

(cl:defmethod #.(swig-lispify "get-object-status" 'method) ((self #.(swig-lispify "foreign-key" 'classname)))
  (#.(swig-lispify "ForeignKey_getObjectStatus" 'function) (ff-pointer self)))

(cl:defmethod #.(swig-lispify "get-object-id" 'method) ((self #.(swig-lispify "foreign-key" 'classname)))
  (#.(swig-lispify "ForeignKey_getObjectId" 'function) (ff-pointer self)))

(cl:defmethod #.(swig-lispify "get-object-version" 'method) ((self #.(swig-lispify "foreign-key" 'classname)))
  (#.(swig-lispify "ForeignKey_getObjectVersion" 'function) (ff-pointer self)))


(cl:defclass #.(swig-lispify "dictionary" 'classname)()
  ((ff-pointer :reader ff-pointer)))

(cl:defmethod #.(swig-lispify "list-objects" 'method) ((self #.(swig-lispify "dictionary" 'classname)) list (type cl:integer))
  (#.(swig-lispify "Dictionary_listObjects" 'function) (ff-pointer self) list type))

(cl:defmethod #.(swig-lispify "list-objects" 'method) ((self #.(swig-lispify "dictionary" 'classname)) list)
  (#.(swig-lispify "Dictionary_listObjects" 'function) (ff-pointer self) list))

(cl:defmethod #.(swig-lispify "list-objects" 'method) ((self #.(swig-lispify "dictionary" 'classname)) list (type cl:integer))
  (#.(swig-lispify "Dictionary_listObjects" 'function) (ff-pointer self) list type))

(cl:defmethod #.(swig-lispify "list-objects" 'method) ((self #.(swig-lispify "dictionary" 'classname)) list)
  (#.(swig-lispify "Dictionary_listObjects" 'function) (ff-pointer self) list))

(cl:defmethod #.(swig-lispify "list-objects" 'method) ((self #.(swig-lispify "dictionary" 'classname)) list (type cl:integer) (fullyQualified cl:t))
  (#.(swig-lispify "Dictionary_listObjects" 'function) (ff-pointer self) list type fullyQualified))

(cl:defmethod #.(swig-lispify "get-ndb-error" 'method) ((self #.(swig-lispify "dictionary" 'classname)))
  (#.(swig-lispify "Dictionary_getNdbError" 'function) (ff-pointer self)))

(cl:defmethod #.(swig-lispify "get-warning-flags" 'method) ((self #.(swig-lispify "dictionary" 'classname)))
  (#.(swig-lispify "Dictionary_getWarningFlags" 'function) (ff-pointer self)))

(cl:defmethod #.(swig-lispify "get-table" 'method) ((self #.(swig-lispify "dictionary" 'classname)) (name cl:string))
  (#.(swig-lispify "Dictionary_getTable" 'function) (ff-pointer self) name))

(cl:defmethod #.(swig-lispify "get-blob-table" 'method) ((self #.(swig-lispify "dictionary" 'classname)) (arg1 #.(swig-lispify "table" 'classname)) (col_name cl:string))
  (#.(swig-lispify "Dictionary_getBlobTable" 'function) (ff-pointer self) arg1 col_name))

(cl:defmethod #.(swig-lispify "get-blob-table" 'method) ((self #.(swig-lispify "dictionary" 'classname)) (arg1 #.(swig-lispify "table" 'classname)) (col_no cl:integer))
  (#.(swig-lispify "Dictionary_getBlobTable" 'function) (ff-pointer self) arg1 col_no))

(cl:defmethod #.(swig-lispify "put-table" 'method) ((self #.(swig-lispify "dictionary" 'classname)) (table #.(swig-lispify "table" 'classname)))
  (#.(swig-lispify "Dictionary_putTable" 'function) (ff-pointer self) table))

(cl:defmethod #.(swig-lispify "get-index" 'method) ((self #.(swig-lispify "dictionary" 'classname)) (indexName cl:string) (tableName cl:string))
  (#.(swig-lispify "Dictionary_getIndex" 'function) (ff-pointer self) indexName tableName))

(cl:defmethod #.(swig-lispify "get-index" 'method) ((self #.(swig-lispify "dictionary" 'classname)) (indexName cl:string) (base #.(swig-lispify "table" 'classname)))
  (#.(swig-lispify "Dictionary_getIndex" 'function) (ff-pointer self) indexName base))

(cl:defmethod #.(swig-lispify "list-indexes" 'method) ((self #.(swig-lispify "dictionary" 'classname)) list (tableName cl:string))
  (#.(swig-lispify "Dictionary_listIndexes" 'function) (ff-pointer self) list tableName))

(cl:defmethod #.(swig-lispify "list-indexes" 'method) ((self #.(swig-lispify "dictionary" 'classname)) list (tableName cl:string))
  (#.(swig-lispify "Dictionary_listIndexes" 'function) (ff-pointer self) list tableName))

(cl:defmethod #.(swig-lispify "list-indexes" 'method) ((self #.(swig-lispify "dictionary" 'classname)) list (tableName cl:string) (fullyQualified cl:t))
  (#.(swig-lispify "Dictionary_listIndexes" 'function) (ff-pointer self) list tableName fullyQualified))

(cl:defmethod #.(swig-lispify "list-indexes" 'method) ((self #.(swig-lispify "dictionary" 'classname)) list (table #.(swig-lispify "table" 'classname)))
  (#.(swig-lispify "Dictionary_listIndexes" 'function) (ff-pointer self) list table))

(cl:defmethod #.(swig-lispify "list-dependent-objects" 'method) ((self #.(swig-lispify "dictionary" 'classname)) list (table #.(swig-lispify "table" 'classname)))
  (#.(swig-lispify "Dictionary_listDependentObjects" 'function) (ff-pointer self) list table))

(cl:defmethod #.(swig-lispify "create-event" 'method) ((self #.(swig-lispify "dictionary" 'classname)) (event #.(swig-lispify "event" 'classname)))
  (#.(swig-lispify "Dictionary_createEvent" 'function) (ff-pointer self) event))

(cl:defmethod #.(swig-lispify "drop-event" 'method) ((self #.(swig-lispify "dictionary" 'classname)) (eventName cl:string) (force cl:integer))
  (#.(swig-lispify "Dictionary_dropEvent" 'function) (ff-pointer self) eventName force))

(cl:defmethod #.(swig-lispify "drop-event" 'method) ((self #.(swig-lispify "dictionary" 'classname)) (eventName cl:string))
  (#.(swig-lispify "Dictionary_dropEvent" 'function) (ff-pointer self) eventName))

(cl:defmethod #.(swig-lispify "get-event" 'method) ((self #.(swig-lispify "dictionary" 'classname)) (eventName cl:string))
  (#.(swig-lispify "Dictionary_getEvent" 'function) (ff-pointer self) eventName))

(cl:defmethod #.(swig-lispify "list-events" 'method) ((self #.(swig-lispify "dictionary" 'classname)) list)
  (#.(swig-lispify "Dictionary_listEvents" 'function) (ff-pointer self) list))

(cl:defmethod #.(swig-lispify "list-events" 'method) ((self #.(swig-lispify "dictionary" 'classname)) list)
  (#.(swig-lispify "Dictionary_listEvents" 'function) (ff-pointer self) list))

(cl:defmethod #.(swig-lispify "create-table" 'method) ((self #.(swig-lispify "dictionary" 'classname)) (table #.(swig-lispify "table" 'classname)))
  (#.(swig-lispify "Dictionary_createTable" 'function) (ff-pointer self) table))

(cl:defmethod #.(swig-lispify "create-table" 'method) ((self #.(swig-lispify "dictionary" 'classname)) (table #.(swig-lispify "table" 'classname)) (objid #.(swig-lispify "object-id" 'classname)))
  (#.(swig-lispify "Dictionary_createTable" 'function) (ff-pointer self) table objid))

(cl:defmethod #.(swig-lispify "optimize-table" 'method) ((self #.(swig-lispify "dictionary" 'classname)) (t-arg1 #.(swig-lispify "table" 'classname)) (h #.(swig-lispify "optimize-table-handle" 'classname)))
  (#.(swig-lispify "Dictionary_optimizeTable" 'function) (ff-pointer self) t-arg1 h))

(cl:defmethod #.(swig-lispify "optimize-index" 'method) ((self #.(swig-lispify "dictionary" 'classname)) (ind #.(swig-lispify "index" 'classname)) (h #.(swig-lispify "optimize-index-handle" 'classname)))
  (#.(swig-lispify "Dictionary_optimizeIndex" 'function) (ff-pointer self) ind h))

(cl:defmethod #.(swig-lispify "drop-table" 'method) ((self #.(swig-lispify "dictionary" 'classname)) (table #.(swig-lispify "table" 'classname)))
  (#.(swig-lispify "Dictionary_dropTable" 'function) (ff-pointer self) table))

(cl:defmethod #.(swig-lispify "drop-table" 'method) ((self #.(swig-lispify "dictionary" 'classname)) (name cl:string))
  (#.(swig-lispify "Dictionary_dropTable" 'function) (ff-pointer self) name))

(cl:defmethod #.(swig-lispify "supported-alter-table" 'method) ((self #.(swig-lispify "dictionary" 'classname)) (f #.(swig-lispify "table" 'classname)) (t-arg2 #.(swig-lispify "table" 'classname)))
  (#.(swig-lispify "Dictionary_supportedAlterTable" 'function) (ff-pointer self) f t-arg2))

(cl:defmethod #.(swig-lispify "alter-table" 'method) ((self #.(swig-lispify "dictionary" 'classname)) (f #.(swig-lispify "table" 'classname)) (t-arg2 #.(swig-lispify "table" 'classname)))
  (#.(swig-lispify "Dictionary_alterTable" 'function) (ff-pointer self) f t-arg2))

(cl:defmethod #.(swig-lispify "invalidate-table" 'method) ((self #.(swig-lispify "dictionary" 'classname)) (name cl:string))
  (#.(swig-lispify "Dictionary_invalidateTable" 'function) (ff-pointer self) name))

(cl:defmethod #.(swig-lispify "remove-cached-table" 'method) ((self #.(swig-lispify "dictionary" 'classname)) (table cl:string))
  (#.(swig-lispify "Dictionary_removeCachedTable" 'function) (ff-pointer self) table))

(cl:defmethod #.(swig-lispify "remove-cached-index" 'method) ((self #.(swig-lispify "dictionary" 'classname)) (index cl:string) (table cl:string))
  (#.(swig-lispify "Dictionary_removeCachedIndex" 'function) (ff-pointer self) index table))

(cl:defmethod #.(swig-lispify "invalidate-index" 'method) ((self #.(swig-lispify "dictionary" 'classname)) (indexName cl:string) (tableName cl:string))
  (#.(swig-lispify "Dictionary_invalidateIndex" 'function) (ff-pointer self) indexName tableName))

(cl:defmethod #.(swig-lispify "create-index" 'method) ((self #.(swig-lispify "dictionary" 'classname)) (index #.(swig-lispify "index" 'classname)) (offline cl:t))
  (#.(swig-lispify "Dictionary_createIndex" 'function) (ff-pointer self) index offline))

(cl:defmethod #.(swig-lispify "create-index" 'method) ((self #.(swig-lispify "dictionary" 'classname)) (index #.(swig-lispify "index" 'classname)))
  (#.(swig-lispify "Dictionary_createIndex" 'function) (ff-pointer self) index))

(cl:defmethod #.(swig-lispify "create-index" 'method) ((self #.(swig-lispify "dictionary" 'classname)) (index #.(swig-lispify "index" 'classname)) (table #.(swig-lispify "table" 'classname)) (offline cl:t))
  (#.(swig-lispify "Dictionary_createIndex" 'function) (ff-pointer self) index table offline))

(cl:defmethod #.(swig-lispify "create-index" 'method) ((self #.(swig-lispify "dictionary" 'classname)) (index #.(swig-lispify "index" 'classname)) (table #.(swig-lispify "table" 'classname)))
  (#.(swig-lispify "Dictionary_createIndex" 'function) (ff-pointer self) index table))

(cl:defmethod #.(swig-lispify "drop-index" 'method) ((self #.(swig-lispify "dictionary" 'classname)) (indexName cl:string) (tableName cl:string))
  (#.(swig-lispify "Dictionary_dropIndex" 'function) (ff-pointer self) indexName tableName))

(cl:defmethod #.(swig-lispify "update-index-stat" 'method) ((self #.(swig-lispify "dictionary" 'classname)) (arg1 #.(swig-lispify "index" 'classname)) (arg2 #.(swig-lispify "table" 'classname)))
  (#.(swig-lispify "Dictionary_updateIndexStat" 'function) (ff-pointer self) arg1 arg2))

(cl:defmethod #.(swig-lispify "update-index-stat" 'method) ((self #.(swig-lispify "dictionary" 'classname)) (indexId cl:integer) (indexVersion cl:integer) (tableId cl:integer))
  (#.(swig-lispify "Dictionary_updateIndexStat" 'function) (ff-pointer self) indexId indexVersion tableId))

(cl:defmethod #.(swig-lispify "delete-index-stat" 'method) ((self #.(swig-lispify "dictionary" 'classname)) (arg1 #.(swig-lispify "index" 'classname)) (arg2 #.(swig-lispify "table" 'classname)))
  (#.(swig-lispify "Dictionary_deleteIndexStat" 'function) (ff-pointer self) arg1 arg2))

(cl:defmethod #.(swig-lispify "delete-index-stat" 'method) ((self #.(swig-lispify "dictionary" 'classname)) (indexId cl:integer) (indexVersion cl:integer) (tableId cl:integer))
  (#.(swig-lispify "Dictionary_deleteIndexStat" 'function) (ff-pointer self) indexId indexVersion tableId))

(cl:defmethod #.(swig-lispify "remove-cached-table" 'method) ((self #.(swig-lispify "dictionary" 'classname)) (table #.(swig-lispify "table" 'classname)))
  (#.(swig-lispify "Dictionary_removeCachedTable" 'function) (ff-pointer self) table))

(cl:defmethod #.(swig-lispify "remove-cached-index" 'method) ((self #.(swig-lispify "dictionary" 'classname)) (index #.(swig-lispify "index" 'classname)))
  (#.(swig-lispify "Dictionary_removeCachedIndex" 'function) (ff-pointer self) index))

(cl:defmethod #.(swig-lispify "invalidate-table" 'method) ((self #.(swig-lispify "dictionary" 'classname)) (table #.(swig-lispify "table" 'classname)))
  (#.(swig-lispify "Dictionary_invalidateTable" 'function) (ff-pointer self) table))

(cl:defmethod #.(swig-lispify "invalidate-index" 'method) ((self #.(swig-lispify "dictionary" 'classname)) (index #.(swig-lispify "index" 'classname)))
  (#.(swig-lispify "Dictionary_invalidateIndex" 'function) (ff-pointer self) index))

(cl:defmethod #.(swig-lispify "force-gcpwait" 'method) ((self #.(swig-lispify "dictionary" 'classname)))
  (#.(swig-lispify "Dictionary_forceGCPWait" 'function) (ff-pointer self)))

(cl:defmethod #.(swig-lispify "force-gcpwait" 'method) ((self #.(swig-lispify "dictionary" 'classname)) (type cl:integer))
  (#.(swig-lispify "Dictionary_forceGCPWait" 'function) (ff-pointer self) type))

(cl:defmethod #.(swig-lispify "get-restart-gci" 'method) ((self #.(swig-lispify "dictionary" 'classname)) gci)
  (#.(swig-lispify "Dictionary_getRestartGCI" 'function) (ff-pointer self) gci))

(cl:defmethod #.(swig-lispify "create-logfile-group" 'method) ((self #.(swig-lispify "dictionary" 'classname)) (arg1 #.(swig-lispify "logfile-group" 'classname)) (arg2 #.(swig-lispify "object-id" 'classname)))
  (#.(swig-lispify "Dictionary_createLogfileGroup" 'function) (ff-pointer self) arg1 arg2))

(cl:defmethod #.(swig-lispify "create-logfile-group" 'method) ((self #.(swig-lispify "dictionary" 'classname)) (arg1 #.(swig-lispify "logfile-group" 'classname)))
  (#.(swig-lispify "Dictionary_createLogfileGroup" 'function) (ff-pointer self) arg1))

(cl:defmethod #.(swig-lispify "drop-logfile-group" 'method) ((self #.(swig-lispify "dictionary" 'classname)) (arg1 #.(swig-lispify "logfile-group" 'classname)))
  (#.(swig-lispify "Dictionary_dropLogfileGroup" 'function) (ff-pointer self) arg1))

(cl:defmethod #.(swig-lispify "get-logfile-group" 'method) ((self #.(swig-lispify "dictionary" 'classname)) (name cl:string))
  (#.(swig-lispify "Dictionary_getLogfileGroup" 'function) (ff-pointer self) name))

(cl:defmethod #.(swig-lispify "create-tablespace" 'method) ((self #.(swig-lispify "dictionary" 'classname)) (arg1 #.(swig-lispify "tablespace" 'classname)) (arg2 #.(swig-lispify "object-id" 'classname)))
  (#.(swig-lispify "Dictionary_createTablespace" 'function) (ff-pointer self) arg1 arg2))

(cl:defmethod #.(swig-lispify "create-tablespace" 'method) ((self #.(swig-lispify "dictionary" 'classname)) (arg1 #.(swig-lispify "tablespace" 'classname)))
  (#.(swig-lispify "Dictionary_createTablespace" 'function) (ff-pointer self) arg1))

(cl:defmethod #.(swig-lispify "drop-tablespace" 'method) ((self #.(swig-lispify "dictionary" 'classname)) (arg1 #.(swig-lispify "tablespace" 'classname)))
  (#.(swig-lispify "Dictionary_dropTablespace" 'function) (ff-pointer self) arg1))

(cl:defmethod #.(swig-lispify "get-tablespace" 'method) ((self #.(swig-lispify "dictionary" 'classname)) (name cl:string))
  (#.(swig-lispify "Dictionary_getTablespace" 'function) (ff-pointer self) name))

(cl:defmethod #.(swig-lispify "get-tablespace" 'method) ((self #.(swig-lispify "dictionary" 'classname)) (tablespaceId cl:integer))
  (#.(swig-lispify "Dictionary_getTablespace" 'function) (ff-pointer self) tablespaceId))

(cl:defmethod #.(swig-lispify "create-datafile" 'method) ((self #.(swig-lispify "dictionary" 'classname)) (arg1 #.(swig-lispify "datafile" 'classname)) (overwrite_existing cl:t) (arg3 #.(swig-lispify "object-id" 'classname)))
  (#.(swig-lispify "Dictionary_createDatafile" 'function) (ff-pointer self) arg1 overwrite_existing arg3))

(cl:defmethod #.(swig-lispify "create-datafile" 'method) ((self #.(swig-lispify "dictionary" 'classname)) (arg1 #.(swig-lispify "datafile" 'classname)) (overwrite_existing cl:t))
  (#.(swig-lispify "Dictionary_createDatafile" 'function) (ff-pointer self) arg1 overwrite_existing))

(cl:defmethod #.(swig-lispify "create-datafile" 'method) ((self #.(swig-lispify "dictionary" 'classname)) (arg1 #.(swig-lispify "datafile" 'classname)))
  (#.(swig-lispify "Dictionary_createDatafile" 'function) (ff-pointer self) arg1))

(cl:defmethod #.(swig-lispify "drop-datafile" 'method) ((self #.(swig-lispify "dictionary" 'classname)) (arg1 #.(swig-lispify "datafile" 'classname)))
  (#.(swig-lispify "Dictionary_dropDatafile" 'function) (ff-pointer self) arg1))

(cl:defmethod #.(swig-lispify "get-datafile" 'method) ((self #.(swig-lispify "dictionary" 'classname)) (node cl:integer) (path cl:string))
  (#.(swig-lispify "Dictionary_getDatafile" 'function) (ff-pointer self) node path))

(cl:defmethod #.(swig-lispify "create-undofile" 'method) ((self #.(swig-lispify "dictionary" 'classname)) (arg1 #.(swig-lispify "undofile" 'classname)) (overwrite_existing cl:t) (arg3 #.(swig-lispify "object-id" 'classname)))
  (#.(swig-lispify "Dictionary_createUndofile" 'function) (ff-pointer self) arg1 overwrite_existing arg3))

(cl:defmethod #.(swig-lispify "create-undofile" 'method) ((self #.(swig-lispify "dictionary" 'classname)) (arg1 #.(swig-lispify "undofile" 'classname)) (overwrite_existing cl:t))
  (#.(swig-lispify "Dictionary_createUndofile" 'function) (ff-pointer self) arg1 overwrite_existing))

(cl:defmethod #.(swig-lispify "create-undofile" 'method) ((self #.(swig-lispify "dictionary" 'classname)) (arg1 #.(swig-lispify "undofile" 'classname)))
  (#.(swig-lispify "Dictionary_createUndofile" 'function) (ff-pointer self) arg1))

(cl:defmethod #.(swig-lispify "drop-undofile" 'method) ((self #.(swig-lispify "dictionary" 'classname)) (arg1 #.(swig-lispify "undofile" 'classname)))
  (#.(swig-lispify "Dictionary_dropUndofile" 'function) (ff-pointer self) arg1))

(cl:defmethod #.(swig-lispify "get-undofile" 'method) ((self #.(swig-lispify "dictionary" 'classname)) (node cl:integer) (path cl:string))
  (#.(swig-lispify "Dictionary_getUndofile" 'function) (ff-pointer self) node path))

(cl:defmethod #.(swig-lispify "create-hash-map" 'method) ((self #.(swig-lispify "dictionary" 'classname)) (arg1 #.(swig-lispify "hash-map" 'classname)) (arg2 #.(swig-lispify "object-id" 'classname)))
  (#.(swig-lispify "Dictionary_createHashMap" 'function) (ff-pointer self) arg1 arg2))

(cl:defmethod #.(swig-lispify "create-hash-map" 'method) ((self #.(swig-lispify "dictionary" 'classname)) (arg1 #.(swig-lispify "hash-map" 'classname)))
  (#.(swig-lispify "Dictionary_createHashMap" 'function) (ff-pointer self) arg1))

(cl:defmethod #.(swig-lispify "get-hash-map" 'method) ((self #.(swig-lispify "dictionary" 'classname)) (dst #.(swig-lispify "hash-map" 'classname)) (name cl:string))
  (#.(swig-lispify "Dictionary_getHashMap" 'function) (ff-pointer self) dst name))

(cl:defmethod #.(swig-lispify "get-hash-map" 'method) ((self #.(swig-lispify "dictionary" 'classname)) (dst #.(swig-lispify "hash-map" 'classname)) (table #.(swig-lispify "table" 'classname)))
  (#.(swig-lispify "Dictionary_getHashMap" 'function) (ff-pointer self) dst table))

(cl:defmethod #.(swig-lispify "get-default-hash-map" 'method) ((self #.(swig-lispify "dictionary" 'classname)) (dst #.(swig-lispify "hash-map" 'classname)) (partitionCount cl:integer))
  (#.(swig-lispify "Dictionary_getDefaultHashMap" 'function) (ff-pointer self) dst partitionCount))

(cl:defmethod #.(swig-lispify "get-default-hash-map" 'method) ((self #.(swig-lispify "dictionary" 'classname)) (dst #.(swig-lispify "hash-map" 'classname)) (buckets cl:integer) (partitionCount cl:integer))
  (#.(swig-lispify "Dictionary_getDefaultHashMap" 'function) (ff-pointer self) dst buckets partitionCount))

(cl:defmethod #.(swig-lispify "init-default-hash-map" 'method) ((self #.(swig-lispify "dictionary" 'classname)) (dst #.(swig-lispify "hash-map" 'classname)) (partitionCount cl:integer))
  (#.(swig-lispify "Dictionary_initDefaultHashMap" 'function) (ff-pointer self) dst partitionCount))

(cl:defmethod #.(swig-lispify "init-default-hash-map" 'method) ((self #.(swig-lispify "dictionary" 'classname)) (dst #.(swig-lispify "hash-map" 'classname)) (buckets cl:integer) (partitionCount cl:integer))
  (#.(swig-lispify "Dictionary_initDefaultHashMap" 'function) (ff-pointer self) dst buckets partitionCount))

(cl:defmethod #.(swig-lispify "prepare-hash-map" 'method) ((self #.(swig-lispify "dictionary" 'classname)) (oldTable #.(swig-lispify "table" 'classname)) (newTable #.(swig-lispify "table" 'classname)))
  (#.(swig-lispify "Dictionary_prepareHashMap" 'function) (ff-pointer self) oldTable newTable))

(cl:defmethod #.(swig-lispify "prepare-hash-map" 'method) ((self #.(swig-lispify "dictionary" 'classname)) (oldTable #.(swig-lispify "table" 'classname)) (newTable #.(swig-lispify "table" 'classname)) (buckets cl:integer))
  (#.(swig-lispify "Dictionary_prepareHashMap" 'function) (ff-pointer self) oldTable newTable buckets))

(cl:defmethod #.(swig-lispify "create-foreign-key" 'method) ((self #.(swig-lispify "dictionary" 'classname)) (arg1 #.(swig-lispify "foreign-key" 'classname)) (arg2 #.(swig-lispify "object-id" 'classname)) (flags cl:integer))
  (#.(swig-lispify "Dictionary_createForeignKey" 'function) (ff-pointer self) arg1 arg2 flags))

(cl:defmethod #.(swig-lispify "create-foreign-key" 'method) ((self #.(swig-lispify "dictionary" 'classname)) (arg1 #.(swig-lispify "foreign-key" 'classname)) (arg2 #.(swig-lispify "object-id" 'classname)))
  (#.(swig-lispify "Dictionary_createForeignKey" 'function) (ff-pointer self) arg1 arg2))

(cl:defmethod #.(swig-lispify "create-foreign-key" 'method) ((self #.(swig-lispify "dictionary" 'classname)) (arg1 #.(swig-lispify "foreign-key" 'classname)))
  (#.(swig-lispify "Dictionary_createForeignKey" 'function) (ff-pointer self) arg1))

(cl:defmethod #.(swig-lispify "get-foreign-key" 'method) ((self #.(swig-lispify "dictionary" 'classname)) (dst #.(swig-lispify "foreign-key" 'classname)) (name cl:string))
  (#.(swig-lispify "Dictionary_getForeignKey" 'function) (ff-pointer self) dst name))

(cl:defmethod #.(swig-lispify "drop-foreign-key" 'method) ((self #.(swig-lispify "dictionary" 'classname)) (arg1 #.(swig-lispify "foreign-key" 'classname)))
  (#.(swig-lispify "Dictionary_dropForeignKey" 'function) (ff-pointer self) arg1))

(cl:defmethod #.(swig-lispify "begin-schema-trans" 'method) ((self #.(swig-lispify "dictionary" 'classname)))
  (#.(swig-lispify "Dictionary_beginSchemaTrans" 'function) (ff-pointer self)))

(cl:defmethod #.(swig-lispify "end-schema-trans" 'method) ((self #.(swig-lispify "dictionary" 'classname)) (flags cl:integer))
  (#.(swig-lispify "Dictionary_endSchemaTrans" 'function) (ff-pointer self) flags))

(cl:defmethod #.(swig-lispify "end-schema-trans" 'method) ((self #.(swig-lispify "dictionary" 'classname)))
  (#.(swig-lispify "Dictionary_endSchemaTrans" 'function) (ff-pointer self)))

(cl:defmethod #.(swig-lispify "has-schema-trans" 'method) ((self #.(swig-lispify "dictionary" 'classname)))
  (#.(swig-lispify "Dictionary_hasSchemaTrans" 'function) (ff-pointer self)))

(cl:defmethod #.(swig-lispify "get-table" 'method) ((self #.(swig-lispify "dictionary" 'classname)) (name cl:string) data)
  (#.(swig-lispify "Dictionary_getTable" 'function) (ff-pointer self) name data))

(cl:defmethod #.(swig-lispify "set-local-table-data-size" 'method) ((self #.(swig-lispify "dictionary" 'classname)) (sz cl:integer))
  (#.(swig-lispify "Dictionary_set_local_table_data_size" 'function) (ff-pointer self) sz))

(cl:defmethod #.(swig-lispify "get-index-global" 'method) ((self #.(swig-lispify "dictionary" 'classname)) (indexName cl:string) (ndbtab #.(swig-lispify "table" 'classname)))
  (#.(swig-lispify "Dictionary_getIndexGlobal" 'function) (ff-pointer self) indexName ndbtab))

(cl:defmethod #.(swig-lispify "get-index-global" 'method) ((self #.(swig-lispify "dictionary" 'classname)) (indexName cl:string) (tableName cl:string))
  (#.(swig-lispify "Dictionary_getIndexGlobal" 'function) (ff-pointer self) indexName tableName))

(cl:defmethod #.(swig-lispify "get-table-global" 'method) ((self #.(swig-lispify "dictionary" 'classname)) (tableName cl:string))
  (#.(swig-lispify "Dictionary_getTableGlobal" 'function) (ff-pointer self) tableName))

(cl:defmethod #.(swig-lispify "alter-table-global" 'method) ((self #.(swig-lispify "dictionary" 'classname)) (f #.(swig-lispify "table" 'classname)) (t-arg2 #.(swig-lispify "table" 'classname)))
  (#.(swig-lispify "Dictionary_alterTableGlobal" 'function) (ff-pointer self) f t-arg2))

(cl:defmethod #.(swig-lispify "drop-table-global" 'method) ((self #.(swig-lispify "dictionary" 'classname)) (ndbtab #.(swig-lispify "table" 'classname)))
  (#.(swig-lispify "Dictionary_dropTableGlobal" 'function) (ff-pointer self) ndbtab))

(cl:defmethod #.(swig-lispify "drop-table-global" 'method) ((self #.(swig-lispify "dictionary" 'classname)) (ndbtab #.(swig-lispify "table" 'classname)) (flags cl:integer))
  (#.(swig-lispify "Dictionary_dropTableGlobal" 'function) (ff-pointer self) ndbtab flags))

(cl:defmethod #.(swig-lispify "drop-index-global" 'method) ((self #.(swig-lispify "dictionary" 'classname)) (index #.(swig-lispify "index" 'classname)))
  (#.(swig-lispify "Dictionary_dropIndexGlobal" 'function) (ff-pointer self) index))

(cl:defmethod #.(swig-lispify "remove-index-global" 'method) ((self #.(swig-lispify "dictionary" 'classname)) (ndbidx #.(swig-lispify "index" 'classname)) (invalidate cl:integer))
  (#.(swig-lispify "Dictionary_removeIndexGlobal" 'function) (ff-pointer self) ndbidx invalidate))

(cl:defmethod #.(swig-lispify "remove-table-global" 'method) ((self #.(swig-lispify "dictionary" 'classname)) (ndbtab #.(swig-lispify "table" 'classname)) (invalidate cl:integer))
  (#.(swig-lispify "Dictionary_removeTableGlobal" 'function) (ff-pointer self) ndbtab invalidate))

(cl:defmethod #.(swig-lispify "invalidate-db-global" 'method) ((self #.(swig-lispify "dictionary" 'classname)) (dbname cl:string))
  (#.(swig-lispify "Dictionary_invalidateDbGlobal" 'function) (ff-pointer self) dbname))

(cl:defmethod #.(swig-lispify "create-record" 'method) ((self #.(swig-lispify "dictionary" 'classname)) (table #.(swig-lispify "table" 'classname)) recSpec (length cl:integer) (elemSize cl:integer) (flags cl:integer))
  (#.(swig-lispify "Dictionary_createRecord" 'function) (ff-pointer self) table recSpec length elemSize flags))

(cl:defmethod #.(swig-lispify "create-record" 'method) ((self #.(swig-lispify "dictionary" 'classname)) (table #.(swig-lispify "table" 'classname)) recSpec (length cl:integer) (elemSize cl:integer))
  (#.(swig-lispify "Dictionary_createRecord" 'function) (ff-pointer self) table recSpec length elemSize))

(cl:defmethod #.(swig-lispify "create-record" 'method) ((self #.(swig-lispify "dictionary" 'classname)) (index #.(swig-lispify "index" 'classname)) (table #.(swig-lispify "table" 'classname)) recSpec (length cl:integer) (elemSize cl:integer) (flags cl:integer))
  (#.(swig-lispify "Dictionary_createRecord" 'function) (ff-pointer self) index table recSpec length elemSize flags))

(cl:defmethod #.(swig-lispify "create-record" 'method) ((self #.(swig-lispify "dictionary" 'classname)) (index #.(swig-lispify "index" 'classname)) (table #.(swig-lispify "table" 'classname)) recSpec (length cl:integer) (elemSize cl:integer))
  (#.(swig-lispify "Dictionary_createRecord" 'function) (ff-pointer self) index table recSpec length elemSize))

(cl:defmethod #.(swig-lispify "create-record" 'method) ((self #.(swig-lispify "dictionary" 'classname)) (index #.(swig-lispify "index" 'classname)) recSpec (length cl:integer) (elemSize cl:integer) (flags cl:integer))
  (#.(swig-lispify "Dictionary_createRecord" 'function) (ff-pointer self) index recSpec length elemSize flags))

(cl:defmethod #.(swig-lispify "create-record" 'method) ((self #.(swig-lispify "dictionary" 'classname)) (index #.(swig-lispify "index" 'classname)) recSpec (length cl:integer) (elemSize cl:integer))
  (#.(swig-lispify "Dictionary_createRecord" 'function) (ff-pointer self) index recSpec length elemSize))

(cl:defmethod #.(swig-lispify "release-record" 'method) ((self #.(swig-lispify "dictionary" 'classname)) rec)
  (#.(swig-lispify "Dictionary_releaseRecord" 'function) (ff-pointer self) rec))

(cl:defmethod #.(swig-lispify "print" 'method) ((self #.(swig-lispify "dictionary" 'classname)) out (idx #.(swig-lispify "index" 'classname)))
  (#.(swig-lispify "Dictionary_print" 'function) (ff-pointer self) out idx))

(cl:defmethod #.(swig-lispify "print" 'method) ((self #.(swig-lispify "dictionary" 'classname)) out (tab #.(swig-lispify "table" 'classname)))
  (#.(swig-lispify "Dictionary_print" 'function) (ff-pointer self) out tab))


(cl:defclass #.(swig-lispify "ndb-data-print-format" 'classname)()
  ((ff-pointer :reader ff-pointer)))

(cl:defmethod initialize-instance :after ((obj #.(swig-lispify "ndb-data-print-format" 'class)) &key)
  (setf (slot-value obj 'ff-pointer) (#.(swig-lispify "new_NdbDataPrintFormat" 'function))))

(cl:defmethod (cl:setf #.(swig-lispify "lines_terminated_by" 'method)) (arg0 (obj #.(swig-lispify "ndb-data-print-format" 'class)))
  (#.(swig-lispify "NdbDataPrintFormat_lines_terminated_by_set" 'function) (ff-pointer obj) arg0))

(cl:defmethod #.(swig-lispify "lines_terminated_by" 'method) ((obj #.(swig-lispify "ndb-data-print-format" 'class)))
  (#.(swig-lispify "NdbDataPrintFormat_lines_terminated_by_get" 'function) (ff-pointer obj)))

(cl:defmethod (cl:setf #.(swig-lispify "fields_terminated_by" 'method)) (arg0 (obj #.(swig-lispify "ndb-data-print-format" 'class)))
  (#.(swig-lispify "NdbDataPrintFormat_fields_terminated_by_set" 'function) (ff-pointer obj) arg0))

(cl:defmethod #.(swig-lispify "fields_terminated_by" 'method) ((obj #.(swig-lispify "ndb-data-print-format" 'class)))
  (#.(swig-lispify "NdbDataPrintFormat_fields_terminated_by_get" 'function) (ff-pointer obj)))

(cl:defmethod (cl:setf #.(swig-lispify "start_array_enclosure" 'method)) (arg0 (obj #.(swig-lispify "ndb-data-print-format" 'class)))
  (#.(swig-lispify "NdbDataPrintFormat_start_array_enclosure_set" 'function) (ff-pointer obj) arg0))

(cl:defmethod #.(swig-lispify "start_array_enclosure" 'method) ((obj #.(swig-lispify "ndb-data-print-format" 'class)))
  (#.(swig-lispify "NdbDataPrintFormat_start_array_enclosure_get" 'function) (ff-pointer obj)))

(cl:defmethod (cl:setf #.(swig-lispify "end_array_enclosure" 'method)) (arg0 (obj #.(swig-lispify "ndb-data-print-format" 'class)))
  (#.(swig-lispify "NdbDataPrintFormat_end_array_enclosure_set" 'function) (ff-pointer obj) arg0))

(cl:defmethod #.(swig-lispify "end_array_enclosure" 'method) ((obj #.(swig-lispify "ndb-data-print-format" 'class)))
  (#.(swig-lispify "NdbDataPrintFormat_end_array_enclosure_get" 'function) (ff-pointer obj)))

(cl:defmethod (cl:setf #.(swig-lispify "fields_enclosed_by" 'method)) (arg0 (obj #.(swig-lispify "ndb-data-print-format" 'class)))
  (#.(swig-lispify "NdbDataPrintFormat_fields_enclosed_by_set" 'function) (ff-pointer obj) arg0))

(cl:defmethod #.(swig-lispify "fields_enclosed_by" 'method) ((obj #.(swig-lispify "ndb-data-print-format" 'class)))
  (#.(swig-lispify "NdbDataPrintFormat_fields_enclosed_by_get" 'function) (ff-pointer obj)))

(cl:defmethod (cl:setf #.(swig-lispify "fields_optionally_enclosed_by" 'method)) (arg0 (obj #.(swig-lispify "ndb-data-print-format" 'class)))
  (#.(swig-lispify "NdbDataPrintFormat_fields_optionally_enclosed_by_set" 'function) (ff-pointer obj) arg0))

(cl:defmethod #.(swig-lispify "fields_optionally_enclosed_by" 'method) ((obj #.(swig-lispify "ndb-data-print-format" 'class)))
  (#.(swig-lispify "NdbDataPrintFormat_fields_optionally_enclosed_by_get" 'function) (ff-pointer obj)))

(cl:defmethod (cl:setf #.(swig-lispify "hex_prefix" 'method)) (arg0 (obj #.(swig-lispify "ndb-data-print-format" 'class)))
  (#.(swig-lispify "NdbDataPrintFormat_hex_prefix_set" 'function) (ff-pointer obj) arg0))

(cl:defmethod #.(swig-lispify "hex_prefix" 'method) ((obj #.(swig-lispify "ndb-data-print-format" 'class)))
  (#.(swig-lispify "NdbDataPrintFormat_hex_prefix_get" 'function) (ff-pointer obj)))

(cl:defmethod (cl:setf #.(swig-lispify "null_string" 'method)) (arg0 (obj #.(swig-lispify "ndb-data-print-format" 'class)))
  (#.(swig-lispify "NdbDataPrintFormat_null_string_set" 'function) (ff-pointer obj) arg0))

(cl:defmethod #.(swig-lispify "null_string" 'method) ((obj #.(swig-lispify "ndb-data-print-format" 'class)))
  (#.(swig-lispify "NdbDataPrintFormat_null_string_get" 'function) (ff-pointer obj)))

(cl:defmethod (cl:setf #.(swig-lispify "hex_format" 'method)) (arg0 (obj #.(swig-lispify "ndb-data-print-format" 'class)))
  (#.(swig-lispify "NdbDataPrintFormat_hex_format_set" 'function) (ff-pointer obj) arg0))

(cl:defmethod #.(swig-lispify "hex_format" 'method) ((obj #.(swig-lispify "ndb-data-print-format" 'class)))
  (#.(swig-lispify "NdbDataPrintFormat_hex_format_get" 'function) (ff-pointer obj)))


(cl:defclass #.(swig-lispify "ndb" 'classname)()
  ((ff-pointer :reader ff-pointer)))

(cl:defmethod initialize-instance :after ((obj #.(swig-lispify "ndb" 'class)) &key (ndb_cluster_connection #.(swig-lispify "ndb-cluster-connection" 'classname)) (aCatalogName cl:string) (aSchemaName cl:string))
  (setf (slot-value obj 'ff-pointer) (#.(swig-lispify "new_Ndb" 'function) ndb_cluster_connection aCatalogName aSchemaName)))

(cl:defmethod initialize-instance :after ((obj #.(swig-lispify "ndb" 'class)) &key (ndb_cluster_connection #.(swig-lispify "ndb-cluster-connection" 'classname)) (aCatalogName cl:string))
  (setf (slot-value obj 'ff-pointer) (#.(swig-lispify "new_Ndb" 'function) ndb_cluster_connection aCatalogName)))

(cl:defmethod initialize-instance :after ((obj #.(swig-lispify "ndb" 'class)) &key (ndb_cluster_connection #.(swig-lispify "ndb-cluster-connection" 'classname)))
  (setf (slot-value obj 'ff-pointer) (#.(swig-lispify "new_Ndb" 'function) ndb_cluster_connection)))

(cl:defmethod #.(swig-lispify "get-ndb-cluster-connection" 'method) ((self #.(swig-lispify "ndb" 'classname)))
  (#.(swig-lispify "Ndb_get_ndb_cluster_connection" 'function) (ff-pointer self)))

(cl:defmethod #.(swig-lispify "get-catalog-name" 'method) ((self #.(swig-lispify "ndb" 'classname)))
  (#.(swig-lispify "Ndb_getCatalogName" 'function) (ff-pointer self)))

(cl:defmethod #.(swig-lispify "set-catalog-name" 'method) ((self #.(swig-lispify "ndb" 'classname)) (aCatalogName cl:string))
  (#.(swig-lispify "Ndb_setCatalogName" 'function) (ff-pointer self) aCatalogName))

(cl:defmethod #.(swig-lispify "get-schema-name" 'method) ((self #.(swig-lispify "ndb" 'classname)))
  (#.(swig-lispify "Ndb_getSchemaName" 'function) (ff-pointer self)))

(cl:defmethod #.(swig-lispify "set-schema-name" 'method) ((self #.(swig-lispify "ndb" 'classname)) (aSchemaName cl:string))
  (#.(swig-lispify "Ndb_setSchemaName" 'function) (ff-pointer self) aSchemaName))

(cl:defmethod #.(swig-lispify "get-ndb-object-name" 'method) ((self #.(swig-lispify "ndb" 'classname)))
  (#.(swig-lispify "Ndb_getNdbObjectName" 'function) (ff-pointer self)))

(cl:defmethod #.(swig-lispify "set-ndb-object-name" 'method) ((self #.(swig-lispify "ndb" 'classname)) (name cl:string))
  (#.(swig-lispify "Ndb_setNdbObjectName" 'function) (ff-pointer self) name))

(cl:defmethod #.(swig-lispify "get-database-name" 'method) ((self #.(swig-lispify "ndb" 'classname)))
  (#.(swig-lispify "Ndb_getDatabaseName" 'function) (ff-pointer self)))

(cl:defmethod #.(swig-lispify "set-database-name" 'method) ((self #.(swig-lispify "ndb" 'classname)) (aDatabaseName cl:string))
  (#.(swig-lispify "Ndb_setDatabaseName" 'function) (ff-pointer self) aDatabaseName))

(cl:defmethod #.(swig-lispify "get-database-schema-name" 'method) ((self #.(swig-lispify "ndb" 'classname)))
  (#.(swig-lispify "Ndb_getDatabaseSchemaName" 'function) (ff-pointer self)))

(cl:defmethod #.(swig-lispify "set-database-schema-name" 'method) ((self #.(swig-lispify "ndb" 'classname)) (aDatabaseSchemaName cl:string))
  (#.(swig-lispify "Ndb_setDatabaseSchemaName" 'function) (ff-pointer self) aDatabaseSchemaName))

(cl:defmethod #.(swig-lispify "set-database-and-schema-name" 'method) ((self #.(swig-lispify "ndb" 'classname)) (t-arg1 #.(swig-lispify "table" 'classname)))
  (#.(swig-lispify "Ndb_setDatabaseAndSchemaName" 'function) (ff-pointer self) t-arg1))

(cl:defmethod #.(swig-lispify "init" 'method) ((self #.(swig-lispify "ndb" 'classname)) (maxNoOfTransactions cl:integer))
  (#.(swig-lispify "Ndb_init" 'function) (ff-pointer self) maxNoOfTransactions))

(cl:defmethod #.(swig-lispify "init" 'method) ((self #.(swig-lispify "ndb" 'classname)))
  (#.(swig-lispify "Ndb_init" 'function) (ff-pointer self)))

(cl:defmethod #.(swig-lispify "set-eventbuf-max-alloc" 'method) ((self #.(swig-lispify "ndb" 'classname)) (sz cl:integer))
  (#.(swig-lispify "Ndb_set_eventbuf_max_alloc" 'function) (ff-pointer self) sz))

(cl:defmethod #.(swig-lispify "get-eventbuf-max-alloc" 'method) ((self #.(swig-lispify "ndb" 'classname)))
  (#.(swig-lispify "Ndb_get_eventbuf_max_alloc" 'function) (ff-pointer self)))

(cl:defmethod #.(swig-lispify "set-eventbuffer-free-percent" 'method) ((self #.(swig-lispify "ndb" 'classname)) (sz cl:integer))
  (#.(swig-lispify "Ndb_set_eventbuffer_free_percent" 'function) (ff-pointer self) sz))

(cl:defmethod #.(swig-lispify "get-eventbuffer-free-percent" 'method) ((self #.(swig-lispify "ndb" 'classname)))
  (#.(swig-lispify "Ndb_get_eventbuffer_free_percent" 'function) (ff-pointer self)))

(cl:defmethod #.(swig-lispify "get-event-buffer-memory-usage" 'method) ((self #.(swig-lispify "ndb" 'classname)) arg1)
  (#.(swig-lispify "Ndb_get_event_buffer_memory_usage" 'function) (ff-pointer self) arg1))

(cl:defmethod #.(swig-lispify "set-event-buffer-queue-empty-epoch" 'method) ((self #.(swig-lispify "ndb" 'classname)) (queue_empty_epoch cl:t))
  (#.(swig-lispify "Ndb_setEventBufferQueueEmptyEpoch" 'function) (ff-pointer self) queue_empty_epoch))

(cl:defmethod #.(swig-lispify "wait-until-ready" 'method) ((self #.(swig-lispify "ndb" 'classname)) (timeout cl:integer))
  (#.(swig-lispify "Ndb_waitUntilReady" 'function) (ff-pointer self) timeout))

(cl:defmethod #.(swig-lispify "wait-until-ready" 'method) ((self #.(swig-lispify "ndb" 'classname)))
  (#.(swig-lispify "Ndb_waitUntilReady" 'function) (ff-pointer self)))

(cl:defmethod #.(swig-lispify "get-dictionary" 'method) ((self #.(swig-lispify "ndb" 'classname)))
  (#.(swig-lispify "Ndb_getDictionary" 'function) (ff-pointer self)))

(cl:defmethod #.(swig-lispify "create-event-operation" 'method) ((self #.(swig-lispify "ndb" 'classname)) (eventName cl:string))
  (#.(swig-lispify "Ndb_createEventOperation" 'function) (ff-pointer self) eventName))

(cl:defmethod #.(swig-lispify "drop-event-operation" 'method) ((self #.(swig-lispify "ndb" 'classname)) eventOp)
  (#.(swig-lispify "Ndb_dropEventOperation" 'function) (ff-pointer self) eventOp))

(cl:defmethod #.(swig-lispify "poll-events2" 'method) ((self #.(swig-lispify "ndb" 'classname)) (aMillisecondNumber cl:integer) highestQueuedEpoch)
  (#.(swig-lispify "Ndb_pollEvents2" 'function) (ff-pointer self) aMillisecondNumber highestQueuedEpoch))

(cl:defmethod #.(swig-lispify "poll-events2" 'method) ((self #.(swig-lispify "ndb" 'classname)) (aMillisecondNumber cl:integer))
  (#.(swig-lispify "Ndb_pollEvents2" 'function) (ff-pointer self) aMillisecondNumber))

(cl:defmethod #.(swig-lispify "is-expecting-higher-queued-epochs" 'method) ((self #.(swig-lispify "ndb" 'classname)))
  (#.(swig-lispify "Ndb_isExpectingHigherQueuedEpochs" 'function) (ff-pointer self)))

(cl:defmethod #.(swig-lispify "poll-events" 'method) ((self #.(swig-lispify "ndb" 'classname)) (aMillisecondNumber cl:integer) latestGCI)
  (#.(swig-lispify "Ndb_pollEvents" 'function) (ff-pointer self) aMillisecondNumber latestGCI))

(cl:defmethod #.(swig-lispify "poll-events" 'method) ((self #.(swig-lispify "ndb" 'classname)) (aMillisecondNumber cl:integer))
  (#.(swig-lispify "Ndb_pollEvents" 'function) (ff-pointer self) aMillisecondNumber))

(cl:defmethod #.(swig-lispify "next-event2" 'method) ((self #.(swig-lispify "ndb" 'classname)))
  (#.(swig-lispify "Ndb_nextEvent2" 'function) (ff-pointer self)))

(cl:defmethod #.(swig-lispify "next-event" 'method) ((self #.(swig-lispify "ndb" 'classname)))
  (#.(swig-lispify "Ndb_nextEvent" 'function) (ff-pointer self)))

(cl:defmethod #.(swig-lispify "is-consistent" 'method) ((self #.(swig-lispify "ndb" 'classname)) gci)
  (#.(swig-lispify "Ndb_isConsistent" 'function) (ff-pointer self) gci))

(cl:defmethod #.(swig-lispify "is-consistent-gci" 'method) ((self #.(swig-lispify "ndb" 'classname)) gci)
  (#.(swig-lispify "Ndb_isConsistentGCI" 'function) (ff-pointer self) gci))

(cl:defmethod #.(swig-lispify "get-gcievent-operations" 'method) ((self #.(swig-lispify "ndb" 'classname)) iter event_types)
  (#.(swig-lispify "Ndb_getGCIEventOperations" 'function) (ff-pointer self) iter event_types))

(cl:defmethod #.(swig-lispify "get-next-event-op-in-epoch2" 'method) ((self #.(swig-lispify "ndb" 'classname)) iter event_types)
  (#.(swig-lispify "Ndb_getNextEventOpInEpoch2" 'function) (ff-pointer self) iter event_types))

(cl:defmethod #.(swig-lispify "get-next-event-op-in-epoch3" 'method) ((self #.(swig-lispify "ndb" 'classname)) iter event_types cumulative_any_value)
  (#.(swig-lispify "Ndb_getNextEventOpInEpoch3" 'function) (ff-pointer self) iter event_types cumulative_any_value))

(cl:defmethod #.(swig-lispify "get-highest-queued-epoch" 'method) ((self #.(swig-lispify "ndb" 'classname)))
  (#.(swig-lispify "Ndb_getHighestQueuedEpoch" 'function) (ff-pointer self)))

(cl:defmethod #.(swig-lispify "flush-incomplete-events" 'method) ((self #.(swig-lispify "ndb" 'classname)) gci)
  (#.(swig-lispify "Ndb_flushIncompleteEvents" 'function) (ff-pointer self) gci))

(cl:defmethod #.(swig-lispify "get-event-operation" 'method) ((self #.(swig-lispify "ndb" 'classname)) eventOp)
  (#.(swig-lispify "Ndb_getEventOperation" 'function) (ff-pointer self) eventOp))

(cl:defmethod #.(swig-lispify "get-event-operation" 'method) ((self #.(swig-lispify "ndb" 'classname)))
  (#.(swig-lispify "Ndb_getEventOperation" 'function) (ff-pointer self)))

(cl:defmethod #.(swig-lispify "get-latest-gci" 'method) ((self #.(swig-lispify "ndb" 'classname)))
  (#.(swig-lispify "Ndb_getLatestGCI" 'function) (ff-pointer self)))

(cl:defmethod #.(swig-lispify "force-gcp" 'method) ((self #.(swig-lispify "ndb" 'classname)))
  (#.(swig-lispify "Ndb_forceGCP" 'function) (ff-pointer self)))

(cl:defmethod #.(swig-lispify "set-report-thresh-event-gcislip" 'method) ((self #.(swig-lispify "ndb" 'classname)) (thresh cl:integer))
  (#.(swig-lispify "Ndb_setReportThreshEventGCISlip" 'function) (ff-pointer self) thresh))

(cl:defmethod #.(swig-lispify "set-report-thresh-event-free-mem" 'method) ((self #.(swig-lispify "ndb" 'classname)) (thresh cl:integer))
  (#.(swig-lispify "Ndb_setReportThreshEventFreeMem" 'function) (ff-pointer self) thresh))

(cl:defmethod #.(swig-lispify "start-transaction" 'method) ((self #.(swig-lispify "ndb" 'classname)) (table #.(swig-lispify "table" 'classname)) (keyData cl:string) (keyLen cl:integer))
  (#.(swig-lispify "Ndb_startTransaction" 'function) (ff-pointer self) table keyData keyLen))

(cl:defmethod #.(swig-lispify "start-transaction" 'method) ((self #.(swig-lispify "ndb" 'classname)) (table #.(swig-lispify "table" 'classname)) (keyData cl:string))
  (#.(swig-lispify "Ndb_startTransaction" 'function) (ff-pointer self) table keyData))

(cl:defmethod #.(swig-lispify "start-transaction" 'method) ((self #.(swig-lispify "ndb" 'classname)) (table #.(swig-lispify "table" 'classname)))
  (#.(swig-lispify "Ndb_startTransaction" 'function) (ff-pointer self) table))

(cl:defmethod #.(swig-lispify "start-transaction" 'method) ((self #.(swig-lispify "ndb" 'classname)))
  (#.(swig-lispify "Ndb_startTransaction" 'function) (ff-pointer self)))

(cl:defmethod #.(swig-lispify "start-transaction" 'method) ((self #.(swig-lispify "ndb" 'classname)) (table #.(swig-lispify "table" 'classname)) keyData xfrmbuf (xfrmbuflen cl:integer))
  (#.(swig-lispify "Ndb_startTransaction" 'function) (ff-pointer self) table keyData xfrmbuf xfrmbuflen))

(cl:defmethod #.(swig-lispify "start-transaction" 'method) ((self #.(swig-lispify "ndb" 'classname)) (table #.(swig-lispify "table" 'classname)) keyData xfrmbuf)
  (#.(swig-lispify "Ndb_startTransaction" 'function) (ff-pointer self) table keyData xfrmbuf))

(cl:defmethod #.(swig-lispify "start-transaction" 'method) ((self #.(swig-lispify "ndb" 'classname)) (table #.(swig-lispify "table" 'classname)) keyData)
  (#.(swig-lispify "Ndb_startTransaction" 'function) (ff-pointer self) table keyData))

(cl:defmethod #.(swig-lispify "start-transaction" 'method) ((self #.(swig-lispify "ndb" 'classname)) keyRec (keyData cl:string) xfrmbuf (xfrmbuflen cl:integer))
  (#.(swig-lispify "Ndb_startTransaction" 'function) (ff-pointer self) keyRec keyData xfrmbuf xfrmbuflen))

(cl:defmethod #.(swig-lispify "start-transaction" 'method) ((self #.(swig-lispify "ndb" 'classname)) (table #.(swig-lispify "table" 'classname)) (partitionId cl:integer))
  (#.(swig-lispify "Ndb_startTransaction" 'function) (ff-pointer self) table partitionId))

(cl:defmethod #.(swig-lispify "start-transaction" 'method) ((self #.(swig-lispify "ndb" 'classname)) (nodeId cl:integer) (instanceId cl:integer))
  (#.(swig-lispify "Ndb_startTransaction" 'function) (ff-pointer self) nodeId instanceId))

(cl:defmethod #.(swig-lispify "close-transaction" 'method) ((self #.(swig-lispify "ndb" 'classname)) arg1)
  (#.(swig-lispify "Ndb_closeTransaction" 'function) (ff-pointer self) arg1))

(cl:defmethod #.(swig-lispify "poll-ndb" 'method) ((self #.(swig-lispify "ndb" 'classname)) (aMillisecondNumber cl:integer) (minNoOfEventsToWakeup cl:integer))
  (#.(swig-lispify "Ndb_pollNdb" 'function) (ff-pointer self) aMillisecondNumber minNoOfEventsToWakeup))

(cl:defmethod #.(swig-lispify "poll-ndb" 'method) ((self #.(swig-lispify "ndb" 'classname)) (aMillisecondNumber cl:integer))
  (#.(swig-lispify "Ndb_pollNdb" 'function) (ff-pointer self) aMillisecondNumber))

(cl:defmethod #.(swig-lispify "poll-ndb" 'method) ((self #.(swig-lispify "ndb" 'classname)))
  (#.(swig-lispify "Ndb_pollNdb" 'function) (ff-pointer self)))

(cl:defmethod #.(swig-lispify "send-prepared-transactions" 'method) ((self #.(swig-lispify "ndb" 'classname)) (forceSend cl:integer))
  (#.(swig-lispify "Ndb_sendPreparedTransactions" 'function) (ff-pointer self) forceSend))

(cl:defmethod #.(swig-lispify "send-prepared-transactions" 'method) ((self #.(swig-lispify "ndb" 'classname)))
  (#.(swig-lispify "Ndb_sendPreparedTransactions" 'function) (ff-pointer self)))

(cl:defmethod #.(swig-lispify "send-poll-ndb" 'method) ((self #.(swig-lispify "ndb" 'classname)) (aMillisecondNumber cl:integer) (minNoOfEventsToWakeup cl:integer) (forceSend cl:integer))
  (#.(swig-lispify "Ndb_sendPollNdb" 'function) (ff-pointer self) aMillisecondNumber minNoOfEventsToWakeup forceSend))

(cl:defmethod #.(swig-lispify "send-poll-ndb" 'method) ((self #.(swig-lispify "ndb" 'classname)) (aMillisecondNumber cl:integer) (minNoOfEventsToWakeup cl:integer))
  (#.(swig-lispify "Ndb_sendPollNdb" 'function) (ff-pointer self) aMillisecondNumber minNoOfEventsToWakeup))

(cl:defmethod #.(swig-lispify "send-poll-ndb" 'method) ((self #.(swig-lispify "ndb" 'classname)) (aMillisecondNumber cl:integer))
  (#.(swig-lispify "Ndb_sendPollNdb" 'function) (ff-pointer self) aMillisecondNumber))

(cl:defmethod #.(swig-lispify "send-poll-ndb" 'method) ((self #.(swig-lispify "ndb" 'classname)))
  (#.(swig-lispify "Ndb_sendPollNdb" 'function) (ff-pointer self)))

(cl:defmethod #.(swig-lispify "get-ndb-error" 'method) ((self #.(swig-lispify "ndb" 'classname)))
  (#.(swig-lispify "Ndb_getNdbError" 'function) (ff-pointer self)))

(cl:defmethod #.(swig-lispify "get-ndb-error" 'method) ((self #.(swig-lispify "ndb" 'classname)) (errorCode cl:integer))
  (#.(swig-lispify "Ndb_getNdbError" 'function) (ff-pointer self) errorCode))

(cl:defmethod #.(swig-lispify "get-ndb-error-detail" 'method) ((self #.(swig-lispify "ndb" 'classname)) err (buff cl:string) (buffLen cl:integer))
  (#.(swig-lispify "Ndb_getNdbErrorDetail" 'function) (ff-pointer self) err buff buffLen))

(cl:defmethod #.(swig-lispify "get-node-id" 'method) ((self #.(swig-lispify "ndb" 'classname)))
  (#.(swig-lispify "Ndb_getNodeId" 'function) (ff-pointer self)))

(cl:defmethod #.(swig-lispify "using-fully-qualified-names" 'method) ((self #.(swig-lispify "ndb" 'classname)))
  (#.(swig-lispify "Ndb_usingFullyQualifiedNames" 'function) (ff-pointer self)))

(cl:defmethod #.(swig-lispify "init-auto-increment" 'method) ((self #.(swig-lispify "ndb" 'classname)))
  (#.(swig-lispify "Ndb_initAutoIncrement" 'function) (ff-pointer self)))

(cl:defmethod #.(swig-lispify "get-auto-increment-value" 'method) ((self #.(swig-lispify "ndb" 'classname)) (aTableName cl:string) autoValue (cacheSize cl:integer) step start)
  (#.(swig-lispify "Ndb_getAutoIncrementValue" 'function) (ff-pointer self) aTableName autoValue cacheSize step start))

(cl:defmethod #.(swig-lispify "get-auto-increment-value" 'method) ((self #.(swig-lispify "ndb" 'classname)) (aTableName cl:string) autoValue (cacheSize cl:integer) step)
  (#.(swig-lispify "Ndb_getAutoIncrementValue" 'function) (ff-pointer self) aTableName autoValue cacheSize step))

(cl:defmethod #.(swig-lispify "get-auto-increment-value" 'method) ((self #.(swig-lispify "ndb" 'classname)) (aTableName cl:string) autoValue (cacheSize cl:integer))
  (#.(swig-lispify "Ndb_getAutoIncrementValue" 'function) (ff-pointer self) aTableName autoValue cacheSize))

(cl:defmethod #.(swig-lispify "get-auto-increment-value" 'method) ((self #.(swig-lispify "ndb" 'classname)) (aTable #.(swig-lispify "table" 'classname)) autoValue (cacheSize cl:integer) step start)
  (#.(swig-lispify "Ndb_getAutoIncrementValue" 'function) (ff-pointer self) aTable autoValue cacheSize step start))

(cl:defmethod #.(swig-lispify "get-auto-increment-value" 'method) ((self #.(swig-lispify "ndb" 'classname)) (aTable #.(swig-lispify "table" 'classname)) autoValue (cacheSize cl:integer) step)
  (#.(swig-lispify "Ndb_getAutoIncrementValue" 'function) (ff-pointer self) aTable autoValue cacheSize step))

(cl:defmethod #.(swig-lispify "get-auto-increment-value" 'method) ((self #.(swig-lispify "ndb" 'classname)) (aTable #.(swig-lispify "table" 'classname)) autoValue (cacheSize cl:integer))
  (#.(swig-lispify "Ndb_getAutoIncrementValue" 'function) (ff-pointer self) aTable autoValue cacheSize))

(cl:defmethod #.(swig-lispify "get-auto-increment-value" 'method) ((self #.(swig-lispify "ndb" 'classname)) (aTable #.(swig-lispify "table" 'classname)) range autoValue (cacheSize cl:integer) step start)
  (#.(swig-lispify "Ndb_getAutoIncrementValue" 'function) (ff-pointer self) aTable range autoValue cacheSize step start))

(cl:defmethod #.(swig-lispify "get-auto-increment-value" 'method) ((self #.(swig-lispify "ndb" 'classname)) (aTable #.(swig-lispify "table" 'classname)) range autoValue (cacheSize cl:integer) step)
  (#.(swig-lispify "Ndb_getAutoIncrementValue" 'function) (ff-pointer self) aTable range autoValue cacheSize step))

(cl:defmethod #.(swig-lispify "get-auto-increment-value" 'method) ((self #.(swig-lispify "ndb" 'classname)) (aTable #.(swig-lispify "table" 'classname)) range autoValue (cacheSize cl:integer))
  (#.(swig-lispify "Ndb_getAutoIncrementValue" 'function) (ff-pointer self) aTable range autoValue cacheSize))

(cl:defmethod #.(swig-lispify "read-auto-increment-value" 'method) ((self #.(swig-lispify "ndb" 'classname)) (aTableName cl:string) autoValue)
  (#.(swig-lispify "Ndb_readAutoIncrementValue" 'function) (ff-pointer self) aTableName autoValue))

(cl:defmethod #.(swig-lispify "read-auto-increment-value" 'method) ((self #.(swig-lispify "ndb" 'classname)) (aTable #.(swig-lispify "table" 'classname)) autoValue)
  (#.(swig-lispify "Ndb_readAutoIncrementValue" 'function) (ff-pointer self) aTable autoValue))

(cl:defmethod #.(swig-lispify "read-auto-increment-value" 'method) ((self #.(swig-lispify "ndb" 'classname)) (aTable #.(swig-lispify "table" 'classname)) range autoValue)
  (#.(swig-lispify "Ndb_readAutoIncrementValue" 'function) (ff-pointer self) aTable range autoValue))

(cl:defmethod #.(swig-lispify "set-auto-increment-value" 'method) ((self #.(swig-lispify "ndb" 'classname)) (aTableName cl:string) autoValue (modify cl:t))
  (#.(swig-lispify "Ndb_setAutoIncrementValue" 'function) (ff-pointer self) aTableName autoValue modify))

(cl:defmethod #.(swig-lispify "set-auto-increment-value" 'method) ((self #.(swig-lispify "ndb" 'classname)) (aTable #.(swig-lispify "table" 'classname)) autoValue (modify cl:t))
  (#.(swig-lispify "Ndb_setAutoIncrementValue" 'function) (ff-pointer self) aTable autoValue modify))

(cl:defmethod #.(swig-lispify "set-auto-increment-value" 'method) ((self #.(swig-lispify "ndb" 'classname)) (aTable #.(swig-lispify "table" 'classname)) range autoValue (modify cl:t))
  (#.(swig-lispify "Ndb_setAutoIncrementValue" 'function) (ff-pointer self) aTable range autoValue modify))

(cl:defmethod #.(swig-lispify "check-update-auto-increment-value" 'method) ((self #.(swig-lispify "ndb" 'classname)) range autoValue)
  (#.(swig-lispify "Ndb_checkUpdateAutoIncrementValue" 'function) (ff-pointer self) range autoValue))

(cl:defmethod #.(swig-lispify "hupp" 'method) ((self #.(swig-lispify "ndb" 'classname)) arg1)
  (#.(swig-lispify "Ndb_hupp" 'function) (ff-pointer self) arg1))

(cl:defmethod #.(swig-lispify "get-reference" 'method) ((self #.(swig-lispify "ndb" 'classname)))
  (#.(swig-lispify "Ndb_getReference" 'function) (ff-pointer self)))

(cl:defmethod #.(swig-lispify "get-free-list-usage" 'method) ((self #.(swig-lispify "ndb" 'classname)) arg1)
  (#.(swig-lispify "Ndb_get_free_list_usage" 'function) (ff-pointer self) arg1))

(cl:defmethod #.(swig-lispify "get-min-db-node-version" 'method) ((self #.(swig-lispify "ndb" 'classname)))
  (#.(swig-lispify "Ndb_getMinDbNodeVersion" 'function) (ff-pointer self)))

(cl:defmethod #.(swig-lispify "set-custom-data" 'method) ((self #.(swig-lispify "ndb" 'classname)) arg1)
  (#.(swig-lispify "Ndb_setCustomData" 'function) (ff-pointer self) arg1))

(cl:defmethod #.(swig-lispify "get-custom-data" 'method) ((self #.(swig-lispify "ndb" 'classname)))
  (#.(swig-lispify "Ndb_getCustomData" 'function) (ff-pointer self)))

(cl:defmethod #.(swig-lispify "set-custom-data64" 'method) ((self #.(swig-lispify "ndb" 'classname)) arg1)
  (#.(swig-lispify "Ndb_setCustomData64" 'function) (ff-pointer self) arg1))

(cl:defmethod #.(swig-lispify "get-custom-data64" 'method) ((self #.(swig-lispify "ndb" 'classname)))
  (#.(swig-lispify "Ndb_getCustomData64" 'function) (ff-pointer self)))

(cl:defmethod #.(swig-lispify "get-next-transaction-id" 'method) ((self #.(swig-lispify "ndb" 'classname)))
  (#.(swig-lispify "Ndb_getNextTransactionId" 'function) (ff-pointer self)))

(cl:defmethod #.(swig-lispify "get-client-stat" 'method) ((self #.(swig-lispify "ndb" 'classname)) (id cl:integer))
  (#.(swig-lispify "Ndb_getClientStat" 'function) (ff-pointer self) id))

(cl:defmethod #.(swig-lispify "get-client-stat-name" 'method) ((self #.(swig-lispify "ndb" 'classname)) (id cl:integer))
  (#.(swig-lispify "Ndb_getClientStatName" 'function) (ff-pointer self) id))


(cl:defclass #.(swig-lispify "ndb-receiver" 'classname)()
  ((ff-pointer :reader ff-pointer)))

(cl:defmethod initialize-instance :after ((obj #.(swig-lispify "ndb-receiver" 'class)) &key (aNdb #.(swig-lispify "ndb" 'classname)))
  (setf (slot-value obj 'ff-pointer) (#.(swig-lispify "new_NdbReceiver" 'function) aNdb)))

(cl:defmethod #.(swig-lispify "init" 'method) ((self #.(swig-lispify "ndb-receiver" 'classname)) (type cl:integer) owner)
  (#.(swig-lispify "NdbReceiver_init" 'function) (ff-pointer self) type owner))

(cl:defmethod #.(swig-lispify "release" 'method) ((self #.(swig-lispify "ndb-receiver" 'classname)))
  (#.(swig-lispify "NdbReceiver_release" 'function) (ff-pointer self)))

(cl:defmethod #.(swig-lispify "get-id" 'method) ((self #.(swig-lispify "ndb-receiver" 'classname)))
  (#.(swig-lispify "NdbReceiver_getId" 'function) (ff-pointer self)))

(cl:defmethod #.(swig-lispify "get-type" 'method) ((self #.(swig-lispify "ndb-receiver" 'classname)))
  (#.(swig-lispify "NdbReceiver_getType" 'function) (ff-pointer self)))

(cl:defmethod #.(swig-lispify "get-transaction" 'method) ((self #.(swig-lispify "ndb-receiver" 'classname)) (type cl:integer))
  (#.(swig-lispify "NdbReceiver_getTransaction" 'function) (ff-pointer self) type))

(cl:defmethod #.(swig-lispify "get-owner" 'method) ((self #.(swig-lispify "ndb-receiver" 'classname)))
  (#.(swig-lispify "NdbReceiver_getOwner" 'function) (ff-pointer self)))

(cl:defmethod #.(swig-lispify "check-magic-number" 'method) ((self #.(swig-lispify "ndb-receiver" 'classname)))
  (#.(swig-lispify "NdbReceiver_checkMagicNumber" 'function) (ff-pointer self)))

(cl:defmethod #.(swig-lispify "get-magic-number-from-object" 'method) ((self #.(swig-lispify "ndb-receiver" 'classname)))
  (#.(swig-lispify "NdbReceiver_getMagicNumberFromObject" 'function) (ff-pointer self)))

(cl:defmethod #.(swig-lispify "next" 'method) ((self #.(swig-lispify "ndb-receiver" 'classname)) (next_arg #.(swig-lispify "ndb-receiver" 'classname)))
  (#.(swig-lispify "NdbReceiver_next" 'function) (ff-pointer self) (ff-pointer next_arg)))

(cl:defmethod #.(swig-lispify "next" 'method) ((self #.(swig-lispify "ndb-receiver" 'classname)))
  (#.(swig-lispify "NdbReceiver_next" 'function) (ff-pointer self)))

(cl:defmethod #.(swig-lispify "set-error-code" 'method) ((self #.(swig-lispify "ndb-receiver" 'classname)) (arg1 cl:integer))
  (#.(swig-lispify "NdbReceiver_setErrorCode" 'function) (ff-pointer self) arg1))

(cl:defmethod #.(swig-lispify "prepare-receive" 'method) ((self #.(swig-lispify "ndb-receiver" 'classname)) buf)
  (#.(swig-lispify "NdbReceiver_prepareReceive" 'function) (ff-pointer self) buf))


(cl:defclass #.(swig-lispify "ndb-operation" 'classname)()
  ((ff-pointer :reader ff-pointer)))

(cl:defmethod #.(swig-lispify "insert-tuple" 'method) ((self #.(swig-lispify "ndb-operation" 'classname)))
  (#.(swig-lispify "NdbOperation_insertTuple" 'function) (ff-pointer self)))

(cl:defmethod #.(swig-lispify "update-tuple" 'method) ((self #.(swig-lispify "ndb-operation" 'classname)))
  (#.(swig-lispify "NdbOperation_updateTuple" 'function) (ff-pointer self)))

(cl:defmethod #.(swig-lispify "write-tuple" 'method) ((self #.(swig-lispify "ndb-operation" 'classname)))
  (#.(swig-lispify "NdbOperation_writeTuple" 'function) (ff-pointer self)))

(cl:defmethod #.(swig-lispify "delete-tuple" 'method) ((self #.(swig-lispify "ndb-operation" 'classname)))
  (#.(swig-lispify "NdbOperation_deleteTuple" 'function) (ff-pointer self)))

(cl:defmethod #.(swig-lispify "read-tuple" 'method) ((self #.(swig-lispify "ndb-operation" 'classname)) (arg1 cl:integer))
  (#.(swig-lispify "NdbOperation_readTuple" 'function) (ff-pointer self) arg1))

(cl:defmethod #.(swig-lispify "read-tuple" 'method) ((self #.(swig-lispify "ndb-operation" 'classname)))
  (#.(swig-lispify "NdbOperation_readTuple" 'function) (ff-pointer self)))

(cl:defmethod #.(swig-lispify "read-tuple-exclusive" 'method) ((self #.(swig-lispify "ndb-operation" 'classname)))
  (#.(swig-lispify "NdbOperation_readTupleExclusive" 'function) (ff-pointer self)))

(cl:defmethod #.(swig-lispify "simple-read" 'method) ((self #.(swig-lispify "ndb-operation" 'classname)))
  (#.(swig-lispify "NdbOperation_simpleRead" 'function) (ff-pointer self)))

(cl:defmethod #.(swig-lispify "dirty-read" 'method) ((self #.(swig-lispify "ndb-operation" 'classname)))
  (#.(swig-lispify "NdbOperation_dirtyRead" 'function) (ff-pointer self)))

(cl:defmethod #.(swig-lispify "committed-read" 'method) ((self #.(swig-lispify "ndb-operation" 'classname)))
  (#.(swig-lispify "NdbOperation_committedRead" 'function) (ff-pointer self)))

(cl:defmethod #.(swig-lispify "dirty-update" 'method) ((self #.(swig-lispify "ndb-operation" 'classname)))
  (#.(swig-lispify "NdbOperation_dirtyUpdate" 'function) (ff-pointer self)))

(cl:defmethod #.(swig-lispify "dirty-write" 'method) ((self #.(swig-lispify "ndb-operation" 'classname)))
  (#.(swig-lispify "NdbOperation_dirtyWrite" 'function) (ff-pointer self)))

(cl:defmethod #.(swig-lispify "interpreted-update-tuple" 'method) ((self #.(swig-lispify "ndb-operation" 'classname)))
  (#.(swig-lispify "NdbOperation_interpretedUpdateTuple" 'function) (ff-pointer self)))

(cl:defmethod #.(swig-lispify "interpreted-delete-tuple" 'method) ((self #.(swig-lispify "ndb-operation" 'classname)))
  (#.(swig-lispify "NdbOperation_interpretedDeleteTuple" 'function) (ff-pointer self)))

(cl:defmethod #.(swig-lispify "equal" 'method) ((self #.(swig-lispify "ndb-operation" 'classname)) (anAttrName cl:string) (aValue cl:string) (len cl:integer))
  (#.(swig-lispify "NdbOperation_equal" 'function) (ff-pointer self) anAttrName aValue len))

(cl:defmethod #.(swig-lispify "equal" 'method) ((self #.(swig-lispify "ndb-operation" 'classname)) (anAttrName cl:string) (aValue cl:string))
  (#.(swig-lispify "NdbOperation_equal" 'function) (ff-pointer self) anAttrName aValue))

(cl:defmethod #.(swig-lispify "equal" 'method) ((self #.(swig-lispify "ndb-operation" 'classname)) (anAttrName cl:string) (aValue cl:integer))
  (#.(swig-lispify "NdbOperation_equal" 'function) (ff-pointer self) anAttrName aValue))

(cl:defmethod #.(swig-lispify "equal" 'method) ((self #.(swig-lispify "ndb-operation" 'classname)) (anAttrName cl:string) (aValue cl:integer))
  (#.(swig-lispify "NdbOperation_equal" 'function) (ff-pointer self) anAttrName aValue))

(cl:defmethod #.(swig-lispify "equal" 'method) ((self #.(swig-lispify "ndb-operation" 'classname)) (anAttrName cl:string) aValue)
  (#.(swig-lispify "NdbOperation_equal" 'function) (ff-pointer self) anAttrName aValue))

(cl:defmethod #.(swig-lispify "equal" 'method) ((self #.(swig-lispify "ndb-operation" 'classname)) (anAttrName cl:string) aValue)
  (#.(swig-lispify "NdbOperation_equal" 'function) (ff-pointer self) anAttrName aValue))

(cl:defmethod #.(swig-lispify "equal" 'method) ((self #.(swig-lispify "ndb-operation" 'classname)) (anAttrId cl:integer) (aValue cl:string) (len cl:integer))
  (#.(swig-lispify "NdbOperation_equal" 'function) (ff-pointer self) anAttrId aValue len))

(cl:defmethod #.(swig-lispify "equal" 'method) ((self #.(swig-lispify "ndb-operation" 'classname)) (anAttrId cl:integer) (aValue cl:string))
  (#.(swig-lispify "NdbOperation_equal" 'function) (ff-pointer self) anAttrId aValue))

(cl:defmethod #.(swig-lispify "equal" 'method) ((self #.(swig-lispify "ndb-operation" 'classname)) (anAttrId cl:integer) (aValue cl:integer))
  (#.(swig-lispify "NdbOperation_equal" 'function) (ff-pointer self) anAttrId aValue))

(cl:defmethod #.(swig-lispify "equal" 'method) ((self #.(swig-lispify "ndb-operation" 'classname)) (anAttrId cl:integer) (aValue cl:integer))
  (#.(swig-lispify "NdbOperation_equal" 'function) (ff-pointer self) anAttrId aValue))

(cl:defmethod #.(swig-lispify "equal" 'method) ((self #.(swig-lispify "ndb-operation" 'classname)) (anAttrId cl:integer) aValue)
  (#.(swig-lispify "NdbOperation_equal" 'function) (ff-pointer self) anAttrId aValue))

(cl:defmethod #.(swig-lispify "equal" 'method) ((self #.(swig-lispify "ndb-operation" 'classname)) (anAttrId cl:integer) aValue)
  (#.(swig-lispify "NdbOperation_equal" 'function) (ff-pointer self) anAttrId aValue))

(cl:defmethod #.(swig-lispify "get-value" 'method) ((self #.(swig-lispify "ndb-operation" 'classname)) (anAttrName cl:string) (aValue cl:string))
  (#.(swig-lispify "NdbOperation_getValue" 'function) (ff-pointer self) anAttrName aValue))

(cl:defmethod #.(swig-lispify "get-value" 'method) ((self #.(swig-lispify "ndb-operation" 'classname)) (anAttrName cl:string))
  (#.(swig-lispify "NdbOperation_getValue" 'function) (ff-pointer self) anAttrName))

(cl:defmethod #.(swig-lispify "get-value" 'method) ((self #.(swig-lispify "ndb-operation" 'classname)) (anAttrId cl:integer) (aValue cl:string))
  (#.(swig-lispify "NdbOperation_getValue" 'function) (ff-pointer self) anAttrId aValue))

(cl:defmethod #.(swig-lispify "get-value" 'method) ((self #.(swig-lispify "ndb-operation" 'classname)) (anAttrId cl:integer))
  (#.(swig-lispify "NdbOperation_getValue" 'function) (ff-pointer self) anAttrId))

(cl:defmethod #.(swig-lispify "get-value" 'method) ((self #.(swig-lispify "ndb-operation" 'classname)) (arg1 #.(swig-lispify "column" 'classname)) (val cl:string))
  (#.(swig-lispify "NdbOperation_getValue" 'function) (ff-pointer self) arg1 val))

(cl:defmethod #.(swig-lispify "get-value" 'method) ((self #.(swig-lispify "ndb-operation" 'classname)) (arg1 #.(swig-lispify "column" 'classname)))
  (#.(swig-lispify "NdbOperation_getValue" 'function) (ff-pointer self) arg1))

(cl:defmethod #.(swig-lispify "set-value" 'method) ((self #.(swig-lispify "ndb-operation" 'classname)) (anAttrName cl:string) (aValue cl:string) (len cl:integer))
  (#.(swig-lispify "NdbOperation_setValue" 'function) (ff-pointer self) anAttrName aValue len))

(cl:defmethod #.(swig-lispify "set-value" 'method) ((self #.(swig-lispify "ndb-operation" 'classname)) (anAttrName cl:string) (aValue cl:string))
  (#.(swig-lispify "NdbOperation_setValue" 'function) (ff-pointer self) anAttrName aValue))

(cl:defmethod #.(swig-lispify "set-value" 'method) ((self #.(swig-lispify "ndb-operation" 'classname)) (anAttrName cl:string) (aValue cl:integer))
  (#.(swig-lispify "NdbOperation_setValue" 'function) (ff-pointer self) anAttrName aValue))

(cl:defmethod #.(swig-lispify "set-value" 'method) ((self #.(swig-lispify "ndb-operation" 'classname)) (anAttrName cl:string) (aValue cl:integer))
  (#.(swig-lispify "NdbOperation_setValue" 'function) (ff-pointer self) anAttrName aValue))

(cl:defmethod #.(swig-lispify "set-value" 'method) ((self #.(swig-lispify "ndb-operation" 'classname)) (anAttrName cl:string) aValue)
  (#.(swig-lispify "NdbOperation_setValue" 'function) (ff-pointer self) anAttrName aValue))

(cl:defmethod #.(swig-lispify "set-value" 'method) ((self #.(swig-lispify "ndb-operation" 'classname)) (anAttrName cl:string) aValue)
  (#.(swig-lispify "NdbOperation_setValue" 'function) (ff-pointer self) anAttrName aValue))

(cl:defmethod #.(swig-lispify "set-value" 'method) ((self #.(swig-lispify "ndb-operation" 'classname)) (anAttrName cl:string) (aValue cl:number))
  (#.(swig-lispify "NdbOperation_setValue" 'function) (ff-pointer self) anAttrName aValue))

(cl:defmethod #.(swig-lispify "set-value" 'method) ((self #.(swig-lispify "ndb-operation" 'classname)) (anAttrName cl:string) (aValue cl:number))
  (#.(swig-lispify "NdbOperation_setValue" 'function) (ff-pointer self) anAttrName aValue))

(cl:defmethod #.(swig-lispify "set-any-value" 'method) ((self #.(swig-lispify "ndb-operation" 'classname)) (aValue cl:integer))
  (#.(swig-lispify "NdbOperation_setAnyValue" 'function) (ff-pointer self) aValue))

(cl:defmethod #.(swig-lispify "set-optimize" 'method) ((self #.(swig-lispify "ndb-operation" 'classname)) (options cl:integer))
  (#.(swig-lispify "NdbOperation_setOptimize" 'function) (ff-pointer self) options))

(cl:defmethod #.(swig-lispify "set-value" 'method) ((self #.(swig-lispify "ndb-operation" 'classname)) (anAttrId cl:integer) (aValue cl:string) (len cl:integer))
  (#.(swig-lispify "NdbOperation_setValue" 'function) (ff-pointer self) anAttrId aValue len))

(cl:defmethod #.(swig-lispify "set-value" 'method) ((self #.(swig-lispify "ndb-operation" 'classname)) (anAttrId cl:integer) (aValue cl:string))
  (#.(swig-lispify "NdbOperation_setValue" 'function) (ff-pointer self) anAttrId aValue))

(cl:defmethod #.(swig-lispify "set-value" 'method) ((self #.(swig-lispify "ndb-operation" 'classname)) (anAttrId cl:integer) (aValue cl:integer))
  (#.(swig-lispify "NdbOperation_setValue" 'function) (ff-pointer self) anAttrId aValue))

(cl:defmethod #.(swig-lispify "set-value" 'method) ((self #.(swig-lispify "ndb-operation" 'classname)) (anAttrId cl:integer) (aValue cl:integer))
  (#.(swig-lispify "NdbOperation_setValue" 'function) (ff-pointer self) anAttrId aValue))

(cl:defmethod #.(swig-lispify "set-value" 'method) ((self #.(swig-lispify "ndb-operation" 'classname)) (anAttrId cl:integer) aValue)
  (#.(swig-lispify "NdbOperation_setValue" 'function) (ff-pointer self) anAttrId aValue))

(cl:defmethod #.(swig-lispify "set-value" 'method) ((self #.(swig-lispify "ndb-operation" 'classname)) (anAttrId cl:integer) aValue)
  (#.(swig-lispify "NdbOperation_setValue" 'function) (ff-pointer self) anAttrId aValue))

(cl:defmethod #.(swig-lispify "set-value" 'method) ((self #.(swig-lispify "ndb-operation" 'classname)) (anAttrId cl:integer) (aValue cl:number))
  (#.(swig-lispify "NdbOperation_setValue" 'function) (ff-pointer self) anAttrId aValue))

(cl:defmethod #.(swig-lispify "set-value" 'method) ((self #.(swig-lispify "ndb-operation" 'classname)) (anAttrId cl:integer) (aValue cl:number))
  (#.(swig-lispify "NdbOperation_setValue" 'function) (ff-pointer self) anAttrId aValue))

(cl:defmethod #.(swig-lispify "get-blob-handle" 'method) ((self #.(swig-lispify "ndb-operation" 'classname)) (anAttrName cl:string))
  (#.(swig-lispify "NdbOperation_getBlobHandle" 'function) (ff-pointer self) anAttrName))

(cl:defmethod #.(swig-lispify "get-blob-handle" 'method) ((self #.(swig-lispify "ndb-operation" 'classname)) (anAttrId cl:integer))
  (#.(swig-lispify "NdbOperation_getBlobHandle" 'function) (ff-pointer self) anAttrId))

(cl:defmethod #.(swig-lispify "get-blob-handle" 'method) ((self #.(swig-lispify "ndb-operation" 'classname)) (anAttrName cl:string))
  (#.(swig-lispify "NdbOperation_getBlobHandle" 'function) (ff-pointer self) anAttrName))

(cl:defmethod #.(swig-lispify "get-blob-handle" 'method) ((self #.(swig-lispify "ndb-operation" 'classname)) (anAttrId cl:integer))
  (#.(swig-lispify "NdbOperation_getBlobHandle" 'function) (ff-pointer self) anAttrId))

(cl:defmethod #.(swig-lispify "inc-value" 'method) ((self #.(swig-lispify "ndb-operation" 'classname)) (anAttrName cl:string) (aValue cl:integer))
  (#.(swig-lispify "NdbOperation_incValue" 'function) (ff-pointer self) anAttrName aValue))

(cl:defmethod #.(swig-lispify "inc-value" 'method) ((self #.(swig-lispify "ndb-operation" 'classname)) (anAttrName cl:string) aValue)
  (#.(swig-lispify "NdbOperation_incValue" 'function) (ff-pointer self) anAttrName aValue))

(cl:defmethod #.(swig-lispify "inc-value" 'method) ((self #.(swig-lispify "ndb-operation" 'classname)) (anAttrId cl:integer) (aValue cl:integer))
  (#.(swig-lispify "NdbOperation_incValue" 'function) (ff-pointer self) anAttrId aValue))

(cl:defmethod #.(swig-lispify "inc-value" 'method) ((self #.(swig-lispify "ndb-operation" 'classname)) (anAttrId cl:integer) aValue)
  (#.(swig-lispify "NdbOperation_incValue" 'function) (ff-pointer self) anAttrId aValue))

(cl:defmethod #.(swig-lispify "sub-value" 'method) ((self #.(swig-lispify "ndb-operation" 'classname)) (anAttrName cl:string) (aValue cl:integer))
  (#.(swig-lispify "NdbOperation_subValue" 'function) (ff-pointer self) anAttrName aValue))

(cl:defmethod #.(swig-lispify "sub-value" 'method) ((self #.(swig-lispify "ndb-operation" 'classname)) (anAttrName cl:string) aValue)
  (#.(swig-lispify "NdbOperation_subValue" 'function) (ff-pointer self) anAttrName aValue))

(cl:defmethod #.(swig-lispify "sub-value" 'method) ((self #.(swig-lispify "ndb-operation" 'classname)) (anAttrId cl:integer) (aValue cl:integer))
  (#.(swig-lispify "NdbOperation_subValue" 'function) (ff-pointer self) anAttrId aValue))

(cl:defmethod #.(swig-lispify "sub-value" 'method) ((self #.(swig-lispify "ndb-operation" 'classname)) (anAttrId cl:integer) aValue)
  (#.(swig-lispify "NdbOperation_subValue" 'function) (ff-pointer self) anAttrId aValue))

(cl:defmethod #.(swig-lispify "def-label" 'method) ((self #.(swig-lispify "ndb-operation" 'classname)) (labelNumber cl:integer))
  (#.(swig-lispify "NdbOperation_def_label" 'function) (ff-pointer self) labelNumber))

(cl:defmethod #.(swig-lispify "add-reg" 'method) ((self #.(swig-lispify "ndb-operation" 'classname)) (RegSource1 cl:integer) (RegSource2 cl:integer) (RegDest cl:integer))
  (#.(swig-lispify "NdbOperation_add_reg" 'function) (ff-pointer self) RegSource1 RegSource2 RegDest))

(cl:defmethod #.(swig-lispify "sub-reg" 'method) ((self #.(swig-lispify "ndb-operation" 'classname)) (RegSource1 cl:integer) (RegSource2 cl:integer) (RegDest cl:integer))
  (#.(swig-lispify "NdbOperation_sub_reg" 'function) (ff-pointer self) RegSource1 RegSource2 RegDest))

(cl:defmethod #.(swig-lispify "load-const-u32" 'method) ((self #.(swig-lispify "ndb-operation" 'classname)) (RegDest cl:integer) (Constant cl:integer))
  (#.(swig-lispify "NdbOperation_load_const_u32" 'function) (ff-pointer self) RegDest Constant))

(cl:defmethod #.(swig-lispify "load-const-u64" 'method) ((self #.(swig-lispify "ndb-operation" 'classname)) (RegDest cl:integer) Constant)
  (#.(swig-lispify "NdbOperation_load_const_u64" 'function) (ff-pointer self) RegDest Constant))

(cl:defmethod #.(swig-lispify "load-const-null" 'method) ((self #.(swig-lispify "ndb-operation" 'classname)) (RegDest cl:integer))
  (#.(swig-lispify "NdbOperation_load_const_null" 'function) (ff-pointer self) RegDest))

(cl:defmethod #.(swig-lispify "read-attr" 'method) ((self #.(swig-lispify "ndb-operation" 'classname)) (anAttrName cl:string) (RegDest cl:integer))
  (#.(swig-lispify "NdbOperation_read_attr" 'function) (ff-pointer self) anAttrName RegDest))

(cl:defmethod #.(swig-lispify "write-attr" 'method) ((self #.(swig-lispify "ndb-operation" 'classname)) (anAttrName cl:string) (RegSource cl:integer))
  (#.(swig-lispify "NdbOperation_write_attr" 'function) (ff-pointer self) anAttrName RegSource))

(cl:defmethod #.(swig-lispify "read-attr" 'method) ((self #.(swig-lispify "ndb-operation" 'classname)) (anAttrId cl:integer) (RegDest cl:integer))
  (#.(swig-lispify "NdbOperation_read_attr" 'function) (ff-pointer self) anAttrId RegDest))

(cl:defmethod #.(swig-lispify "write-attr" 'method) ((self #.(swig-lispify "ndb-operation" 'classname)) (anAttrId cl:integer) (RegSource cl:integer))
  (#.(swig-lispify "NdbOperation_write_attr" 'function) (ff-pointer self) anAttrId RegSource))

(cl:defmethod #.(swig-lispify "branch-ge" 'method) ((self #.(swig-lispify "ndb-operation" 'classname)) (RegLvalue cl:integer) (RegRvalue cl:integer) (Label cl:integer))
  (#.(swig-lispify "NdbOperation_branch_ge" 'function) (ff-pointer self) RegLvalue RegRvalue Label))

(cl:defmethod #.(swig-lispify "branch-gt" 'method) ((self #.(swig-lispify "ndb-operation" 'classname)) (RegLvalue cl:integer) (RegRvalue cl:integer) (Label cl:integer))
  (#.(swig-lispify "NdbOperation_branch_gt" 'function) (ff-pointer self) RegLvalue RegRvalue Label))

(cl:defmethod #.(swig-lispify "branch-le" 'method) ((self #.(swig-lispify "ndb-operation" 'classname)) (RegLvalue cl:integer) (RegRvalue cl:integer) (Label cl:integer))
  (#.(swig-lispify "NdbOperation_branch_le" 'function) (ff-pointer self) RegLvalue RegRvalue Label))

(cl:defmethod #.(swig-lispify "branch-lt" 'method) ((self #.(swig-lispify "ndb-operation" 'classname)) (RegLvalue cl:integer) (RegRvalue cl:integer) (Label cl:integer))
  (#.(swig-lispify "NdbOperation_branch_lt" 'function) (ff-pointer self) RegLvalue RegRvalue Label))

(cl:defmethod #.(swig-lispify "branch-eq" 'method) ((self #.(swig-lispify "ndb-operation" 'classname)) (RegLvalue cl:integer) (RegRvalue cl:integer) (Label cl:integer))
  (#.(swig-lispify "NdbOperation_branch_eq" 'function) (ff-pointer self) RegLvalue RegRvalue Label))

(cl:defmethod #.(swig-lispify "branch-ne" 'method) ((self #.(swig-lispify "ndb-operation" 'classname)) (RegLvalue cl:integer) (RegRvalue cl:integer) (Label cl:integer))
  (#.(swig-lispify "NdbOperation_branch_ne" 'function) (ff-pointer self) RegLvalue RegRvalue Label))

(cl:defmethod #.(swig-lispify "branch-ne-null" 'method) ((self #.(swig-lispify "ndb-operation" 'classname)) (RegLvalue cl:integer) (Label cl:integer))
  (#.(swig-lispify "NdbOperation_branch_ne_null" 'function) (ff-pointer self) RegLvalue Label))

(cl:defmethod #.(swig-lispify "branch-eq-null" 'method) ((self #.(swig-lispify "ndb-operation" 'classname)) (RegLvalue cl:integer) (Label cl:integer))
  (#.(swig-lispify "NdbOperation_branch_eq_null" 'function) (ff-pointer self) RegLvalue Label))

(cl:defmethod #.(swig-lispify "branch-label" 'method) ((self #.(swig-lispify "ndb-operation" 'classname)) (Label cl:integer))
  (#.(swig-lispify "NdbOperation_branch_label" 'function) (ff-pointer self) Label))

(cl:defmethod #.(swig-lispify "branch-col-eq-null" 'method) ((self #.(swig-lispify "ndb-operation" 'classname)) (ColId cl:integer) (Label cl:integer))
  (#.(swig-lispify "NdbOperation_branch_col_eq_null" 'function) (ff-pointer self) ColId Label))

(cl:defmethod #.(swig-lispify "branch-col-ne-null" 'method) ((self #.(swig-lispify "ndb-operation" 'classname)) (ColId cl:integer) (Label cl:integer))
  (#.(swig-lispify "NdbOperation_branch_col_ne_null" 'function) (ff-pointer self) ColId Label))

(cl:defmethod #.(swig-lispify "branch-col-eq" 'method) ((self #.(swig-lispify "ndb-operation" 'classname)) (ColId cl:integer) val (len cl:integer) (nopad cl:t) (Label cl:integer))
  (#.(swig-lispify "NdbOperation_branch_col_eq" 'function) (ff-pointer self) ColId val len nopad Label))

(cl:defmethod #.(swig-lispify "branch-col-ne" 'method) ((self #.(swig-lispify "ndb-operation" 'classname)) (ColId cl:integer) val (len cl:integer) (nopad cl:t) (Label cl:integer))
  (#.(swig-lispify "NdbOperation_branch_col_ne" 'function) (ff-pointer self) ColId val len nopad Label))

(cl:defmethod #.(swig-lispify "branch-col-lt" 'method) ((self #.(swig-lispify "ndb-operation" 'classname)) (ColId cl:integer) val (len cl:integer) (nopad cl:t) (Label cl:integer))
  (#.(swig-lispify "NdbOperation_branch_col_lt" 'function) (ff-pointer self) ColId val len nopad Label))

(cl:defmethod #.(swig-lispify "branch-col-le" 'method) ((self #.(swig-lispify "ndb-operation" 'classname)) (ColId cl:integer) val (len cl:integer) (nopad cl:t) (Label cl:integer))
  (#.(swig-lispify "NdbOperation_branch_col_le" 'function) (ff-pointer self) ColId val len nopad Label))

(cl:defmethod #.(swig-lispify "branch-col-gt" 'method) ((self #.(swig-lispify "ndb-operation" 'classname)) (ColId cl:integer) val (len cl:integer) (nopad cl:t) (Label cl:integer))
  (#.(swig-lispify "NdbOperation_branch_col_gt" 'function) (ff-pointer self) ColId val len nopad Label))

(cl:defmethod #.(swig-lispify "branch-col-ge" 'method) ((self #.(swig-lispify "ndb-operation" 'classname)) (ColId cl:integer) val (len cl:integer) (nopad cl:t) (Label cl:integer))
  (#.(swig-lispify "NdbOperation_branch_col_ge" 'function) (ff-pointer self) ColId val len nopad Label))

(cl:defmethod #.(swig-lispify "branch-col-like" 'method) ((self #.(swig-lispify "ndb-operation" 'classname)) (ColId cl:integer) arg2 (len cl:integer) (nopad cl:t) (Label cl:integer))
  (#.(swig-lispify "NdbOperation_branch_col_like" 'function) (ff-pointer self) ColId arg2 len nopad Label))

(cl:defmethod #.(swig-lispify "branch-col-notlike" 'method) ((self #.(swig-lispify "ndb-operation" 'classname)) (ColId cl:integer) arg2 (len cl:integer) (nopad cl:t) (Label cl:integer))
  (#.(swig-lispify "NdbOperation_branch_col_notlike" 'function) (ff-pointer self) ColId arg2 len nopad Label))

(cl:defmethod #.(swig-lispify "branch-col-and-mask-eq-mask" 'method) ((self #.(swig-lispify "ndb-operation" 'classname)) (ColId cl:integer) arg2 (len cl:integer) (nopad cl:t) (Label cl:integer))
  (#.(swig-lispify "NdbOperation_branch_col_and_mask_eq_mask" 'function) (ff-pointer self) ColId arg2 len nopad Label))

(cl:defmethod #.(swig-lispify "branch-col-and-mask-ne-mask" 'method) ((self #.(swig-lispify "ndb-operation" 'classname)) (ColId cl:integer) arg2 (len cl:integer) (nopad cl:t) (Label cl:integer))
  (#.(swig-lispify "NdbOperation_branch_col_and_mask_ne_mask" 'function) (ff-pointer self) ColId arg2 len nopad Label))

(cl:defmethod #.(swig-lispify "branch-col-and-mask-eq-zero" 'method) ((self #.(swig-lispify "ndb-operation" 'classname)) (ColId cl:integer) arg2 (len cl:integer) (nopad cl:t) (Label cl:integer))
  (#.(swig-lispify "NdbOperation_branch_col_and_mask_eq_zero" 'function) (ff-pointer self) ColId arg2 len nopad Label))

(cl:defmethod #.(swig-lispify "branch-col-and-mask-ne-zero" 'method) ((self #.(swig-lispify "ndb-operation" 'classname)) (ColId cl:integer) arg2 (len cl:integer) (nopad cl:t) (Label cl:integer))
  (#.(swig-lispify "NdbOperation_branch_col_and_mask_ne_zero" 'function) (ff-pointer self) ColId arg2 len nopad Label))

(cl:defmethod #.(swig-lispify "interpret-exit-ok" 'method) ((self #.(swig-lispify "ndb-operation" 'classname)))
  (#.(swig-lispify "NdbOperation_interpret_exit_ok" 'function) (ff-pointer self)))

(cl:defmethod #.(swig-lispify "interpret-exit-nok" 'method) ((self #.(swig-lispify "ndb-operation" 'classname)) (ErrorCode cl:integer))
  (#.(swig-lispify "NdbOperation_interpret_exit_nok" 'function) (ff-pointer self) ErrorCode))

(cl:defmethod #.(swig-lispify "interpret-exit-nok" 'method) ((self #.(swig-lispify "ndb-operation" 'classname)))
  (#.(swig-lispify "NdbOperation_interpret_exit_nok" 'function) (ff-pointer self)))

(cl:defmethod #.(swig-lispify "interpret-exit-last-row" 'method) ((self #.(swig-lispify "ndb-operation" 'classname)))
  (#.(swig-lispify "NdbOperation_interpret_exit_last_row" 'function) (ff-pointer self)))

(cl:defmethod #.(swig-lispify "def-subroutine" 'method) ((self #.(swig-lispify "ndb-operation" 'classname)) (SubroutineNumber cl:integer))
  (#.(swig-lispify "NdbOperation_def_subroutine" 'function) (ff-pointer self) SubroutineNumber))

(cl:defmethod #.(swig-lispify "call-sub" 'method) ((self #.(swig-lispify "ndb-operation" 'classname)) (Subroutine cl:integer))
  (#.(swig-lispify "NdbOperation_call_sub" 'function) (ff-pointer self) Subroutine))

(cl:defmethod #.(swig-lispify "ret-sub" 'method) ((self #.(swig-lispify "ndb-operation" 'classname)))
  (#.(swig-lispify "NdbOperation_ret_sub" 'function) (ff-pointer self)))

(cl:defmethod #.(swig-lispify "get-ndb-error" 'method) ((self #.(swig-lispify "ndb-operation" 'classname)))
  (#.(swig-lispify "NdbOperation_getNdbError" 'function) (ff-pointer self)))

(cl:defmethod #.(swig-lispify "get-ndb-error-line" 'method) ((self #.(swig-lispify "ndb-operation" 'classname)))
  (#.(swig-lispify "NdbOperation_getNdbErrorLine" 'function) (ff-pointer self)))

(cl:defmethod #.(swig-lispify "get-ndb-error-line" 'method) ((self #.(swig-lispify "ndb-operation" 'classname)))
  (#.(swig-lispify "NdbOperation_getNdbErrorLine" 'function) (ff-pointer self)))

(cl:defmethod #.(swig-lispify "get-table-name" 'method) ((self #.(swig-lispify "ndb-operation" 'classname)))
  (#.(swig-lispify "NdbOperation_getTableName" 'function) (ff-pointer self)))

(cl:defmethod #.(swig-lispify "get-table" 'method) ((self #.(swig-lispify "ndb-operation" 'classname)))
  (#.(swig-lispify "NdbOperation_getTable" 'function) (ff-pointer self)))

(cl:defmethod #.(swig-lispify "get-type" 'method) ((self #.(swig-lispify "ndb-operation" 'classname)))
  (#.(swig-lispify "NdbOperation_getType" 'function) (ff-pointer self)))

(cl:defmethod #.(swig-lispify "get-lock-mode" 'method) ((self #.(swig-lispify "ndb-operation" 'classname)))
  (#.(swig-lispify "NdbOperation_getLockMode" 'function) (ff-pointer self)))

(cl:defmethod #.(swig-lispify "get-abort-option" 'method) ((self #.(swig-lispify "ndb-operation" 'classname)))
  (#.(swig-lispify "NdbOperation_getAbortOption" 'function) (ff-pointer self)))

(cl:defmethod #.(swig-lispify "set-abort-option" 'method) ((self #.(swig-lispify "ndb-operation" 'classname)) (arg1 cl:integer))
  (#.(swig-lispify "NdbOperation_setAbortOption" 'function) (ff-pointer self) arg1))

(cl:defmethod #.(swig-lispify "get-ndb-transaction" 'method) ((self #.(swig-lispify "ndb-operation" 'classname)))
  (#.(swig-lispify "NdbOperation_getNdbTransaction" 'function) (ff-pointer self)))

(cl:defmethod #.(swig-lispify "set-partition-id" 'method) ((self #.(swig-lispify "ndb-operation" 'classname)) (id cl:integer))
  (#.(swig-lispify "NdbOperation_setPartitionId" 'function) (ff-pointer self) id))

(cl:defmethod #.(swig-lispify "get-partition-id" 'method) ((self #.(swig-lispify "ndb-operation" 'classname)))
  (#.(swig-lispify "NdbOperation_getPartitionId" 'function) (ff-pointer self)))

(cl:defmethod #.(swig-lispify "get-lock-handle" 'method) ((self #.(swig-lispify "ndb-operation" 'classname)))
  (#.(swig-lispify "NdbOperation_getLockHandle" 'function) (ff-pointer self)))

(cl:defmethod #.(swig-lispify "get-lock-handle" 'method) ((self #.(swig-lispify "ndb-operation" 'classname)))
  (#.(swig-lispify "NdbOperation_getLockHandle" 'function) (ff-pointer self)))

(cl:defmethod #.(swig-lispify "set-disable-fk" 'method) ((self #.(swig-lispify "ndb-operation" 'classname)))
  (#.(swig-lispify "NdbOperation_set_disable_fk" 'function) (ff-pointer self)))

(cl:defmethod #.(swig-lispify "set-no-wait" 'method) ((self #.(swig-lispify "ndb-operation" 'classname)))
  (#.(swig-lispify "NdbOperation_setNoWait" 'function) (ff-pointer self)))

(cl:defmethod #.(swig-lispify "next" 'method) ((self #.(swig-lispify "ndb-operation" 'classname)))
  (#.(swig-lispify "NdbOperation_next" 'function) (ff-pointer self)))

(cl:defmethod #.(swig-lispify "get-first-rec-attr" 'method) ((self #.(swig-lispify "ndb-operation" 'classname)))
  (#.(swig-lispify "NdbOperation_getFirstRecAttr" 'function) (ff-pointer self)))

(cl:defmethod #.(swig-lispify "get-custom-data" 'method) ((self #.(swig-lispify "ndb-operation" 'classname)))
  (#.(swig-lispify "NdbOperation_getCustomData" 'function) (ff-pointer self)))

(cl:defmethod #.(swig-lispify "set-custom-data" 'method) ((self #.(swig-lispify "ndb-operation" 'classname)) p)
  (#.(swig-lispify "NdbOperation_setCustomData" 'function) (ff-pointer self) p))


(cl:defclass #.(swig-lispify "ndb-scan-operation" 'classname)(#.(swig-lispify "ndb-operation" 'classname))
  ((ff-pointer :reader ff-pointer)))

(cl:defmethod #.(swig-lispify "read-tuples" 'method) ((self #.(swig-lispify "ndb-scan-operation" 'classname)) (lock_mode cl:integer) (scan_flags cl:integer) (parallel cl:integer) (batch cl:integer))
  (#.(swig-lispify "NdbScanOperation_readTuples" 'function) (ff-pointer self) lock_mode scan_flags parallel batch))

(cl:defmethod #.(swig-lispify "read-tuples" 'method) ((self #.(swig-lispify "ndb-scan-operation" 'classname)) (lock_mode cl:integer) (scan_flags cl:integer) (parallel cl:integer))
  (#.(swig-lispify "NdbScanOperation_readTuples" 'function) (ff-pointer self) lock_mode scan_flags parallel))

(cl:defmethod #.(swig-lispify "read-tuples" 'method) ((self #.(swig-lispify "ndb-scan-operation" 'classname)) (lock_mode cl:integer) (scan_flags cl:integer))
  (#.(swig-lispify "NdbScanOperation_readTuples" 'function) (ff-pointer self) lock_mode scan_flags))

(cl:defmethod #.(swig-lispify "read-tuples" 'method) ((self #.(swig-lispify "ndb-scan-operation" 'classname)) (lock_mode cl:integer))
  (#.(swig-lispify "NdbScanOperation_readTuples" 'function) (ff-pointer self) lock_mode))

(cl:defmethod #.(swig-lispify "read-tuples" 'method) ((self #.(swig-lispify "ndb-scan-operation" 'classname)))
  (#.(swig-lispify "NdbScanOperation_readTuples" 'function) (ff-pointer self)))

(cl:defmethod #.(swig-lispify "read-tuples" 'method) ((self #.(swig-lispify "ndb-scan-operation" 'classname)) (parallell cl:integer))
  (#.(swig-lispify "NdbScanOperation_readTuples" 'function) (ff-pointer self) parallell))

(cl:defmethod #.(swig-lispify "read-tuples-exclusive" 'method) ((self #.(swig-lispify "ndb-scan-operation" 'classname)) (parallell cl:integer))
  (#.(swig-lispify "NdbScanOperation_readTuplesExclusive" 'function) (ff-pointer self) parallell))

(cl:defmethod #.(swig-lispify "read-tuples-exclusive" 'method) ((self #.(swig-lispify "ndb-scan-operation" 'classname)))
  (#.(swig-lispify "NdbScanOperation_readTuplesExclusive" 'function) (ff-pointer self)))

(cl:defmethod #.(swig-lispify "get-blob-handle" 'method) ((self #.(swig-lispify "ndb-scan-operation" 'classname)) (anAttrName cl:string))
  (#.(swig-lispify "NdbScanOperation_getBlobHandle" 'function) (ff-pointer self) anAttrName))

(cl:defmethod #.(swig-lispify "get-blob-handle" 'method) ((self #.(swig-lispify "ndb-scan-operation" 'classname)) (anAttrId cl:integer))
  (#.(swig-lispify "NdbScanOperation_getBlobHandle" 'function) (ff-pointer self) anAttrId))

(cl:defmethod #.(swig-lispify "set-interpreted-code" 'method) ((self #.(swig-lispify "ndb-scan-operation" 'classname)) code)
  (#.(swig-lispify "NdbScanOperation_setInterpretedCode" 'function) (ff-pointer self) code))

(cl:defmethod #.(swig-lispify "next-result" 'method) ((self #.(swig-lispify "ndb-scan-operation" 'classname)) (fetchAllowed cl:t) (forceSend cl:t))
  (#.(swig-lispify "NdbScanOperation_nextResult" 'function) (ff-pointer self) fetchAllowed forceSend))

(cl:defmethod #.(swig-lispify "next-result" 'method) ((self #.(swig-lispify "ndb-scan-operation" 'classname)) (fetchAllowed cl:t))
  (#.(swig-lispify "NdbScanOperation_nextResult" 'function) (ff-pointer self) fetchAllowed))

(cl:defmethod #.(swig-lispify "next-result" 'method) ((self #.(swig-lispify "ndb-scan-operation" 'classname)))
  (#.(swig-lispify "NdbScanOperation_nextResult" 'function) (ff-pointer self)))

(cl:defmethod #.(swig-lispify "next-result" 'method) ((self #.(swig-lispify "ndb-scan-operation" 'classname)) out_row_ptr (fetchAllowed cl:t) (forceSend cl:t))
  (#.(swig-lispify "NdbScanOperation_nextResult" 'function) (ff-pointer self) out_row_ptr fetchAllowed forceSend))

(cl:defmethod #.(swig-lispify "next-result-copy-out" 'method) ((self #.(swig-lispify "ndb-scan-operation" 'classname)) (buffer cl:string) (fetchAllowed cl:t) (forceSend cl:t))
  (#.(swig-lispify "NdbScanOperation_nextResultCopyOut" 'function) (ff-pointer self) buffer fetchAllowed forceSend))

(cl:defmethod #.(swig-lispify "close" 'method) ((self #.(swig-lispify "ndb-scan-operation" 'classname)) (forceSend cl:t) (releaseOp cl:t))
  (#.(swig-lispify "NdbScanOperation_close" 'function) (ff-pointer self) forceSend releaseOp))

(cl:defmethod #.(swig-lispify "close" 'method) ((self #.(swig-lispify "ndb-scan-operation" 'classname)) (forceSend cl:t))
  (#.(swig-lispify "NdbScanOperation_close" 'function) (ff-pointer self) forceSend))

(cl:defmethod #.(swig-lispify "close" 'method) ((self #.(swig-lispify "ndb-scan-operation" 'classname)))
  (#.(swig-lispify "NdbScanOperation_close" 'function) (ff-pointer self)))

(cl:defmethod #.(swig-lispify "lock-current-tuple" 'method) ((self #.(swig-lispify "ndb-scan-operation" 'classname)))
  (#.(swig-lispify "NdbScanOperation_lockCurrentTuple" 'function) (ff-pointer self)))

(cl:defmethod #.(swig-lispify "lock-current-tuple" 'method) ((self #.(swig-lispify "ndb-scan-operation" 'classname)) lockTrans)
  (#.(swig-lispify "NdbScanOperation_lockCurrentTuple" 'function) (ff-pointer self) lockTrans))

(cl:defmethod #.(swig-lispify "update-current-tuple" 'method) ((self #.(swig-lispify "ndb-scan-operation" 'classname)))
  (#.(swig-lispify "NdbScanOperation_updateCurrentTuple" 'function) (ff-pointer self)))

(cl:defmethod #.(swig-lispify "update-current-tuple" 'method) ((self #.(swig-lispify "ndb-scan-operation" 'classname)) updateTrans)
  (#.(swig-lispify "NdbScanOperation_updateCurrentTuple" 'function) (ff-pointer self) updateTrans))

(cl:defmethod #.(swig-lispify "delete-current-tuple" 'method) ((self #.(swig-lispify "ndb-scan-operation" 'classname)))
  (#.(swig-lispify "NdbScanOperation_deleteCurrentTuple" 'function) (ff-pointer self)))

(cl:defmethod #.(swig-lispify "delete-current-tuple" 'method) ((self #.(swig-lispify "ndb-scan-operation" 'classname)) takeOverTransaction)
  (#.(swig-lispify "NdbScanOperation_deleteCurrentTuple" 'function) (ff-pointer self) takeOverTransaction))

(cl:defmethod #.(swig-lispify "lock-current-tuple" 'method) ((self #.(swig-lispify "ndb-scan-operation" 'classname)) takeOverTrans result_rec (result_row cl:string) result_mask opts (sizeOfOptions cl:integer))
  (#.(swig-lispify "NdbScanOperation_lockCurrentTuple" 'function) (ff-pointer self) takeOverTrans result_rec result_row result_mask opts sizeOfOptions))

(cl:defmethod #.(swig-lispify "lock-current-tuple" 'method) ((self #.(swig-lispify "ndb-scan-operation" 'classname)) takeOverTrans result_rec (result_row cl:string) result_mask opts)
  (#.(swig-lispify "NdbScanOperation_lockCurrentTuple" 'function) (ff-pointer self) takeOverTrans result_rec result_row result_mask opts))

(cl:defmethod #.(swig-lispify "lock-current-tuple" 'method) ((self #.(swig-lispify "ndb-scan-operation" 'classname)) takeOverTrans result_rec (result_row cl:string) result_mask)
  (#.(swig-lispify "NdbScanOperation_lockCurrentTuple" 'function) (ff-pointer self) takeOverTrans result_rec result_row result_mask))

(cl:defmethod #.(swig-lispify "lock-current-tuple" 'method) ((self #.(swig-lispify "ndb-scan-operation" 'classname)) takeOverTrans result_rec (result_row cl:string))
  (#.(swig-lispify "NdbScanOperation_lockCurrentTuple" 'function) (ff-pointer self) takeOverTrans result_rec result_row))

(cl:defmethod #.(swig-lispify "lock-current-tuple" 'method) ((self #.(swig-lispify "ndb-scan-operation" 'classname)) takeOverTrans result_rec)
  (#.(swig-lispify "NdbScanOperation_lockCurrentTuple" 'function) (ff-pointer self) takeOverTrans result_rec))

(cl:defmethod #.(swig-lispify "update-current-tuple" 'method) ((self #.(swig-lispify "ndb-scan-operation" 'classname)) takeOverTrans attr_rec (attr_row cl:string) mask opts (sizeOfOptions cl:integer))
  (#.(swig-lispify "NdbScanOperation_updateCurrentTuple" 'function) (ff-pointer self) takeOverTrans attr_rec attr_row mask opts sizeOfOptions))

(cl:defmethod #.(swig-lispify "update-current-tuple" 'method) ((self #.(swig-lispify "ndb-scan-operation" 'classname)) takeOverTrans attr_rec (attr_row cl:string) mask opts)
  (#.(swig-lispify "NdbScanOperation_updateCurrentTuple" 'function) (ff-pointer self) takeOverTrans attr_rec attr_row mask opts))

(cl:defmethod #.(swig-lispify "update-current-tuple" 'method) ((self #.(swig-lispify "ndb-scan-operation" 'classname)) takeOverTrans attr_rec (attr_row cl:string) mask)
  (#.(swig-lispify "NdbScanOperation_updateCurrentTuple" 'function) (ff-pointer self) takeOverTrans attr_rec attr_row mask))

(cl:defmethod #.(swig-lispify "update-current-tuple" 'method) ((self #.(swig-lispify "ndb-scan-operation" 'classname)) takeOverTrans attr_rec (attr_row cl:string))
  (#.(swig-lispify "NdbScanOperation_updateCurrentTuple" 'function) (ff-pointer self) takeOverTrans attr_rec attr_row))

(cl:defmethod #.(swig-lispify "delete-current-tuple" 'method) ((self #.(swig-lispify "ndb-scan-operation" 'classname)) takeOverTrans result_rec (result_row cl:string) result_mask opts (sizeOfOptions cl:integer))
  (#.(swig-lispify "NdbScanOperation_deleteCurrentTuple" 'function) (ff-pointer self) takeOverTrans result_rec result_row result_mask opts sizeOfOptions))

(cl:defmethod #.(swig-lispify "delete-current-tuple" 'method) ((self #.(swig-lispify "ndb-scan-operation" 'classname)) takeOverTrans result_rec (result_row cl:string) result_mask opts)
  (#.(swig-lispify "NdbScanOperation_deleteCurrentTuple" 'function) (ff-pointer self) takeOverTrans result_rec result_row result_mask opts))

(cl:defmethod #.(swig-lispify "delete-current-tuple" 'method) ((self #.(swig-lispify "ndb-scan-operation" 'classname)) takeOverTrans result_rec (result_row cl:string) result_mask)
  (#.(swig-lispify "NdbScanOperation_deleteCurrentTuple" 'function) (ff-pointer self) takeOverTrans result_rec result_row result_mask))

(cl:defmethod #.(swig-lispify "delete-current-tuple" 'method) ((self #.(swig-lispify "ndb-scan-operation" 'classname)) takeOverTrans result_rec (result_row cl:string))
  (#.(swig-lispify "NdbScanOperation_deleteCurrentTuple" 'function) (ff-pointer self) takeOverTrans result_rec result_row))

(cl:defmethod #.(swig-lispify "delete-current-tuple" 'method) ((self #.(swig-lispify "ndb-scan-operation" 'classname)) takeOverTrans result_rec)
  (#.(swig-lispify "NdbScanOperation_deleteCurrentTuple" 'function) (ff-pointer self) takeOverTrans result_rec))

(cl:defmethod #.(swig-lispify "get-ndb-transaction" 'method) ((self #.(swig-lispify "ndb-scan-operation" 'classname)))
  (#.(swig-lispify "NdbScanOperation_getNdbTransaction" 'function) (ff-pointer self)))

(cl:defmethod #.(swig-lispify "get-pruned" 'method) ((self #.(swig-lispify "ndb-scan-operation" 'classname)))
  (#.(swig-lispify "NdbScanOperation_getPruned" 'function) (ff-pointer self)))


(cl:defclass #.(swig-lispify "ndb-index-scan-operation" 'classname)(#.(swig-lispify "ndb-scan-operation" 'classname))
  ((ff-pointer :reader ff-pointer)))

(cl:defmethod #.(swig-lispify "read-tuples" 'method) ((self #.(swig-lispify "ndb-index-scan-operation" 'classname)) (lock_mode cl:integer) (scan_flags cl:integer) (parallel cl:integer) (batch cl:integer))
  (#.(swig-lispify "NdbIndexScanOperation_readTuples" 'function) (ff-pointer self) lock_mode scan_flags parallel batch))

(cl:defmethod #.(swig-lispify "read-tuples" 'method) ((self #.(swig-lispify "ndb-index-scan-operation" 'classname)) (lock_mode cl:integer) (scan_flags cl:integer) (parallel cl:integer))
  (#.(swig-lispify "NdbIndexScanOperation_readTuples" 'function) (ff-pointer self) lock_mode scan_flags parallel))

(cl:defmethod #.(swig-lispify "read-tuples" 'method) ((self #.(swig-lispify "ndb-index-scan-operation" 'classname)) (lock_mode cl:integer) (scan_flags cl:integer))
  (#.(swig-lispify "NdbIndexScanOperation_readTuples" 'function) (ff-pointer self) lock_mode scan_flags))

(cl:defmethod #.(swig-lispify "read-tuples" 'method) ((self #.(swig-lispify "ndb-index-scan-operation" 'classname)) (lock_mode cl:integer))
  (#.(swig-lispify "NdbIndexScanOperation_readTuples" 'function) (ff-pointer self) lock_mode))

(cl:defmethod #.(swig-lispify "read-tuples" 'method) ((self #.(swig-lispify "ndb-index-scan-operation" 'classname)))
  (#.(swig-lispify "NdbIndexScanOperation_readTuples" 'function) (ff-pointer self)))

(cl:defmethod #.(swig-lispify "read-tuples" 'method) ((self #.(swig-lispify "ndb-index-scan-operation" 'classname)) (lock_mode cl:integer) (batch cl:integer) (parallel cl:integer) (order_by cl:t) (order_desc cl:t) (read_range_no cl:t) (keyinfo cl:t) (multi_range cl:t))
  (#.(swig-lispify "NdbIndexScanOperation_readTuples" 'function) (ff-pointer self) lock_mode batch parallel order_by order_desc read_range_no keyinfo multi_range))

(cl:defmethod #.(swig-lispify "read-tuples" 'method) ((self #.(swig-lispify "ndb-index-scan-operation" 'classname)) (lock_mode cl:integer) (batch cl:integer) (parallel cl:integer) (order_by cl:t) (order_desc cl:t) (read_range_no cl:t) (keyinfo cl:t))
  (#.(swig-lispify "NdbIndexScanOperation_readTuples" 'function) (ff-pointer self) lock_mode batch parallel order_by order_desc read_range_no keyinfo))

(cl:defmethod #.(swig-lispify "read-tuples" 'method) ((self #.(swig-lispify "ndb-index-scan-operation" 'classname)) (lock_mode cl:integer) (batch cl:integer) (parallel cl:integer) (order_by cl:t) (order_desc cl:t) (read_range_no cl:t))
  (#.(swig-lispify "NdbIndexScanOperation_readTuples" 'function) (ff-pointer self) lock_mode batch parallel order_by order_desc read_range_no))

(cl:defmethod #.(swig-lispify "read-tuples" 'method) ((self #.(swig-lispify "ndb-index-scan-operation" 'classname)) (lock_mode cl:integer) (batch cl:integer) (parallel cl:integer) (order_by cl:t) (order_desc cl:t))
  (#.(swig-lispify "NdbIndexScanOperation_readTuples" 'function) (ff-pointer self) lock_mode batch parallel order_by order_desc))

(cl:defmethod #.(swig-lispify "read-tuples" 'method) ((self #.(swig-lispify "ndb-index-scan-operation" 'classname)) (lock_mode cl:integer) (batch cl:integer) (parallel cl:integer) (order_by cl:t))
  (#.(swig-lispify "NdbIndexScanOperation_readTuples" 'function) (ff-pointer self) lock_mode batch parallel order_by))

(cl:defmethod #.(swig-lispify "set-bound" 'method) ((self #.(swig-lispify "ndb-index-scan-operation" 'classname)) (attr cl:string) (type cl:integer) value (len cl:integer))
  (#.(swig-lispify "NdbIndexScanOperation_setBound" 'function) (ff-pointer self) attr type value len))

(cl:defmethod #.(swig-lispify "set-bound" 'method) ((self #.(swig-lispify "ndb-index-scan-operation" 'classname)) (attr cl:string) (type cl:integer) value)
  (#.(swig-lispify "NdbIndexScanOperation_setBound" 'function) (ff-pointer self) attr type value))

(cl:defmethod #.(swig-lispify "set-bound" 'method) ((self #.(swig-lispify "ndb-index-scan-operation" 'classname)) (anAttrId cl:integer) (type cl:integer) aValue (len cl:integer))
  (#.(swig-lispify "NdbIndexScanOperation_setBound" 'function) (ff-pointer self) anAttrId type aValue len))

(cl:defmethod #.(swig-lispify "set-bound" 'method) ((self #.(swig-lispify "ndb-index-scan-operation" 'classname)) (anAttrId cl:integer) (type cl:integer) aValue)
  (#.(swig-lispify "NdbIndexScanOperation_setBound" 'function) (ff-pointer self) anAttrId type aValue))

(cl:defmethod #.(swig-lispify "end-of-bound" 'method) ((self #.(swig-lispify "ndb-index-scan-operation" 'classname)) (range_no cl:integer))
  (#.(swig-lispify "NdbIndexScanOperation_end_of_bound" 'function) (ff-pointer self) range_no))

(cl:defmethod #.(swig-lispify "end-of-bound" 'method) ((self #.(swig-lispify "ndb-index-scan-operation" 'classname)))
  (#.(swig-lispify "NdbIndexScanOperation_end_of_bound" 'function) (ff-pointer self)))

(cl:defmethod #.(swig-lispify "get-range-no" 'method) ((self #.(swig-lispify "ndb-index-scan-operation" 'classname)))
  (#.(swig-lispify "NdbIndexScanOperation_get_range_no" 'function) (ff-pointer self)))

(cl:defmethod #.(swig-lispify "set-bound" 'method) ((self #.(swig-lispify "ndb-index-scan-operation" 'classname)) key_record bound partInfo (sizeOfPartInfo cl:integer))
  (#.(swig-lispify "NdbIndexScanOperation_setBound" 'function) (ff-pointer self) key_record bound partInfo sizeOfPartInfo))

(cl:defmethod #.(swig-lispify "set-bound" 'method) ((self #.(swig-lispify "ndb-index-scan-operation" 'classname)) key_record bound partInfo)
  (#.(swig-lispify "NdbIndexScanOperation_setBound" 'function) (ff-pointer self) key_record bound partInfo))

(cl:defmethod #.(swig-lispify "set-bound" 'method) ((self #.(swig-lispify "ndb-index-scan-operation" 'classname)) key_record bound)
  (#.(swig-lispify "NdbIndexScanOperation_setBound" 'function) (ff-pointer self) key_record bound))

(cl:defmethod #.(swig-lispify "get-current-key-size" 'method) ((self #.(swig-lispify "ndb-index-scan-operation" 'classname)))
  (#.(swig-lispify "NdbIndexScanOperation_getCurrentKeySize" 'function) (ff-pointer self)))

(cl:defmethod #.(swig-lispify "get-sorted" 'method) ((self #.(swig-lispify "ndb-index-scan-operation" 'classname)))
  (#.(swig-lispify "NdbIndexScanOperation_getSorted" 'function) (ff-pointer self)))

(cl:defmethod #.(swig-lispify "get-descending" 'method) ((self #.(swig-lispify "ndb-index-scan-operation" 'classname)))
  (#.(swig-lispify "NdbIndexScanOperation_getDescending" 'function) (ff-pointer self)))


(cl:defclass #.(swig-lispify "ndb-transaction" 'classname)()
  ((ff-pointer :reader ff-pointer)))

(cl:defmethod #.(swig-lispify "get-ndb" 'method) ((self #.(swig-lispify "ndb-transaction" 'classname)))
  (#.(swig-lispify "NdbTransaction_getNdb" 'function) (ff-pointer self)))

(cl:defmethod #.(swig-lispify "get-ndb-operation" 'method) ((self #.(swig-lispify "ndb-transaction" 'classname)) (aTableName cl:string))
  (#.(swig-lispify "NdbTransaction_getNdbOperation" 'function) (ff-pointer self) aTableName))

(cl:defmethod #.(swig-lispify "get-ndb-operation" 'method) ((self #.(swig-lispify "ndb-transaction" 'classname)) (aTable #.(swig-lispify "table" 'classname)))
  (#.(swig-lispify "NdbTransaction_getNdbOperation" 'function) (ff-pointer self) aTable))

(cl:defmethod #.(swig-lispify "get-ndb-scan-operation" 'method) ((self #.(swig-lispify "ndb-transaction" 'classname)) (aTableName cl:string))
  (#.(swig-lispify "NdbTransaction_getNdbScanOperation" 'function) (ff-pointer self) aTableName))

(cl:defmethod #.(swig-lispify "get-ndb-scan-operation" 'method) ((self #.(swig-lispify "ndb-transaction" 'classname)) (aTable #.(swig-lispify "table" 'classname)))
  (#.(swig-lispify "NdbTransaction_getNdbScanOperation" 'function) (ff-pointer self) aTable))

(cl:defmethod #.(swig-lispify "get-ndb-index-scan-operation" 'method) ((self #.(swig-lispify "ndb-transaction" 'classname)) (anIndexName cl:string) (aTableName cl:string))
  (#.(swig-lispify "NdbTransaction_getNdbIndexScanOperation" 'function) (ff-pointer self) anIndexName aTableName))

(cl:defmethod #.(swig-lispify "get-ndb-index-scan-operation" 'method) ((self #.(swig-lispify "ndb-transaction" 'classname)) (anIndex #.(swig-lispify "index" 'classname)) (aTable #.(swig-lispify "table" 'classname)))
  (#.(swig-lispify "NdbTransaction_getNdbIndexScanOperation" 'function) (ff-pointer self) anIndex aTable))

(cl:defmethod #.(swig-lispify "get-ndb-index-scan-operation" 'method) ((self #.(swig-lispify "ndb-transaction" 'classname)) (anIndex #.(swig-lispify "index" 'classname)))
  (#.(swig-lispify "NdbTransaction_getNdbIndexScanOperation" 'function) (ff-pointer self) anIndex))

(cl:defmethod #.(swig-lispify "get-ndb-index-operation" 'method) ((self #.(swig-lispify "ndb-transaction" 'classname)) (anIndexName cl:string) (aTableName cl:string))
  (#.(swig-lispify "NdbTransaction_getNdbIndexOperation" 'function) (ff-pointer self) anIndexName aTableName))

(cl:defmethod #.(swig-lispify "get-ndb-index-operation" 'method) ((self #.(swig-lispify "ndb-transaction" 'classname)) (anIndex #.(swig-lispify "index" 'classname)) (aTable #.(swig-lispify "table" 'classname)))
  (#.(swig-lispify "NdbTransaction_getNdbIndexOperation" 'function) (ff-pointer self) anIndex aTable))

(cl:defmethod #.(swig-lispify "get-ndb-index-operation" 'method) ((self #.(swig-lispify "ndb-transaction" 'classname)) (anIndex #.(swig-lispify "index" 'classname)))
  (#.(swig-lispify "NdbTransaction_getNdbIndexOperation" 'function) (ff-pointer self) anIndex))

(cl:defmethod #.(swig-lispify "set-schema-obj-owner-checks" 'method) ((self #.(swig-lispify "ndb-transaction" 'classname)) (runChecks cl:t))
  (#.(swig-lispify "NdbTransaction_setSchemaObjOwnerChecks" 'function) (ff-pointer self) runChecks))

(cl:defmethod #.(swig-lispify "execute" 'method) ((self #.(swig-lispify "ndb-transaction" 'classname)) (execType cl:integer) (arg2 cl:integer) (force cl:integer))
  (#.(swig-lispify "NdbTransaction_execute" 'function) (ff-pointer self) execType arg2 force))

(cl:defmethod #.(swig-lispify "execute" 'method) ((self #.(swig-lispify "ndb-transaction" 'classname)) (execType cl:integer) (arg2 cl:integer))
  (#.(swig-lispify "NdbTransaction_execute" 'function) (ff-pointer self) execType arg2))

(cl:defmethod #.(swig-lispify "execute" 'method) ((self #.(swig-lispify "ndb-transaction" 'classname)) (execType cl:integer))
  (#.(swig-lispify "NdbTransaction_execute" 'function) (ff-pointer self) execType))

(cl:defmethod #.(swig-lispify "execute" 'method) ((self #.(swig-lispify "ndb-transaction" 'classname)) (execType cl:integer) (abortOption cl:integer) (force cl:integer))
  (#.(swig-lispify "NdbTransaction_execute" 'function) (ff-pointer self) execType abortOption force))

(cl:defmethod #.(swig-lispify "execute" 'method) ((self #.(swig-lispify "ndb-transaction" 'classname)) (execType cl:integer) (abortOption cl:integer))
  (#.(swig-lispify "NdbTransaction_execute" 'function) (ff-pointer self) execType abortOption))

(cl:defmethod #.(swig-lispify "execute" 'method) ((self #.(swig-lispify "ndb-transaction" 'classname)) (execType cl:integer))
  (#.(swig-lispify "NdbTransaction_execute" 'function) (ff-pointer self) execType))

(cl:defmethod #.(swig-lispify "execute-asynch-prepare" 'method) ((self #.(swig-lispify "ndb-transaction" 'classname)) (execType cl:integer) callback anyObject (arg4 cl:integer))
  (#.(swig-lispify "NdbTransaction_executeAsynchPrepare" 'function) (ff-pointer self) execType callback anyObject arg4))

(cl:defmethod #.(swig-lispify "execute-asynch-prepare" 'method) ((self #.(swig-lispify "ndb-transaction" 'classname)) (execType cl:integer) callback anyObject)
  (#.(swig-lispify "NdbTransaction_executeAsynchPrepare" 'function) (ff-pointer self) execType callback anyObject))

(cl:defmethod #.(swig-lispify "execute-asynch-prepare" 'method) ((self #.(swig-lispify "ndb-transaction" 'classname)) (execType cl:integer) callback anyObject (ao cl:integer))
  (#.(swig-lispify "NdbTransaction_executeAsynchPrepare" 'function) (ff-pointer self) execType callback anyObject ao))

(cl:defmethod #.(swig-lispify "execute-asynch-prepare" 'method) ((self #.(swig-lispify "ndb-transaction" 'classname)) (execType cl:integer) callback anyObject)
  (#.(swig-lispify "NdbTransaction_executeAsynchPrepare" 'function) (ff-pointer self) execType callback anyObject))

(cl:defmethod #.(swig-lispify "execute-asynch" 'method) ((self #.(swig-lispify "ndb-transaction" 'classname)) (aTypeOfExec cl:integer) aCallback anyObject (arg4 cl:integer) (forceSend cl:integer))
  (#.(swig-lispify "NdbTransaction_executeAsynch" 'function) (ff-pointer self) aTypeOfExec aCallback anyObject arg4 forceSend))

(cl:defmethod #.(swig-lispify "execute-asynch" 'method) ((self #.(swig-lispify "ndb-transaction" 'classname)) (aTypeOfExec cl:integer) aCallback anyObject (arg4 cl:integer))
  (#.(swig-lispify "NdbTransaction_executeAsynch" 'function) (ff-pointer self) aTypeOfExec aCallback anyObject arg4))

(cl:defmethod #.(swig-lispify "execute-asynch" 'method) ((self #.(swig-lispify "ndb-transaction" 'classname)) (aTypeOfExec cl:integer) aCallback anyObject)
  (#.(swig-lispify "NdbTransaction_executeAsynch" 'function) (ff-pointer self) aTypeOfExec aCallback anyObject))

(cl:defmethod #.(swig-lispify "execute-asynch" 'method) ((self #.(swig-lispify "ndb-transaction" 'classname)) (aTypeOfExec cl:integer) aCallback anyObject (abortOption cl:integer) (forceSend cl:integer))
  (#.(swig-lispify "NdbTransaction_executeAsynch" 'function) (ff-pointer self) aTypeOfExec aCallback anyObject abortOption forceSend))

(cl:defmethod #.(swig-lispify "execute-asynch" 'method) ((self #.(swig-lispify "ndb-transaction" 'classname)) (aTypeOfExec cl:integer) aCallback anyObject (abortOption cl:integer))
  (#.(swig-lispify "NdbTransaction_executeAsynch" 'function) (ff-pointer self) aTypeOfExec aCallback anyObject abortOption))

(cl:defmethod #.(swig-lispify "execute-asynch" 'method) ((self #.(swig-lispify "ndb-transaction" 'classname)) (aTypeOfExec cl:integer) aCallback anyObject)
  (#.(swig-lispify "NdbTransaction_executeAsynch" 'function) (ff-pointer self) aTypeOfExec aCallback anyObject))

(cl:defmethod #.(swig-lispify "refresh" 'method) ((self #.(swig-lispify "ndb-transaction" 'classname)))
  (#.(swig-lispify "NdbTransaction_refresh" 'function) (ff-pointer self)))

(cl:defmethod #.(swig-lispify "close" 'method) ((self #.(swig-lispify "ndb-transaction" 'classname)))
  (#.(swig-lispify "NdbTransaction_close" 'function) (ff-pointer self)))

(cl:defmethod #.(swig-lispify "restart" 'method) ((self #.(swig-lispify "ndb-transaction" 'classname)))
  (#.(swig-lispify "NdbTransaction_restart" 'function) (ff-pointer self)))

(cl:defmethod #.(swig-lispify "get-gci" 'method) ((self #.(swig-lispify "ndb-transaction" 'classname)) gciptr)
  (#.(swig-lispify "NdbTransaction_getGCI" 'function) (ff-pointer self) gciptr))

(cl:defmethod #.(swig-lispify "get-gci" 'method) ((self #.(swig-lispify "ndb-transaction" 'classname)))
  (#.(swig-lispify "NdbTransaction_getGCI" 'function) (ff-pointer self)))

(cl:defmethod #.(swig-lispify "get-transaction-id" 'method) ((self #.(swig-lispify "ndb-transaction" 'classname)))
  (#.(swig-lispify "NdbTransaction_getTransactionId" 'function) (ff-pointer self)))

(cl:defmethod #.(swig-lispify "commit-status" 'method) ((self #.(swig-lispify "ndb-transaction" 'classname)))
  (#.(swig-lispify "NdbTransaction_commitStatus" 'function) (ff-pointer self)))

(cl:defmethod #.(swig-lispify "get-ndb-error" 'method) ((self #.(swig-lispify "ndb-transaction" 'classname)))
  (#.(swig-lispify "NdbTransaction_getNdbError" 'function) (ff-pointer self)))

(cl:defmethod #.(swig-lispify "get-ndb-error-operation" 'method) ((self #.(swig-lispify "ndb-transaction" 'classname)))
  (#.(swig-lispify "NdbTransaction_getNdbErrorOperation" 'function) (ff-pointer self)))

(cl:defmethod #.(swig-lispify "get-ndb-error-operation" 'method) ((self #.(swig-lispify "ndb-transaction" 'classname)))
  (#.(swig-lispify "NdbTransaction_getNdbErrorOperation" 'function) (ff-pointer self)))

(cl:defmethod #.(swig-lispify "get-ndb-error-line" 'method) ((self #.(swig-lispify "ndb-transaction" 'classname)))
  (#.(swig-lispify "NdbTransaction_getNdbErrorLine" 'function) (ff-pointer self)))

(cl:defmethod #.(swig-lispify "get-next-completed-operation" 'method) ((self #.(swig-lispify "ndb-transaction" 'classname)) (op #.(swig-lispify "ndb-operation" 'classname)))
  (#.(swig-lispify "NdbTransaction_getNextCompletedOperation" 'function) (ff-pointer self) op))

(cl:defmethod #.(swig-lispify "get-first-defined-operation" 'method) ((self #.(swig-lispify "ndb-transaction" 'classname)))
  (#.(swig-lispify "NdbTransaction_getFirstDefinedOperation" 'function) (ff-pointer self)))

(cl:defmethod #.(swig-lispify "get-last-defined-operation" 'method) ((self #.(swig-lispify "ndb-transaction" 'classname)))
  (#.(swig-lispify "NdbTransaction_getLastDefinedOperation" 'function) (ff-pointer self)))

(cl:defmethod #.(swig-lispify "execute-pending-blob-ops" 'method) ((self #.(swig-lispify "ndb-transaction" 'classname)) (flags cl:integer))
  (#.(swig-lispify "NdbTransaction_executePendingBlobOps" 'function) (ff-pointer self) flags))

(cl:defmethod #.(swig-lispify "execute-pending-blob-ops" 'method) ((self #.(swig-lispify "ndb-transaction" 'classname)))
  (#.(swig-lispify "NdbTransaction_executePendingBlobOps" 'function) (ff-pointer self)))

(cl:defmethod #.(swig-lispify "get-connected-node-id" 'method) ((self #.(swig-lispify "ndb-transaction" 'classname)))
  (#.(swig-lispify "NdbTransaction_getConnectedNodeId" 'function) (ff-pointer self)))

(cl:defmethod #.(swig-lispify "read-tuple" 'method) ((self #.(swig-lispify "ndb-transaction" 'classname)) key_rec (key_row cl:string) result_rec (result_row cl:string) (lock_mode cl:integer) result_mask opts (sizeOfOptions cl:integer))
  (#.(swig-lispify "NdbTransaction_readTuple" 'function) (ff-pointer self) key_rec key_row result_rec result_row lock_mode result_mask opts sizeOfOptions))

(cl:defmethod #.(swig-lispify "read-tuple" 'method) ((self #.(swig-lispify "ndb-transaction" 'classname)) key_rec (key_row cl:string) result_rec (result_row cl:string) (lock_mode cl:integer) result_mask opts)
  (#.(swig-lispify "NdbTransaction_readTuple" 'function) (ff-pointer self) key_rec key_row result_rec result_row lock_mode result_mask opts))

(cl:defmethod #.(swig-lispify "read-tuple" 'method) ((self #.(swig-lispify "ndb-transaction" 'classname)) key_rec (key_row cl:string) result_rec (result_row cl:string) (lock_mode cl:integer) result_mask)
  (#.(swig-lispify "NdbTransaction_readTuple" 'function) (ff-pointer self) key_rec key_row result_rec result_row lock_mode result_mask))

(cl:defmethod #.(swig-lispify "read-tuple" 'method) ((self #.(swig-lispify "ndb-transaction" 'classname)) key_rec (key_row cl:string) result_rec (result_row cl:string) (lock_mode cl:integer))
  (#.(swig-lispify "NdbTransaction_readTuple" 'function) (ff-pointer self) key_rec key_row result_rec result_row lock_mode))

(cl:defmethod #.(swig-lispify "read-tuple" 'method) ((self #.(swig-lispify "ndb-transaction" 'classname)) key_rec (key_row cl:string) result_rec (result_row cl:string))
  (#.(swig-lispify "NdbTransaction_readTuple" 'function) (ff-pointer self) key_rec key_row result_rec result_row))

(cl:defmethod #.(swig-lispify "insert-tuple" 'method) ((self #.(swig-lispify "ndb-transaction" 'classname)) key_rec (key_row cl:string) attr_rec (attr_row cl:string) mask opts (sizeOfOptions cl:integer))
  (#.(swig-lispify "NdbTransaction_insertTuple" 'function) (ff-pointer self) key_rec key_row attr_rec attr_row mask opts sizeOfOptions))

(cl:defmethod #.(swig-lispify "insert-tuple" 'method) ((self #.(swig-lispify "ndb-transaction" 'classname)) key_rec (key_row cl:string) attr_rec (attr_row cl:string) mask opts)
  (#.(swig-lispify "NdbTransaction_insertTuple" 'function) (ff-pointer self) key_rec key_row attr_rec attr_row mask opts))

(cl:defmethod #.(swig-lispify "insert-tuple" 'method) ((self #.(swig-lispify "ndb-transaction" 'classname)) key_rec (key_row cl:string) attr_rec (attr_row cl:string) mask)
  (#.(swig-lispify "NdbTransaction_insertTuple" 'function) (ff-pointer self) key_rec key_row attr_rec attr_row mask))

(cl:defmethod #.(swig-lispify "insert-tuple" 'method) ((self #.(swig-lispify "ndb-transaction" 'classname)) key_rec (key_row cl:string) attr_rec (attr_row cl:string))
  (#.(swig-lispify "NdbTransaction_insertTuple" 'function) (ff-pointer self) key_rec key_row attr_rec attr_row))

(cl:defmethod #.(swig-lispify "insert-tuple" 'method) ((self #.(swig-lispify "ndb-transaction" 'classname)) combined_rec (combined_row cl:string) mask opts (sizeOfOptions cl:integer))
  (#.(swig-lispify "NdbTransaction_insertTuple" 'function) (ff-pointer self) combined_rec combined_row mask opts sizeOfOptions))

(cl:defmethod #.(swig-lispify "insert-tuple" 'method) ((self #.(swig-lispify "ndb-transaction" 'classname)) combined_rec (combined_row cl:string) mask opts)
  (#.(swig-lispify "NdbTransaction_insertTuple" 'function) (ff-pointer self) combined_rec combined_row mask opts))

(cl:defmethod #.(swig-lispify "insert-tuple" 'method) ((self #.(swig-lispify "ndb-transaction" 'classname)) combined_rec (combined_row cl:string) mask)
  (#.(swig-lispify "NdbTransaction_insertTuple" 'function) (ff-pointer self) combined_rec combined_row mask))

(cl:defmethod #.(swig-lispify "insert-tuple" 'method) ((self #.(swig-lispify "ndb-transaction" 'classname)) combined_rec (combined_row cl:string))
  (#.(swig-lispify "NdbTransaction_insertTuple" 'function) (ff-pointer self) combined_rec combined_row))

(cl:defmethod #.(swig-lispify "update-tuple" 'method) ((self #.(swig-lispify "ndb-transaction" 'classname)) key_rec (key_row cl:string) attr_rec (attr_row cl:string) mask opts (sizeOfOptions cl:integer))
  (#.(swig-lispify "NdbTransaction_updateTuple" 'function) (ff-pointer self) key_rec key_row attr_rec attr_row mask opts sizeOfOptions))

(cl:defmethod #.(swig-lispify "update-tuple" 'method) ((self #.(swig-lispify "ndb-transaction" 'classname)) key_rec (key_row cl:string) attr_rec (attr_row cl:string) mask opts)
  (#.(swig-lispify "NdbTransaction_updateTuple" 'function) (ff-pointer self) key_rec key_row attr_rec attr_row mask opts))

(cl:defmethod #.(swig-lispify "update-tuple" 'method) ((self #.(swig-lispify "ndb-transaction" 'classname)) key_rec (key_row cl:string) attr_rec (attr_row cl:string) mask)
  (#.(swig-lispify "NdbTransaction_updateTuple" 'function) (ff-pointer self) key_rec key_row attr_rec attr_row mask))

(cl:defmethod #.(swig-lispify "update-tuple" 'method) ((self #.(swig-lispify "ndb-transaction" 'classname)) key_rec (key_row cl:string) attr_rec (attr_row cl:string))
  (#.(swig-lispify "NdbTransaction_updateTuple" 'function) (ff-pointer self) key_rec key_row attr_rec attr_row))

(cl:defmethod #.(swig-lispify "write-tuple" 'method) ((self #.(swig-lispify "ndb-transaction" 'classname)) key_rec (key_row cl:string) attr_rec (attr_row cl:string) mask opts (sizeOfOptions cl:integer))
  (#.(swig-lispify "NdbTransaction_writeTuple" 'function) (ff-pointer self) key_rec key_row attr_rec attr_row mask opts sizeOfOptions))

(cl:defmethod #.(swig-lispify "write-tuple" 'method) ((self #.(swig-lispify "ndb-transaction" 'classname)) key_rec (key_row cl:string) attr_rec (attr_row cl:string) mask opts)
  (#.(swig-lispify "NdbTransaction_writeTuple" 'function) (ff-pointer self) key_rec key_row attr_rec attr_row mask opts))

(cl:defmethod #.(swig-lispify "write-tuple" 'method) ((self #.(swig-lispify "ndb-transaction" 'classname)) key_rec (key_row cl:string) attr_rec (attr_row cl:string) mask)
  (#.(swig-lispify "NdbTransaction_writeTuple" 'function) (ff-pointer self) key_rec key_row attr_rec attr_row mask))

(cl:defmethod #.(swig-lispify "write-tuple" 'method) ((self #.(swig-lispify "ndb-transaction" 'classname)) key_rec (key_row cl:string) attr_rec (attr_row cl:string))
  (#.(swig-lispify "NdbTransaction_writeTuple" 'function) (ff-pointer self) key_rec key_row attr_rec attr_row))

(cl:defmethod #.(swig-lispify "delete-tuple" 'method) ((self #.(swig-lispify "ndb-transaction" 'classname)) key_rec (key_row cl:string) result_rec (result_row cl:string) result_mask opts (sizeOfOptions cl:integer))
  (#.(swig-lispify "NdbTransaction_deleteTuple" 'function) (ff-pointer self) key_rec key_row result_rec result_row result_mask opts sizeOfOptions))

(cl:defmethod #.(swig-lispify "delete-tuple" 'method) ((self #.(swig-lispify "ndb-transaction" 'classname)) key_rec (key_row cl:string) result_rec (result_row cl:string) result_mask opts)
  (#.(swig-lispify "NdbTransaction_deleteTuple" 'function) (ff-pointer self) key_rec key_row result_rec result_row result_mask opts))

(cl:defmethod #.(swig-lispify "delete-tuple" 'method) ((self #.(swig-lispify "ndb-transaction" 'classname)) key_rec (key_row cl:string) result_rec (result_row cl:string) result_mask)
  (#.(swig-lispify "NdbTransaction_deleteTuple" 'function) (ff-pointer self) key_rec key_row result_rec result_row result_mask))

(cl:defmethod #.(swig-lispify "delete-tuple" 'method) ((self #.(swig-lispify "ndb-transaction" 'classname)) key_rec (key_row cl:string) result_rec (result_row cl:string))
  (#.(swig-lispify "NdbTransaction_deleteTuple" 'function) (ff-pointer self) key_rec key_row result_rec result_row))

(cl:defmethod #.(swig-lispify "delete-tuple" 'method) ((self #.(swig-lispify "ndb-transaction" 'classname)) key_rec (key_row cl:string) result_rec)
  (#.(swig-lispify "NdbTransaction_deleteTuple" 'function) (ff-pointer self) key_rec key_row result_rec))

(cl:defmethod #.(swig-lispify "refresh-tuple" 'method) ((self #.(swig-lispify "ndb-transaction" 'classname)) key_rec (key_row cl:string) opts (sizeOfOptions cl:integer))
  (#.(swig-lispify "NdbTransaction_refreshTuple" 'function) (ff-pointer self) key_rec key_row opts sizeOfOptions))

(cl:defmethod #.(swig-lispify "refresh-tuple" 'method) ((self #.(swig-lispify "ndb-transaction" 'classname)) key_rec (key_row cl:string) opts)
  (#.(swig-lispify "NdbTransaction_refreshTuple" 'function) (ff-pointer self) key_rec key_row opts))

(cl:defmethod #.(swig-lispify "refresh-tuple" 'method) ((self #.(swig-lispify "ndb-transaction" 'classname)) key_rec (key_row cl:string))
  (#.(swig-lispify "NdbTransaction_refreshTuple" 'function) (ff-pointer self) key_rec key_row))

(cl:defmethod #.(swig-lispify "scan-table" 'method) ((self #.(swig-lispify "ndb-transaction" 'classname)) result_record (lock_mode cl:integer) result_mask options (sizeOfOptions cl:integer))
  (#.(swig-lispify "NdbTransaction_scanTable" 'function) (ff-pointer self) result_record lock_mode result_mask options sizeOfOptions))

(cl:defmethod #.(swig-lispify "scan-table" 'method) ((self #.(swig-lispify "ndb-transaction" 'classname)) result_record (lock_mode cl:integer) result_mask options)
  (#.(swig-lispify "NdbTransaction_scanTable" 'function) (ff-pointer self) result_record lock_mode result_mask options))

(cl:defmethod #.(swig-lispify "scan-table" 'method) ((self #.(swig-lispify "ndb-transaction" 'classname)) result_record (lock_mode cl:integer) result_mask)
  (#.(swig-lispify "NdbTransaction_scanTable" 'function) (ff-pointer self) result_record lock_mode result_mask))

(cl:defmethod #.(swig-lispify "scan-table" 'method) ((self #.(swig-lispify "ndb-transaction" 'classname)) result_record (lock_mode cl:integer))
  (#.(swig-lispify "NdbTransaction_scanTable" 'function) (ff-pointer self) result_record lock_mode))

(cl:defmethod #.(swig-lispify "scan-table" 'method) ((self #.(swig-lispify "ndb-transaction" 'classname)) result_record)
  (#.(swig-lispify "NdbTransaction_scanTable" 'function) (ff-pointer self) result_record))

(cl:defmethod #.(swig-lispify "scan-index" 'method) ((self #.(swig-lispify "ndb-transaction" 'classname)) key_record result_record (lock_mode cl:integer) result_mask bound options (sizeOfOptions cl:integer))
  (#.(swig-lispify "NdbTransaction_scanIndex" 'function) (ff-pointer self) key_record result_record lock_mode result_mask bound options sizeOfOptions))

(cl:defmethod #.(swig-lispify "scan-index" 'method) ((self #.(swig-lispify "ndb-transaction" 'classname)) key_record result_record (lock_mode cl:integer) result_mask bound options)
  (#.(swig-lispify "NdbTransaction_scanIndex" 'function) (ff-pointer self) key_record result_record lock_mode result_mask bound options))

(cl:defmethod #.(swig-lispify "scan-index" 'method) ((self #.(swig-lispify "ndb-transaction" 'classname)) key_record result_record (lock_mode cl:integer) result_mask bound)
  (#.(swig-lispify "NdbTransaction_scanIndex" 'function) (ff-pointer self) key_record result_record lock_mode result_mask bound))

(cl:defmethod #.(swig-lispify "scan-index" 'method) ((self #.(swig-lispify "ndb-transaction" 'classname)) key_record result_record (lock_mode cl:integer) result_mask)
  (#.(swig-lispify "NdbTransaction_scanIndex" 'function) (ff-pointer self) key_record result_record lock_mode result_mask))

(cl:defmethod #.(swig-lispify "scan-index" 'method) ((self #.(swig-lispify "ndb-transaction" 'classname)) key_record result_record (lock_mode cl:integer))
  (#.(swig-lispify "NdbTransaction_scanIndex" 'function) (ff-pointer self) key_record result_record lock_mode))

(cl:defmethod #.(swig-lispify "scan-index" 'method) ((self #.(swig-lispify "ndb-transaction" 'classname)) key_record result_record)
  (#.(swig-lispify "NdbTransaction_scanIndex" 'function) (ff-pointer self) key_record result_record))

(cl:defmethod #.(swig-lispify "create-query" 'method) ((self #.(swig-lispify "ndb-transaction" 'classname)) query paramValue (lock_mode cl:integer))
  (#.(swig-lispify "NdbTransaction_createQuery" 'function) (ff-pointer self) query paramValue lock_mode))

(cl:defmethod #.(swig-lispify "create-query" 'method) ((self #.(swig-lispify "ndb-transaction" 'classname)) query paramValue)
  (#.(swig-lispify "NdbTransaction_createQuery" 'function) (ff-pointer self) query paramValue))

(cl:defmethod #.(swig-lispify "create-query" 'method) ((self #.(swig-lispify "ndb-transaction" 'classname)) query)
  (#.(swig-lispify "NdbTransaction_createQuery" 'function) (ff-pointer self) query))

(cl:defmethod #.(swig-lispify "unlock" 'method) ((self #.(swig-lispify "ndb-transaction" 'classname)) lockHandle (ao cl:integer))
  (#.(swig-lispify "NdbTransaction_unlock" 'function) (ff-pointer self) lockHandle ao))

(cl:defmethod #.(swig-lispify "unlock" 'method) ((self #.(swig-lispify "ndb-transaction" 'classname)) lockHandle)
  (#.(swig-lispify "NdbTransaction_unlock" 'function) (ff-pointer self) lockHandle))

(cl:defmethod #.(swig-lispify "release-lock-handle" 'method) ((self #.(swig-lispify "ndb-transaction" 'classname)) lockHandle)
  (#.(swig-lispify "NdbTransaction_releaseLockHandle" 'function) (ff-pointer self) lockHandle))

(cl:defmethod #.(swig-lispify "get-max-pending-blob-read-bytes" 'method) ((self #.(swig-lispify "ndb-transaction" 'classname)))
  (#.(swig-lispify "NdbTransaction_getMaxPendingBlobReadBytes" 'function) (ff-pointer self)))

(cl:defmethod #.(swig-lispify "get-max-pending-blob-write-bytes" 'method) ((self #.(swig-lispify "ndb-transaction" 'classname)))
  (#.(swig-lispify "NdbTransaction_getMaxPendingBlobWriteBytes" 'function) (ff-pointer self)))

(cl:defmethod #.(swig-lispify "set-max-pending-blob-read-bytes" 'method) ((self #.(swig-lispify "ndb-transaction" 'classname)) (bytes cl:integer))
  (#.(swig-lispify "NdbTransaction_setMaxPendingBlobReadBytes" 'function) (ff-pointer self) bytes))

(cl:defmethod #.(swig-lispify "set-max-pending-blob-write-bytes" 'method) ((self #.(swig-lispify "ndb-transaction" 'classname)) (bytes cl:integer))
  (#.(swig-lispify "NdbTransaction_setMaxPendingBlobWriteBytes" 'function) (ff-pointer self) bytes))


(cl:defclass #.(swig-lispify "ndb-index-operation" 'classname)(#.(swig-lispify "ndb-operation" 'classname))
  ((ff-pointer :reader ff-pointer)))

(cl:defmethod #.(swig-lispify "insert-tuple" 'method) ((self #.(swig-lispify "ndb-index-operation" 'classname)))
  (#.(swig-lispify "NdbIndexOperation_insertTuple" 'function) (ff-pointer self)))

(cl:defmethod #.(swig-lispify "read-tuple" 'method) ((self #.(swig-lispify "ndb-index-operation" 'classname)) (arg1 cl:integer))
  (#.(swig-lispify "NdbIndexOperation_readTuple" 'function) (ff-pointer self) arg1))

(cl:defmethod #.(swig-lispify "read-tuple" 'method) ((self #.(swig-lispify "ndb-index-operation" 'classname)))
  (#.(swig-lispify "NdbIndexOperation_readTuple" 'function) (ff-pointer self)))

(cl:defmethod #.(swig-lispify "read-tuple-exclusive" 'method) ((self #.(swig-lispify "ndb-index-operation" 'classname)))
  (#.(swig-lispify "NdbIndexOperation_readTupleExclusive" 'function) (ff-pointer self)))

(cl:defmethod #.(swig-lispify "simple-read" 'method) ((self #.(swig-lispify "ndb-index-operation" 'classname)))
  (#.(swig-lispify "NdbIndexOperation_simpleRead" 'function) (ff-pointer self)))

(cl:defmethod #.(swig-lispify "dirty-read" 'method) ((self #.(swig-lispify "ndb-index-operation" 'classname)))
  (#.(swig-lispify "NdbIndexOperation_dirtyRead" 'function) (ff-pointer self)))

(cl:defmethod #.(swig-lispify "committed-read" 'method) ((self #.(swig-lispify "ndb-index-operation" 'classname)))
  (#.(swig-lispify "NdbIndexOperation_committedRead" 'function) (ff-pointer self)))

(cl:defmethod #.(swig-lispify "update-tuple" 'method) ((self #.(swig-lispify "ndb-index-operation" 'classname)))
  (#.(swig-lispify "NdbIndexOperation_updateTuple" 'function) (ff-pointer self)))

(cl:defmethod #.(swig-lispify "delete-tuple" 'method) ((self #.(swig-lispify "ndb-index-operation" 'classname)))
  (#.(swig-lispify "NdbIndexOperation_deleteTuple" 'function) (ff-pointer self)))

(cl:defmethod #.(swig-lispify "get-index" 'method) ((self #.(swig-lispify "ndb-index-operation" 'classname)))
  (#.(swig-lispify "NdbIndexOperation_getIndex" 'function) (ff-pointer self)))

(cl:defmethod #.(swig-lispify "dirty-update" 'method) ((self #.(swig-lispify "ndb-index-operation" 'classname)))
  (#.(swig-lispify "NdbIndexOperation_dirtyUpdate" 'function) (ff-pointer self)))

(cl:defmethod #.(swig-lispify "interpreted-update-tuple" 'method) ((self #.(swig-lispify "ndb-index-operation" 'classname)))
  (#.(swig-lispify "NdbIndexOperation_interpretedUpdateTuple" 'function) (ff-pointer self)))

(cl:defmethod #.(swig-lispify "interpreted-delete-tuple" 'method) ((self #.(swig-lispify "ndb-index-operation" 'classname)))
  (#.(swig-lispify "NdbIndexOperation_interpretedDeleteTuple" 'function) (ff-pointer self)))


(cl:defclass #.(swig-lispify "ndb-index-stat" 'classname)()
  ((ff-pointer :reader ff-pointer)))

(cl:defmethod initialize-instance :after ((obj #.(swig-lispify "ndb-index-stat" 'class)) &key)
  (setf (slot-value obj 'ff-pointer) (#.(swig-lispify "new_NdbIndexStat" 'function))))

(cl:defmethod #.(swig-lispify "get-ndb-error" 'method) ((self #.(swig-lispify "ndb-index-stat" 'classname)))
  (#.(swig-lispify "NdbIndexStat_getNdbError" 'function) (ff-pointer self)))

(cl:defmethod #.(swig-lispify "records-in-range" 'method) ((self #.(swig-lispify "ndb-index-stat" 'classname)) (index #.(swig-lispify "index" 'classname)) (trans #.(swig-lispify "ndb-transaction" 'classname)) key_record result_record ib table_rows count (flags cl:integer))
  (#.(swig-lispify "NdbIndexStat_records_in_range" 'function) (ff-pointer self) index trans key_record result_record ib table_rows count flags))

(cl:defmethod #.(swig-lispify "create-systables" 'method) ((self #.(swig-lispify "ndb-index-stat" 'classname)) (ndb #.(swig-lispify "ndb" 'classname)))
  (#.(swig-lispify "NdbIndexStat_create_systables" 'function) (ff-pointer self) ndb))

(cl:defmethod #.(swig-lispify "drop-systables" 'method) ((self #.(swig-lispify "ndb-index-stat" 'classname)) (ndb #.(swig-lispify "ndb" 'classname)))
  (#.(swig-lispify "NdbIndexStat_drop_systables" 'function) (ff-pointer self) ndb))

(cl:defmethod #.(swig-lispify "check-systables" 'method) ((self #.(swig-lispify "ndb-index-stat" 'classname)) (ndb #.(swig-lispify "ndb" 'classname)))
  (#.(swig-lispify "NdbIndexStat_check_systables" 'function) (ff-pointer self) ndb))

(cl:defmethod #.(swig-lispify "set-index" 'method) ((self #.(swig-lispify "ndb-index-stat" 'classname)) (index #.(swig-lispify "index" 'classname)) (table #.(swig-lispify "table" 'classname)))
  (#.(swig-lispify "NdbIndexStat_set_index" 'function) (ff-pointer self) index table))

(cl:defmethod #.(swig-lispify "reset-index" 'method) ((self #.(swig-lispify "ndb-index-stat" 'classname)))
  (#.(swig-lispify "NdbIndexStat_reset_index" 'function) (ff-pointer self)))

(cl:defmethod #.(swig-lispify "update-stat" 'method) ((self #.(swig-lispify "ndb-index-stat" 'classname)) (ndb #.(swig-lispify "ndb" 'classname)))
  (#.(swig-lispify "NdbIndexStat_update_stat" 'function) (ff-pointer self) ndb))

(cl:defmethod #.(swig-lispify "delete-stat" 'method) ((self #.(swig-lispify "ndb-index-stat" 'classname)) (ndb #.(swig-lispify "ndb" 'classname)))
  (#.(swig-lispify "NdbIndexStat_delete_stat" 'function) (ff-pointer self) ndb))

(cl:defmethod #.(swig-lispify "move-cache" 'method) ((self #.(swig-lispify "ndb-index-stat" 'classname)))
  (#.(swig-lispify "NdbIndexStat_move_cache" 'function) (ff-pointer self)))

(cl:defmethod #.(swig-lispify "clean-cache" 'method) ((self #.(swig-lispify "ndb-index-stat" 'classname)))
  (#.(swig-lispify "NdbIndexStat_clean_cache" 'function) (ff-pointer self)))

(cl:defmethod #.(swig-lispify "get-cache-info" 'method) ((self #.(swig-lispify "ndb-index-stat" 'classname)) info (type cl:integer))
  (#.(swig-lispify "NdbIndexStat_get_cache_info" 'function) (ff-pointer self) info type))

(cl:defmethod #.(swig-lispify "get-head" 'method) ((self #.(swig-lispify "ndb-index-stat" 'classname)) head)
  (#.(swig-lispify "NdbIndexStat_get_head" 'function) (ff-pointer self) head))

(cl:defmethod #.(swig-lispify "read-head" 'method) ((self #.(swig-lispify "ndb-index-stat" 'classname)) (ndb #.(swig-lispify "ndb" 'classname)))
  (#.(swig-lispify "NdbIndexStat_read_head" 'function) (ff-pointer self) ndb))

(cl:defmethod #.(swig-lispify "read-stat" 'method) ((self #.(swig-lispify "ndb-index-stat" 'classname)) (ndb #.(swig-lispify "ndb" 'classname)))
  (#.(swig-lispify "NdbIndexStat_read_stat" 'function) (ff-pointer self) ndb))

(cl:defmethod #.(swig-lispify "add-bound" 'method) ((self #.(swig-lispify "ndb-index-stat" 'classname)) bound value)
  (#.(swig-lispify "NdbIndexStat_add_bound" 'function) (ff-pointer self) bound value))

(cl:defmethod #.(swig-lispify "add-bound-null" 'method) ((self #.(swig-lispify "ndb-index-stat" 'classname)) bound)
  (#.(swig-lispify "NdbIndexStat_add_bound_null" 'function) (ff-pointer self) bound))

(cl:defmethod #.(swig-lispify "set-bound-strict" 'method) ((self #.(swig-lispify "ndb-index-stat" 'classname)) bound (strict cl:integer))
  (#.(swig-lispify "NdbIndexStat_set_bound_strict" 'function) (ff-pointer self) bound strict))

(cl:defmethod #.(swig-lispify "reset-bound" 'method) ((self #.(swig-lispify "ndb-index-stat" 'classname)) bound)
  (#.(swig-lispify "NdbIndexStat_reset_bound" 'function) (ff-pointer self) bound))

(cl:defmethod #.(swig-lispify "finalize-range" 'method) ((self #.(swig-lispify "ndb-index-stat" 'classname)) range)
  (#.(swig-lispify "NdbIndexStat_finalize_range" 'function) (ff-pointer self) range))

(cl:defmethod #.(swig-lispify "reset-range" 'method) ((self #.(swig-lispify "ndb-index-stat" 'classname)) range)
  (#.(swig-lispify "NdbIndexStat_reset_range" 'function) (ff-pointer self) range))

(cl:defmethod #.(swig-lispify "convert-range" 'method) ((self #.(swig-lispify "ndb-index-stat" 'classname)) range key_record ib)
  (#.(swig-lispify "NdbIndexStat_convert_range" 'function) (ff-pointer self) range key_record ib))

(cl:defmethod #.(swig-lispify "query-stat" 'method) ((self #.(swig-lispify "ndb-index-stat" 'classname)) range stat)
  (#.(swig-lispify "NdbIndexStat_query_stat" 'function) (ff-pointer self) range stat))

(cl:defmethod #.(swig-lispify "create-sysevents" 'method) ((self #.(swig-lispify "ndb-index-stat" 'classname)) (ndb #.(swig-lispify "ndb" 'classname)))
  (#.(swig-lispify "NdbIndexStat_create_sysevents" 'function) (ff-pointer self) ndb))

(cl:defmethod #.(swig-lispify "drop-sysevents" 'method) ((self #.(swig-lispify "ndb-index-stat" 'classname)) (ndb #.(swig-lispify "ndb" 'classname)))
  (#.(swig-lispify "NdbIndexStat_drop_sysevents" 'function) (ff-pointer self) ndb))

(cl:defmethod #.(swig-lispify "check-sysevents" 'method) ((self #.(swig-lispify "ndb-index-stat" 'classname)) (ndb #.(swig-lispify "ndb" 'classname)))
  (#.(swig-lispify "NdbIndexStat_check_sysevents" 'function) (ff-pointer self) ndb))

(cl:defmethod #.(swig-lispify "create-listener" 'method) ((self #.(swig-lispify "ndb-index-stat" 'classname)) (ndb #.(swig-lispify "ndb" 'classname)))
  (#.(swig-lispify "NdbIndexStat_create_listener" 'function) (ff-pointer self) ndb))

(cl:defmethod #.(swig-lispify "has-listener" 'method) ((self #.(swig-lispify "ndb-index-stat" 'classname)))
  (#.(swig-lispify "NdbIndexStat_has_listener" 'function) (ff-pointer self)))

(cl:defmethod #.(swig-lispify "execute-listener" 'method) ((self #.(swig-lispify "ndb-index-stat" 'classname)) (ndb #.(swig-lispify "ndb" 'classname)))
  (#.(swig-lispify "NdbIndexStat_execute_listener" 'function) (ff-pointer self) ndb))

(cl:defmethod #.(swig-lispify "poll-listener" 'method) ((self #.(swig-lispify "ndb-index-stat" 'classname)) (ndb #.(swig-lispify "ndb" 'classname)) (max_wait_ms cl:integer))
  (#.(swig-lispify "NdbIndexStat_poll_listener" 'function) (ff-pointer self) ndb max_wait_ms))

(cl:defmethod #.(swig-lispify "next-listener" 'method) ((self #.(swig-lispify "ndb-index-stat" 'classname)) (ndb #.(swig-lispify "ndb" 'classname)))
  (#.(swig-lispify "NdbIndexStat_next_listener" 'function) (ff-pointer self) ndb))

(cl:defmethod #.(swig-lispify "drop-listener" 'method) ((self #.(swig-lispify "ndb-index-stat" 'classname)) (ndb #.(swig-lispify "ndb" 'classname)))
  (#.(swig-lispify "NdbIndexStat_drop_listener" 'function) (ff-pointer self) ndb))

(cl:defmethod #.(swig-lispify "set-mem-handler" 'method) ((self #.(swig-lispify "ndb-index-stat" 'classname)) mem)
  (#.(swig-lispify "NdbIndexStat_set_mem_handler" 'function) (ff-pointer self) mem))

(cl:defmethod #.(swig-lispify "get-impl" 'method) ((self #.(swig-lispify "ndb-index-stat" 'classname)))
  (#.(swig-lispify "NdbIndexStat_getImpl" 'function) (ff-pointer self)))


(cl:defclass #.(swig-lispify "ndb-interpreted-code" 'classname)()
  ((ff-pointer :reader ff-pointer)))

(cl:defmethod initialize-instance :after ((obj #.(swig-lispify "ndb-interpreted-code" 'class)) &key (table #.(swig-lispify "table" 'classname)) buffer (buffer_word_size cl:integer))
  (setf (slot-value obj 'ff-pointer) (#.(swig-lispify "new_NdbInterpretedCode" 'function) table buffer buffer_word_size)))

(cl:defmethod initialize-instance :after ((obj #.(swig-lispify "ndb-interpreted-code" 'class)) &key (table #.(swig-lispify "table" 'classname)) buffer)
  (setf (slot-value obj 'ff-pointer) (#.(swig-lispify "new_NdbInterpretedCode" 'function) table buffer)))

(cl:defmethod initialize-instance :after ((obj #.(swig-lispify "ndb-interpreted-code" 'class)) &key (table #.(swig-lispify "table" 'classname)))
  (setf (slot-value obj 'ff-pointer) (#.(swig-lispify "new_NdbInterpretedCode" 'function) table)))

(cl:defmethod initialize-instance :after ((obj #.(swig-lispify "ndb-interpreted-code" 'class)) &key)
  (setf (slot-value obj 'ff-pointer) (#.(swig-lispify "new_NdbInterpretedCode" 'function))))

(cl:defmethod initialize-instance :after ((obj #.(swig-lispify "ndb-interpreted-code" 'class)) &key arg0 buffer (buffer_word_size cl:integer))
  (setf (slot-value obj 'ff-pointer) (#.(swig-lispify "new_NdbInterpretedCode" 'function) arg0 buffer buffer_word_size)))

(cl:defmethod initialize-instance :after ((obj #.(swig-lispify "ndb-interpreted-code" 'class)) &key arg0 buffer)
  (setf (slot-value obj 'ff-pointer) (#.(swig-lispify "new_NdbInterpretedCode" 'function) arg0 buffer)))

(cl:defmethod initialize-instance :after ((obj #.(swig-lispify "ndb-interpreted-code" 'class)) &key arg0)
  (setf (slot-value obj 'ff-pointer) (#.(swig-lispify "new_NdbInterpretedCode" 'function) arg0)))

(cl:defmethod #.(swig-lispify "reset" 'method) ((self #.(swig-lispify "ndb-interpreted-code" 'classname)))
  (#.(swig-lispify "NdbInterpretedCode_reset" 'function) (ff-pointer self)))

(cl:defmethod #.(swig-lispify "load-const-null" 'method) ((self #.(swig-lispify "ndb-interpreted-code" 'classname)) (RegDest cl:integer))
  (#.(swig-lispify "NdbInterpretedCode_load_const_null" 'function) (ff-pointer self) RegDest))

(cl:defmethod #.(swig-lispify "load-const-u16" 'method) ((self #.(swig-lispify "ndb-interpreted-code" 'classname)) (RegDest cl:integer) (Constant cl:integer))
  (#.(swig-lispify "NdbInterpretedCode_load_const_u16" 'function) (ff-pointer self) RegDest Constant))

(cl:defmethod #.(swig-lispify "load-const-u32" 'method) ((self #.(swig-lispify "ndb-interpreted-code" 'classname)) (RegDest cl:integer) (Constant cl:integer))
  (#.(swig-lispify "NdbInterpretedCode_load_const_u32" 'function) (ff-pointer self) RegDest Constant))

(cl:defmethod #.(swig-lispify "load-const-u64" 'method) ((self #.(swig-lispify "ndb-interpreted-code" 'classname)) (RegDest cl:integer) Constant)
  (#.(swig-lispify "NdbInterpretedCode_load_const_u64" 'function) (ff-pointer self) RegDest Constant))

(cl:defmethod #.(swig-lispify "read-attr" 'method) ((self #.(swig-lispify "ndb-interpreted-code" 'classname)) (RegDest cl:integer) (attrId cl:integer))
  (#.(swig-lispify "NdbInterpretedCode_read_attr" 'function) (ff-pointer self) RegDest attrId))

(cl:defmethod #.(swig-lispify "read-attr" 'method) ((self #.(swig-lispify "ndb-interpreted-code" 'classname)) (RegDest cl:integer) (column #.(swig-lispify "column" 'classname)))
  (#.(swig-lispify "NdbInterpretedCode_read_attr" 'function) (ff-pointer self) RegDest column))

(cl:defmethod #.(swig-lispify "write-attr" 'method) ((self #.(swig-lispify "ndb-interpreted-code" 'classname)) (attrId cl:integer) (RegSource cl:integer))
  (#.(swig-lispify "NdbInterpretedCode_write_attr" 'function) (ff-pointer self) attrId RegSource))

(cl:defmethod #.(swig-lispify "write-attr" 'method) ((self #.(swig-lispify "ndb-interpreted-code" 'classname)) (column #.(swig-lispify "column" 'classname)) (RegSource cl:integer))
  (#.(swig-lispify "NdbInterpretedCode_write_attr" 'function) (ff-pointer self) column RegSource))

(cl:defmethod #.(swig-lispify "add-reg" 'method) ((self #.(swig-lispify "ndb-interpreted-code" 'classname)) (RegDest cl:integer) (RegSource1 cl:integer) (RegSource2 cl:integer))
  (#.(swig-lispify "NdbInterpretedCode_add_reg" 'function) (ff-pointer self) RegDest RegSource1 RegSource2))

(cl:defmethod #.(swig-lispify "sub-reg" 'method) ((self #.(swig-lispify "ndb-interpreted-code" 'classname)) (RegDest cl:integer) (RegSource1 cl:integer) (RegSource2 cl:integer))
  (#.(swig-lispify "NdbInterpretedCode_sub_reg" 'function) (ff-pointer self) RegDest RegSource1 RegSource2))

(cl:defmethod #.(swig-lispify "def-label" 'method) ((self #.(swig-lispify "ndb-interpreted-code" 'classname)) (LabelNum cl:integer))
  (#.(swig-lispify "NdbInterpretedCode_def_label" 'function) (ff-pointer self) LabelNum))

(cl:defmethod #.(swig-lispify "branch-label" 'method) ((self #.(swig-lispify "ndb-interpreted-code" 'classname)) (Label cl:integer))
  (#.(swig-lispify "NdbInterpretedCode_branch_label" 'function) (ff-pointer self) Label))

(cl:defmethod #.(swig-lispify "branch-ge" 'method) ((self #.(swig-lispify "ndb-interpreted-code" 'classname)) (RegLvalue cl:integer) (RegRvalue cl:integer) (Label cl:integer))
  (#.(swig-lispify "NdbInterpretedCode_branch_ge" 'function) (ff-pointer self) RegLvalue RegRvalue Label))

(cl:defmethod #.(swig-lispify "branch-gt" 'method) ((self #.(swig-lispify "ndb-interpreted-code" 'classname)) (RegLvalue cl:integer) (RegRvalue cl:integer) (Label cl:integer))
  (#.(swig-lispify "NdbInterpretedCode_branch_gt" 'function) (ff-pointer self) RegLvalue RegRvalue Label))

(cl:defmethod #.(swig-lispify "branch-le" 'method) ((self #.(swig-lispify "ndb-interpreted-code" 'classname)) (RegLvalue cl:integer) (RegRvalue cl:integer) (Label cl:integer))
  (#.(swig-lispify "NdbInterpretedCode_branch_le" 'function) (ff-pointer self) RegLvalue RegRvalue Label))

(cl:defmethod #.(swig-lispify "branch-lt" 'method) ((self #.(swig-lispify "ndb-interpreted-code" 'classname)) (RegLvalue cl:integer) (RegRvalue cl:integer) (Label cl:integer))
  (#.(swig-lispify "NdbInterpretedCode_branch_lt" 'function) (ff-pointer self) RegLvalue RegRvalue Label))

(cl:defmethod #.(swig-lispify "branch-eq" 'method) ((self #.(swig-lispify "ndb-interpreted-code" 'classname)) (RegLvalue cl:integer) (RegRvalue cl:integer) (Label cl:integer))
  (#.(swig-lispify "NdbInterpretedCode_branch_eq" 'function) (ff-pointer self) RegLvalue RegRvalue Label))

(cl:defmethod #.(swig-lispify "branch-ne" 'method) ((self #.(swig-lispify "ndb-interpreted-code" 'classname)) (RegLvalue cl:integer) (RegRvalue cl:integer) (Label cl:integer))
  (#.(swig-lispify "NdbInterpretedCode_branch_ne" 'function) (ff-pointer self) RegLvalue RegRvalue Label))

(cl:defmethod #.(swig-lispify "branch-ne-null" 'method) ((self #.(swig-lispify "ndb-interpreted-code" 'classname)) (RegLvalue cl:integer) (Label cl:integer))
  (#.(swig-lispify "NdbInterpretedCode_branch_ne_null" 'function) (ff-pointer self) RegLvalue Label))

(cl:defmethod #.(swig-lispify "branch-eq-null" 'method) ((self #.(swig-lispify "ndb-interpreted-code" 'classname)) (RegLvalue cl:integer) (Label cl:integer))
  (#.(swig-lispify "NdbInterpretedCode_branch_eq_null" 'function) (ff-pointer self) RegLvalue Label))

(cl:defmethod #.(swig-lispify "branch-col-eq" 'method) ((self #.(swig-lispify "ndb-interpreted-code" 'classname)) val (unused cl:integer) (attrId cl:integer) (Label cl:integer))
  (#.(swig-lispify "NdbInterpretedCode_branch_col_eq" 'function) (ff-pointer self) val unused attrId Label))

(cl:defmethod #.(swig-lispify "branch-col-ne" 'method) ((self #.(swig-lispify "ndb-interpreted-code" 'classname)) val (unused cl:integer) (attrId cl:integer) (Label cl:integer))
  (#.(swig-lispify "NdbInterpretedCode_branch_col_ne" 'function) (ff-pointer self) val unused attrId Label))

(cl:defmethod #.(swig-lispify "branch-col-lt" 'method) ((self #.(swig-lispify "ndb-interpreted-code" 'classname)) val (unused cl:integer) (attrId cl:integer) (Label cl:integer))
  (#.(swig-lispify "NdbInterpretedCode_branch_col_lt" 'function) (ff-pointer self) val unused attrId Label))

(cl:defmethod #.(swig-lispify "branch-col-le" 'method) ((self #.(swig-lispify "ndb-interpreted-code" 'classname)) val (unused cl:integer) (attrId cl:integer) (Label cl:integer))
  (#.(swig-lispify "NdbInterpretedCode_branch_col_le" 'function) (ff-pointer self) val unused attrId Label))

(cl:defmethod #.(swig-lispify "branch-col-gt" 'method) ((self #.(swig-lispify "ndb-interpreted-code" 'classname)) val (unused cl:integer) (attrId cl:integer) (Label cl:integer))
  (#.(swig-lispify "NdbInterpretedCode_branch_col_gt" 'function) (ff-pointer self) val unused attrId Label))

(cl:defmethod #.(swig-lispify "branch-col-ge" 'method) ((self #.(swig-lispify "ndb-interpreted-code" 'classname)) val (unused cl:integer) (attrId cl:integer) (Label cl:integer))
  (#.(swig-lispify "NdbInterpretedCode_branch_col_ge" 'function) (ff-pointer self) val unused attrId Label))

(cl:defmethod #.(swig-lispify "branch-col-eq" 'method) ((self #.(swig-lispify "ndb-interpreted-code" 'classname)) (attrId1 cl:integer) (attrId2 cl:integer) (label cl:integer))
  (#.(swig-lispify "NdbInterpretedCode_branch_col_eq" 'function) (ff-pointer self) attrId1 attrId2 label))

(cl:defmethod #.(swig-lispify "branch-col-ne" 'method) ((self #.(swig-lispify "ndb-interpreted-code" 'classname)) (attrId1 cl:integer) (attrId2 cl:integer) (label cl:integer))
  (#.(swig-lispify "NdbInterpretedCode_branch_col_ne" 'function) (ff-pointer self) attrId1 attrId2 label))

(cl:defmethod #.(swig-lispify "branch-col-lt" 'method) ((self #.(swig-lispify "ndb-interpreted-code" 'classname)) (attrId1 cl:integer) (attrId2 cl:integer) (label cl:integer))
  (#.(swig-lispify "NdbInterpretedCode_branch_col_lt" 'function) (ff-pointer self) attrId1 attrId2 label))

(cl:defmethod #.(swig-lispify "branch-col-le" 'method) ((self #.(swig-lispify "ndb-interpreted-code" 'classname)) (attrId1 cl:integer) (attrId2 cl:integer) (label cl:integer))
  (#.(swig-lispify "NdbInterpretedCode_branch_col_le" 'function) (ff-pointer self) attrId1 attrId2 label))

(cl:defmethod #.(swig-lispify "branch-col-gt" 'method) ((self #.(swig-lispify "ndb-interpreted-code" 'classname)) (attrId1 cl:integer) (attrId2 cl:integer) (label cl:integer))
  (#.(swig-lispify "NdbInterpretedCode_branch_col_gt" 'function) (ff-pointer self) attrId1 attrId2 label))

(cl:defmethod #.(swig-lispify "branch-col-ge" 'method) ((self #.(swig-lispify "ndb-interpreted-code" 'classname)) (attrId1 cl:integer) (attrId2 cl:integer) (label cl:integer))
  (#.(swig-lispify "NdbInterpretedCode_branch_col_ge" 'function) (ff-pointer self) attrId1 attrId2 label))

(cl:defmethod #.(swig-lispify "branch-col-eq-null" 'method) ((self #.(swig-lispify "ndb-interpreted-code" 'classname)) (attrId cl:integer) (Label cl:integer))
  (#.(swig-lispify "NdbInterpretedCode_branch_col_eq_null" 'function) (ff-pointer self) attrId Label))

(cl:defmethod #.(swig-lispify "branch-col-ne-null" 'method) ((self #.(swig-lispify "ndb-interpreted-code" 'classname)) (attrId cl:integer) (Label cl:integer))
  (#.(swig-lispify "NdbInterpretedCode_branch_col_ne_null" 'function) (ff-pointer self) attrId Label))

(cl:defmethod #.(swig-lispify "branch-col-like" 'method) ((self #.(swig-lispify "ndb-interpreted-code" 'classname)) val (len cl:integer) (attrId cl:integer) (Label cl:integer))
  (#.(swig-lispify "NdbInterpretedCode_branch_col_like" 'function) (ff-pointer self) val len attrId Label))

(cl:defmethod #.(swig-lispify "branch-col-notlike" 'method) ((self #.(swig-lispify "ndb-interpreted-code" 'classname)) val (len cl:integer) (attrId cl:integer) (Label cl:integer))
  (#.(swig-lispify "NdbInterpretedCode_branch_col_notlike" 'function) (ff-pointer self) val len attrId Label))

(cl:defmethod #.(swig-lispify "branch-col-and-mask-eq-mask" 'method) ((self #.(swig-lispify "ndb-interpreted-code" 'classname)) mask (unused cl:integer) (attrId cl:integer) (Label cl:integer))
  (#.(swig-lispify "NdbInterpretedCode_branch_col_and_mask_eq_mask" 'function) (ff-pointer self) mask unused attrId Label))

(cl:defmethod #.(swig-lispify "branch-col-and-mask-ne-mask" 'method) ((self #.(swig-lispify "ndb-interpreted-code" 'classname)) mask (unused cl:integer) (attrId cl:integer) (Label cl:integer))
  (#.(swig-lispify "NdbInterpretedCode_branch_col_and_mask_ne_mask" 'function) (ff-pointer self) mask unused attrId Label))

(cl:defmethod #.(swig-lispify "branch-col-and-mask-eq-zero" 'method) ((self #.(swig-lispify "ndb-interpreted-code" 'classname)) mask (unused cl:integer) (attrId cl:integer) (Label cl:integer))
  (#.(swig-lispify "NdbInterpretedCode_branch_col_and_mask_eq_zero" 'function) (ff-pointer self) mask unused attrId Label))

(cl:defmethod #.(swig-lispify "branch-col-and-mask-ne-zero" 'method) ((self #.(swig-lispify "ndb-interpreted-code" 'classname)) mask (unused cl:integer) (attrId cl:integer) (Label cl:integer))
  (#.(swig-lispify "NdbInterpretedCode_branch_col_and_mask_ne_zero" 'function) (ff-pointer self) mask unused attrId Label))

(cl:defmethod #.(swig-lispify "interpret-exit-ok" 'method) ((self #.(swig-lispify "ndb-interpreted-code" 'classname)))
  (#.(swig-lispify "NdbInterpretedCode_interpret_exit_ok" 'function) (ff-pointer self)))

(cl:defmethod #.(swig-lispify "interpret-exit-nok" 'method) ((self #.(swig-lispify "ndb-interpreted-code" 'classname)) (ErrorCode cl:integer))
  (#.(swig-lispify "NdbInterpretedCode_interpret_exit_nok" 'function) (ff-pointer self) ErrorCode))

(cl:defmethod #.(swig-lispify "interpret-exit-nok" 'method) ((self #.(swig-lispify "ndb-interpreted-code" 'classname)))
  (#.(swig-lispify "NdbInterpretedCode_interpret_exit_nok" 'function) (ff-pointer self)))

(cl:defmethod #.(swig-lispify "interpret-exit-last-row" 'method) ((self #.(swig-lispify "ndb-interpreted-code" 'classname)))
  (#.(swig-lispify "NdbInterpretedCode_interpret_exit_last_row" 'function) (ff-pointer self)))

(cl:defmethod #.(swig-lispify "add-val" 'method) ((self #.(swig-lispify "ndb-interpreted-code" 'classname)) (attrId cl:integer) (aValue cl:integer))
  (#.(swig-lispify "NdbInterpretedCode_add_val" 'function) (ff-pointer self) attrId aValue))

(cl:defmethod #.(swig-lispify "add-val" 'method) ((self #.(swig-lispify "ndb-interpreted-code" 'classname)) (attrId cl:integer) aValue)
  (#.(swig-lispify "NdbInterpretedCode_add_val" 'function) (ff-pointer self) attrId aValue))

(cl:defmethod #.(swig-lispify "sub-val" 'method) ((self #.(swig-lispify "ndb-interpreted-code" 'classname)) (attrId cl:integer) (aValue cl:integer))
  (#.(swig-lispify "NdbInterpretedCode_sub_val" 'function) (ff-pointer self) attrId aValue))

(cl:defmethod #.(swig-lispify "sub-val" 'method) ((self #.(swig-lispify "ndb-interpreted-code" 'classname)) (attrId cl:integer) aValue)
  (#.(swig-lispify "NdbInterpretedCode_sub_val" 'function) (ff-pointer self) attrId aValue))

(cl:defmethod #.(swig-lispify "def-sub" 'method) ((self #.(swig-lispify "ndb-interpreted-code" 'classname)) (SubroutineNumber cl:integer))
  (#.(swig-lispify "NdbInterpretedCode_def_sub" 'function) (ff-pointer self) SubroutineNumber))

(cl:defmethod #.(swig-lispify "call-sub" 'method) ((self #.(swig-lispify "ndb-interpreted-code" 'classname)) (SubroutineNumber cl:integer))
  (#.(swig-lispify "NdbInterpretedCode_call_sub" 'function) (ff-pointer self) SubroutineNumber))

(cl:defmethod #.(swig-lispify "ret-sub" 'method) ((self #.(swig-lispify "ndb-interpreted-code" 'classname)))
  (#.(swig-lispify "NdbInterpretedCode_ret_sub" 'function) (ff-pointer self)))

(cl:defmethod #.(swig-lispify "finalise" 'method) ((self #.(swig-lispify "ndb-interpreted-code" 'classname)))
  (#.(swig-lispify "NdbInterpretedCode_finalise" 'function) (ff-pointer self)))

(cl:defmethod #.(swig-lispify "get-table" 'method) ((self #.(swig-lispify "ndb-interpreted-code" 'classname)))
  (#.(swig-lispify "NdbInterpretedCode_getTable" 'function) (ff-pointer self)))

(cl:defmethod #.(swig-lispify "get-ndb-error" 'method) ((self #.(swig-lispify "ndb-interpreted-code" 'classname)))
  (#.(swig-lispify "NdbInterpretedCode_getNdbError" 'function) (ff-pointer self)))

(cl:defmethod #.(swig-lispify "get-words-used" 'method) ((self #.(swig-lispify "ndb-interpreted-code" 'classname)))
  (#.(swig-lispify "NdbInterpretedCode_getWordsUsed" 'function) (ff-pointer self)))

(cl:defmethod #.(swig-lispify "copy" 'method) ((self #.(swig-lispify "ndb-interpreted-code" 'classname)) (src #.(swig-lispify "ndb-interpreted-code" 'classname)))
  (#.(swig-lispify "NdbInterpretedCode_copy" 'function) (ff-pointer self) (ff-pointer src)))


(cl:defclass #.(swig-lispify "ndb-scan-filter" 'classname)()
  ((ff-pointer :reader ff-pointer)))

(cl:defmethod initialize-instance :after ((obj #.(swig-lispify "ndb-scan-filter" 'class)) &key (code #.(swig-lispify "ndb-interpreted-code" 'classname)))
  (setf (slot-value obj 'ff-pointer) (#.(swig-lispify "new_NdbScanFilter" 'function) code)))

(cl:defmethod initialize-instance :after ((obj #.(swig-lispify "ndb-scan-filter" 'class)) &key (op #.(swig-lispify "ndb-operation" 'classname)))
  (setf (slot-value obj 'ff-pointer) (#.(swig-lispify "new_NdbScanFilter" 'function) op)))

(cl:defmethod #.(swig-lispify "begin" 'method) ((self #.(swig-lispify "ndb-scan-filter" 'classname)) (group cl:integer))
  (#.(swig-lispify "NdbScanFilter_begin" 'function) (ff-pointer self) group))

(cl:defmethod #.(swig-lispify "begin" 'method) ((self #.(swig-lispify "ndb-scan-filter" 'classname)))
  (#.(swig-lispify "NdbScanFilter_begin" 'function) (ff-pointer self)))

(cl:defmethod #.(swig-lispify "end" 'method) ((self #.(swig-lispify "ndb-scan-filter" 'classname)))
  (#.(swig-lispify "NdbScanFilter_end" 'function) (ff-pointer self)))

(cl:defmethod #.(swig-lispify "reset" 'method) ((self #.(swig-lispify "ndb-scan-filter" 'classname)))
  (#.(swig-lispify "NdbScanFilter_reset" 'function) (ff-pointer self)))

(cl:defmethod #.(swig-lispify "istrue" 'method) ((self #.(swig-lispify "ndb-scan-filter" 'classname)))
  (#.(swig-lispify "NdbScanFilter_istrue" 'function) (ff-pointer self)))

(cl:defmethod #.(swig-lispify "isfalse" 'method) ((self #.(swig-lispify "ndb-scan-filter" 'classname)))
  (#.(swig-lispify "NdbScanFilter_isfalse" 'function) (ff-pointer self)))

(cl:defmethod #.(swig-lispify "cmp" 'method) ((self #.(swig-lispify "ndb-scan-filter" 'classname)) (cond cl:integer) (ColId cl:integer) val (len cl:integer))
  (#.(swig-lispify "NdbScanFilter_cmp" 'function) (ff-pointer self) cond ColId val len))

(cl:defmethod #.(swig-lispify "cmp" 'method) ((self #.(swig-lispify "ndb-scan-filter" 'classname)) (cond cl:integer) (ColId cl:integer) val)
  (#.(swig-lispify "NdbScanFilter_cmp" 'function) (ff-pointer self) cond ColId val))

(cl:defmethod #.(swig-lispify "cmp" 'method) ((self #.(swig-lispify "ndb-scan-filter" 'classname)) (cond cl:integer) (ColId1 cl:integer) (ColId2 cl:integer))
  (#.(swig-lispify "NdbScanFilter_cmp" 'function) (ff-pointer self) cond ColId1 ColId2))

(cl:defmethod #.(swig-lispify "eq" 'method) ((self #.(swig-lispify "ndb-scan-filter" 'classname)) (ColId cl:integer) (value cl:integer))
  (#.(swig-lispify "NdbScanFilter_eq" 'function) (ff-pointer self) ColId value))

(cl:defmethod #.(swig-lispify "ne" 'method) ((self #.(swig-lispify "ndb-scan-filter" 'classname)) (ColId cl:integer) (value cl:integer))
  (#.(swig-lispify "NdbScanFilter_ne" 'function) (ff-pointer self) ColId value))

(cl:defmethod #.(swig-lispify "lt" 'method) ((self #.(swig-lispify "ndb-scan-filter" 'classname)) (ColId cl:integer) (value cl:integer))
  (#.(swig-lispify "NdbScanFilter_lt" 'function) (ff-pointer self) ColId value))

(cl:defmethod #.(swig-lispify "le" 'method) ((self #.(swig-lispify "ndb-scan-filter" 'classname)) (ColId cl:integer) (value cl:integer))
  (#.(swig-lispify "NdbScanFilter_le" 'function) (ff-pointer self) ColId value))

(cl:defmethod #.(swig-lispify "gt" 'method) ((self #.(swig-lispify "ndb-scan-filter" 'classname)) (ColId cl:integer) (value cl:integer))
  (#.(swig-lispify "NdbScanFilter_gt" 'function) (ff-pointer self) ColId value))

(cl:defmethod #.(swig-lispify "ge" 'method) ((self #.(swig-lispify "ndb-scan-filter" 'classname)) (ColId cl:integer) (value cl:integer))
  (#.(swig-lispify "NdbScanFilter_ge" 'function) (ff-pointer self) ColId value))

(cl:defmethod #.(swig-lispify "eq" 'method) ((self #.(swig-lispify "ndb-scan-filter" 'classname)) (ColId cl:integer) value)
  (#.(swig-lispify "NdbScanFilter_eq" 'function) (ff-pointer self) ColId value))

(cl:defmethod #.(swig-lispify "ne" 'method) ((self #.(swig-lispify "ndb-scan-filter" 'classname)) (ColId cl:integer) value)
  (#.(swig-lispify "NdbScanFilter_ne" 'function) (ff-pointer self) ColId value))

(cl:defmethod #.(swig-lispify "lt" 'method) ((self #.(swig-lispify "ndb-scan-filter" 'classname)) (ColId cl:integer) value)
  (#.(swig-lispify "NdbScanFilter_lt" 'function) (ff-pointer self) ColId value))

(cl:defmethod #.(swig-lispify "le" 'method) ((self #.(swig-lispify "ndb-scan-filter" 'classname)) (ColId cl:integer) value)
  (#.(swig-lispify "NdbScanFilter_le" 'function) (ff-pointer self) ColId value))

(cl:defmethod #.(swig-lispify "gt" 'method) ((self #.(swig-lispify "ndb-scan-filter" 'classname)) (ColId cl:integer) value)
  (#.(swig-lispify "NdbScanFilter_gt" 'function) (ff-pointer self) ColId value))

(cl:defmethod #.(swig-lispify "ge" 'method) ((self #.(swig-lispify "ndb-scan-filter" 'classname)) (ColId cl:integer) value)
  (#.(swig-lispify "NdbScanFilter_ge" 'function) (ff-pointer self) ColId value))

(cl:defmethod #.(swig-lispify "isnull" 'method) ((self #.(swig-lispify "ndb-scan-filter" 'classname)) (ColId cl:integer))
  (#.(swig-lispify "NdbScanFilter_isnull" 'function) (ff-pointer self) ColId))

(cl:defmethod #.(swig-lispify "isnotnull" 'method) ((self #.(swig-lispify "ndb-scan-filter" 'classname)) (ColId cl:integer))
  (#.(swig-lispify "NdbScanFilter_isnotnull" 'function) (ff-pointer self) ColId))

(cl:defmethod #.(swig-lispify "get-ndb-error" 'method) ((self #.(swig-lispify "ndb-scan-filter" 'classname)))
  (#.(swig-lispify "NdbScanFilter_getNdbError" 'function) (ff-pointer self)))

(cl:defmethod #.(swig-lispify "get-interpreted-code" 'method) ((self #.(swig-lispify "ndb-scan-filter" 'classname)))
  (#.(swig-lispify "NdbScanFilter_getInterpretedCode" 'function) (ff-pointer self)))

(cl:defmethod #.(swig-lispify "get-ndb-operation" 'method) ((self #.(swig-lispify "ndb-scan-filter" 'classname)))
  (#.(swig-lispify "NdbScanFilter_getNdbOperation" 'function) (ff-pointer self)))


(cl:defclass #.(swig-lispify "ndb-rec-attr" 'classname)()
  ((ff-pointer :reader ff-pointer)))

(cl:defmethod #.(swig-lispify "get-column" 'method) ((self #.(swig-lispify "ndb-rec-attr" 'classname)))
  (#.(swig-lispify "NdbRecAttr_getColumn" 'function) (ff-pointer self)))

(cl:defmethod #.(swig-lispify "get-type" 'method) ((self #.(swig-lispify "ndb-rec-attr" 'classname)))
  (#.(swig-lispify "NdbRecAttr_getType" 'function) (ff-pointer self)))

(cl:defmethod #.(swig-lispify "get-size-in-bytes" 'method) ((self #.(swig-lispify "ndb-rec-attr" 'classname)))
  (#.(swig-lispify "NdbRecAttr_get_size_in_bytes" 'function) (ff-pointer self)))

(cl:defmethod #.(swig-lispify "is-null" 'method) ((self #.(swig-lispify "ndb-rec-attr" 'classname)))
  (#.(swig-lispify "NdbRecAttr_isNULL" 'function) (ff-pointer self)))

(cl:defmethod #.(swig-lispify "int64-value" 'method) ((self #.(swig-lispify "ndb-rec-attr" 'classname)))
  (#.(swig-lispify "NdbRecAttr_int64_value" 'function) (ff-pointer self)))

(cl:defmethod #.(swig-lispify "int32-value" 'method) ((self #.(swig-lispify "ndb-rec-attr" 'classname)))
  (#.(swig-lispify "NdbRecAttr_int32_value" 'function) (ff-pointer self)))

(cl:defmethod #.(swig-lispify "medium-value" 'method) ((self #.(swig-lispify "ndb-rec-attr" 'classname)))
  (#.(swig-lispify "NdbRecAttr_medium_value" 'function) (ff-pointer self)))

(cl:defmethod #.(swig-lispify "short-value" 'method) ((self #.(swig-lispify "ndb-rec-attr" 'classname)))
  (#.(swig-lispify "NdbRecAttr_short_value" 'function) (ff-pointer self)))

(cl:defmethod #.(swig-lispify "char-value" 'method) ((self #.(swig-lispify "ndb-rec-attr" 'classname)))
  (#.(swig-lispify "NdbRecAttr_char_value" 'function) (ff-pointer self)))

(cl:defmethod #.(swig-lispify "int8-value" 'method) ((self #.(swig-lispify "ndb-rec-attr" 'classname)))
  (#.(swig-lispify "NdbRecAttr_int8_value" 'function) (ff-pointer self)))

(cl:defmethod #.(swig-lispify "u-64-value" 'method) ((self #.(swig-lispify "ndb-rec-attr" 'classname)))
  (#.(swig-lispify "NdbRecAttr_u_64_value" 'function) (ff-pointer self)))

(cl:defmethod #.(swig-lispify "u-32-value" 'method) ((self #.(swig-lispify "ndb-rec-attr" 'classname)))
  (#.(swig-lispify "NdbRecAttr_u_32_value" 'function) (ff-pointer self)))

(cl:defmethod #.(swig-lispify "u-medium-value" 'method) ((self #.(swig-lispify "ndb-rec-attr" 'classname)))
  (#.(swig-lispify "NdbRecAttr_u_medium_value" 'function) (ff-pointer self)))

(cl:defmethod #.(swig-lispify "u-short-value" 'method) ((self #.(swig-lispify "ndb-rec-attr" 'classname)))
  (#.(swig-lispify "NdbRecAttr_u_short_value" 'function) (ff-pointer self)))

(cl:defmethod #.(swig-lispify "u-char-value" 'method) ((self #.(swig-lispify "ndb-rec-attr" 'classname)))
  (#.(swig-lispify "NdbRecAttr_u_char_value" 'function) (ff-pointer self)))

(cl:defmethod #.(swig-lispify "u-8-value" 'method) ((self #.(swig-lispify "ndb-rec-attr" 'classname)))
  (#.(swig-lispify "NdbRecAttr_u_8_value" 'function) (ff-pointer self)))

(cl:defmethod #.(swig-lispify "float-value" 'method) ((self #.(swig-lispify "ndb-rec-attr" 'classname)))
  (#.(swig-lispify "NdbRecAttr_float_value" 'function) (ff-pointer self)))

(cl:defmethod #.(swig-lispify "double-value" 'method) ((self #.(swig-lispify "ndb-rec-attr" 'classname)))
  (#.(swig-lispify "NdbRecAttr_double_value" 'function) (ff-pointer self)))

(cl:defmethod #.(swig-lispify "a-ref" 'method) ((self #.(swig-lispify "ndb-rec-attr" 'classname)))
  (#.(swig-lispify "NdbRecAttr_aRef" 'function) (ff-pointer self)))

(cl:defmethod #.(swig-lispify "clone" 'method) ((self #.(swig-lispify "ndb-rec-attr" 'classname)))
  (#.(swig-lispify "NdbRecAttr_clone" 'function) (ff-pointer self)))

(cl:defmethod #.(swig-lispify "next" 'method) ((self #.(swig-lispify "ndb-rec-attr" 'classname)))
  (#.(swig-lispify "NdbRecAttr_next" 'function) (ff-pointer self)))


(cl:defclass #.(swig-lispify "ndb-record-print-format" 'classname)(#.(swig-lispify "ndb-dictionary::-ndb-data-print-format" 'classname))
  ((ff-pointer :reader ff-pointer)))

(cl:defmethod initialize-instance :after ((obj #.(swig-lispify "ndb-record-print-format" 'class)) &key)
  (setf (slot-value obj 'ff-pointer) (#.(swig-lispify "new_NdbRecordPrintFormat" 'function))))


(cl:defclass #.(swig-lispify "ndb-event-operation" 'classname)()
  ((ff-pointer :reader ff-pointer)))

(cl:defmethod #.(swig-lispify "get-state" 'method) ((self #.(swig-lispify "ndb-event-operation" 'classname)))
  (#.(swig-lispify "NdbEventOperation_getState" 'function) (ff-pointer self)))

(cl:defmethod #.(swig-lispify "merge-events" 'method) ((self #.(swig-lispify "ndb-event-operation" 'classname)) (flag cl:t))
  (#.(swig-lispify "NdbEventOperation_mergeEvents" 'function) (ff-pointer self) flag))

(cl:defmethod #.(swig-lispify "execute" 'method) ((self #.(swig-lispify "ndb-event-operation" 'classname)))
  (#.(swig-lispify "NdbEventOperation_execute" 'function) (ff-pointer self)))

(cl:defmethod #.(swig-lispify "get-value" 'method) ((self #.(swig-lispify "ndb-event-operation" 'classname)) (anAttrName cl:string) (aValue cl:string))
  (#.(swig-lispify "NdbEventOperation_getValue" 'function) (ff-pointer self) anAttrName aValue))

(cl:defmethod #.(swig-lispify "get-value" 'method) ((self #.(swig-lispify "ndb-event-operation" 'classname)) (anAttrName cl:string))
  (#.(swig-lispify "NdbEventOperation_getValue" 'function) (ff-pointer self) anAttrName))

(cl:defmethod #.(swig-lispify "get-pre-value" 'method) ((self #.(swig-lispify "ndb-event-operation" 'classname)) (anAttrName cl:string) (aValue cl:string))
  (#.(swig-lispify "NdbEventOperation_getPreValue" 'function) (ff-pointer self) anAttrName aValue))

(cl:defmethod #.(swig-lispify "get-pre-value" 'method) ((self #.(swig-lispify "ndb-event-operation" 'classname)) (anAttrName cl:string))
  (#.(swig-lispify "NdbEventOperation_getPreValue" 'function) (ff-pointer self) anAttrName))

(cl:defmethod #.(swig-lispify "get-blob-handle" 'method) ((self #.(swig-lispify "ndb-event-operation" 'classname)) (anAttrName cl:string))
  (#.(swig-lispify "NdbEventOperation_getBlobHandle" 'function) (ff-pointer self) anAttrName))

(cl:defmethod #.(swig-lispify "get-pre-blob-handle" 'method) ((self #.(swig-lispify "ndb-event-operation" 'classname)) (anAttrName cl:string))
  (#.(swig-lispify "NdbEventOperation_getPreBlobHandle" 'function) (ff-pointer self) anAttrName))

(cl:defmethod #.(swig-lispify "is-overrun" 'method) ((self #.(swig-lispify "ndb-event-operation" 'classname)))
  (#.(swig-lispify "NdbEventOperation_isOverrun" 'function) (ff-pointer self)))

(cl:defmethod #.(swig-lispify "is-consistent" 'method) ((self #.(swig-lispify "ndb-event-operation" 'classname)))
  (#.(swig-lispify "NdbEventOperation_isConsistent" 'function) (ff-pointer self)))

(cl:defmethod #.(swig-lispify "get-event-type2" 'method) ((self #.(swig-lispify "ndb-event-operation" 'classname)))
  (#.(swig-lispify "NdbEventOperation_getEventType2" 'function) (ff-pointer self)))

(cl:defmethod #.(swig-lispify "get-event-type" 'method) ((self #.(swig-lispify "ndb-event-operation" 'classname)))
  (#.(swig-lispify "NdbEventOperation_getEventType" 'function) (ff-pointer self)))

(cl:defmethod #.(swig-lispify "table-name-changed" 'method) ((self #.(swig-lispify "ndb-event-operation" 'classname)))
  (#.(swig-lispify "NdbEventOperation_tableNameChanged" 'function) (ff-pointer self)))

(cl:defmethod #.(swig-lispify "table-frm-changed" 'method) ((self #.(swig-lispify "ndb-event-operation" 'classname)))
  (#.(swig-lispify "NdbEventOperation_tableFrmChanged" 'function) (ff-pointer self)))

(cl:defmethod #.(swig-lispify "table-fragmentation-changed" 'method) ((self #.(swig-lispify "ndb-event-operation" 'classname)))
  (#.(swig-lispify "NdbEventOperation_tableFragmentationChanged" 'function) (ff-pointer self)))

(cl:defmethod #.(swig-lispify "table-range-list-changed" 'method) ((self #.(swig-lispify "ndb-event-operation" 'classname)))
  (#.(swig-lispify "NdbEventOperation_tableRangeListChanged" 'function) (ff-pointer self)))

(cl:defmethod #.(swig-lispify "get-epoch" 'method) ((self #.(swig-lispify "ndb-event-operation" 'classname)))
  (#.(swig-lispify "NdbEventOperation_getEpoch" 'function) (ff-pointer self)))

(cl:defmethod #.(swig-lispify "get-gci" 'method) ((self #.(swig-lispify "ndb-event-operation" 'classname)))
  (#.(swig-lispify "NdbEventOperation_getGCI" 'function) (ff-pointer self)))

(cl:defmethod #.(swig-lispify "get-any-value" 'method) ((self #.(swig-lispify "ndb-event-operation" 'classname)))
  (#.(swig-lispify "NdbEventOperation_getAnyValue" 'function) (ff-pointer self)))

(cl:defmethod #.(swig-lispify "get-latest-gci" 'method) ((self #.(swig-lispify "ndb-event-operation" 'classname)))
  (#.(swig-lispify "NdbEventOperation_getLatestGCI" 'function) (ff-pointer self)))

(cl:defmethod #.(swig-lispify "get-trans-id" 'method) ((self #.(swig-lispify "ndb-event-operation" 'classname)))
  (#.(swig-lispify "NdbEventOperation_getTransId" 'function) (ff-pointer self)))

(cl:defmethod #.(swig-lispify "get-ndb-error" 'method) ((self #.(swig-lispify "ndb-event-operation" 'classname)))
  (#.(swig-lispify "NdbEventOperation_getNdbError" 'function) (ff-pointer self)))

(cl:defmethod #.(swig-lispify "set-allow-empty-update" 'method) ((self #.(swig-lispify "ndb-event-operation" 'classname)) (allow cl:t))
  (#.(swig-lispify "NdbEventOperation_setAllowEmptyUpdate" 'function) (ff-pointer self) allow))

(cl:defmethod #.(swig-lispify "get-allow-empty-update" 'method) ((self #.(swig-lispify "ndb-event-operation" 'classname)))
  (#.(swig-lispify "NdbEventOperation_getAllowEmptyUpdate" 'function) (ff-pointer self)))

(cl:defmethod #.(swig-lispify "is-empty-epoch" 'method) ((self #.(swig-lispify "ndb-event-operation" 'classname)))
  (#.(swig-lispify "NdbEventOperation_isEmptyEpoch" 'function) (ff-pointer self)))

(cl:defmethod #.(swig-lispify "is-error-epoch" 'method) ((self #.(swig-lispify "ndb-event-operation" 'classname)) error_type)
  (#.(swig-lispify "NdbEventOperation_isErrorEpoch" 'function) (ff-pointer self) error_type))

(cl:defmethod #.(swig-lispify "is-error-epoch" 'method) ((self #.(swig-lispify "ndb-event-operation" 'classname)))
  (#.(swig-lispify "NdbEventOperation_isErrorEpoch" 'function) (ff-pointer self)))

(cl:defmethod #.(swig-lispify "get-table" 'method) ((self #.(swig-lispify "ndb-event-operation" 'classname)))
  (#.(swig-lispify "NdbEventOperation_getTable" 'function) (ff-pointer self)))

(cl:defmethod #.(swig-lispify "get-event" 'method) ((self #.(swig-lispify "ndb-event-operation" 'classname)))
  (#.(swig-lispify "NdbEventOperation_getEvent" 'function) (ff-pointer self)))

(cl:defmethod #.(swig-lispify "get-first-pk-attr" 'method) ((self #.(swig-lispify "ndb-event-operation" 'classname)))
  (#.(swig-lispify "NdbEventOperation_getFirstPkAttr" 'function) (ff-pointer self)))

(cl:defmethod #.(swig-lispify "get-first-pk-pre-attr" 'method) ((self #.(swig-lispify "ndb-event-operation" 'classname)))
  (#.(swig-lispify "NdbEventOperation_getFirstPkPreAttr" 'function) (ff-pointer self)))

(cl:defmethod #.(swig-lispify "get-first-data-attr" 'method) ((self #.(swig-lispify "ndb-event-operation" 'classname)))
  (#.(swig-lispify "NdbEventOperation_getFirstDataAttr" 'function) (ff-pointer self)))

(cl:defmethod #.(swig-lispify "get-first-data-pre-attr" 'method) ((self #.(swig-lispify "ndb-event-operation" 'classname)))
  (#.(swig-lispify "NdbEventOperation_getFirstDataPreAttr" 'function) (ff-pointer self)))

(cl:defmethod #.(swig-lispify "set-custom-data" 'method) ((self #.(swig-lispify "ndb-event-operation" 'classname)) data)
  (#.(swig-lispify "NdbEventOperation_setCustomData" 'function) (ff-pointer self) data))

(cl:defmethod #.(swig-lispify "get-custom-data" 'method) ((self #.(swig-lispify "ndb-event-operation" 'classname)))
  (#.(swig-lispify "NdbEventOperation_getCustomData" 'function) (ff-pointer self)))

(cl:defmethod #.(swig-lispify "clear-error" 'method) ((self #.(swig-lispify "ndb-event-operation" 'classname)))
  (#.(swig-lispify "NdbEventOperation_clearError" 'function) (ff-pointer self)))

(cl:defmethod #.(swig-lispify "has-error" 'method) ((self #.(swig-lispify "ndb-event-operation" 'classname)))
  (#.(swig-lispify "NdbEventOperation_hasError" 'function) (ff-pointer self)))

(cl:defmethod #.(swig-lispify "get-req-node-id" 'method) ((self #.(swig-lispify "ndb-event-operation" 'classname)))
  (#.(swig-lispify "NdbEventOperation_getReqNodeId" 'function) (ff-pointer self)))

(cl:defmethod #.(swig-lispify "get-ndbd-node-id" 'method) ((self #.(swig-lispify "ndb-event-operation" 'classname)))
  (#.(swig-lispify "NdbEventOperation_getNdbdNodeId" 'function) (ff-pointer self)))

(cl:defmethod #.(swig-lispify "print" 'method) ((self #.(swig-lispify "ndb-event-operation" 'classname)))
  (#.(swig-lispify "NdbEventOperation_print" 'function) (ff-pointer self)))


(cl:defclass #.(swig-lispify "ndb-blob" 'classname)()
  ((ff-pointer :reader ff-pointer)))

(cl:defmethod #.(swig-lispify "get-state" 'method) ((self #.(swig-lispify "ndb-blob" 'classname)))
  (#.(swig-lispify "NdbBlob_getState" 'function) (ff-pointer self)))

(cl:defmethod #.(swig-lispify "get-version" 'method) ((self #.(swig-lispify "ndb-blob" 'classname)) version)
  (#.(swig-lispify "NdbBlob_getVersion" 'function) (ff-pointer self) version))

(cl:defmethod #.(swig-lispify "get-value" 'method) ((self #.(swig-lispify "ndb-blob" 'classname)) data (bytes cl:integer))
  (#.(swig-lispify "NdbBlob_getValue" 'function) (ff-pointer self) data bytes))

(cl:defmethod #.(swig-lispify "set-value" 'method) ((self #.(swig-lispify "ndb-blob" 'classname)) data (bytes cl:integer))
  (#.(swig-lispify "NdbBlob_setValue" 'function) (ff-pointer self) data bytes))

(cl:defmethod #.(swig-lispify "set-active-hook" 'method) ((self #.(swig-lispify "ndb-blob" 'classname)) activeHook arg)
  (#.(swig-lispify "NdbBlob_setActiveHook" 'function) (ff-pointer self) activeHook arg))

(cl:defmethod #.(swig-lispify "get-defined" 'method) ((self #.(swig-lispify "ndb-blob" 'classname)) isNull)
  (#.(swig-lispify "NdbBlob_getDefined" 'function) (ff-pointer self) isNull))

(cl:defmethod #.(swig-lispify "get-null" 'method) ((self #.(swig-lispify "ndb-blob" 'classname)) isNull)
  (#.(swig-lispify "NdbBlob_getNull" 'function) (ff-pointer self) isNull))

(cl:defmethod #.(swig-lispify "get-null" 'method) ((self #.(swig-lispify "ndb-blob" 'classname)) isNull)
  (#.(swig-lispify "NdbBlob_getNull" 'function) (ff-pointer self) isNull))

(cl:defmethod #.(swig-lispify "set-null" 'method) ((self #.(swig-lispify "ndb-blob" 'classname)))
  (#.(swig-lispify "NdbBlob_setNull" 'function) (ff-pointer self)))

(cl:defmethod #.(swig-lispify "get-length" 'method) ((self #.(swig-lispify "ndb-blob" 'classname)) length)
  (#.(swig-lispify "NdbBlob_getLength" 'function) (ff-pointer self) length))

(cl:defmethod #.(swig-lispify "truncate" 'method) ((self #.(swig-lispify "ndb-blob" 'classname)) length)
  (#.(swig-lispify "NdbBlob_truncate" 'function) (ff-pointer self) length))

(cl:defmethod #.(swig-lispify "truncate" 'method) ((self #.(swig-lispify "ndb-blob" 'classname)))
  (#.(swig-lispify "NdbBlob_truncate" 'function) (ff-pointer self)))

(cl:defmethod #.(swig-lispify "get-pos" 'method) ((self #.(swig-lispify "ndb-blob" 'classname)) pos)
  (#.(swig-lispify "NdbBlob_getPos" 'function) (ff-pointer self) pos))

(cl:defmethod #.(swig-lispify "set-pos" 'method) ((self #.(swig-lispify "ndb-blob" 'classname)) pos)
  (#.(swig-lispify "NdbBlob_setPos" 'function) (ff-pointer self) pos))

(cl:defmethod #.(swig-lispify "read-data" 'method) ((self #.(swig-lispify "ndb-blob" 'classname)) data bytes)
  (#.(swig-lispify "NdbBlob_readData" 'function) (ff-pointer self) data bytes))

(cl:defmethod #.(swig-lispify "write-data" 'method) ((self #.(swig-lispify "ndb-blob" 'classname)) data (bytes cl:integer))
  (#.(swig-lispify "NdbBlob_writeData" 'function) (ff-pointer self) data bytes))

(cl:defmethod #.(swig-lispify "get-column" 'method) ((self #.(swig-lispify "ndb-blob" 'classname)))
  (#.(swig-lispify "NdbBlob_getColumn" 'function) (ff-pointer self)))

(cl:defmethod #.(swig-lispify "get-ndb-error" 'method) ((self #.(swig-lispify "ndb-blob" 'classname)))
  (#.(swig-lispify "NdbBlob_getNdbError" 'function) (ff-pointer self)))

(cl:defmethod #.(swig-lispify "get-ndb-operation" 'method) ((self #.(swig-lispify "ndb-blob" 'classname)))
  (#.(swig-lispify "NdbBlob_getNdbOperation" 'function) (ff-pointer self)))

(cl:defmethod #.(swig-lispify "blobs-first-blob" 'method) ((self #.(swig-lispify "ndb-blob" 'classname)))
  (#.(swig-lispify "NdbBlob_blobsFirstBlob" 'function) (ff-pointer self)))

(cl:defmethod #.(swig-lispify "blobs-next-blob" 'method) ((self #.(swig-lispify "ndb-blob" 'classname)))
  (#.(swig-lispify "NdbBlob_blobsNextBlob" 'function) (ff-pointer self)))

(cl:defmethod #.(swig-lispify "close" 'method) ((self #.(swig-lispify "ndb-blob" 'classname)) (execPendingBlobOps cl:t))
  (#.(swig-lispify "NdbBlob_close" 'function) (ff-pointer self) execPendingBlobOps))

(cl:defmethod #.(swig-lispify "close" 'method) ((self #.(swig-lispify "ndb-blob" 'classname)))
  (#.(swig-lispify "NdbBlob_close" 'function) (ff-pointer self)))


;;; Copyright (c) 2022 Max-Gerd Retzlaff <mgr@matroid.org>, Datagraph GmbH.
;;; Distributed under the terms of the GNU General Public License, Version 2.0,
;;; see file LICENSE in the top level directory of this repository.

(in-package :cl-user)

;; load libndbapi / libndbclient library

;;(push #p"/mnt/ssd/home/rondb/local/rondb/lib/" cffi:*foreign-library-directories*)
(pushnew #p"/mnt/ssd/home/rondb/code/rondb/prod_build/lib/" cffi:*foreign-library-directories*)

(cffi:define-foreign-library :libndbapi
  (:unix  (:or "libndbclient.so" "libndbclient.so.6.1.0"))
  (t (:default "libndbclient")))

(cffi:use-foreign-library :libndbapi)


;; load ndbapi wrapper library

;;(pushnew #p"/development/source/library/com/github/lisp/ndbapi/" cffi:*foreign-library-directories*)
(pushnew ndbapi:*ndbapi-directory* cffi:*foreign-library-directories*)

(cffi:define-foreign-library :ndbapi-wrap
  (:unix  (:or "ndbapi_wrap.so"))
  (t (:default "ndbapi_wrap")))

(cffi:use-foreign-library :ndbapi-wrap)

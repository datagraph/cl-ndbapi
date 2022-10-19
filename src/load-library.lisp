;;; Copyright (c) 2022 Max-Gerd Retzlaff <mgr@matroid.org>, Datagraph GmbH.
;;; Distributed under the terms of the GNU General Public License, Version 2.0,
;;; see file LICENSE in the top level directory of this repository.

(in-package :ndbapi.implementation)

;; Push the directory of the ndbapi.asd to cffi's load path.
;; The ndbapi_wrap.so library will be created in that directory and
;; I also put a link to "libndbclient.so.6.1.0" there so that that
;; library is found easily as well.
(pushnew (asdf:system-source-directory (asdf:find-system :ndbapi))
         cffi:*foreign-library-directories*)


;; load libndbapi / libndbclient library

(cffi:define-foreign-library :libndbapi
  (:unix  (:or "libndbclient.so" "libndbclient.so.6.1.0"))
  (t (:default "libndbclient")))

(cffi:use-foreign-library :libndbapi)


;; load ndbapi wrapper library

(cffi:define-foreign-library :ndbapi-wrap
  (:unix  (:or "ndbapi_wrap.so"))
  (t (:default "ndbapi_wrap")))

(cffi:use-foreign-library :ndbapi-wrap)

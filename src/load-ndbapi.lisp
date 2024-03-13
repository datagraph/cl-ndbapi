;;; Copyright (c) 2022 Max-Gerd Retzlaff <mgr@matroid.org>, Datagraph GmbH.
;;; Distributed under the terms of the GNU General Public License, Version 2.0,
;;; see file LICENSE in the top level directory of this repository.

(in-package :ndbapi.implementation)


;; libndbapi / libndbclient library

(cffi:define-foreign-library :libndbapi
  (:unix  (:or "libndbclient.so" "libndbclient.so.6.1.0"))
  (t (:default "libndbclient")))


;; ndbapi wrapper library

(cffi:define-foreign-library :ndbapi-wrap
  (:unix  (:or "ndbapi_wrap.so"))
  (t (:default "ndbapi_wrap")))


;; load ndbapi libraries

(defvar *ndbapi-loaded* nil)

;; The ndbapi_wrap.so library will be created in the ndbapi directory
;; and  I also put a link to "libndbclient.so.6.1.0" there so that that
;; library is found easily as well. The library can be found via:
;;   (asdf:system-source-directory (asdf:find-system :ndbapi))

(defun load-ndbapi (&key (search-path (asdf:system-source-directory (asdf:find-system :ndbapi))))
  (unless *ndbapi-loaded*
    (cffi:load-foreign-library :libndbapi :search-path search-path)
    (cffi:load-foreign-library :ndbapi-wrap :search-path search-path)
    (setf *ndbapi-loaded* t)))

;; to ensure early loading, you could add to your image:
;; #+sbcl(pushnew 'load-ndbapi sb-ext:*init-hooks*)

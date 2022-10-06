;;; Copyright (c) 2022 Max-Gerd Retzlaff <mgr@matroid.org>, Datagraph GmbH.
;;; Distributed under the terms of the GNU General Public License, Version 2.0,
;;; see file LICENSE in the top level directory of this repository.

(asdf:defsystem :libndbapi
  :description "The low-level C++ NDB API interface (for RonDB)."
  :author "Max-Gerd Retzlaff <mgr@matroid.org>"
  :depends-on (:cffi :trivial-features)
  :components ((:file "package")
               (:file "lispify" :depends-on ("package"))
               (:file "ndbapi-types" :depends-on ("lispify"))
               (:file "ndbapi" :depends-on ("ndbapi-types"))
               #+(or)(:file "ndbapi-clos" :depends-on ("ndbapi" #| really? or just "lispfy"? |#))
               (:file "ndbapi-wrappers" :depends-on ("ndbapi"))
               (:file "ndbapi-load" :depends-on ("package"))))

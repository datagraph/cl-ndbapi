;;; Copyright (c) 2022 Max-Gerd Retzlaff <mgr@matroid.org>, Datagraph GmbH.
;;; Distributed under the terms of the GNU General Public License, Version 2.0,
;;; see file LICENSE in the top level directory of this repository.

(asdf:defsystem :ndbapi
  :description "A high level and a low-level C++ NDB API interface (for RonDB)."
  :author "Max-Gerd Retzlaff <mgr@matroid.org>"
  :depends-on (:cffi :trivial-features)
  :components ((:file "package")
               (:file "lispify" :depends-on ("package"))
               (:file "ndbapi-types" :depends-on ("lispify"))
               (:file "ndbapi" :depends-on ("ndbapi-types"))
               #+(or)(:file "ndbapi-clos" :depends-on ("ndbapi" #| really? or just "lispfy"? |#))
               (:file "ndbapi-constructors" :depends-on ("ndbapi"))
               (:file "ndbapi-implementation" :depends-on ("ndbapi-constructors"))
               (:file "ndbapi-interface" :depends-on ("ndbapi-implementation"))
               (:file "ndbapi-load-library" :depends-on ("package"))
               ;; examples
               (:file "ndb-quads" :depends-on ("package"))
               #+(or)
               (:file "ndbapi-simple-scan-example" :depends-on ("ndb-quads" "ndbapi-interface"))))

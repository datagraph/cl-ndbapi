;;; Copyright (c) 2022 Max-Gerd Retzlaff <mgr@matroid.org>, Datagraph GmbH.
;;; Distributed under the terms of the GNU General Public License, Version 2.0,
;;; see file LICENSE in the top level directory of this repository.

(asdf:defsystem :ndbapi
  :description "A high level and a low-level C++ NDB API interface (for RonDB)."
  :author "Max-Gerd Retzlaff <mgr@matroid.org>"
  :depends-on (:cffi :bordeaux-threads)
  :components ((:module "src"
                :components ((:file "package")
                             (:file "lispify" :depends-on ("package"))
                             (:file "types" :depends-on ("lispify"))
                             (:file "ndbapi" :depends-on ("types"))
                             #+(or)(:file "ndbapi-clos" :depends-on ("ndbapi" #| really? or just "lispfy"? |#))
                             (:file "constructors" :depends-on ("ndbapi"))
                             (:file "overloading" :depends-on ("constructors"))
                             (:file "implementation" :depends-on ("overloading"))
                             (:file "interface" :depends-on ("implementation"))
                             (:file "load-library" :depends-on ("package"))))))

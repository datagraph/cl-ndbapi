;;; Copyright (c) 2022 Max-Gerd Retzlaff <mgr@matroid.org>, Datagraph GmbH.
;;; Distributed under the terms of the GNU General Public License, Version 2.0,
;;; see file LICENSE in the top level directory of this repository.

(asdf:defsystem :ndbapi-examples
  :description "Examples for NDB API interface (for RonDB)."
  :author "Max-Gerd Retzlaff <mgr@matroid.org>"
  :depends-on (:cffi :ndbapi)
  :components ((:module :examples
                :components ((:file "package")
                             (:file "ndb-quads" :depends-on ("package"))
                             (:file "ndbapi-simple-scan" :depends-on ("ndb-quads"))))))

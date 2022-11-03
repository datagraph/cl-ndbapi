;;; Copyright (c) 2022 Max-Gerd Retzlaff <mgr@matroid.org>, Datagraph GmbH.
;;; Distributed under the terms of the GNU General Public License, Version 2.0,
;;; see file LICENSE in the top level directory of this repository.

(in-package :cl-user)

(defpackage :ndb.quads
  (:use :cl)
  (:export :quad :triple :tuple :single
           :+quad-size+ :+tuple-size+ :+tuple-size+ :+single-size+
           :+quad-count+ :+triple-count+ :+tuple-count+ :+single-count+
           :with-foreign-quad :convert-foreign-quad
           :list-to-quad :list-to-quad*
           :quad-to-list :quad-to-list*)
  (:documentation "Definitions to access quads in tables"))

(defpackage :ndb.simple-scan
  (:use :cl)
  (:export :simple-scan)
  (:documentation "Simple example using scan"))

(defpackage :ndb.scan-count
  (:use :cl)
  (:export :scan-count :scan-count/recattr)
  (:documentation "Simple example using scan"))

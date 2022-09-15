;;; Copyright (c) 2022 Max-Gerd Retzlaff <mgr@matroid.org>, Datagraph GmbH.
;;; Distributed under the terms of the GNU General Public License, Version 2.0,
;;; see file LICENSE in the top level directory of this repository.

(in-package :cl-user)
(defpackage :libndbapi
  (:use #| :cl |#)
  (:documentation "The low-level C++ NDB API interface (for RonDB)."))

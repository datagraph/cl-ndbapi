;;; Copyright (c) 2022 Max-Gerd Retzlaff <mgr@matroid.org>, Datagraph GmbH.
;;; Distributed under the terms of the GNU General Public License, Version 2.0,
;;; see file LICENSE in the top level directory of this repository.

(defpackage :cbso
  (:use :cl :cffi))

(in-package :cbso)

#+(or)
(asdf:oos 'asdf:load-op :ndbapi)

(let ((directory (make-pathname :name nil :type nil :version nil
                                :defaults *load-pathname*)))
  (pushnew directory cffi:*foreign-library-directories*))

(define-foreign-library libcbso
  (:unix "cbso.so")
  (t (:Default "cbso")))

(cffi:defcallback my-cb :unsigned-int ()
  23)

(defun cbso ()
  (use-foreign-library libcbso)
  (unwind-protect
       (ndbapi:with-foreign-struct (scan-options (list ;;:size (cffi:callback ndbapi.i::scan-options-size)
                                                  :options-present :+SO-SCANFLAGS+
                                                  :scan-flags :+SF-ORDER-BY+
                                                  :parallel 0
                                                  :batch 0
                                                  :extra-get-values (cffi:null-pointer)
                                                  :num-extra-get-values 0
                                                  :partition-id 0
                                                  :interpreted-code (cffi:null-pointer)
                                                  :custom-data (cffi:null-pointer)
                                                  :partition-info (cffi:null-pointer)
                                                  :size-of-part-info 0)
                                                 '(:struct ndbapi:scan-options))
         (foreign-funcall "show_scan_options"
                          :pointer scan-options ;; pointer to '(:struct ndbapi:scan-options)
                          :void))
    (close-foreign-library 'libcbso)))

#|
(cbso::cbso)
; No value

Prints on the console:
size scanoptions: 72
scanoptions.size: 72
scanoptions.size: 0x7fd2eac657c6
scanoptions.optionsPresent: 1
scanoptions.scan_flags: 16777216
scanoptions.parallel: 0
scanoptions.batch: 0
scanoptions.extraGetValues: (nil)
scanoptions.numExtraGetValues: 0
scanoptions.partitionId: 0
scanoptions.interpretedCode: (nil)
scanoptions.customData: (nil)
scanoptions.partitionInfo: (nil)
scanoptions.sizeOfPartInfo: 0

scanoptions.optionsPresent offset: 0
scanoptions.optionsPresent sizeof: 8
scanoptions.scan_flags offset: 8
scanoptions.scan_flags sizeof: 4
scanoptions.parallel offset: 12
scanoptions.parallel sizeof: 4
scanoptions.batch offset: 16
scanoptions.batch sizeof: 4
scanoptions.extraGetValues offset: 24
scanoptions.extraGetValues sizeof: 8
scanoptions.numExtraGetValues offset: 32
scanoptions.numExtraGetValues sizeof: 4
scanoptions.partitionId offset: 36
scanoptions.partitionId sizeof: 4
scanoptions.interpretedCode offset: 40
scanoptions.interpretedCode sizeof: 8
scanoptions.customData offset: 48
scanoptions.customData sizeof: 8
scanoptions.partitionInfo offset: 56
scanoptions.partitionInfo sizeof: 8
scanoptions.sizeOfPartInfo offset: 64
scanoptions.sizeOfPartInfo sizeof: 4
|#

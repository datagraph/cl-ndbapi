;;; Copyright (c) 2022 Max-Gerd Retzlaff <mgr@matroid.org>, Datagraph GmbH.
;;; Distributed under the terms of the GNU General Public License, Version 2.0,
;;; see file LICENSE in the top level directory of this repository.

(in-package :ndbapi.ffi.overloading)

#+(or) ;; old version that only works if the number at the end of the function name indicates the arity
(defmacro overload-function (name)
  "simple dispatch on number of arguments"
  `(defun ,name (&rest args)
     (apply (symbol-function
                (find-symbol (format nil "~a-~a" ',name (length args))
                                :ndbapi.ffi))
               args)))

;;(overload-function #.(ndbapi.ffi::swig-lispify "Ndb_init" 'function))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun list-functions-by-arity (base-symbol package)
    (loop for i from 0 below 100
          for sym = (find-symbol (format nil "~s~a~d" base-symbol
                                         "/SWIG-"
                                         i) package)
          while sym
          for fn = sym
          for arity = (length (sb-introspect:function-lambda-list fn))
          collect (cons arity fn))))

(defmacro overload-function-by-arity (name &optional (package 'ndbapi.ffi))
  "simple dispatch on number of arguments
WARNING: this only works when there are no multiple functions with the same arity!"
  `(defun ,name (&rest args)
     (let* ((arity (length args))
            (fn (cdr (assoc arity ',(list-functions-by-arity name package)))))
       (assert fn
               (fn)
               "no variant of function ~a with arity ~a: ~a" ',name arity args)
       (when *ndbapi-verbose*
         (format *trace-output* "~&Calling ~a with arity ~a: ~a" ',name arity fn))
       (apply (symbol-function fn) args))))

(overload-function-by-arity #.(ndbapi.ffi::swig-lispify "Ndb_init" 'function))
(overload-function-by-arity #.(ndbapi.ffi::swig-lispify "NdbTransaction_scanIndex" 'function))
(overload-function-by-arity #.(ndbapi.ffi::swig-lispify "NdbScanOperation_close" 'function))
(overload-function-by-arity #.(ndbapi.ffi::swig-lispify "NdbScanOperation_nextResult" 'function))

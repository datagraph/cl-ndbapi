(in-package #:cffi)

(defmethod translate-into-foreign-memory :after ((object list)
                                                 (type foreign-struct-type)
                                                 p)
  "explicitly reset all foreign-boolean-type slots without double translation to foreign;
see: cffi issue #300:
  Struct assignment from plist calls convert-to-foreign twice
  https://github.com/cffi/cffi/issues/300"
  (unless (bare-struct-type-p type)
    (loop for (name value) on object by #'cddr
          do (let* ((slot (gethash name (structure-slots type)))
                    (stype (slot-type slot))
                    (ptype (parse-type stype)))
               ;; (break "slot ~a s: ~a p: ~a a: ~a" slot stype ptype (and (typep ptype 'enhanced-typedef) (actual-type ptype)))
               (when (or (typep ptype 'foreign-boolean-type)
                         ;; explictly check for :bool, where the ptype is an ENHANCED-TYPEDEF
                         ;; and only its actual-type is the foreign-boolean-type
                         (and (typep ptype 'enhanced-typedef)
                              (typep (actual-type ptype) 'foreign-boolean-type)))
                 (setf (foreign-slot-value p (unparse-type type) name)
                       value))))))

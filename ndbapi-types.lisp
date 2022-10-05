;;; Copyright (c) 2022 Max-Gerd Retzlaff <mgr@matroid.org>, Datagraph GmbH.
;;; Distributed under the terms of the GNU General Public License, Version 2.0,
;;; see file LICENSE in the top level directory of this repository.

(cl:in-package :libndbapi)

(cl:defvar *ndbapi-verbose* cl:t)

(cl:defun debug (object foreign-pointer is-null-pinter cl:&optional (verbose *ndbapi-verbose*))
  (cl:when verbose
    (cl:format cl:*trace-output* "~&Destructor called for ~a object: ~8,'0x (~:[do free~;do NOT free~])"
               object
               foreign-pointer
               is-null-pinter)
    (cl:force-output cl:*trace-output*)))

(cffi:define-foreign-type garbage-collected-type ()
  ((garbage-collect :reader garbage-collect :initform cl:nil :initarg :garbage-collect)
   (lisp-class :allocation :class :reader lisp-class))
  (:actual-type :pointer))


(cl:defclass garbage-collected-class ()
  ((foreign-pointer :reader foreign-pointer :initarg :foreign-pointer)))


(cl:defgeneric delete-foreign-object (class pointer))

(cl:defmethod delete-foreign-object (class pointer)
  (cl:error "Do not how to delete ~a object on pointer: ~8,'0x" class pointer))

(cl:defmethod delete-foreign-object :before (class pointer)
  (cl:when *ndbapi-verbose*
    (cl:format cl:*trace-output* "~&calling DELETE for ~a object on pointer: ~8,'0x"
               class
               pointer)
    (cl:force-output cl:*trace-output*)))

(cl:defmethod cffi:translate-to-foreign ((lisp-object garbage-collected-class)
                                         (foreign-type garbage-collected-type))
  (cl:when *ndbapi-verbose*
    (cl:format cl:*trace-output* "~&translate ~a to ~a"
               (cl:class-name (cl:class-of lisp-object))
               (cl:class-name (cl:class-of foreign-type))))
  (foreign-pointer lisp-object))

(cl:defmethod cffi:translate-from-foreign (foreign-pointer (foreign-type garbage-collected-type))
  (cl:let* ((class (lisp-class foreign-type))
            (lisp-object (cl:make-instance class :foreign-pointer foreign-pointer)))
    (cl:when *ndbapi-verbose*
      (cl:format cl:*trace-output* "~&translate ~a to ~a"
                 (cl:class-name (cl:class-of foreign-type))
                 class))
    (cl:when (garbage-collect foreign-type)
      (sb-ext:finalize lisp-object (cl:lambda ()
                                     (cl:let ((is-null-pointer (cffi:null-pointer-p foreign-pointer)))
                                       (debug class foreign-pointer is-null-pointer)
                                       (cl:unless is-null-pointer
                                         (delete-foreign-object class foreign-pointer)
                                         ;; to be extra careful:
                                         (cl:setf foreign-pointer (cffi:null-pointer)))))))
    lisp-object))

;; macro

(cl:defmacro make-concrete-foreign-type (type class delete-fn)
  `(cl:progn
     (cffi:define-foreign-type ,type (garbage-collected-type)
       ((lisp-class :initform ',class :allocation :class))
       (:simple-parser ,type))

     (cl:defclass ,class (garbage-collected-class)
       ())

     (cl:defmethod delete-foreign-object ((class (cl:eql ',class)) pointer)
       (,delete-fn pointer))))

(make-concrete-foreign-type ndb-type ndb delete-ndb)


;; ndb

#|
(cffi:define-foreign-type ndb-type (garbage-collected-type)
  ((lisp-class :initform 'ndb :allocation :class))
  (:simple-parser ndb-type))

(cl:defclass ndb (garbage-collected-class)
  ())

(cl:defmethod delete-foreign-object ((class (cl:eql 'ndb)) pointer)
  (delete-ndb pointer))
|#

(make-concrete-foreign-type ndb-type ndb delete-ndb)

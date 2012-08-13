(in-package #:burning-ffi)

;;
;; FFI file class
;;

(defclass ffi-file (asdf:source-file)
  ((type :initform "ffi")))

(defmethod asdf:output-files ((o asdf:load-op) (c ffi-file))
  ())

(defmethod asdf:output-files :around ((o asdf:compile-op) (c ffi-file))
  (values (asdf:input-files o c) t))

(defmethod asdf:perform ((o asdf:compile-op) (c ffi-file))
  ())

(defmethod asdf:perform ((o asdf:load-op) (c ffi-file))
  (mapc #'load-ffi (asdf:input-files o c)))

(defmethod asdf:perform ((o asdf:load-source-op) (c ffi-file))
  (asdf:perform (make-instance 'asdf:load-op) c))

;;
;; UUID file class
;;

(defclass uuid-file (asdf:source-file)
  ((type :initform "uuid")))

(defmethod asdf:output-files ((o asdf:load-op) (c uuid-file))
  ())

(defmethod asdf:output-files :around ((o asdf:compile-op) (c uuid-file))
  (values (asdf:input-files o c) t))

(defmethod asdf:perform ((o asdf:compile-op) (c uuid-file))
  ())

(defmethod asdf:perform ((o asdf:load-op) (c uuid-file))
  (mapc #'load-uuid (asdf:input-files o c)))

(defmethod asdf:perform ((o asdf:load-source-op) (c uuid-file))
  (asdf:perform (make-instance 'asdf:load-op) c))




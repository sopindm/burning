(in-package #:burning-protobuf)

(defun protobuf-write (stream value tag &optional (type nil type-p))
  (unless type-p
	(setf type (type-of value)))
  (%protobuf-write stream value tag type))

(defgeneric %protobuf-write (stream value tag type))

(defmethod %protobuf-write :before (stream value tag type)
  (write-byte (* tag 8) stream))

(defun write-varint (stream value)
  (when (< value 0)
	(incf value (expt 2 64)))
  (labels ((to-varbytes (value)
			 (if (< value 128)
				 (list value)
				 (cons (+ 128 (mod value 128)) (to-varbytes (floor (/ value 128)))))))
	(let ((bytes (to-varbytes value)))
	  (mapc (lambda (x) (write-byte x stream)) bytes))))

(defmethod %protobuf-write (stream value tag (type (eql :int)))
  (write-varint stream value))

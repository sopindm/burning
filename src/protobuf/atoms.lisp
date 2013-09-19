(in-package #:burning-protobuf)

(defun protobuf-write (stream value tag &optional (type nil type-p))
  (unless type-p
	(setf type (type-of value)))
  (%protobuf-write stream value tag type))

(defgeneric %protobuf-write (stream value tag type))

(defun write-varint (stream value)
  (when (< value 0)
	(incf value (expt 2 64)))
  (labels ((to-varbytes (value)
			 (if (< value 128)
				 (list value)
				 (cons (+ 128 (mod value 128)) (to-varbytes (floor (/ value 128)))))))
	(let ((bytes (to-varbytes value)))
	  (mapc (lambda (x) (write-byte x stream)) bytes))))

(defmethod %protobuf-write (stream value tag (type (eql :varint)))
  (write-byte (* tag 8) stream)
  (write-varint stream value))

(defmethod %protobuf-write (stream value tag (type (eql :int32)))
  (when (or (>= value (expt 2 31)) (< value (- (expt 2 31))))
	(error "Wrong int32 value ~a" value))
  (%protobuf-write stream value tag :varint))

(defmethod %protobuf-write (stream value tag (type (eql :int64)))
  (when (or (>= value (expt 2 63)) (< value (- (expt 2 63))))
	(error "Wrong int64 value ~a" value))
  (%protobuf-write stream value tag :varint))

(defun write-fixnum (stream value bytes)
  (when (< value 0)
    (incf value (expt 2 (* bytes 8))))
  (if (= bytes 0)
      t
      (progn (write-fixnum stream (floor (/ value 256)) (1- bytes))
	     (write-byte (mod value 256) stream))))

(defmethod %protobuf-write (stream value tag (type (eql :fixnum32)))
  (write-byte (+ (* tag 8) 5) stream)
  (write-fixnum stream value 4))

(make-float-converters from-float32 to-float32 8 23 nil)
(make-float-converters from-float64 to-float64 11 52 nil)

(defmethod %protobuf-write (stream value tag (type (eql :float32)))
  (let ((integer (from-float32 (coerce value 'float))))
	(%protobuf-write stream integer tag :fixnum32)))

(defmethod %protobuf-write (stream value tag (type (eql :fixnum64)))
  (write-byte (+ (* tag 8) 1) stream)
  (write-fixnum stream value 8))

(defmethod %protobuf-write (stream value tag (type (eql :length-delimited)))
  (write-byte (+ (* tag 8) 2) stream)
  (write-varint stream (length value))
  (write-sequence value stream))

(defun read-varint (stream)
  (labels ((from-varbytes ()
	     (let ((value (read-byte stream)))
	       (if (< value 128)
		   value
		   (+ (- value 128) (* (from-varbytes) 128))))))
    (let ((value (from-varbytes)))
      (if (>= value (expt 2 63))
	  (- value (expt 2 64))
	  value))))

(defun read-fixnum (stream bytes)
  (labels ((do-read (bytes)
	     (if (= bytes 0)
		 0
		 (let ((value (do-read (1- bytes))))
		   (+ (* 256 value) (read-byte stream))))))
    (let ((value (do-read bytes)))
      (if (>= value (expt 2 (1- (* bytes 8))))
	  (- value (expt 2 (* 8 bytes)))
	  value))))

(defun read-length-delimited (stream)
  (let* ((size (read-varint stream))
		 (seq (make-list size)))
	(read-sequence seq stream)
	seq))

(defun protobuf-read (stream)
  (let* ((tag-byte (read-byte stream))
	 (tag (floor (/ tag-byte 8)))
	 (wire (mod tag-byte 8)))
    (values (ecase wire
	      (0 (read-varint stream))
	      (1 (read-fixnum stream 8))
	      (2 (read-length-delimited stream))
	      (5 (read-fixnum stream 4)))
	    tag)))

(defun cast-from-protobuf (value slot type)
  (let ((slot-type (message-slot-type type slot)))
	(if (message-enum-p slot-type type)
		(values (message-enum-number value slot-type type) :varint)
		(values value slot-type))))

(defun cast-to-protobuf (value slot type)
  (let ((slot-type (message-slot-type type slot)))
	(if (message-enum-p slot-type type)
		(message-enum-value value slot-type type)
		(case slot-type
		  (:float32 (to-float32 value))
		  (otherwise value)))))
	


  

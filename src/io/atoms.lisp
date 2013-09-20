(in-package #:burning-io)

;;
;; Type reading/writing globals
;;

(defvar *type-readers* (make-hash-table :test #'eq))

(defun type-reader (type)
  (let ((value (gethash type *type-readers*)))
    (unless value (error "Unknown IO type ~a." type))
    value))

(defun (setf type-reader) (value type)
  (setf (gethash type *type-readers*) value))

(defmacro defreader (type (stream-arg) &body body)
  `(setf (type-reader ,type)
	 (lambda (,stream-arg)
	   ,@body)))

(defvar *type-writers* (make-hash-table :test #'eq))

(defun type-writer (type)
  (let ((value (gethash type *type-writers*)))
    (unless value (error "Unknown IO type ~a." type))
    value))

(defun (setf type-writer) (value type)
  (setf (gethash type *type-writers*) value))

(defmacro defwriter (type (stream-arg value-arg) &body body)
  `(setf (type-writer ,type)
	 (lambda (,stream-arg ,value-arg)
	   ,@body)))

(defmacro define-type-alias (new-name old-name)
  `(progn (setf (type-writer ,new-name) (type-writer ,old-name))
	  (setf (type-reader ,new-name) (type-reader ,old-name))))

(defun stream-read (stream &optional (type nil type-p))
  (let ((type (if type-p type (stream-read-type stream))))
    (funcall (type-reader type) stream)))

(defun stream-write (stream value type)
  (funcall (type-writer type) stream value))
  
;;
;; Integer types
;;

(defun make-buffer (content)
  (unless (listp content)
    (setf content (list content)))
  (make-array (length content) :element-type '(unsigned-byte 8) :initial-contents content))

(defun stream-read-int (stream &optional (bytes 1) (signed-p t) (big-endian nil))
  (let ((value 0)
	(start-bite (if big-endian (* (1- bytes) 8) 0)))
    (dotimes (i bytes)
      (setf (ldb (byte 8 start-bite) value) (read-byte stream))
      (if big-endian (decf start-bite 8) (incf start-bite 8)))
    (when (and signed-p (> value (1- (expt 2 (1- (* bytes 8))))))
      (decf value (expt 2 (* bytes 8))))
    value))

(defun stream-write-int (stream value &optional (bytes 1) (signed-p t) (big-endian nil))
  (when (and signed-p (< value 0))
    (incf value (expt 2 (* bytes 8))))
  (let ((start-bite (if big-endian (* (1- bytes) 8) 0)))
    (dotimes (i bytes)
      (write-byte (ldb (byte 8 start-bite) value) stream)
      (if big-endian (decf start-bite 8) (incf start-bite 8))))
  value)

(defun integer-type-head (bytes signed-p big-endian-p)
  (check-type bytes (member 1 2 4 8 16 32 64 128))
  (labels ((log2 (value)
	     (if (= value 1) 0 (1+ (log2 (floor value 2))))))
    (setf bytes (log2 bytes)))
  (let ((value 0))
    (setf (ldb (byte 3 0) value) bytes)
    (setf (ldb (byte 1 3) value) (if big-endian-p 1 0))
    (setf (ldb (byte 1 4) value) (if signed-p 1 0))
    (make-buffer value)))

(defmacro define-integer-type (type bytes signed-p big-endian-p)
  `(progn (defreader ,type (stream) 
	    (stream-read-int stream ,bytes ,signed-p ,big-endian-p))
	  (defwriter ,type (stream value)
	    (stream-write-int stream value ,bytes, signed-p ,big-endian-p))))

(define-integer-type :int8 1 t nil)
(define-integer-type :int16 2 t nil)
(define-integer-type :int32 4 t nil)
(define-integer-type :int64 8 t nil)
(define-integer-type :int128 16 t nil)

(define-integer-type :uint8 1 nil nil)
(define-integer-type :uint16 2 nil nil)
(define-integer-type :uint32 4 nil nil)
(define-integer-type :uint64 8 nil nil)
(define-integer-type :uint128 16 nil nil)

(define-type-alias :byte :int8)
(define-type-alias :ubyte :uint8)
(define-type-alias :short :int16)
(define-type-alias :ushort :uint16)
(define-type-alias :int :int32)
(define-type-alias :uint :uint32)
(define-type-alias :long :int64)
(define-type-alias :ulong :uint64)

;;
;; Float types
;;

(defmacro define-float-type (type bytes exponent fraction)
  (let ((encoder-name (gensym))
	(decoder-name (gensym)))
    `(progn (make-float-converters ,encoder-name ,decoder-name ,exponent ,fraction nil)
	    (defreader ,type (stream)
	      (,decoder-name (stream-read-int stream ,bytes nil nil)))
	    (defwriter ,type (stream value)
	      (stream-write-int stream (,encoder-name value) ,bytes nil nil)))))

(define-float-type :float16 2 5 10)
(define-float-type :float32 4 8 23)
(define-float-type :float64 8 11 52)
(define-float-type :float128 16 15 112)

(define-type-alias :float :float32)
(define-type-alias :double :float64)

;;
;; Characters
;; 

(defun parse-lead-byte (byte)
  (flet ((head (byte size)
	   (ldb (byte size (- 8 size)) byte))
	 (tail (byte size)
	   (ldb (byte (- 8 size) 0) byte)))
    (cond ((= (head byte 1) 0) (values (tail byte 1) 0))
	  ((= (head byte 3) 6) (values (tail byte 3) 1))
	  ((= (head byte 4) 14) (values (tail byte 4) 2))
	  ((= (head byte 5) 30) (values (tail byte 5) 3))
	  ((= (head byte 6) 62) (values (tail byte 6) 4))
	  ((= (head byte 7) 126) (values (tail byte 7) 5))
	  (t (values nil nil)))))

(defun stream-read-char (stream)
  (let ((code 0))
    (flet ((read-lead-byte ()
	     (multiple-value-bind (value tail) (parse-lead-byte (read-byte stream))
	       (setf code value)
	       tail))
	   (read-continuation-byte ()
	     (setf code (* code 64))
	     (incf code (ldb (byte 6 0) (read-byte stream)))))
      (let ((size (read-lead-byte)))
	(dotimes (i size)
	  (read-continuation-byte))))
    (code-char code)))

(defun stream-write-char (stream value)
  (setf value (char-code value))
  (flet ((write-lead-byte ()
	   (cond ((< value 128) (write-byte value stream) 0)
		 ((< value 2048) (write-byte (+ 192 (ldb (byte 5 6) value)) stream) 6)
		 ((< value 65536) (write-byte (+ 224 (ldb (byte 4 12) value)) stream) 12)
		 ((< value 2097152) (write-byte (+ 240 (ldb (byte 3 18) value)) stream) 18)
		 ((< value 67108864) (write-byte (+ 248 (ldb (byte 2 25) value)) stream) 24)
		 ((< value 2147483648) (write-byte (+ 252 (ldb (byte 1 30) value)) stream) 30)
		 (t nil)))
	 (write-continuation-byte (bits)
	   (write-byte (+ 128 (ldb (byte 6 (- bits 6)) value)) stream)
	   6))
    (let ((bits (write-lead-byte)))
      (while (> bits 0)
	(decf bits (write-continuation-byte bits))))))

(defreader :char (stream)
  (stream-read-char stream))

(defwriter :char (stream value)
  (stream-write-char stream value))


;;
;; Arrays
;;

(defun write-

  
      
  


      
    
    

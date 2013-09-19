(in-package #:burning-protobuf)

(defun type-default (type)
  (ecase type
	(:int32 0)
	(:int64 0)
	(:float32 0)
	(:float64 0)))

(defclass message () ())

(defvar *messages* (make-hash-table))
(defvar *enums* (make-hash-table))

(defun define-slots (name slots)
  (setf (gethash name *messages*)
		slots))

(defun define-enums (name enums)
  (setf (gethash name *enums*)
		enums))

(defmacro defmessage (name superclasses &rest slots)
  (let ((slots (remove :enum slots :key #'first))
		(enums (remove-if-not (lambda (x) (eq x :enum)) slots :key #'first)))
	(flet ((make-slot-definition (spec)
			 (dbind (sname tag type &key optional) spec
			   `(,sname ,tag ,type ,@(if optional (list :optional)))))
		   (make-slot-class-definition (spec)
			 (dbind (sname tag type &key optional) spec
			   `(,sname :initarg ,(intern (symbol-name sname) "KEYWORD") 
						:accessor ,(intern (string+ (symbol-name name) "-" (symbol-name sname)) *package*)))))
	  `(progn (defclass ,name (message ,@superclasses)
				(,@(mapcar #'make-slot-class-definition slots)))
			  (define-slots ',name ',(mapcar #'make-slot-definition slots))
			  (define-enums ',name ',(mapcar #'rest enums))))))

(defun message-slots (message)
  (gethash message *messages*))

(defun message-slot-type (message slot)
  (let* ((slots (message-slots message))
		 (slot (find slot slots :key #'first)))
	(third slot)))

(defun message-slot-tag (message slot)
  (let* ((slots (message-slots message))
		 (slot (find slot slots :key #'first)))
	(second slot)))

(defun message-tag-slot (message tag)
  (let* ((slots (message-slots message))
		 (slot (find tag slots :key #'second :test #'eql)))
	(first slot)))

(defun message-slot-optional-p (message slot)
  (let* ((slots (message-slots message))
		 (slot (find slot slots :key #'first)))
	(if (find :optional slot) t nil)))

(defun message-enums (message)
  (gethash message *enums*))
	
(defun message-enum-p (type message-type)
  (if (find type (message-enums message-type) :key #'first)
	  t 
	  nil))

(defun message-enum-number (value type message-type)
  (let ((enum (find type (message-enums message-type) :key #'first)))
	(second (find value (rest enum) :key #'first))))

(defun message-enum-value (value type message-type)
  (let ((enum (find type (message-enums message-type) :key #'first)))
	(first (find value (rest enum) :key #'second :test #'eql))))

(defun check-slots (message)
  (let* ((type (type-of message))
		 (slots (message-slots type)))
	(let ((slots (remove-if (lambda (x) (or (slot-boundp message (first x)) (find :optional x))) slots)))
	  (unless (null slots)
		(error "Wrong message ~a. Missed required fields ~{~a~^, ~}" message (mapcar #'first slots)))
	  t)))

(defun write-slot (stream spec message)
  (when (slot-boundp message (first spec))
	(multiple-value-bind (value type) (cast-from-protobuf (slot-value message (first spec)) (first spec) (type-of message))
	  (protobuf-write stream value (second spec) type))))

(defun protobuf-write-message (stream message)
  (check-slots message)
  (let ((seq (with-output-to-sequence (output)
			   (let ((slots (message-slots (type-of message))))
				 (mapc (lambda (x) (write-slot output x message)) slots)))))
	(write-varint stream (length seq)) 
	(write-sequence seq stream)))

(defun read-slot (stream message type)
  (multiple-value-bind (value tag) (protobuf-read stream)
	(let ((slot (message-tag-slot type tag)))
	  (setf (slot-value message slot) (cast-to-protobuf value slot type)))))

(defun protobuf-read-message (stream type)
  (let* ((length (read-varint stream))
		 (seq (make-array length))
		 (message (make-instance type)))
	(read-sequence seq stream)
	(with-input-from-sequence (input seq)
	  (while (< (file-position input) length)
		(read-slot input message type)))
	(check-slots message)
	message))
		
	
		
	
  

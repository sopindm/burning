(in-package #:burning-protobuf)

(defun type-default (type)
  (ecase type
	(:int32 0)
	(:int64 0)
	(:float32 0)
	(:float64 0)))

(defclass message () ())

(defvar *messages* (make-hash-table))

(defun define-slots (name slots)
  (setf (gethash name *messages*)
		slots))

(defmacro defmessage (name superclasses &rest slots)
  (flet ((make-slot-definition (spec)
		   (dbind (sname tag type &key optional) spec
			 `(,sname ,tag ,type ,@(if optional (list :optional)))))
		 (make-slot-class-definition (spec)
		   (dbind (sname tag type &key optional) spec
			 `(,sname :initarg ,(intern (symbol-name sname) "KEYWORD") 
					  :initform ,(type-default type) 
					  :accessor ,(intern (string+ (symbol-name name) "-" (symbol-name sname)) *package*)))))
	`(progn (defclass ,name (message ,@superclasses)
			  (,@(mapcar #'make-slot-class-definition slots)))
			(define-slots ',name ',(mapcar #'make-slot-definition slots)))))

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
	
(defun write-slot (stream spec message)
  (protobuf-write stream (slot-value message (first spec)) (second spec) (third spec)))

(defun protobuf-write-message (stream message)
  (let ((slots (message-slots (type-of message))))
	(mapc (lambda (x) (write-slot stream x message)) slots)))

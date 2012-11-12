(in-package #:bsdf-streams)

(defclass counting-stream (trivial-gray-stream-mixin fundamental-character-input-stream) 
  ((base :initarg :base-stream :reader counting-stream-base)
   (line :initform 1 :reader counting-stream-line)
   (column :initform 1 :reader counting-stream-column)
   (chars :initform 1 :reader counting-stream-chars)
   (last-columns :initform 0)))

(defun make-counting-stream (stream)
  (make-instance 'counting-stream :base-stream stream))

(defmethod stream-element-type ((stream counting-stream))
  (stream-element-type (counting-stream-base stream)))
     
(defmethod stream-read-char ((stream counting-stream))
  (with-slots (base line column last-columns chars) stream
    (let ((char (read-char base nil :eof)))
      (cond ((eq char :eof) :eof)
	    ((char= char #\Newline) (incf line) (setf last-columns column) (setf column 1) (incf chars) char)
	    (t (incf column) (incf chars) char)))))
	   
(defmethod stream-unread-char ((stream counting-stream) char)
  (with-slots (base line column last-columns chars) stream
    (cond ((char= char #\Newline) (decf line) (setf column last-columns) (decf chars))
	  (t (decf column) (decf chars)))
    (unread-char char base)))

(defstruct stream-position
  char
  line 
  column)

(defun counting-stream-position (stream)
  (make-stream-position :char (counting-stream-chars stream) 
			:line (counting-stream-line stream)
			:column (counting-stream-column stream)))






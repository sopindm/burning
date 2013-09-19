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
			 (dbind (sname tag type &key optional repeated) spec
			   `(,sname ,tag ,type ,@(if optional (list :optional)) ,@(if repeated (list :repeated)))))
		   (make-slot-class-definition (spec)
			 (dbind (sname tag type &key optional repeated) spec
			   `(,sname :initarg ,(intern (symbol-name sname) "KEYWORD") 
						,@(if repeated '(:initform ()))
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

(defun message-slot-repeated-p (message slot)
  (let* ((slots (message-slots message))
		 (slot (find slot slots :key #'first)))
	(if (find :repeated slot) t nil)))

(defun message-type-p (type)
  (if (gethash type *messages*)
	  t 
	  nil))

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
	(flet ((%write (value)
			 (multiple-value-bind (value type) (cast-from-protobuf value (first spec) (type-of message))
			   (protobuf-write stream value (second spec) type))))
	  (if (find :repeated spec)
		  (mapc #'%write (slot-value message (first spec)))
		  (%write (slot-value message (first spec)))))))

(defun protobuf-write-message (stream message &optional (with-length t))
  (check-slots message)
  (let ((seq (with-output-to-sequence (output)
			   (let ((slots (message-slots (type-of message))))
				 (mapc (lambda (x) (write-slot output x message)) slots)))))
	(when with-length
	  (write-varint stream (length seq)))
	(write-sequence seq stream)))

(defun read-slot (stream message type)
  (multiple-value-bind (value tag) (protobuf-read stream)
	(let* ((slot (message-tag-slot type tag))
		   (value (cast-to-protobuf value slot type)))
	  (if (message-slot-repeated-p type slot)
		  (push value (slot-value message slot))
		  (setf (slot-value message slot) value)))))

(defun protobuf-read-message (stream type &optional (length -1 length-p))
  (let* ((length (if length-p length (read-varint stream)))
		 (seq (make-array length))
		 (message (make-instance type)))
	(read-sequence seq stream)
	(with-input-from-sequence (input seq)
	  (while (< (file-position input) length)
		(read-slot input message type)))
	(check-slots message)
	(mapc (lambda (spec) (when (message-slot-repeated-p type (first spec))
						   (setf (slot-value message (first spec)) (reverse (slot-value message (first spec))))))
		  (message-slots type))
	message))

(defun string-replace (string c1 c2)
  (let ((p (position c1 string)))
	(if p
		(concatenate 'string
					 (subseq string 0 p)
					 (make-string 1 :initial-element c2)
					 (string-replace (subseq string (1+ p)) c1 c2))
		string)))

(defun translate-type (name type)
  (if (or (message-type-p name) (message-enum-p name type))
	  (remove #\- (string-capitalize (string-downcase (symbol-name name))) :test #'char=)
	  (translate-name name)))

(defun translate-name (name)
  (string-replace (string-downcase (symbol-name name)) #\- #\_))

(defun generate-protocol (path &rest message-types)
  (with-open-file (output path :direction :output :if-does-not-exist :create) 
	(labels ((generate-enum-slot (spec)
			   (let ((name (first spec))
					 (value (second spec)))
				 (format nil "        ~a = ~a;~%"
						 (string-replace (string-upcase (symbol-name name)) #\- #\_)
						 value)))
			 (generate-enum (spec mtype)
			   (let ((name (first spec))
					 (slots (rest spec)))
				 (format nil "    enum ~a {~%~{~a~}    }~%"
						 (translate-type name mtype)
						 (mapcar #'generate-enum-slot slots))))
			 (generate-slot (spec mtype)
			   (let ((name (first spec))
					 (tag (second spec))
					 (type (third spec))
					 (optional-p (find :optional spec))
					 (repeated-p (find :repeated spec)))
				 (format nil "    ~a ~a ~a = ~a;~%"
						 (cond (optional-p "optional") (repeated-p "repeated") (t "required"))
						 (translate-type type mtype)
						 (translate-name name)
						 tag)))
			 (generate-message (type)
			   (format output "message ~a {~%~{~a~}~{~a~}}~%~%"
					   (translate-type type nil)
					   (mapcar (lambda (x) (generate-enum x type)) (message-enums type))
					   (mapcar (lambda (x) (generate-slot x type)) (message-slots type)))))
	  (mapc #'generate-message message-types))))
		
	
  

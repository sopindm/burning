(in-package :burning-ffi)

(defun c-name-to-lisp (name)
  (string-upcase (substitute #\- #\_ name)))

(defclass ffi-object ()
  ((raw-value :initarg :raw-value :reader ffi-object-value)))

(defgeneric delete-object (object))

(defmethod delete-object ((object ffi-object))
  t)

(defun ensure-type (object type)
  (unless (or (typep object type) (null object))
    (error "Cannot convert object of type ~a to type ~a." (type-of object) type))
  object)

(cffi:defcstruct c-ffi-object
  (value :pointer)
  (type :pointer))

(defun make-ffi-object (class object)
  (let ((value (cffi:foreign-slot-value object 'c-ffi-object 'value))
	(type (cffi:foreign-slot-value object 'c-ffi-object 'type)))
    (cffi:foreign-free object)
    (let ((real-class (gethash (get-uuid-from-c type) *uuid-table*)))
      (if real-class
	  (ensure-type (make-instance real-class :raw-value value) class)
	  (make-instance class :raw-value value)))))

(defun def-class-ctype (class-name)
  (eval `(cffi:defctype ,(gensym (symbol-name class-name)) 
	     (:wrapper :pointer
		       :from-c ,#'(lambda (x) 
				    (if (cffi:null-pointer-p x)
					nil
					(make-ffi-object class-name x)))
		       :to-c ,#'(lambda (x)
				  (ensure-type x class-name)
				  (if (null x) (cffi:null-pointer) (ffi-object-value x)))))))

(defaction class (name &rest options)
  (let ((name (intern (c-name-to-lisp name)))
	(base-classes (mapcar #'(lambda (x) (intern (c-name-to-lisp (second x))))
			      (remove-if-not #'(lambda (x) (eq (first x) :inherit)) options))))
    (flet ((parse-constructor (args)
	     (when (or (< (length args) 2) (> (length args) 3))
	       (error "Wrong constructor specification ~a." args))
	     (let ((lisp-name (if (= (length args) 3) (first args)))
		   (c-name (if (= (length args) 3) (second args) (first args)))
		   (arguments (first (last args))))
	       (do-ffi-action :function `(,@(if lisp-name `(,lisp-name)) ,c-name ,name) arguments)))
	   (parse-destructor (args)
	     (when (or (< (length args) 1) (> (length args) 2))
	       (error "Wrong destructor specification ~a." args))
	     (let ((lisp-name (if (= (length args) 2) (first args)))
		   (c-name (first (last args))))
	       (do-ffi-action :function `(,@(if lisp-name `(,lisp-name)) ,c-name :void) `((object ,name))))))
      (eval 
       `(defclass ,name 
	    ,(if (null base-classes) '(ffi-object) base-classes)
	  ()))
      (%add-type name (def-class-ctype name))
      (mapc #'(lambda (x) (parse-constructor (rest x)))
	    (remove-if-not #'(lambda (x) (eq (first x) :constructor)) options))
      (let ((destructors (remove-if-not #'(lambda (x) (eq (first x) :destructor)) options)))
	(when (> (length destructors) 1)
	  (error "Cannot define more than one destructor for class ~a." name))
	(if destructors
	    (let ((destructor-name (parse-destructor (rest (first destructors)))))
	      (eval `(defmethod delete-object ((object ,name))
		       (funcall #',destructor-name object)))))))))

(defmacro with-object ((name init-form) &body body)
  `(let (,name) 
     (unwind-protect 
	  (progn
	    (setf ,name ,init-form)
	    ,@body)
       (delete-object ,name))))

(defmacro with-objects (forms &body body)
  `(let (,@(mapcar #'(lambda (x) `(,(first x) ,(second x))) forms))
     (unwind-protect
	  (progn ,@body)
       ,@(mapcar #'(lambda (x) `(delete-object ,(first x))) forms))))

(defmacro with-objects* (forms &body body)
  (cond
    ((null forms) `(progn ,@body))
    ((null (rest forms)) `(with-object ,(first forms) ,@body))
    (t `(with-object ,(first forms)
	  (with-objects* ,(rest forms)
	    ,@body)))))
;;
;; Class UUID's
;;

(defvar *uuid-table* (make-hash-table))

(defgeneric do-ffi-generate-uuid (action &rest args))

(defmethod do-ffi-generate-uuid (action &rest args)
  (declare (ignore action args))
  nil)

(defmethod do-ffi-generate-uuid ((action (eql ':class)) &rest args)
  (let ((class-name (first (first args))))
    `((:uuid ,class-name ,(generate-uuid)))))

(defun ffi-generate-uuid (&rest actions)
  (reduce 'append (mapcar #'(lambda (x) (do-ffi-generate-uuid (first x) (rest x))) actions)))

(defun generate-uuid-file (filename &rest actions)
  (let ((uuids (apply #'ffi-generate-uuid actions)))
    (cl:with-open-file (file filename :direction :output :if-exists :supersede)
      (mapc #'(lambda (x) (format file "~s~%" x)) uuids))
    (cl:with-open-file (file (concatenate 'string filename ".h") :direction :output :if-exists :supersede)
      (labels ((print-uuid (uuid)
		 (cond 
		   ((null uuid) t)
		   ((null (rest uuid)) (format nil "~a" (first uuid)))
		   (t (concatenate 'string (format nil "~a, " (first uuid)) (print-uuid (rest uuid))))))
	       (print-uuid-string (uuid)
		 (format file "const unsigned char ~a_uuid [] = { ~a };~%" 
			 (second uuid) 
			 (print-uuid (%uuid-int-to-list (third uuid))))))
	(mapc #'print-uuid-string uuids)))))

(defun generate-uuids-from-file (filename uuid-filename)
  (flet ((stream-to-actions (stream)
	   (let (actions)
	     (awhile (read stream nil)
	       (push it actions))
	     (reverse actions))))
    (cl:with-open-file (stream filename)
      (apply #'generate-uuid-file uuid-filename (stream-to-actions stream)))))

(defun load-ffi-uuid (&rest actions)
  (mapc #'(lambda (x) (setf (gethash (third x) *uuid-table*)
			    (intern (c-name-to-lisp (second x))))) actions))

(defun read-uuid-file (filename)
  (cl:with-open-file (file filename :direction :input)
    (do ((action (read file nil) (read file nil))
	 (actions ()))
	((null action) actions)
      (push action actions))))

(defun load-uuid (filename)
  (apply #'load-ffi-uuid (read-uuid-file filename)))


       
       
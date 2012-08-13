(in-package :burning-ffi)

(defun type-name (type)
  (cond
    ((atom type) (symbol-name type))
    ((null (rest type)) (type-name (first type)))
    (t (format nil "~a-~a" (symbol-name (first type)) (type-name (rest type))))))
		   
(defun define-array-type (base-type)
  (eval `(cffi:defctype ,(gensym (concatenate 'string 
					      (type-name base-type)
					      "-ARRAY"))
	     (:wrapper :pointer
		       :from-c ,#'(lambda (x) 
				    (declare (ignore x))
				    (error "Cannot return arrays from c."))
		       :to-c ,#'(lambda (x)
				  (let* ((length (length x))
					 (c-array (cffi:foreign-alloc base-type :count length)))
				    (dotimes (i length)
				      (setf (cffi:mem-aref c-array base-type i) 
					    (cffi:translate-to-foreign (elt x i) base-type)))
				    c-array))))))

(defun make-array-type (base-type)
  (let ((base-type (if (atom base-type) (list base-type) base-type)))
    (let ((type (define-array-type (apply #'get-type base-type))))
      (eval `(defmethod cffi:free-translated-object (value (type (eql ',type)) param)
	       (cffi:foreign-free value)))
      type)))

(defmethod get-type ((name (eql :array)) &rest args)
  (let ((type-name (cons :array (list (first args)))))
    (if (%get-type type-name)
	(%get-type type-name)
      (progn
	(%add-type type-name (make-array-type (first args)))
	(%get-type type-name)))))
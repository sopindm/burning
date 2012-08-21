(defpackage #:burning-ctypes
  (:use #:burning-lisp #:named-readtables)
  (:export burning-ctypes

	   make-type
	   copy-type

	   type-name
	   type-args-list
	   imagine-type-p

	   type=

	   make-type-table
	   copy-type-table
	   type-table-size

	   define-type
	   get-type
	   set-type
	   remove-type
	   
	   with-type-table
	   with-local-type-table

	   make-type-instance
	   instance-type
	   instance-args
	   
	   define-relation
	   call-relation
	   remove-relation

	   define-relation-method
	   remove-relation-method))

(in-package #:burning-ctypes)

(defreadtable burning-ctypes
  (:merge :standard)
  (:dispatch-macro-char #\# #\T (lambda (stream c1 c2) 
				  (declare (ignore c1 c2))
				  (let ((args (read stream t nil t)))
				    `(make-type-instance ',(if (listp args) (first args) args)
							 ,@(if (listp args) (rest args))))))
  (:dispatch-macro-char #\# #\R	(lambda (stream c1 c2)
				  (declare (ignore c1 c2))
				  `(lambda (type1 type2)
				     (call-relation ',(read stream) type1 type2)))))
			  








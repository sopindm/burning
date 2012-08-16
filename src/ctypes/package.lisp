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
	   
	   define-relation
	   define-relation-method
	   have-relation-p))

(in-package #:burning-ctypes)

(defreadtable burning-ctypes
  (:merge :standard)
  (:dispatch-macro-char #\# #\T (lambda (stream c1 c2) 
				  (declare (ignore c1 c2))
				  `(get-type ',(read stream))))
  (:dispatch-macro-char #\# #\R	(lambda (stream c1 c2)
				  (declare (ignore c1 c2))
				  `(lambda (type1 type2)
				     (have-relation-p ',(read stream) type1 type2)))))
			  








(defpackage #:burning-ctypes
  (:use #:burning-lisp)
  (:export make-type
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
	   with-local-type-table))








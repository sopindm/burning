(defpackage #:burning-ctypes
  (:use #:burning-lisp)
  (:export make-type
	   type-arguments
	   type-modifiers
	   type-relations

	   ctypes-undefined-type
	   ctypes-undefined-type-name

	   symbol-type
	   unbind-type

	   make-type-context
	   in-type-context
	   tlet

	   define-type
	   instance-type

	   make-type-block
	   block-addvar
	   block-addexpr
	   block-vartype

	   inherit-types

	   undefined-type
	   true-type
	   false-type
	   and-type))





(defpackage #:burning-cgen
  (:use #:burning-lisp)
  (:export *generator*
	   make-generator

	   *type-table*
	   make-type-table

	   generate-code))

(defpackage #:burning-cgen-source
  (:use)
  (:import-from #:burning-lisp #:in-package)
  (:export defun
	   defvar
	   setf

	   + - * /

	   bool
	   int
	   float

	   cast

	   let

	   if

	   in-package))




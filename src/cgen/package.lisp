(defpackage #:burning-cgen
  (:use #:burning-lisp)
  (:export *generator*
	   make-generator

	   generate-code))

(defpackage #:burning-cgen-source
  (:use)
  (:import-from #:burning-lisp #:in-package)
  (:export defun
	   defvar

	   in-package))




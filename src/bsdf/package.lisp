(defpackage #:burning-bsdf
  (:use #:burning-lisp #:burning-filesystem)
  (:export bsdf-condition
	   bsdf-condition-message

	   bsdf-error
	   bsdf-warning

	   bsdf-compilation-error
	   bsdf-compilation-warning

	   make-target
	   target-name
	   target-command
	   target-input
	   target-output
	   target-depends-on

	   get-target
	   set-target
	   remove-target

	   get-targets

	   *targets*
	   copy-targets-table))



	   

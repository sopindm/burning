(defpackage #:burning-bsdf
  (:use #:burning-lisp #:burning-filesystem)
  (:export bsdf-condition
	   bsdf-condition-message

	   bsdf-error
	   bsdf-warning

	   bsdf-compilation-error
	   bsdf-compilation-warning

	   add-target

	   target-name
	   target-command
	   target-input
	   target-output
	   target-depends-on))

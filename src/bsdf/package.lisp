(defpackage #:burning-bsdf-errors
  (:use #:burning-lisp)
  (:export bsdf-condition
	   bsdf-condition-message

	   bsdf-error
	   bsdf-warning

	   bsdf-compilation-error
	   bsdf-compilation-warning

	   bsdf-compilation-warn))

(defpackage #:burning-bsdf-targets
  (:use #:burning-lisp #:burning-bsdf-errors #:alexandria)
  (:export make-target
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
	   copy-targets-table

	   get-depends

	   map-depends
	   mapc-depends))

(burning-lisp:define-merged-package #:burning-bsdf 
  #:burning-bsdf-errors
  #:burning-bsdf-targets)




	   

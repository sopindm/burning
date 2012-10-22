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
  (:export deftarget
	   make-target
	   target-name
	   target-command
	   target-input
	   target-output
	   target-depends-on
	   target-print-name

	   file-target
	   file-depends

	   get-target
	   set-target

	   add-input
	   add-output
	   add-dependency

	   get-file

	   get-targets

	   *targets*
	   copy-targets-table

	   get-depends

	   map-depends
	   mapc-depends))

(defpackage #:burning-bsdf-generator
  (:use #:burning-lisp #:burning-bsdf-targets #:burning-filesystem)
  (:export *bsdf-generator*

	   generate

	   generator-file-name

	   makefile
	   ninja
	   bsc))

(burning-lisp:define-merged-package #:burning-bsdf 
  #:burning-bsdf-errors
  #:burning-bsdf-targets
  #:burning-bsdf-generator)




	   

(defpackage #:burning-bsdf-errors
  (:use #:burning-lisp)
  (:export bsdf-condition
	   bsdf-condition-message
	   
	   bsdf-condition-format-control
	   bsdf-condition-format-args

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

(defpackage #:burning-bsdf-variables
  (:use #:burning-lisp #:burning-bsdf-errors #:burning-filesystem #:alexandria)
  (:shadowing-import-from #:burning-filesystem #:copy-file)
  (:export variable
	   make-variable
	   variable-name
	   variable-expression
	   variable-type
	   variable-description
	   variable-visible-p

	   variable-value
	   variable-string
	   
	   ++
	   substring))

(defpackage #:burning-bsdf-generator
  (:use #:burning-lisp #:burning-bsdf-targets #:burning-filesystem)
  (:export *bsdf-generator*

	   define-generator

	   define-generator-method

	   define-command-method

	   generator-make-context
	   generator-write-context
	   generator-close-context
	   
	   generator-context-add-target
	   generator-parse-target

	   generate-command

	   generate-file
	   generate-from-file
	   
	   makefile
	   ninja
	   bsc

	   echo
	   echo-command))

(burning-lisp:define-merged-package #:burning-bsdf 
  #:burning-bsdf-errors
  #:burning-bsdf-targets
  #:burning-bsdf-generator
  #:burning-bsdf-variables)

(defpackage #:burning-bsdf-user
  (:use #:burning-lisp #:burning-filesystem #:burning-bsdf))


	   

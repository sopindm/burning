(defpackage #:bsdf-errors
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

(defpackage #:bsdf-variables
  (:use #:burning-lisp #:bsdf-errors #:burning-filesystem #:alexandria)
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
	   
	   expression-value

	   ++
	   substring

	   as-absolute
	   as-relative

	   directory-path
	   
	   defoperation
	   defoperation-macro))

(defpackage #:bsdf-targets
  (:use #:burning-lisp #:bsdf-errors #:alexandria #:bsdf-variables)
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

	   get-variable
	   set-variable
	   defvariable

	   gen-tmp-name
	   free-tmp-name

	   add-input
	   add-output
	   add-dependency

	   get-file

	   get-targets

	   *context*
	   copy-context

	   get-depends

	   map-depends
	   mapc-depends))

(defpackage #:bsdf-generator
  (:use #:burning-lisp #:bsdf-targets #:burning-filesystem)
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

(burning-lisp:define-merged-package #:bsdf 
  #:bsdf-errors
  #:bsdf-targets
  #:bsdf-generator
  #:bsdf-variables)

(defpackage #:bsdf-user
  (:use #:burning-lisp #:burning-filesystem #:bsdf))


	   

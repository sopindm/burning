(defpackage #:bsdf-streams
  (:use #:burning-lisp #:trivial-gray-streams)
  (:export counting-stream
	   make-counting-stream

	   counting-stream-position
	   stream-position-char
	   stream-position-line
	   stream-position-column))

(defpackage #:bsdf-errors
  (:use #:burning-lisp #:bsdf-streams)
  (:export bsdf-condition
	   bsdf-condition-message
	   
	   bsdf-condition-format-control
	   bsdf-condition-format-args

	   bsdf-error
	   bsdf-warning

	   bsdf-compilation-error
	   bsdf-compilation-warning

	   bsdf-warn
	   bsdf-compilation-warn))

(defpackage #:bsdf-expressions
  (:use #:burning-lisp #:bsdf-errors #:burning-filesystem)
  (:export cast-type
	   bsdf-type-p

	   bsdf-check-type

	   expression-type
	   expression-value
	   expression-string
	   expression-dependencies

	   check-expression
	   expand-expression

	   ++
	   substring

	   as-absolute
	   as-relative

	   directory-path

	   bsdf-defmacro

	   with-input-files
	   with-output-files))

(defpackage #:bsdf-variables
  (:use #:burning-lisp #:bsdf-errors #:bsdf-expressions #:alexandria)
  (:shadowing-import-from #:burning-filesystem #:copy-file)
  (:export variable
	   make-variable

	   variable-name
	   variable-expression
	   variable-type))

(defpackage #:bsdf-targets
  (:use #:burning-lisp #:bsdf-errors #:alexandria #:bsdf-expressions #:bsdf-variables)
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

	   getvar
	   $

	   add-input
	   add-output
	   add-dependency

	   get-file

	   get-targets
	   get-variables

	   *context*
	   copy-context

	   get-depends

	   map-depends
	   mapc-depends))

(defpackage #:bsdf-generator
  (:use #:burning-lisp 
	#:burning-filesystem
	#:bsdf-errors
	#:bsdf-expressions
	#:bsdf-variables
	#:bsdf-targets
	#:bsdf-streams)
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
  #:bsdf-streams
  #:bsdf-errors
  #:bsdf-targets
  #:bsdf-generator
  #:bsdf-expressions
  #:bsdf-variables)

(defpackage #:bsdf-user
  (:use #:burning-lisp #:burning-filesystem #:bsdf))


	   

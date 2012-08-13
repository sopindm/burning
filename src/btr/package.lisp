(defpackage #:burning-btr
  (:use #:burning-lisp #:burning-command-line #:burning-filesystem #:burning-xml)

  (:export #:*base-filesystem*
	   #:repository
	   #:make-repository
	   #:repository-version

	   #:entity
	   #:entity-name
	   #:entities

	   #:btr-error
	   #:btr-error-string

	   #:wrong-xml-node-name
	   #:wrong-xml-node-name-expected
	   #:wrong-xml-node-name-got

	   #:missed-xml-attribute-warning
	   #:missed-xml-attribute-warning-name
	   #:missed-xml-attribute-warning-node-name

	   #:add-entity
	   #:remove-entity

	   #:unit
	   #:make-unit
	   #:unit-name
	   #:unit-file
	   #:unit-files

	   #:group
	   #:make-group
	   #:group-name

	   #:entity-with-same-name-already-exists
	   #:entity-with-same-name-already-exists-name

	   #:*repository-class*
	   #:*unit-class*

	   #:define-btr-class
	   #:define-repository-action
	   #:define-action-arguments

	   #:define-repository-function

	   #:define-run-action
	   #:define-run-function
	   #:define-run-class

	   #:repository-path
	   #:write-repository

	   #:btr-run

	   #:no-action-specified-error

	   #:too-much-actions-specified-error
	   #:too-much-actions-specified-error-actions

	   #:no-run-function-error
	   #:no-run-function-error-action

	   #:repository-already-exists-error
	   #:repository-already-exists-error-path

	   #:repository-does-not-exist-error
	   #:repository-does-not-exist-error-path

	   #:path-is-not-in-repository-error
	   #:path-is-not-in-repository-error-path
	   #:path-is-not-in-repository-error-repository-path

	   #:file-is-not-in-repository-error
	   #:file-is-not-in-repository-error-path
	   #:file-is-not-in-repository-error-repository-path
	   
	   #:directory-is-not-empty-error
	   #:directory-is-not-empty-error-path
	   #:directory-is-not-empty-error-repository-path))

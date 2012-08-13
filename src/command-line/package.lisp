(defpackage #:burning-command-line
  (:use #:burning-lisp #:parse-number #:burning-filesystem)
  (:export #:make-argument
	   #:argument-name
	   #:argument-description
	   #:argument-short-name
	   #:argument-group

	   #:make-arguments-spec
	   #:add-argument
	   #:add-arguments
	   #:update-argument
	   #:argument
	   #:have-argument-p

	   #:arguments-list-arguments

	   #:help-message
	   #:parse-arguments
	   #:command-line-arguments
	   #:parse-command-line
	   #:help-set-signal
	   #:help-set-signal-action

	   #:argument-set-p
	   #:argument-value

	   #:group-value

	   #:argument-already-exists-error
	   #:argument-already-exists-error-name

	   #:short-name-already-exists-error
	   #:short-name-already-exists-error-char

	   #:cmd-parsing-error
	   #:cmd-parsing-error-argument
	   #:cmd-parsing-error-message

	   #:wrong-argument-error
	   #:wrong-short-argument-error

	   #:wrong-key-value-error
	   #:wrong-key-value-error-value
	   #:wrong-key-value-error-type

	   #:missed-key-value-error
	   #:missed-key-value-error-type

	   #:too-few-arguments-in-group-set

	   #:too-much-arguments-in-group-set
	   #:too-much-arguments-in-group-set-arguments

	   #:argument-value-too-low-error
	   #:argument-value-too-low-error-min-value

	   #:argument-value-too-high-error
	   #:argument-value-too-high-error-max-value

	   #:tuple

	   #:path

	   #:wrong-path-argument-error
	   #:wrong-file-path-argument-error
	   #:wrong-directory-path-argument-error

	   #:file-path
	   #:directory-path

	   #:existing-path
	   #:existing-file-path
	   #:existing-directory-path

	   #:creatable-path
	   #:creatable-file-path
	   #:creatable-directory-path))


	   
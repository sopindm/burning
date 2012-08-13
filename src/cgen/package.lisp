(defpackage #:burning-cgen
  (:use #:burning-lisp #:alexandria #:trivial-gray-streams #:burning-filesystem)
  (:shadow macroexpand-1 macroexpand)
  (:shadowing-import-from #:burning-filesystem :copy-file)
  (:export cgen-source-code
	   cgen-source-position

	   defgenerator
	   generator-add-source

	   def-cg-macro
	   in-language

	   cgen-error
	   cgen-error-type
	   cgen-error-message
	   cgen-macroexpansion-error

;; c language	   
	   comma
	   for
	   define))



(defpackage #:burning-cgen-test-generated
  (:use #:burning-cgen-source)
  (:export def-empty-function
	   def-fun
	   def-var))

(defpackage #:burning-cgen-test
  (:use #:burning-lisp #:burning-testing #:burning-cgen #:burning-cgen-test-generated)
  (:shadowing-import-from #:burning-cgen-source int float boolean))


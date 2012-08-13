(in-package :asdf)

(defsystem #:burning-cgen-test
    :description "A tests"
    :version "0.1"
    :author "Dmitry Sopin <sopindm@gmail.com>"
    :licence "PRIVATE"
    :serial t
    :components ((:file "test-package")
		 (:file "expander-test")
		 (:file "reader-test")
		 (:module "c"
			  :serial t
			  :components ((:file "macros-test"))))
    :depends-on (#:burning-cgen #:burning-lisp #:alexandria #:burning-testing #:burning-filesystem))


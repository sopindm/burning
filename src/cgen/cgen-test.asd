(in-package :asdf)

(defsystem #:burning-cgen-test
    :description "A tests"
    :version "0.1"
    :author "Dmitry Sopin <sopindm@gmail.com>"
    :licence "PRIVATE"
    :serial t
    :components ((:file "test-package")
		 (:file "generator-test"))
    :depends-on (#:burning-cgen #:burning-lisp #:burning-testing))


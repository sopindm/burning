(in-package #:asdf)

(defsystem #:burning-io-test
    :description "Tests"
    :version "0.1"
    :author "Dmitry Sopin <sopindm@gmail.com>"
    :licence "PRIVATE"
    :serial t
    :components ((:file "test-package")
		 (:file "atoms-test"))
    :depends-on (#:burning-lisp #:burning-testing #:burning-io #:flexi-streams))

(in-package :asdf)

(defsystem #:burning-protobuf-test
    :description "A tests"
    :version "0.1"
    :author "Dmitry Sopin <sopindm@gmail.com>"
    :licence "GPL v3"
    :serial t
    :components ((:file "test-package")
				 (:file "atoms-test"))
	:depends-on (#:burning-lisp #:burning-testing #:burning-protobuf #:flexi-streams))



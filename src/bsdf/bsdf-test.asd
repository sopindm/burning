(in-package :asdf)

(defsystem #:burning-bsdf-test
    :description "tests"
    :version "0.1"
    :author "Dmitry Sopin <sopindm@gmail.com>"
    :licence "PRIVATE"
    :serial t
    :components ((:file "test-package")
		 (:file "errors-test")
		 (:file "targets-test"))
    :depends-on (#:burning-bsdf #:burning-testing))



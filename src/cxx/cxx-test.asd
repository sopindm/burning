(in-package :asdf)

(defsystem burning-cxx-test
    :description "Tests"
    :version "0.1"
    :author "Dmitry Sopin <sopindm@gmail.com>"
    :components ((:file "test-package")
		 (:file "printer-test" :depends-on ("test-package")))
    :depends-on (:burning-testing :burning-cxx))
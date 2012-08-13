(in-package :asdf)

(oos 'load-op 'burning-ffi)

(defsystem burning-ffi-test
    :version "0.1"
    :author "Dmitry Sopin <sopindm@gmail.com>"
    :licence "GPL v3"
    :components ((:file "test-package")
		 (:file "library-test" :depends-on ("test-package"))
		 (:file "function-test" :depends-on ("test-package"))
		 (burning-ffi:ffi-file "simple")
		 (burning-ffi:uuid-file "handler")
		 (:file "object-test" :depends-on ("test-package" "simple" "handler"))
		 (:file "array-test" :depends-on ("test-package")))
    :depends-on (:burning-testing :burning-ffi))
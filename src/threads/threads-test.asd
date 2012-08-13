(in-package :asdf)

(defsystem burning-threads-test
    :description "Tests"
    :version "0.1"
    :author "Dmitry Sopin <sopindm@gmail.com>"
    :components ((:file "test-package")
		 (:file "base-test" :depends-on ("test-package"))
		 (:file "mutex-test" :depends-on ("test-package" "base-test"))
		 #+(or sbcl ccl)
		 (:file "semaphore-test" :depends-on ("test-package" "base-test"))
		 (:file "condition-variable-test" :depends-on ("test-package" "base-test"))
		 (:file "monitor-test" :depends-on ("test-package" "base-test"))
		 (:file "pipe-test" :depends-on ("test-package" "base-test")))
    :depends-on (:burning-testing :burning-threads))


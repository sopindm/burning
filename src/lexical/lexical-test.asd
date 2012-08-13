(in-package :asdf)

(defsystem burning-lexical-test
    :description "Tests"
    :version "0.1"
    :author "Dmitry Sopin <sopindm@gmail.com>"
    :licence "GPL v3"
    :depends-on (:burning-testing :burning-lexical)
    :components ((:file "package-test")
		 (:file "core-test" :depends-on ("package-test"))
		 (:file "regular-language-test" :depends-on ("package-test" "core-test"))
		 (:file "input-test" :depends-on ("package-test"))
		 (:file "state-machine-test" :depends-on ("package-test" "core-test"))))

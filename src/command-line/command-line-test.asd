(in-package #:asdf)

(defsystem #:burning-command-line-test
  :description "A tests"
  :version "0.1"
  :author "Dmitry Sopin <sopindm@gmail.com>"
  :licence "GPL v3"
  :serial t
  :components ((:file "test-package")
	       (:file "base-test")
	       (:file "types-test"))
  :depends-on (:burning-command-line :burning-testing))
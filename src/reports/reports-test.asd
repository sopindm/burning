(in-package :asdf)

(defsystem burning-reports-test
  :description "Tests"
  :version "0.1"
  :author "Dmitry Sopin <sopindm@gmail.com>"
  :licence "GPL v3"
  :components ((:file "test-package")
	       (:file "report-test" :depends-on ("test-package")))
  :depends-on (:burning-reports :burning-xml :burning-testing))
(in-package :asdf)

(defsystem burning-xml-test
  :author "Dmitry Sopin <sopindm@gmail.com>"
  :licence "GPL v3"
  :components ((:file "test-package")
	       (:file "core-test" :depends-on ("test-package")))
  :depends-on (:burning-xml :burning-testing))
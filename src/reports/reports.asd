(in-package :asdf)

(defsystem burning-reports
  :description "A reports analyzing tool"
  :version "0.1"
  :author "Dmitry Sopin <sopindm@gmail.com>"
  :licence "GPL v3"
  :components ((:file "package")
	       (:file "report" :depends-on ("package")))
  :depends-on (:burning-xml))
(in-package #:asdf)

(defsystem #:burning-btr
  :description "A repository testing tool"
  :version "0.1"
  :author "Dmitry Sopin <sopindm@gmail.com>"
  :licence "GPL v3"
  :serial t
  :components ((:file "package")
	       (:file "repository")
	       (:file "filesystem")
	       (:file "actions"))
  :depends-on (#:burning-command-line #:burning-filesystem #:burning-xml))

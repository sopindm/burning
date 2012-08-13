(in-package #:asdf)

(defsystem #:burning-command-line
  :description "A command line arguments parser"
  :version "0.1"
  :author "Dmitry Sopin <sopindm@gmail.com>"
  :licence "GPL v3"
  :serial t
  :components ((:file "package")
	       (:file "types")
	       (:file "base"))
  :depends-on (#:parse-number #:burning-filesystem))

(in-package :asdf)

(defsystem #:burning-protobuf
    :description "A protobuf common lisp implementation"
    :version "0.1"
    :author "Dmitry Sopin <sopindm@gmail.com>"
    :licence "GPL v3"
    :serial t
    :components ((:file "package")
				 (:file "atoms"))
	:depends-on (#:burning-lisp))



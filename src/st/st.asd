(in-package :asdf)

(defsystem #:burning-st
    :description "A top secret project"
    :version "0.1"
    :author "Dmitry Sopin <sopindm@gmail.com>"
    :licence "GPL v3"
    :serial t
    :components ((:file "package")
				 (:file "protocol")
				 (:file "server")
				 (:file "game"))
	:depends-on (#:burning-lisp #:burning-protobuf #:usocket #:burning-threads))



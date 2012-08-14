(in-package :asdf)

(defsystem #:burning-ctypes
    :description "An universal type system"
    :version "0.1"
    :author "Dmitry Sopin <sopindm@gmail.com>"
    :licence "GPL v3"
    :serial t
    :components ((:file "package")
		 (:file "types"))
    :depends-on (#:burning-lisp))


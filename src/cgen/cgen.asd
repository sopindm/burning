(in-package :asdf)

(defsystem #:burning-cgen
    :description "A C code generator"
    :version "0.1"
    :author "Dmitry Sopin <sopindm@gmail.com>"
    :licence "GPL v3"
    :serial t
    :components ((:file "package")
		 (:file "generator"))
    :depends-on (#:burning-lisp))
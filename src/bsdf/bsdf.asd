(in-package :asdf)

(defsystem #:burning-bsdf
    :description "Burning system definition and building system."
    :version "0.1"
    :author "Dmitry Sopin <sopindm@gmail.com>"
    :licence "GPL v3"
    :serial t
    :components ((:file "package")
		 (:file "errors")
		 (:file "targets")
		 (:file "generator")
		 (:file "makefile-generator"))
    :depends-on (#:burning-lisp #:burning-filesystem))



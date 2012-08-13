(in-package :asdf)

(defsystem #:burning-cgen
    :description "A C code generator"
    :version "0.1"
    :author "Dmitry Sopin <sopindm@gmail.com>"
    :licence "GPL v3"
    :serial t
    :components ((:file "package")
		 (:file "reader")
		 (:file "core")
		 (:file "errors")
		 (:file "expander")
		 (:module "c"
			  :serial t
			  :components ((:file "macros")
				       (:file "generator"))))
    :depends-on (#:burning-lisp #:alexandria #:trivial-gray-streams #:burning-filesystem))


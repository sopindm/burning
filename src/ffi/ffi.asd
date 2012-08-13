(in-package :asdf)

(defsystem burning-ffi
    :description "Library for operating with C-side functions."
    :version "0.1"
    :author "Dmitry Sopin <sopindm@gmail.com>"
    :licence "GPL v3"
    :serial t
    :components ((:file "package")
		 (:file "core")
		 (:file "common-types")
		 (:file "function")
		 (:file "object")
		 (:file "array")
		 (:file "asdf-classes"))
    :depends-on (:burning-lisp :cffi))


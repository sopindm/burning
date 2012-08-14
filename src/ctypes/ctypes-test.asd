(in-package :asdf)

(defsystem #:burning-ctypes-test
    :description "An universal type system"
    :version "0.1"
    :author "Dmitry Sopin <sopindm@gmail.com>"
    :licence "GPL v3"
    :serial t
    :components ((:file "test-package")
		 (:file "types-test"))
    :depends-on (#:burning-ctypes #:burning-lisp #:burning-testing))


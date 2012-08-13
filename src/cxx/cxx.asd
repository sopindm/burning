(in-package :asdf)

(defsystem burning-cxx
    :description "A burning c/c++ code generator"
    :version "0.1"
    :author "Dmitry Sopin <sopindm@gmail.com>"
    :components ((:file "package")
		 (:file "printer" :depends-on ("package"))))
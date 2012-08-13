(in-package :asdf)

(defsystem burning-threads
    :description "Cross-implementation library for threading routines"
    :version "0.1"
    :author "Dmitry Sopin <sopindm@gmail.com>"
    :licence "GPL v3"
    :components ((:file "package")
		 (:file "base" :depends-on ("package"))
		 (:file "mutex" :depends-on ("package" "base"))
		 #+(or sbcl ccl)
		 (:file "semaphore" :depends-on ("package" "base"))
		 (:file "condition-variable" :depends-on ("package" "base" "mutex"))
		 (:file "monitor" :depends-on ("package" "mutex" "condition-variable"))
		 (:file "pipe" :depends-on ("package" "monitor"))))


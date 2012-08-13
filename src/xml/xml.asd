(in-package :asdf)

(defsystem burning-xml
  :description "A library to work with xml data."
  :version "0.1"
  :author "Dmitry Sopin <sopindm@gmail.com>"
  :licence "GPL v3"
  :components ((:file "package")
	       (:file "lexic" :depends-on ("package"))
	       (:file "core" :depends-on ("package" "lexic")))
  :depends-on (:burning-lexical :burning-syntax :parse-number))
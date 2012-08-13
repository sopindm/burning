(defpackage :burning-xml
  (:use :common-lisp :burning-lexical :burning-syntax :parse-number)
  (:export xml-node
	   make-xml-node
	   xml-node-name
	   xml-attributes
	   xml-attribute
	   xml-attribute-p
	   xml-remove-attribute
	   xml-print
	   xml-childs
	   xml-find-childs
	   xml-add-child
	   xml-remove-child
	   parse-xml))


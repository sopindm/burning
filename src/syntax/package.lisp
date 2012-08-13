(defpackage :burning-syntax
  (:use :common-lisp :burning-lexical)
  (:export make-rule
	   rule-productions
	   rule-result
	   aux-rules

	   make-grammar
	   grammar-productions
	   grammar-terminals
	   grammar-non-terminals
	   nullable-p
	   production-first
	   defgrammar
	   
	   make-lr-table
	   lr-parser
	   make-lr-parser
	   parser-next
	   parser-value
	   parse-input))
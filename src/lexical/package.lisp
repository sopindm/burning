(defpackage :burning-lexical
  (:use :common-lisp)
  (:export make-iterator
	   eof-p
	   with-input-iterator
	   get-next
	   commit
	   reset

	   make-node

	   oequal
	   character-node
	   range-node
	   final-node
	   empty-node
	   star-node
	   left-node
	   right-node
	   and-node
	   or-node
	   node=

	   parse-regular
	   
	   to-range-set
	   lexeme
	   make-lexeme
	   lexeme=
	   lexeme-name
	   lexeme-expression
	   deflexeme
	   en
	   ru

	   eps

	   lexic
	   nullable
	   first-pos
	   last-pos

	   make-lexic
	   deflexic
	   lexic-expression
	   lexic-translation
	   lexic-follows
	   lexic-values
	   lexic-nexts
	   
	   create-state-machine
	   values
	   transitions
	   machine-value
	   get-token
	   get-lexeme
	   print-stream))
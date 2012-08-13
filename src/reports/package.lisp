(defpackage :burning-reports
  (:use :common-lisp :burning-xml)
  (:export make-phase
	   make-iteration 
	   phase=
	   parse-phase
	   parse-loop
	   phase-goto
	   get-table))



(defvar *test-systems* '(
			 burning-testing-test
			 burning-lexical-test
;			 burning-syntax-test
			 ))

(progn (with-output-to-string (*standard-output*)
	 (with-output-to-string (*trace-output*)
	   (mapcar #'(lambda (system) (asdf:oos 'asdf:load-op system)) *test-systems*)))
       nil)

(burning-testing:run-tests)
(quit)
  


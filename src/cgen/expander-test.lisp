(in-package #:burning-cgen-test)

(in-case expander-test)
(in-language :none)

(defmacro def-expander-test (name &body body)
  `(deftest ,name
     (in-language :none)
     ,@body))

(def-expander-test default-language
  (!eq burning-cgen::*cgen-language* :none))

(def-cg-macro simple-macro (a b c)
  (list a b c))

(def-expander-test expand-simple-macro 
  (!equal (burning-cgen::macroexpand '(simple-macro 1 a d))
	  '(1 a d)))

(def-expander-test recursive-macros
  (!equal (burning-cgen::macroexpand '(simple-macro 0 (simple-macro 1 2 3) (call (simple-macro 4 5 6))))
	  '(0 (1 2 3) (call (4 5 6)))))

(def-cg-macro macro2 (a)
  `(simple-macro ,a ,a ,a))

(def-expander-test suquenced-expansion
  (!equal (burning-cgen::macroexpand '(macro2 (simple-macro 1 2 3)))
	  '((simple-macro 1 2 3) (1 2 3) (1 2 3))))

(def-expander-test macrolet-expansion
  (!equal (burning-cgen::macroexpand '(macrolet ((a (b) `(macro2 ,b))) (a 4) 5 6))
	  '(progn (4 4 4) 5 6)))

(def-expander-test macrolet-locality
  (!equal (burning-cgen::macroexpand '(progn (macrolet ((a (b) `(macro2 ,b))) (a 4) 5 6) (a 7)))
	  '(progn (progn (4 4 4) 5 6) (a 7))))

(def-expander-test macrolet-local-context
  (!equal (burning-cgen::macroexpand '(macrolet ((macro2 (a) `(,a 1 2 3))) (macro2 simple-macro)))
	  '(1 2 3)))

(def-expander-test symbol-macrolet-expansion
  (!equal (burning-cgen::macroexpand '(symbol-macrolet ((a `(simple-macro 1 2 3))) a))
	  '(1 2 3)))

(def-expander-test symbol-macrolet-locality
  (!equal (burning-cgen::macroexpand '(progn (symbol-macrolet ((a '(simple-macro 1 2 3)))
					       a)
				       a))
	  '(progn (1 2 3) a)))

(def-expander-test symbol-macrolet-local-context
  (!equal (burning-cgen::macroexpand '(symbol-macrolet ((a '(simple-macro 1 2 3))
							(b '(simple-macro 4 5 6)))
				       a
				       (symbol-macrolet ((a '(macro2 b)))
					 a)))
	  '(progn (1 2 3) (b (4 5 6) (4 5 6)))))

(burning-cgen::defexpander define (symbol &optional value) `(define ,symbol (:expand ,value)))

(def-expander-test custom-expander
  (!equal (burning-cgen::macroexpand '(symbol-macrolet ((a '(bla))) (define a (bla))))
	  '(define a (bla)))
  (!equal (burning-cgen::macroexpand '(macrolet ((bla () '(1 2 3))) (define a (bla))))
	  '(define a (1 2 3)))
  (!equal (burning-cgen::macroexpand '(macrolet ((bla () '(1 2 3))) (define a (b (bl (bla)) (bla)))))
	  '(define a (b (bl (1 2 3)) (1 2 3)))))

(def-expander-test error-test
  (!condition (burning-cgen::macroexpand '(macrolet ((bla () (error "Wrong macro"))) (bla)))
	      cgen-macroexpansion-error
	      (cgen-error-message (lines "CGen macroexpansion error: Wrong macro") :test equal)))

(def-expander-test vfs-expansion 
  (let ((*default-filesystem* (make-virtual-filesystem)))
    (write-file (path-from-string "a.file") (lines "(in-package #:burning-cgen)"
						   "(in-language :none)" 
						   "(def-cg-macro wrong-macro ()"
						   "  (error \"Another wrong macro\"))"
						   ""
						   "#G"
						   "(defun example ()"
						   "  (wrong-macro))"))
    (!condition (burning-cgen::generate-source "a.file")
		cgen-macroexpansion-error
		(cgen-error-message (lines "CGen macroexpansion error: Another wrong macro"
					   "in lines 7 to 8") :test lines=))))

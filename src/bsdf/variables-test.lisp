(in-package #:bsdf-test)

(in-case variables-test)

(deftest making-variables
  (let ((var (make-variable 'var "123")))
    (?eq (variable-name var) 'var)
    (?equal (variable-expression var) "123")
    (?eq (variable-type var) t)))

(deftest wrong-variables-name-error
  (?bsdf-compilation-error (make-variable 123 1)
			   (lines "Wrong variable name '123'")))

(deftest wrong-variable-expressions-error
  (?bsdf-compilation-error (make-variable 'var "123" :type :list)
			   (lines "In definition of variable '~a':"
				  "Cannot convert ~a from type ~a to ~a")
			   'var "123" :string :list)
  (?bsdf-compilation-error (make-variable 'var2 #(1 2 3))
			   (lines "In definition of variable '~a':"
				  "Unknown BSDF type for ~a")
			   'var2 #(1 2 3)))

(bsdf-defmacro ct-+ (a b)
  (+ a b))

(deftest variables-expansion-test
  (let ((var (make-variable 'var '(* (ct-+ 1 2) 3))))
    (?equal (variable-expression var) '(* 3 3))))


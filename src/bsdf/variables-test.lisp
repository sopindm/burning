(in-package #:bsdf-test)

(in-case variables-test)

(deftest making-variables
  (let ((var (make-variable "var" "123")))
    (?equal (variable-name var) "var")
    (?equal (variable-expression var) "123")
    (?equal (variable-description var) "")
    (?eq (variable-type var) t)
    (?null (variable-visible-p var)))
  (let ((var (make-variable "var2" "456" :description "sample variable")))
    (?equal (variable-description var) "sample variable"))
  (let ((var (make-variable "var3" "789" :visible-p t)))
    (?t (variable-visible-p var))))

(deftest wrong-variables-name-error
  (make-variable "var" 1)
  (make-variable 'var 2)
  (?bsdf-compilation-error (make-variable 123 1)
			   (lines "Wrong variable name '123'")))

(deftest wrong-variable-expressions-error
  (?bsdf-compilation-error (make-variable "var" "123" :type :list)
			   (lines "In definition of variable 'var':"
				  "Cannot convert ~a from type ~a to ~a")
			   "123" :string :list)
  (?bsdf-compilation-error (make-variable "var2" #(1 2 3))
			   (lines "In definition of variable 'var2':"
				  "Unknown BSDF type for ~a")
			   #(1 2 3)))




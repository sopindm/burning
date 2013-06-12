(in-package #:burning-cgen-test)

(in-case generator-test)

(in-package #:burning-cgen-test)

(defmacro def-generator-test (name &body body)
  `(deftest ,name 
     (let ((*generator* (make-generator)))
       ,@body)))

(defmacro cg-defun (name (&rest args) &body body)
  (let ((symbol (make-symbol (symbol-name name))))
    `(progn (burning-cgen-source:defun ,symbol (,@args)
	      ,@body)
	    ',symbol)))

(defmacro cg-defvar (name value)
  (let ((symbol (make-symbol (symbol-name name))))
    `(progn (burning-cgen-source:defvar ,symbol ,value)
	    ',symbol)))

(def-generator-test empty-function-generation
  (cg-defun empty-function ())
  (?lines= (generate-code)
	   (lines "empty_function ()"
		  "{"
		  "}")))

(def-generator-test generated-function-names
  (cg-defun other-empty-function ())
  (?lines= (generate-code)
	   (lines "other_empty_function ()"
		  "{"
		  "}")))

(def-generator-test generating-simple-variables
  (cg-defvar a-var 123)
  (?lines= (generate-code)
	   (lines "int a_var = 123")))

(def-generator-test generating-with-several-definitions
  (cg-defvar a-var 222)
  (cg-defun my-function ())
  (?lines= (generate-code)
	   (lines "int a_var = 222"
		  ""
		  "my_function ()"
		  "{"
		  "}")))

(def-generator-test defvar-with-float-type
  (cg-defvar a-var 1.23)
  (?lines= (generate-code)
	   (lines "float a_var = 1.23")))

(def-generator-test defvar-with-variable-value
  (let* ((var1 (cg-defvar a-var 1.23))
	 (var2 (cg-defvar b-var 234)))
    (eval `(cg-defvar c-var ,var1))
    (eval `(cg-defvar d-var ,var2))
    (?lines= (generate-code)
	     (lines "float a_var = 1.23"
		    "int b_var = 234"
		    "float c_var = a_var"
		    "int d_var = b_var"))))

;defvar with variable type
;defvar with other variables in value
;defvar with expressions in value

(def-generator-test defining-function-with-body
  (cg-defun simple-fun () 42)
  (?lines= (generate-code)
	   (lines "simple_fun ()"
		  "{"
		  "  42"
		  "}")))

(def-generator-test defining-functions-with-args
  (cg-defun one-more-fun (a int b int c int) 123)
  (?lines= (generate-code)
	   (lines "one_more_fun (int a, int b, int c)"
		  "{"
		  "  123"
		  "}")))

(def-generator-test defining-function-returing-argument
  (cg-defun func-returning-arg (a int) a)
  (?lines= (generate-code)
	   (lines "func_returning_arg (int a)"
		  "{"
		  "  a"
		  "}")))

(defun cg-+ (&rest args) (apply #'burning-cgen-source:+ args))
(defun cg-- (&rest args) (apply #'burning-cgen-source:- args))
(defun cg-* (&rest args) (apply #'burning-cgen-source:* args))
(defun cg-/ (&rest args) (apply #'burning-cgen-source:/ args))

(def-generator-test simple-ariphmetic-functions
  (cg-defun simple-plus-function (a int b int) (cg-+ a b))
  (cg-defun simple-minus-function (a int b int) (cg-- a b))
  (cg-defun simple-multiply-function (a int b int) (cg-* a b))
  (cg-defun simple-divide-function (a int b int) (cg-/ a b))
  (cg-defun complex-ariphmetic-function (a int b int) (cg-+ a (cg-- b a)))
  (?lines= (generate-code)
	   (lines "simple_plus_function (int a, int b)"
		  "{"
		  "  a + b"
		  "}"
		  ""
		  "simple_minus_function (int a, int b)"
		  "{"
		  "  a - b"
		  "}"
		  ""
		  "simple_multiply_function (int a, int b)"
		  "{"
		  "  a * b"
		  "}"
		  ""
		  "simple_divide_function (int a, int b)"
		  "{"
		  "  a / b"
		  "}"
		  ""
		  "complex_ariphmetic_function (int a, int b)"
		  "{"
		  "  a + b - a"
		  "}")))

(def-generator-test ariphmetic-functions-with-multiple-args
  (cg-defun multiple-ariphmetic-function () (cg-+ 1 (cg-* 2 4 6 8) 3 (cg-/ 4 7 10) (cg-- 5 3 1) 6))
  (?lines= (generate-code)
	   (lines "multiple_ariphmetic_function ()"
		  "{"
		  "  1 + 2 * 4 * 6 * 8 + 3 + 4 / 7 / 10 + 5 - 3 - 1 + 6"
		  "}")))

(defmacro cg-let ((&rest bindings) &body body)
  `(burning-cgen-source:let (,@bindings) ,@body))

(def-generator-test local-variable-bindings-test
  (cg-defun function-with-local-bindings (a int)
    (cg-let ((b 1) (c 2))
      (cg-+ (cg-* b a) c)))
  (?string= (generate-code)
	   (lines "function_with_local_bindings (int a)"
		  "{"
		  "  {"
		  "    b = 1"
		  "    c = 2"
		  "    b * a + c"
		  "  }"
		  "}")))

(defmacro cg-if (expr then-form &optional else-form)
  `(burning-cgen-source:if ,expr ,then-form ,@(aif else-form (list it))))

(def-generator-test generating-if-forms
  (cg-defun function-with-if (a bool b int)
    (cg-if a b))
  (burning-cgen-source:defun function-with-if-and-else (a bool b int c int)
    (cg-if a (cg-+ b 1) (cg-+ c 2)))
  (?string= (generate-code)
	    (lines "function_with_if (bool a, int b)"
		   "{"
		   "  if( a )"
		   "  {"
		   "    b"
		   "  }"
		   "}"
		   ""
		   "function_with_if_and_else (bool a, int b, int c)"
		   "{"
		   "  if( a )"
		   "  {"
		   "    b + 1"
		   "  }"
		   "  else"
		   "  {"
		   "    c + 2"
		   "  }"
		   "}")))

(def-generator-test calling-functions
  (let ((callee (cg-defun empty-function ())))
    (flet ((form (arg)
	     (eval `(cg-defun function-calling-function () (,arg)))))
      (form callee)))
  (?string= (generate-code)
	    (lines "empty_function ()"
		   "{"
		   "}"
		   ""
		   "function_calling_function ()"
		   "{"
		   "  empty_function()"
		   "}")))

(def-generator-test calling-functions-with-arguments
  (burning-cgen-source:defun function-calling-function-with-arguments (a int b int)
    (function-with-if-and-else a b 0))
  (?string= (generate-code)
	    (lines "function_calling_function_with_arguments (int a, int b)"
		   "{"
		   "  function_with_if_and_else(a, b, 0)"
		   "}")))

(def-generator-test integer-variable-type
  (burning-cgen-source:defun function-with-int-arg (a int)
    (cg-+ a 1))
  (?string= (generate-code)
	    (lines "function_with_int_arg (int a)"
		   "{"
		   "  a + 1"
		   "}")))
    


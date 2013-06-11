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
	   (lines "a_var = 123")))

(def-generator-test generating-with-several-definitions
  (cg-defvar a-var 222)
  (cg-defun my-function ())
  (?lines= (generate-code)
	   (lines "a_var = 222"
		  ""
		  "my_function ()"
		  "{"
		  "}")))

(def-generator-test defining-function-with-body
  (cg-defun simple-fun () 42)
  (?lines= (generate-code)
	   (lines "simple_fun ()"
		  "{"
		  "  42"
		  "}")))

(def-generator-test defining-functions-with-args
  (cg-defun one-more-fun (a b c) 123)
  (?lines= (generate-code)
	   (lines "one_more_fun (a, b, c)"
		  "{"
		  "  123"
		  "}")))

(def-generator-test defining-function-returing-argument
  (cg-defun func-returning-arg (a) a)
  (?lines= (generate-code)
	   (lines "func_returning_arg (a)"
		  "{"
		  "  a"
		  "}")))

(defun cg-+ (a b) (burning-cgen-source:+ a b))
(defun cg-- (a b) (burning-cgen-source:- a b))
(defun cg-* (a b) (burning-cgen-source:* a b))
(defun cg-/ (a b) (burning-cgen-source:/ a b))

(def-generator-test simple-ariphmetic-functions
  (cg-defun simple-plus-function (a b) (cg-+ a b))
  (cg-defun simple-minus-function (a b) (cg-- a b))
  (cg-defun simple-multiply-function (a b) (cg-* a b))
  (cg-defun simple-divide-function (a b) (cg-/ a b))
  (cg-defun complex-ariphmetic-function (a b) (cg-+ a (cg-- b a)))
  (?lines= (generate-code)
	   (lines "simple_plus_function (a, b)"
		  "{"
		  "  a + b"
		  "}"
		  ""
		  "simple_minus_function (a, b)"
		  "{"
		  "  a - b"
		  "}"
		  ""
		  "simple_multiply_function (a, b)"
		  "{"
		  "  a * b"
		  "}"
		  ""
		  "simple_divide_function (a, b)"
		  "{"
		  "  a / b"
		  "}"
		  ""
		  "complex_ariphmetic_function (a, b)"
		  "{"
		  "  a + b - a"
		  "}")))

;ariphmetic with multiple args

(defmacro cg-let ((&rest bindings) &body body)
  `(burning-cgen-source:let (,@bindings) ,@body))

(def-generator-test local-variable-bindings-test
  (cg-defun function-with-local-bindings (a)
    (cg-let ((b 1) (c 2))
      (cg-+ (cg-* b a) c)))
  (?string= (generate-code)
	   (lines "function_with_local_bindings (a)"
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
  (cg-defun function-with-if (a b)
    (cg-if a b))
  (burning-cgen-source:defun function-with-if-and-else (a b c)
    (cg-if a (cg-+ b 1) (cg-+ c 2)))
  (?string= (generate-code)
	    (lines "function_with_if (a, b)"
		   "{"
		   "  if( a )"
		   "  {"
		   "    b"
		   "  }"
		   "}"
		   ""
		   "function_with_if_and_else (a, b, c)"
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

#|
(def-generator-test calling-functions-with-arguments
  (burning-cgen-source:defun function-calling-function-with-arguments (a b)
    (function-with-if-and-else a b 0))
  (?string= (generate-code)
	    (lines "function_calling_function_with_arguments (a, b)"
		   "{"
		   "  function_with_if_and_else(a, b, 0)"
		   "}")))
|#      

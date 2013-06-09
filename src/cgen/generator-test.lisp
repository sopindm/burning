(in-package #:burning-cgen-test)

(in-case generator-test)

(in-package #:burning-cgen-test-generated)

(cl:defmacro def-empty-function (name)
  `(defun ,name ()))

(cl:defmacro def-fun (name () cl:&body body)
  `(defun ,name () ,@body))

(cl:defmacro def-var (name value)
  `(defvar ,name ,value))

(in-package #:burning-cgen-test)

(defmacro def-generator-test (name &body body)
  `(deftest ,name 
     (let ((*generator* (make-generator)))
       ,@body)))

(def-generator-test empty-function-generation
  (def-empty-function empty-function)
  (?lines= (generate-code)
	   (lines "empty_function ()"
		  "{"
		  "}")))

(def-generator-test generated-function-names
  (def-empty-function other-empty-function)
  (?lines= (generate-code)
	   (lines "other_empty_function ()"
		  "{"
		  "}")))

(def-generator-test generating-simple-variables
  (def-var a-var 123)
  (?lines= (generate-code)
	   (lines "a_var = 123")))

(def-generator-test generating-with-several-definitions
  (def-var a-var 222)
  (def-empty-function my-function)
  (?lines= (generate-code)
	   (lines "a_var = 222"
		  ""
		  "my_function ()"
		  "{"
		  "}")))

(def-generator-test defining-function-with-body
  (burning-cgen-source::defun simple-fun () 42)
  (?lines= (generate-code)
	   (lines "simple_fun ()"
		  "{"
		  "  42"
		  "}")))

(def-generator-test defining-functions-with-args
  (burning-cgen-source::defun one-more-fun (a b c) 123)
  (?lines= (generate-code)
	   (lines "one_more_fun (a, b, c)"
		  "{"
		  "  123"
		  "}")))

(def-generator-test defining-function-returing-argument
  (burning-cgen-source::defun func-returning-arg (a) a)
  (?lines= (generate-code)
	   (lines "func_returning_arg (a)"
		  "{"
		  "  a"
		  "}")))

(def-generator-test simple-ariphmetic-functions
  (burning-cgen-source:defun simple-plus-function (a b) (burning-cgen-source:+ a b))
  (burning-cgen-source:defun simple-minus-function (a b) (burning-cgen-source:- a b))
  (burning-cgen-source:defun simple-multiply-function (a b) (burning-cgen-source:* a b))
  (burning-cgen-source:defun simple-divide-function (a b) (burning-cgen-source:/ a b))
  (burning-cgen-source:defun complex-ariphmetic-function (a b) (burning-cgen-source:+ a (burning-cgen-source:- b a)))
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

(def-generator-test local-variable-bindings-test
  (burning-cgen-source:defun function-with-local-bindings (a)
    (burning-cgen-source:let ((b 1) (c 2))
      (burning-cgen-source:+ (burning-cgen-source:* b a) c)))
  (?string= (generate-code)
	   (lines "function_with_local_bindings (a)"
		  "{"
		  "  {"
		  "    b = 1"
		  "    c = 2"
		  "    b * a + c"
		  "  }"
		  "}")))

(def-generator-test generating-if-forms
  (burning-cgen-source:defun function-with-if (a b)
    (burning-cgen-source:if a b))
  (burning-cgen-source:defun function-with-if-and-else (a b c)
    (burning-cgen-source:if a (burning-cgen-source:+ b 1) (burning-cgen-source:+ c 2)))
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
      

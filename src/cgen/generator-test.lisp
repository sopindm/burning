(in-package #:burning-cgen-test)

(in-case generator-test)

(in-package #:burning-cgen-test)

(defmacro def-generator-test (name &body body)
  `(deftest ,name 
     (let ((*generator* (make-generator))
	   (*type-table* (make-type-table)))
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
	   (lines "void empty_function ()"
		  "{"
		  "}")))

(def-generator-test generated-function-names
  (cg-defun other-empty-function ())
  (?lines= (generate-code)
	   (lines "void other_empty_function ()"
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
		  "void my_function ()"
		  "{"
		  "}")))

(def-generator-test defvar-with-float-type
  (cg-defvar a-var 1.23)
  (?lines= (generate-code)
	   (lines "float a_var = 1.23")))

(def-generator-test defvar-with-bool-type
  (cg-defvar a-var nil)
  (cg-defvar b-var t)
  (?lines= (generate-code)
	   (lines "boolean a_var = false"
		  "boolean b_var = true")))

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

(def-generator-test defining-function-with-body
  (cg-defun simple-fun () 42)
  (?lines= (generate-code)
	   (lines "int simple_fun ()"
		  "{"
		  "  return 42"
		  "}")))

(def-generator-test defining-functions-with-args
  (cg-defun one-more-fun (a int b int c int) 123)
  (?lines= (generate-code)
	   (lines "int one_more_fun (int a, int b, int c)"
		  "{"
		  "  return 123"
		  "}")))

(def-generator-test defining-function-returing-argument
  (cg-defun func-returning-arg (a int) a)
  (?lines= (generate-code)
	   (lines "int func_returning_arg (int a)"
		  "{"
		  "  return a"
		  "}")))

(defun cg-+ (&rest args) (apply #'burning-cgen-source:+ args))
(defun cg-- (&rest args) (apply #'burning-cgen-source:- args))
(defun cg-* (&rest args) (apply #'burning-cgen-source:* args))
(defun cg-/ (&rest args) (apply #'burning-cgen-source:/ args))

(def-generator-test simple-ariphmetic-functions
  (cg-defun simple-plus-function (a int b int) (cg-+ a b))
  (cg-defun simple-minus-function (a int b int) (cg-- a b))
  (cg-defun simple-multiply-function (a int b int) (cg-* a b))
  (cg-defun simple-divide-function (a float b int) (cg-/ a b))
  (cg-defun complex-ariphmetic-function (a int b int) (cg-+ a (cg-- b a)))
  (?lines= (generate-code)
	   (lines "int simple_plus_function (int a, int b)"
		  "{"
		  "  return a + b"
		  "}"
		  ""
		  "int simple_minus_function (int a, int b)"
		  "{"
		  "  return a - b"
		  "}"
		  ""
		  "int simple_multiply_function (int a, int b)"
		  "{"
		  "  return a * b"
		  "}"
		  ""
		  "float simple_divide_function (float a, int b)"
		  "{"
		  "  return a / b"
		  "}"
		  ""
		  "int complex_ariphmetic_function (int a, int b)"
		  "{"
		  "  return a + b - a"
		  "}")))

(def-generator-test ariphmetic-functions-with-multiple-args
  (cg-defun multiple-ariphmetic-function () (cg-+ 1 (cg-* 2 4 6 8) 3 (cg-/ 4.0 7 10) (cg-- 5 3 1) 6))
  (?lines= (generate-code)
	   (lines "float multiple_ariphmetic_function ()"
		  "{"
		  "  return 1 + 2 * 4 * 6 * 8 + 3 + 4.0 / 7 / 10 + 5 - 3 - 1 + 6"
		  "}")))

(defun cg-cast (value type)
  (burning-cgen-source:cast value type))

(defvar cg-float 'burning-cgen-source:float)

(def-generator-test cast-expressions
  (cg-defvar a-var (cg-cast 1 cg-float))
  (cg-defvar b-var (cg-cast (cg-+ 1 (cg-* 2 3)) cg-float))
  (?lines= (generate-code)
	   (lines "float a_var = type_cast<float>( 1 )"
		  "float b_var = type_cast<float>( 1 + 2 * 3 )")))

(def-generator-test defvars-with-expressions
  (let* ((var1 (cg-defvar a-var (cg-+ 1 2 3)))
	 (var2 (cg-defvar b-var (cg-- 1 1.5))))
    (eval `(cg-defvar c-var (cg-* ,var1 5)))
    (cg-defvar d-var (cg-/ 1 2)))
  (?lines= (generate-code)
	   (lines "int a_var = 1 + 2 + 3"
		  "float b_var = 1 - 1.5"
		  "int c_var = a_var * 5"
		  "float d_var = type_cast<float>( 1 ) / 2")))

(defmacro cg-let ((&rest bindings) &body body)
  `(burning-cgen-source:let (,@bindings) ,@body))

(def-generator-test local-variable-bindings-test
  (cg-defun function-with-local-bindings (a int)
    (cg-let ((b 1) (c 2))
      (cg-+ (cg-* b a) c)))
  (?string= (generate-code)
	   (lines "int function_with_local_bindings (int a)"
		  "{"
		  "  {"
		  "    int b = 1"
		  "    int c = 2"
		  "    return b * a + c"
		  "  }"
		  "}")))

(defmacro cg-if (expr then-form &optional else-form)
  `(burning-cgen-source:if ,expr ,then-form ,@(aif else-form (list it))))

(def-generator-test generating-if-forms
  (cg-defun function-with-if (a boolean b int)
    (cg-if a b))
  (burning-cgen-source:defun function-with-if-and-else (a boolean b int c int)
    (cg-if a (cg-+ b 1) (cg-+ c 2)))
  (?string= (generate-code)
	    (lines "void function_with_if (boolean a, int b)"
		   "{"
		   "  if( a )"
		   "  {"
		   "    b"
		   "  }"
		   "}"
		   ""
		   "int function_with_if_and_else (boolean a, int b, int c)"
		   "{"
		   "  if( a )"
		   "  {"
		   "    return b + 1"
		   "  }"
		   "  else"
		   "  {"
		   "    return c + 2"
		   "  }"
		   "}")))

(def-generator-test calling-functions
  (let ((callee (cg-defun empty-function ())))
    (flet ((form (arg)
	     (eval `(cg-defun function-calling-function () (,arg)))))
      (form callee)))
  (?string= (generate-code)
	    (lines "void empty_function ()"
		   "{"
		   "}"
		   ""
		   "void function_calling_function ()"
		   "{"
		   "  return empty_function()"
		   "}")))

(def-generator-test calling-functions-with-arguments
  (let ((f1 (cg-defun sample-function (a boolean b int c int)
	      (cg-if a b c))))
    (eval `(cg-defun function-calling-function-with-arguments (a int b int)
	     (,f1 a b 0))))
  (?lines= (generate-code)
	   (lines "int sample_function (boolean a, int b, int c)"
		  "{"
		  "  if( a )"
		  "  {"
		  "    return b"
		  "  }"
		  "  else"
		  "  {"
		  "    return c"
		  "  }"
		  "}"
		  ""
		  "int function_calling_function_with_arguments (int a, int b)"
		  "{"
		  "  return sample_function(a, b, 0)"
		  "}")))

(def-generator-test integer-variable-type
  (burning-cgen-source:defun function-with-int-arg (a int)
    (cg-+ a 1))
  (?string= (generate-code)
	    (lines "int function_with_int_arg (int a)"
		   "{"
		   "  return a + 1"
		   "}")))
    
(def-generator-test let-expression-type
  (cg-defun let-expression-type (a int)
    (cg-let ((b 2) (c 3) (d 4.5) (e (cg-+ a 1)) (f (cg-* a 0.5)))
      (cg-+ b c d e f)))
  (?lines= (generate-code)
	   (lines "float let_expression_type (int a)"
		  "{"
		  "  {"
		  "    int b = 2"
		  "    int c = 3"
		  "    float d = 4.5"
		  "    int e = a + 1"
		  "    float f = a * 0.5"
		  "    return b + c + d + e + f"
		  "  }"
		  "}")))

(def-generator-test let-with-type-argument
  (cg-defun let-with-type-argument ()
    (cg-let ((a 2 float)) (cg-+ a 3)))
  (?lines= (generate-code)
	   (lines "float let_with_type_argument ()"
		  "{"
		  "  {"
		  "    float a = 2"
		  "    return a + 3"
		  "  }"
		  "}")))

(def-generator-test simple-function-type
  (cg-defun simple-function-with-type ()
    (cg-+ 2 2))
  (?lines= (generate-code)
	   (lines "int simple_function_with_type ()"
		  "{"
		  "  return 2 + 2"
		  "}")))

(def-generator-test if-function-type
  (cg-defun function-with-if ()
    (cg-if t 10))
  (?lines= (generate-code)
	   (lines "void function_with_if ()"
		  "{"
		  "  if( true )"
		  "  {"
		  "    10"
		  "  }"
		  "}")))

(defun cg-return (&optional (value nil value-p))
  (if value-p (burning-cgen-source:return value) (burning-cgen-source:return)))

(def-generator-test explicit-return
  (cg-defun function-with-return (a boolean b int)
    (cg-if a (cg-return b))
    0)
  (?lines= (generate-code)
	   (lines "int function_with_return (boolean a, int b)"
		  "{"
		  "  if( a )"
		  "  {"
		  "    return b"
		  "  }"
		  "  return 0"
		  "}")))

(def-generator-test last-form-without-return
  (cg-defun last-form-without-return ()
    (cg-if nil 10))
  (?lines= (generate-code)
	   (lines "void last_form_without_return ()"
		  "{"
		  "  if( false )"
		  "  {"
		  "    10"
		  "  }"
		  "}")))

(def-generator-test void-return
  (cg-defun void-return ()
    (cg-if t (cg-return)))
  (?lines= (generate-code)
	   (lines "void void_return ()"
		  "{"
		  "  if( true )"
		  "  {"
		  "    return"
		  "  }"
		  "}")))

(def-generator-test no-symbol-function-name
  (?condition (eval '(burning-cgen-source:defun "a_function" ())) type-error))

(def-generator-test duplicating-function-name
  (burning-cgen-source:defun a-function ())
  (?condition (burning-cgen-source:defun a-function (a int b float c int) (cg-+ a b c))
	      simple-error))

(def-generator-test function-name-already-a-symbol-name
  (burning-cgen-source:defvar a-name 123)
  (?condition (burning-cgen-source:defun a-name ()) simple-error))

(def-generator-test function-name-has-wrong-characters
  (?condition (burning-cgen-source:defun a_wrong_name ()) simple-error)
  (?condition (burning-cgen-source:defun @is-wrong@ ()) simple-error)
  (?condition (burning-cgen-source:defun 0-starts-with-digit ()) simple-error))

(def-generator-test function-lambda-list-without-type
  (?condition (cg-defun function-with-wrong-lambda-list (a)) error)
  (?condition (cg-defun other-function-with-wrong-lambda-list (a int b)) error))

(def-generator-test function-lambda-list-with-wrong-type
  (?condition (cg-defun funciton-with-wrong-type (a a-wrong-type)) error))

(def-generator-test lambda-list-argument-isnt-a-symbol
  (?condition (eval '(cg-defun wrong-lambda-list-argument ("an arg" int))) error))

(def-generator-test lambda-list-arguments-are-wrong-symbols
  (?condition (cg-defun wrong-lambda-list-symbols (a_wrong_symbol int)) error))

;;body errors
;;;body isn't a statment












  




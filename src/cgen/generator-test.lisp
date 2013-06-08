(in-package #:burning-cgen-test)

(in-case generator-test)

(in-package #:burning-cgen-test-generated)

(cl:defmacro def-empty-function (name)
  `(defun ,name ()))

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
	   (lines "a_var = 123;")))



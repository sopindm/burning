(in-package :burning-ffi-test)

(in-case function-test)

(burning-ffi::load-ffi-actions  
 '(:library "ffi_test" :path "~/burning/c/lib/")
 '(:function ("int_sum" :int) ((a b :int)))
 '(:function ("short_sum" :short) ((a b :short)))
 '(:function ("long_sum" :long) ((a b :long)))
 '(:function ("uint_sum" :uint) ((a b :uint)))
 '(:function ("ushort_sum" :ushort) ((a b :ushort)))
 '(:function ("ulong_sum" :ulong) ((a b :ulong)))
 '(:function (sum-floats "float_sum" :float) ((a b :float)))
 '(:function ("double_sum" :double) ((a b :double)))
 '(:function ("bool_and" :bool) ((a b :bool)))
 '(:function ("sum_generated" :int) ((generator (:function :int))))
 '(:function ("double_generated" :int) ((generator1 generator2 (:function :int))))
 '(:function ("ranged_generator" :int) ((generator (:function :int (:int :int)))))
 '(:function ("get_generator" (:function :int)) ())
 '(:function ("string_length" :int) ((string :string)))
 '(:function ("sample_string" :string) ())
 '(:function ("generate_string" :dynamic-string) ((value :int))))
 

(deftest wrong-type-test
  (!error (burning-ffi::load-ffi-actions '(:function ("bla-bla" :very-wrong-type) ()))
	  "Wrong ffi type - VERY-WRONG-TYPE."))

(defun int-generator (first step)
  (lambda ()
    (incf first step)
    (- first step)))

(defun my-ranged-generator (first last)
  (floor (/ (+ first last) 2)))

(deftest sum-test
  (!= (int-sum -2 -2) -4)
  (!= (short-sum -2048 -1024) -3072)
  (!= (long-sum -123 -234) -357)
  (!= (uint-sum 123 234) 357)
  (!= (ushort-sum 1024 2048) 3072)
  (!= (ulong-sum 100000 200000) 300000)
  (!= (sum-floats 1.23 3.21) 4.44)
  (!= (double-sum 1.2345d0 5.4321d0) 6.6666d0)
  (!t (bool-and t t))
  (!null (bool-and t nil))
  (!null (bool-and nil t))
  (!null (bool-and nil nil)))

(deftest callback-test
  (!= (sum-generated (int-generator 0 1)) 1)
  (!= (double-generated (int-generator 1 1) (int-generator 3 2)) 11)
  (!= (ranged-generator #'my-ranged-generator) 1))

(deftest foriegn-fpointer-test
  (let ((generator (get-generator)))
    (let* ((value1 (funcall generator))
	   (value2 (funcall generator))
	   (value3 (funcall generator)))
      (!= (1+ value1) value2)
      (!= (1+ value2) value3))))

(deftest string-type-test
  (!= (string-length "bla-bla") 7)
  (!equal (sample-string) "Hello, Lisp!!!"))

(deftest dynamic-string-type-test
  (?equal (generate-string 3) "3")
  (?equal (generate-string 33) "33"))


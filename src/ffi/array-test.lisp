(in-package :burning-ffi-test)

(in-case array-test)

(burning-ffi::load-ffi-actions 
 '(:library "ffi_test")
 '(:function ("sum_array" :int) ((array (:array :int)) (size :int)))
 '(:function ("complex_sum_array" :int) ((array (:array (:array :int))) (sizes (:array :int)) (c_size :int))))

(deftest simple-array-call
  (!= (sum-array '(0 1 2 3 4) 5) 10))

(deftest nested-array-call
  (!= (complex-sum-array '((0 1 2 3 4) (0 10 20 30) (0 100 200 300 400 500 600) (0 1000 2000)) '(5 4 7 3) 4)
      5170))


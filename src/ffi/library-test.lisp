(in-package :burning-ffi-test)

(in-case library-test)

(deftest load-test 
  (let ((action '(:library "burning_ffi")))
    (burning-ffi::load-ffi-actions action))
  (let ((action '(:library "ffi_test" :path "~/burning/c/lib/")))
    (burning-ffi::load-ffi-actions action)))
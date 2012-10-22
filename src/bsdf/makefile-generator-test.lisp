(in-package #:burning-bsdf-test)

(defmacro def-makefile-test (name &body body)
  `(deftest ,name
     (let ((*targets* (copy-targets-table))
	   (*bsdf-generator* 'makefile)
	   (*default-filesystem* (make-virtual-filesystem)))
       ,@body)))

(def-makefile-test simple-makefile-generation
  (deftarget nil nil "file.in" "file.out")
  (generate)
  (?equal (read-file "Makefile")
	  (lines "file.out : file.in")))

(def-makefile-test generating-targets-with-multiple-input-and-output
  (deftarget nil nil ("input1" "input2" "input3") "output1 output2 output3")
  (generate)
  (?equal (read-file "Makefile")
	  (lines "output1 output2 output3 : input1 input2 input3")))

;generating targets with name
;generating targets with depends-on
;generating targets with no input

;escaping spaces (something else too???)

;generating several targets

;generating from file
;generating with output file
;generating with generator

;generating commands


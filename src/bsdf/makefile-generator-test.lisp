(in-package #:burning-bsdf-test)

(in-case makefile-generation-tests)

(defmacro def-makefile-test (name &body body)
  `(deftest ,name
     (let ((*targets* (copy-targets-table))
	   (*bsdf-generator* 'makefile)
	   (*default-filesystem* (make-virtual-filesystem)))
       ,@body)))

(def-makefile-test simple-makefile-generation
  (deftarget nil nil "file.in" "file.out")
  (generate-file)
  (?equal (read-file "Makefile")
	  (lines "file.out: file.in")))

(def-makefile-test generating-target-with-simple-command
  (deftarget "target" (echo "Hello, world!!!") nil nil)
  (generate-file)
  (?equal (read-file "Makefile")
	  (lines ".PHONY: target"
		 "target: "
		 (format nil "~cecho 'Hello, world!!!'" #\Tab))))

(def-makefile-test generating-targets-with-multiple-input-and-output
  (deftarget nil nil ("input1" "input2" "input3") "output1 output2 output3")
  (generate-file)
  (?equal (read-file "Makefile")
	  (lines "output1 output2 output3: input1 input2 input3")))

(defmacro without-warnings (&body forms)
  `(handler-bind ((warning #'muffle-warning))
     ,@forms))

(def-makefile-test generating-targets-with-name-and-input
  (without-warnings
    (deftarget "target" (echo "hi") ("in1" "in2") nil ("target1" "target2")))
  (generate-file)
  (?equal (read-file "Makefile")
	  (lines ".PHONY: target"
		 "target: in1 in2"
		 (format nil "~cecho 'hi'" #\Tab)
		 "target: target1 target2")))

(def-makefile-test generating-targets-with-name-and-output
  (without-warnings
    (deftarget "target" (echo "more") nil ("out1" "out2") ("t1" "t2")))
  (generate-file)
  (?equal (read-file "Makefile")
	  (lines "out1 out2: "
		 (format nil "~cecho 'more'" #\Tab)
		 "out1 out2: t1 t2"
		 ".PHONY: target"
		 "target: out1 out2")))

(def-makefile-test generating-targets-with-name-input-and-output
  (deftarget "target" (echo "and more") ("in1" "in2" "in3") ("out1" "out2") ("t1" "t2" "t4"))
  (generate-file)
  (?equal (read-file "Makefile")
	  (lines "out1 out2: in1 in2 in3"
		 (format nil "~cecho 'and more'" #\Tab)
		 "out1 out2: t1 t2 t4"
		 ".PHONY: target"
		 "target: out1 out2")))

(def-makefile-test generating-serveral-files 
  (deftarget nil nil ("in1" "in2" "in3") ("out1" "out2"))
  (deftarget "tt" nil ("out1" "out2") "total")
  (generate-file)
  (?equal (read-file "Makefile")
	  (lines "out1 out2: in1 in2 in3"
		 "total: out1 out2"
		 ".PHONY: tt"
		 "tt: total")))

;generating from file
;generating with output file (or directory)
;generating with generator

;escaping spaces (something else too???)

;using input and output variables in commands
;using variables


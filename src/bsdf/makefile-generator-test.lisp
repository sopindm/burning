(in-package #:bsdf-test)

(in-case makefile-generation-tests)

(defmacro def-makefile-test (name &body body)
  `(deftest ,name
     (let ((*context* (copy-context))
	   (*bsdf-generator* 'makefile)
	   (*default-filesystem* (make-virtual-filesystem)))
       ,@body)))

(def-makefile-test simple-makefile-generation
  (deftarget nil nil "file.in" "file.out")
  (generate-file)
  (?equal (read-file "Makefile")
	  (lines "file.out: file.in" "")))

(def-makefile-test generating-target-with-simple-command
  (deftarget "target" (echo "Hello, world!!!") nil nil)
  (generate-file)
  (?equal (read-file "Makefile")
	  (lines ".PHONY: target"
		 "target: "
		 (format nil "~c@echo 'Hello, world!!!'" #\Tab)
		 "")))

(def-makefile-test generating-targets-with-multiple-input-and-output
  (deftarget nil nil ("input1" "input2" "input3") ("output1" "output2" "output3"))
  (generate-file)
  (?equal (read-file "Makefile")
	  (lines ".PHONY: __output1\\ output2\\ output3"
		 "__output1\\ output2\\ output3: input1 input2 input3" 
		 "output1 output2 output3: __output1\\ output2\\ output3"
		 "")))

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
		 (format nil "~c@echo 'hi'" #\Tab)
		 ""
		 "target: target1 target2"
		 "")))

(def-makefile-test generating-targets-with-name-and-output
  (without-warnings
    (deftarget "target" (echo "more") nil ("out1" "out2") ("t1" "t2")))
  (generate-file)
  (?equal (read-file "Makefile")
	  (lines ".PHONY: target"
		 "target: "
		 (format nil "~c@echo 'more'" #\Tab)
		 ""
		 "out1 out2: target"
		 "out1 out2: t1 t2"
		 "")))

(def-makefile-test generating-targets-with-name-input-and-output
  (deftarget "target" (echo "and more") ("in1" "in2" "in3") ("out1" "out2") ("t1" "t2" "t4"))
  (generate-file)
  (?equal (read-file "Makefile")
	  (lines ".PHONY: target"
		 "target: in1 in2 in3"
		 (format nil "~c@echo 'and more'" #\Tab)
		 ""
		 "out1 out2: target"
		 "out1 out2: t1 t2 t4"
		 "")))

(def-makefile-test generating-serveral-files 
  (deftarget nil nil ("in1" "in2" "in3") ("out1" "out2"))
  (deftarget "tt" nil ("out1" "out2") "total")
  (generate-file)
  (?equal (read-file "Makefile")
	  (lines ".PHONY: __out1\\ out2"
		 "__out1\\ out2: in1 in2 in3"
		 "out1 out2: __out1\\ out2"
		 ".PHONY: tt"
		 "tt: out1 out2"
		 "total: tt"
		 "")))

(def-makefile-test generating-makefile-from-file
  (write-file "a.file"
	      (lines "(deftarget \"target1\" (echo \"~a+~a is ~a\" 2 2 4) nil nil)"
		     "(deftarget \"target2\" nil (\"in1\" \"in2\") (\"out1\" \"out2\"))"))
  (generate-from-file "a.file")
  (?equal (read-file "Makefile")
	  (lines ".PHONY: target1"
		 "target1: "
		 (format nil "~c@echo '2+2 is 4'" #\Tab)
		 ""
		 ".PHONY: target2"
		 "target2: in1 in2"
		 "out1 out2: target2"
		 "")))

(def-makefile-test compilation-errors-with-source-line
  (write-file "a.file"
	      (lines "(defvariable var '(+ 123 :456))"))
  (?bsdf-compilation-error (generate-from-file "a.file")
			   (lines "In lines from 1 to 1"
				  "In definition of variable 'VAR':"
				  "In argument 'ARGS' of '+':"
				  "Cannot convert ~a from type ~a to ~a")
			   '(123 :456) :LIST '(:LIST :INT)))

;compilation errors with source line
    
(def-makefile-test generating-with-output-directory
  (deftarget nil nil ("in1" "in2") ("out1" "out2"))
  (let ((*context* (copy-context)))
    (generate-file :path "other.file"))
  (make-directory "a.dir/")
  (generate-file :path "a.dir/")
  (let ((text (lines ".PHONY: __out1\\ out2"  "__out1\\ out2: in1 in2" "out1 out2: __out1\\ out2" "")))
    (?equal (read-file "Makefile") text)
    (?equal (read-file "a.dir/Makefile") text)))

(def-makefile-test generating-with-generator
  (deftarget nil nil ("f1" "f2" "f3") "out")
  (let ((*bsdf-generator* 'some-wrong-generator))
    (generate-file :generator 'makefile)
    (?equal (read-file "Makefile") (lines "out: f1 f2 f3" ""))))

(def-makefile-test escaping-spaces
  (without-warnings 
    (deftarget "a target" nil nil nil))
  (generate-file)
  (?equal (read-file "Makefile") (lines ".PHONY: a\\ target" "a\\ target: " "")))

;escaping dollars
;escaping wildcard characters (*, %, ?)
;working with newlines

;special rule names escaping

;using input and output variables in commands
;using variables


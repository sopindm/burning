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
	  (lines "file.out: file.in")))

(def-makefile-test generating-target-with-simple-command
  (deftarget "target" (echo "Hello, world!!!") nil nil)
  (generate-file)
  (?equal (read-file "Makefile")
	  (lines ".PHONY: target"
		 "target: "
		 (format nil "~c@echo 'Hello, world!!!'" #\Tab))))

(def-makefile-test generating-targets-with-multiple-input-and-output
  (deftarget nil nil ("input1" "input2" "input3") ("output1" "output2" "output3"))
  (generate-file)
  (?equal (read-file "Makefile")
	  (lines ".PHONY: __output1\\ output2\\ output3"
		 "output1 output2 output3: __output1\\ output2\\ output3"
		 "__output1\\ output2\\ output3: input1 input2 input3")))

(defmacro without-warnings (&body forms)
  `(handler-bind ((warning #'muffle-warning))
     ,@forms))

(def-makefile-test generating-targets-with-name-and-input
  (without-warnings
    (deftarget "target" (echo "hi") ("in1" "in2") nil ("target1" "target2")))
  (generate-file)
  (?equal (read-file "Makefile")
	  (lines ".PHONY: target"
		 "target: target1 target2"
		 "target: in1 in2"
		 (format nil "~c@echo 'hi'" #\Tab))))

(def-makefile-test generating-targets-with-name-and-output
  (without-warnings
    (deftarget "target" (echo "more") nil ("out1" "out2") ("t1" "t2")))
  (generate-file)
  (?equal (read-file "Makefile")
	  (lines ".PHONY: target"
		 "out1 out2: target"
		 "out1 out2: t1 t2"
		 "target: "
		 (format nil "~c@echo 'more'" #\Tab))))

(def-makefile-test generating-targets-with-name-input-and-output
  (deftarget "target" (echo "and more") ("in1" "in2" "in3") ("out1" "out2") ("t1" "t2" "t4"))
  (generate-file)
  (?equal (read-file "Makefile")
	  (lines ".PHONY: target"
		 "out1 out2: target"
		 "out1 out2: t1 t2 t4"
		 "target: in1 in2 in3"
		 (format nil "~c@echo 'and more'" #\Tab))))

(def-makefile-test generating-several-files 
  (deftarget nil (echo "hi") ("in1" "in2" "in3") ("out1" "out2"))
  (deftarget "tt" nil ("out1" "out2") "total")
  (generate-file)
  (?equal (read-file "Makefile")
	  (lines ".PHONY: __out1\\ out2"
		 "out1 out2: __out1\\ out2"
		 "__out1\\ out2: in1 in2 in3"
		 (format nil "~c@echo 'hi'" #\Tab)
		 ""
		 ".PHONY: tt"
		 "total: tt"
		 "tt: out1 out2")))

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
		 "out1 out2: target2"
		 "target2: in1 in2")))

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
  (let ((text (lines ".PHONY: __out1\\ out2"  
		     "out1 out2: __out1\\ out2"
		     "__out1\\ out2: in1 in2")))
    (?equal (read-file "Makefile") text)
    (?equal (read-file "a.dir/Makefile") text)))

(def-makefile-test generating-with-generator
  (deftarget nil nil ("f1" "f2" "f3") "out")
  (let ((*bsdf-generator* 'some-wrong-generator))
    (generate-file :generator 'makefile)
    (?equal (read-file "Makefile") (lines "out: f1 f2 f3"))))

(def-makefile-test escaping-spaces
  (without-warnings 
    (deftarget "a target" nil nil nil))
  (generate-file)
  (?equal (read-file "Makefile") (lines ".PHONY: a\\ target" "a\\ target: ")))

(def-makefile-test escaping-dollars
  (without-warnings
    (deftarget "some target with $" nil nil nil))
  (generate-file)
  (?equal (read-file "Makefile") (lines ".PHONY: some\\ target\\ with\\ $$"
					"some\\ target\\ with\\ $$: ")))

(defmacro def-wildcard-test (name string)
  `(def-makefile-test ,name 
     (deftarget ,string nil "input" "output")
     (?bsdf-warning (generate-file)
		    (lines "Makefile entity's name '~a' contains wildcard characters") ,string)))
  
(def-wildcard-test *-character-in-target-warning "a target with *")
(def-wildcard-test ?-character-in-target-warning "a target with ?")
(def-wildcard-test %-character-in-target-warning "a target with %")

(defmacro ?var= (value &optional (result value) (type t))
  `(let ((*context* (copy-context)))
     (defvariable var ,value :type ,type)
     (generate-file)
     (?lines= (read-file "Makefile")
	      (format nil (lines "VAR = ~a") ,result))))

(def-makefile-test generating-string-variables
  (?var= "abc")
  (?var= "abc def"))
  
(def-makefile-test generating-path-variables
  (?var= (path-from-string "/some/file.path") "/some/file.path")
  (?var= (path-from-string "/some/dir/") "/some/dir/"))

(def-makefile-test generating-int-variables
  (?var= 123 "123")
  (?var= -123 "-123"))

(def-makefile-test generating-bool-variables
  (?var= t "true")
  (?var= nil "false" :bool))

(def-makefile-test generating-enum-variables
  (?var= :bla "BLA")
  (?var= :|bla bla| "bla bla"))

(def-makefile-test generating-list-variables
  (?var= '(1 2 3) "__( 1 __NEXT 2 __NEXT 3 __)")
  (?var= '("a b c" "d e f" "g h i") "__( a b c __NEXT d e f __NEXT g h i __)")
  (?var= '((1 2) 3 ((4))) "__( __( 1 __NEXT 2 __) __NEXT 3 __NEXT __( __( 4 __) __) __)"))

;keywords in variable names warnings (__NEXT__, __SPACE__, __(, __))
;operation's generation
;;nth
;;remove

#+never
(def-makefile-test generating-list-variable
  (defvariable var1 '(1 2 3))
  (defvariable var2 '("a b c" "d e f" "g h i"))
  (generate-file)
  (?lines= (read-file "Makefile") (lines "VAR1 = 1__NEXT__2__NEXT__3"
					 "VAR2 = a b c__NEXT__d e f__NEXTg h i")))

;escaping dollars in variable values

;working with newlines

;special rule names escaping

;working with expressions

;using input and output variables in commands
;using variables


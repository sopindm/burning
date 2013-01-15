(in-package #:bsdf-test)

(in-case targets-test)

(defmacro def-targets-test (name &body body)
  `(deftest ,name 
     (let ((*context* (copy-context)))
       ,@body)))

(def-targets-test making-targets-test
  (let ((target (make-target nil :name "a target" :input "some-input" :output "some-output")))
    (?equal (target-name target) "a target")
    (?null (target-command target))
    (?equal (target-input target) '("some-input"))
    (?equal (target-output target) '("some-output"))
    (?null (target-depends-on target)))
  (let ((target2 (make-target nil 
			      :name "test"
			      :input '("input1" "input2")
			      :output '("output1" "output2" "output3"))))
    (?equal (target-input target2) '("input1" "input2"))
    (?equal (target-output target2) '("output1" "output2" "output3")))
  (let ((target2 (make-target #'identity :name "test")))
    (?eq (target-command target2) #'identity)))

(def-targets-test wrong-argument-types-error
  (?bsdf-compilation-error (make-target nil :name '(1 2 3))
			   (lines "Wrong target name '~a'") '(1 2 3))
  (?bsdf-compilation-error (make-target nil :name "target" :input '(1 2 3))
			   (lines "Wrong input '~a' in target 'target'") '(1 2 3))
  (?bsdf-compilation-error (make-target nil :name "target" :output '(1 2 3))
			   (lines "Wrong output '~a' in target 'target'") '(1 2 3))
  (?bsdf-compilation-error (make-target nil :name "target" :depends-on '(1 2 3))
			   (lines "Wrong dependencies list '~a' in target 'target'") '(1 2 3)))

;removing duplicates

(def-targets-test checking-target-having-name-or-one-output-file
  (?bsdf-compilation-error (make-target nil)
			   (lines "No output file or name in target"))
  (make-target nil :name "simple")
  (make-target nil :output "some output")
  (make-target nil :output '("some other output")))

(def-targets-test wrong-command-test
  (?bsdf-compilation-error (make-target 42 :name "wrong target")
			   (lines "Wrong command 42 in target 'wrong target'"))
  (?bsdf-compilation-error (make-target "bla bla" :output "sample")
			   (lines "Wrong command bla bla in target 'sample'")))

(def-targets-test accessing-targets-by-name
  (let ((target (make-target nil :name "some target" :input "file1" :output "file2")))
    (set-target target)
    (?eq (get-target "some target") target))
  (let ((target (make-target #'identity :output "some file")))
    (set-target target)
    (?eq (get-target '("some file")) target))
  (let ((target (make-target #'identity :output '("file1" "file2" "file3"))))
    (set-target target)
    (?eq (get-target '("file1" "file2" "file3")) target)))

(def-targets-test listing-targets
  (let ((target1 (make-target #'identity :name "target1"))
	(target2 (make-target #'identity :output "some file"))
	(target3 (make-target #'identity :name "target3")))
    (set-target target2)
    (set-target target1)
    (set-target target3)
    (?equal (get-targets) (list target2 target1 target3))))

(def-targets-test listing-file-targets
  (let ((target1 (make-target #'identity :output "file"))
	(target2 (make-target nil
			      :input "file2"
			      :output "file"))
	(target3 (make-target nil
			      :input '("file3" "file4")
			      :output '("file" "file5"))))
    (set-target target1)
    (set-target target2)
    (set-target target3)
    (?equal (get-targets) (list target1 target2 target3))))

(def-targets-test adding-targets-with-same-key-name-error
  (let ((target (make-target #'identity :name "target")))
    (set-target target)
    (let ((target2 (make-target nil :name "target"))
	  (target3 (make-target nil :output "target"))
	  (target4 (make-target #'identity :name "other-target" :input "target")))
      (?bsdf-compilation-error (set-target target2)
			       (lines "Target with name 'target' already exists"))
      (?bsdf-compilation-error (set-target target3)
			       (lines "File name 'target' is already a target name"))
      (?bsdf-compilation-error (set-target target4)
			       (lines "File name 'target' is already a target name"))))
  (let ((target (make-target #'identity :output "file1")))
    (set-target target)
    (let ((target2 (make-target nil :output "file1" :input "no"))
	  (target3 (make-target nil :name "file1"))
	  (target4 (make-target #'identity :name "new-target" :input "file2"))
	  (target5 (make-target #'identity :name "file2")))
      (set-target target2)
      (?bsdf-compilation-error (set-target target3)
			       (lines "Target name 'file1' is already a file name"))
      (set-target target4)
      (?bsdf-compilation-error (set-target target5)
			       (lines "Target name 'file2' is already a file name")))
    (?eq (get-target '("file1")) target))
  (let ((target1 (make-target nil :output "file" :input "no"))
	(target2 (make-target #'identity :output "file")))
    (set-target target1)
    (set-target target2)
    (?eq (get-target '("file")) target2)))

(def-targets-test adding-several-targets-with-one-output
  (let ((target1 (make-target #'identity :output "file"))
	(target2 (make-target nil :output "file" :input "no"))
	(target3 (make-target #'identity :output '("file2" "file"))))
    (set-target target1)
    (set-target target2)
    (?bsdf-compilation-error (set-target target3)
			     (lines "Found two targets generating 'file':"
				    "'file', 'file2 file'"))))

(def-targets-test empty-target-warning
  (let ((target (make-target nil :input "file1" :output "file2"))
	(target2 (make-target nil :name "target" :input "file"))
	(target3 (make-target nil :output "file4"))
	(target4 (make-target #'identity :name "target4" :input "file"))
	(target5 (make-target #'identity :output "file5")))
    (?condition-safe (set-target target))
    (?bsdf-compilation-warning (set-target target2)
			       (lines "Target 'target' is empty"))
    (?bsdf-compilation-warning (set-target target3)
			       (lines "Target 'file4' is empty"))
    (?condition-safe (set-target target4))
    (?condition-safe (set-target target5))))

(def-targets-test target-depends
  (let ((target (make-target #'identity 
			     :name "target" 
			     :input '("file1" "file2") 
			     :depends-on '("file3" "target2"))))
    (set-target target)
    (let ((depends '("file1" "file2" "file3" "target2")))
      (?equal (get-depends "target") depends)
      (?equal (get-depends target) depends)))
  (set-target (make-target #'identity 
			   :input "file1"
			   :output "file4"
			   :depends-on "target2"))
  (?equal (get-depends '("file4")) '("file1" "target2")))

(def-targets-test file-depends
  (let ((target1 (make-target #'identity
			      :name "target"
			      :output "file"
			      :input "file2"
			      :depends-on "target2"))
	(target2 (make-target nil
			      :output "file"
			      :input '("file3")
			      :depends-on '("target3" "target4")))
	(target3 (make-target #'identity
			      :output "file3")))
    (set-target target1)
    (set-target target2)
    (set-target target3)
    (let ((depends '("target" "file3" "target3" "target4")))
      (?equal (get-depends "file") depends))
    (?equal (get-depends "file3") '(("file3")))))

(def-targets-test dependencies-map
  (let ((target (make-target #'identity :name "target"
			     :input '("f1" "f2" "f3")
			     :output '("f4")
			     :depends-on '("t2" "t3"))))
    (let ((depends '("f1" "f2" "f3" "t2" "t3")))
      (set-target target)
      (?equal (map-depends #'identity "target") depends)
      (?equal (map-depends #'identity target) depends)
      (?equal (with-output-to-string (output)
		(mapc-depends (lambda (x) (format output "~a~%" x)) "target"))
	      (apply #'lines depends)))))

(def-targets-test recursive-map-for-dependencies
  (let ((target (make-target #'identity :name "target"
			     :depends-on '("target2" "target3")))
	(target2 (make-target #'identity :name "target2" :input '("file2" "file")))
	(target3 (make-target #'identity :name "target3" :input '("file3" "file"))))
    (set-target target)
    (set-target target2)
    (set-target target3)
    (let ((depends '("target2" "target3" "file2" "file" "file3")))
      (?equal (map-depends #'identity "target" :recursive t) depends)
      (?equal (with-output-to-string (output)
		(mapc-depends (lambda (x) (format output "~a" x)) "target" :recursive t))
	      (apply #'string+ depends)))))

(def-targets-test self-dependency-error
  (let ((target1 (make-target #'identity
			      :name "target1"
			      :depends-on "target2"))
	(target2 (make-target #'identity
			      :name "target2"
			      :depends-on "target1"))
	(target3 (make-target #'identity
			      :name "target3"
			      :depends-on "target1")))
    (set-target target1)
    (set-target target2)
    (set-target target3)
    (?bsdf-error (mapc-depends #'identity target1 :recursive t)
		 (lines "Circular dependency for target 'target1' found"))
    (?bsdf-error (mapc-depends #'identity "target3" :recursive t)
		 (lines "Circular dependency for target 'target1' found"))))

;dependencies optimization

(def-targets-test adding-input-to-target
  (let ((target (make-target #'identity :name "target"
			     :input "file1"
			     :output "file2")))
    (set-target target)
    (add-input target "file3")
    (?equal (target-input target) '("file1" "file3"))
    (?equal (get-depends target) '("file1" "file3"))
    (add-input "target" "file4")
    (?equal (get-depends target) '("file1" "file3" "file4")))
  (let ((target (make-target nil
			     :input "file"
			     :output "file1")))
    (set-target target)
    (add-input target "file2")
    (?equal (get-depends "file1") '("file" "file2")))
  (?condition-safe (add-input nil "file"))
  (?condition-safe (add-input "blabla" "file")))

(def-targets-test adding-output-to-target
  (let ((target (make-target #'identity :name "target"
			     :input "file1"
			     :output "file2")))
    (set-target target)
    (add-output target "file3")
    (?equal (target-output target) '("file2" "file3"))
    (?equal (get-depends "file3") '("target")))
  (let ((target (make-target nil
			     :input "file2"
			     :output "file4")))
    (set-target target)
    (add-output target "file5")
    (?equal (get-depends "file5") '("file2")))
  (let ((target (make-target #'identity
			     :input "input"
			     :output "output")))
    (set-target target)
    (add-output target "output2")
    (?equal (get-target '("output" "output2")) target)
    (?null (get-target '("output")))
    (?equal (get-depends "output") '(("output" "output2")))
    (?equal (get-depends "output2") '(("output" "output2")))))

(def-targets-test adding-existing-file-as-an-output
  (set-target (make-target nil :input "file1" :output '("file2" "file3")))
  (let ((target (make-target #'identity :name "target")))
    (set-target target)
    (add-output target "file2")
    (?equal (get-depends "file2") '("target" "file1")))
  (let ((target (make-target nil :output "file3" :input "file4")))
    (set-target target)
    (add-output target "file2")
    (?equal (get-depends "file2") '("target" "file1" "file4"))))

(def-targets-test adding-dependencies-to-targets
  (let ((target (make-target #'identity 
			     :name "target"
			     :input "input"
			     :output "output"
			     :depends-on "depend")))
    (set-target target)
    (add-dependency "target" "depend2")
    (?equal (get-depends "target") '("input" "depend" "depend2"))))

(def-targets-test simply-deftarget-test
  (deftarget "target" #'identity
    ("input1" "input2")
    "output"
    "target2")
  (let ((target (get-target "target")))
    (?eq (target-command target) #'identity)
    (?equal (target-name target) "target")
    (?equal (target-input target) '("input1" "input2"))
    (?equal (target-output target) '("output"))
    (?equal (target-depends-on target) '("target2"))))

(def-targets-test setting-and-accessing-variables
  (let ((var (make-variable "var" 123)))
    (set-variable var)
    (?equal (get-variable "var") var)
    (?equal (get-variable '|var|) var)
    (?equal (get-variable '#:|var|) var))
  (let ((var (make-variable '#:other-var 456)))
    (set-variable var)
    (?equal (get-variable "OTHER-VAR") var)))

(def-targets-test defining-variables
  (defvariable var '(append 1 2 3)
    :type (:list :int)
    :description "simple integer variable"
    :visible-p t)
  (?equalp (get-variable "VAR")
	   (make-variable 'var '(append 1 2 3)
			  :type '(:list :int)
			  :description "simple integer variable"
			  :visible-p t)))

(def-targets-test variables-name-conflicts
  (deftarget "var" #'identity () ())
  (?bsdf-compilation-error (set-variable (make-variable "var" 123))
			   (lines "Variable name 'var' is already a target name"))
  (deftarget nil #'identity "input" "output")
  (?bsdf-compilation-error (set-variable (make-variable "output" 456))
			   (lines "Variable name 'output' is already a file name"))
  (defvariable "a-var" 123)
  (?bsdf-compilation-error (set-variable (make-variable "a-var" 789))
			   (lines "Variable with name 'a-var' already exists"))
  (?bsdf-compilation-error (deftarget "a-var" #'identity () ())
			   (lines "Target name 'a-var' is already a variable name"))
  (?bsdf-compilation-error (deftarget nil #'identity "input" "a-var")
			   (lines "File name 'a-var' is already a variable name")))

(def-targets-test getting-variables
  (defvariable var1 123)
  (defvariable var2 "345")
  (defvariable "var3" 123)
  (?equalp (get-variables)
	   (mapcar #'get-variable (list "VAR1" "VAR2" "var3"))))

(def-targets-test simple-generating-temporal-names
  (let ((name (gen-tmp-name "name"))
	(name2 (gen-tmp-name "name")))
    (?equal name "__name")
    (?equal name2 "__name_2"))
  (deftarget "__name_3" #'identity nil nil)
  (?equal (gen-tmp-name "name") "__name_4"))

(def-targets-test freeing-tmp-names
  (let ((name (gen-tmp-name "name"))
	(name2 (gen-tmp-name "name")))
    (free-tmp-name name2)
    (?equal (gen-tmp-name "name") name2)
    (free-tmp-name name)
    (?equal (gen-tmp-name "name") name)))

(def-targets-test temporal-names-callback
  (let ((new-name nil))
    (gen-tmp-name "name" (lambda (x) (setf new-name x)))
    (defvariable "__name" 123)
    (?equal new-name "__name_2")
    (defvariable "__name_2" 234)
    (?equal new-name "__name_3")
    (defvariable "__name_4" 345)
    (defvariable "__name_3" 234)
    (?equal new-name "__name_5")))

(def-targets-test with-tmp-name-test
  (defvariable "__name" 123)
  (with-tmp-name (name "name")
    (?equal name "__name_2"))
  (?equal (gen-tmp-name "name") "__name_2")
  (with-tmp-names ((name1 "name") (name2 "name") (name3 "name2"))
    (?equal name1 "__name_3")
    (?equal name2 "__name_4")
    (?equal name3 "__name2")))

(def-targets-test getvar-operation-test
  (defvariable "VAR" 123)
  (?expr= ('(getvar var) :int) 123)
  (?expr= ('(getvar "VAR") :string) "123")
  (?wrong-expr (getvar 123) "Wrong variable name '123'")
  (?wrong-expr (getvar "var2") "Variable 'var2' does not exists")) 

(def-targets-test $-operation-test
  (defvariable "VAR" '(append (1 2) (3 4)))
  (let ((var (make-variable "var2" '(second ($ var)))))
    (?equal (variable-expression var) '(second (append (1 2) (3 4)))))
  (?wrong-expr ($ 123) "Wrong variable name '123'")
  (?wrong-expr (getvar "other var") "Variable 'other var' does not exists"))

;;
;;targets hierarchy
;;

;; generators (separate files)

;generating makefile's
;generating ninja files?
;generating bsc files
;generating lisp sources
;generating pure lisp sources
;generating asd systesm???


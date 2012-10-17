(in-package #:burning-bsdf-test)

(in-case targets-test)

(defmacro def-targets-test (name &body body)
  `(deftest ,name 
     (let ((*targets* (copy-targets-table)))
       ,@body)))

(def-targets-test making-targets-test
  (let ((target (make-target nil :name "a target" :input 'some-input :output 'some-output)))
    (?equal (target-name target) "a target")
    (?null (target-command target))
    (?equal (target-input target) '(some-input))
    (?equal (target-output target) '(some-output))
    (?null (target-depends-on target)))
  (let ((target2 (make-target nil :name "test" :input '(input1 input2) :output '(output1 output2 output3))))
    (?equal (target-input target2) '(input1 input2))
    (?equal (target-output target2) '(output1 output2 output3)))
  (let ((target2 (make-target #'identity :name "test")))
    (?eq (target-command target2) #'identity)))

(def-targets-test checking-target-having-name-or-one-output-file
  (?bsdf-compilation-error (make-target nil)
			   (lines "No output file or name for target"))
  (make-target nil :name "simple")
  (make-target nil :output "some output")
  (make-target nil :output '("some other output")))

(def-targets-test wrong-command-test
  (?bsdf-compilation-error (make-target 42 :name "wrong target")
			   (lines "Wrong command 42 for target 'wrong target'"))
  (?bsdf-compilation-error (make-target "bla bla" :output "sample")
			   (lines "Wrong command bla bla for target 'sample'")))

(def-targets-test accessing-targets-by-name
  (let ((target (make-target nil :name "some target")))
    (set-target target)
    (?eq (get-target "some target") target))
  (let ((target (make-target #'identity :output "some file")))
    (set-target target)
    (?eq (get-target "some file") target))
  (let ((target (make-target #'identity :output '("file1" "file2" "file3"))))
    (set-target target)
    (?eq (get-target '("file1" "file2" "file3")) target)))

(def-targets-test listing-targets
  (let ((target1 (make-target nil :name "target1"))
	(target2 (make-target nil :output "some file"))
	(target3 (make-target nil :name "target3")))
    (set-target target2)
    (set-target target1)
    (set-target target3)
    (?equal (get-targets) (list target2 target1 target3))))

(def-targets-test adding-targets-with-same-key-name-error
  (let ((target (make-target nil :name "target")))
    (set-target target)
    (let ((target2 (make-target nil :name "target"))
	  (target3 (make-target nil :output "target")))
      (?bsdf-compilation-error (set-target target2)
			       (lines "Target with name 'target' already exists"))
      (?bsdf-compilation-error (set-target target3)
			       (lines "Target with name 'target' already exists"))))
  (let ((target (make-target #'identity :output "file1")))
    (set-target target)
    (let ((target2 (make-target nil :output "file1"))
	  (target3 (make-target nil :name "file1")))
      (set-target target2)
      (?bsdf-compilation-error (set-target target3)
			       (lines "Target with name 'file1' already exists")))
    (?eq (get-target "file1") target))
  (let ((target1 (make-target nil :output "file"))
	(target2 (make-target #'identity :output "file")))
    (set-target target1)
    (set-target target2)
    (?eq (get-target "file") target2)))

(def-targets-test adding-several-targets-with-one-output
  (let ((target1 (make-target #'identity :output "file"))
	(target2 (make-target nil :output "file"))
	(target3 (make-target #'identity :output '("file2" "file"))))
    (set-target target1)
    (set-target target2)
    (?bsdf-compilation-error (set-target target3)
			     (lines "Found two targets generating 'file':"
				    "'file', 'file2 file'"))))


			     
;empty rule warning

;wrong target depends-on error    

;target tables test

;generating makefile's
;generating ninja files
;generating bsc files
;generating lisp sources
;generating pure lisp sources
;generating asd systesm

;targets runtime api (adding and removing dependencies, input and output)

;targets subtables

;variables
;include's
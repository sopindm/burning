(in-package #:burning-bsdf-test)

(in-case targets-test)

(defmacro def-targets-test (name &body body)
  `(deftest ,name ,@body))

(def-targets-test making-targets-test
  (let ((target (add-target "a target" nil :input 'some-input :output 'some-output)))
    (?equal (target-name target) "a target")
    (?null (target-command target))
    (?equal (target-input target) '(some-input))
    (?equal (target-output target) '(some-output))
    (?null (target-depends-on target)))
  (let ((target2 (add-target "second target" nil :input '(input1 input2) :output '(output1 output2 output3))))
    (?equal (target-input target2) '(input1 input2))
    (?equal (target-output target2) '(output1 output2 output3)))
  (let ((target2 (add-target "target3" #'identity)))
    (?eq (target-command target2) #'identity)))

;;bsdf compilation errors

;checking that target has name or one output file

;no function or nil command error
;adding targets with same name error

;accessing targets by name
;listing targets

;wrong target depends-on error    

;generating makefile's
;generating ninja files
;generating bsc files

;targets locality

;variables
;include's
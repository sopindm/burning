(in-package #:burning-cgen-test)

(in-case c-macros-test)

(defmacro def-cmacros-test (name &body body)
  `(deftest ,name
     (in-language :c)
     ,@body))

(def-cmacros-test let-macro-test
  (?equal (burning-cgen::macroexpand '(let ((a b)) c d e))
	  '(progn (burning-cgen::define a b) c d e)))

(def-cmacros-test simple-for-test
  (?equal (burning-cgen::macroexpand '(for ((i :from 0 :to 10))
				       (do-something i)))
	  `(progn (define i) (burning-cgen::c-for ((setf i 0) (<= i 10) (incf i))
				(do-something i))))
  (?equal (burning-cgen::macroexpand '(for ((i :from 10 :downto 0))
				       (do-something i)))
	  '(progn (define i) (burning-cgen::c-for ((setf i 10) (>= i 0) (decf i))
			      (do-something i)))))

(def-cmacros-test open-for-test
  (?equal (burning-cgen::macroexpand '(for ((i :from 0 :lessto 15)) (do-something i)))
	  '(progn (define i) (burning-cgen::c-for ((setf i 0) (< i 15) (incf i)) (do-something i))))
  (?equal (burning-cgen::macroexpand '(for ((i :from 10 :moreto -5)) (do-something i)))
	  '(progn (define i) (burning-cgen::c-for ((setf i 10) (> i -5) (decf i)) (do-something i)))))

(def-cmacros-test for-step-test
  (?equal (burning-cgen::macroexpand '(for ((i :from 0 :to 10 :step 3)) (do-something i)))
	  '(progn (define i) (burning-cgen::c-for ((setf i 0) (<= i 10) (incf i 3)) (do-something i)))))

(def-cmacros-test multiple-variables-in-for
  (?equal (burning-cgen::macroexpand '(for ((i :from 0 :to 10 :step 2)
					    (j :from 15 :moreto -7))
				         (some-1 i) (some-2 j)))
	  `(progn (define i) (define j) 
		  (burning-cgen::c-for ((comma (setf i 0) (setf j 15)) 
					(comma (<= i 10) (> j -7))
					(comma (incf i 2) (decf j)))
				       (some-1 i) (some-2 j)))))

(def-cmacros-test simple-do-macro
  (?equal (burning-cgen::macroexpand '(do ((v1 value1 (inc1 v1)) (v2 value2 (inc2 v2))) 
				       ((check1 value1) (check2 value2))
				       (op1 arg1) (op2 arg1 arg2 arg3)))
	  `(progn (define v1) (define v2)
		  (burning-cgen::c-for ((comma (setf v1 value1) (setf v2 value2))
					(comma (check1 value1) (check2 value2))
					(comma (inc1 v1) (inc2 v2)))
				       (op1 arg1) (op2 arg1 arg2 arg3)))))

(def-cmacros-test anaphoric-do
  (?equal (burning-cgen::macroexpand '(do ((v1 value1 (incf it))) ((check v1)) (do-something v1)))
	  `(progn (define v1) 
		  (burning-cgen::c-for ((setf v1 value1) (check v1) (incf v1)) (do-something v1)))))

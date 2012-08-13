(in-package #:burning-ctypes-test)

(in-case type-system-test)

(deftest empty-block-inheritance
  (let ((block (make-type-block)))
    (inherit-types block)))

(deftest adding-variables-to-block
  (let ((block (make-type-block)))
    (block-addvar block 'x)
    (?equalp (block-vartype block 'x) (instance-type 'undefined-type))))

(deftest making-block-with-variables
  (let ((block (make-type-block :vars '(x))))
    (?equalp (block-vartype block 'x) (instance-type 'undefined-type))))

(deftest adding-variable-twice 
  (let ((block (make-type-block)))
    (block-addvar block 'x)
    (setf (block-vartype block 'x) (instance-type 'true-type))
    (block-addvar block 'x)
    (?equalp (block-vartype block 'x) (instance-type 'true-type))))

(deftest simple-type-inheritance
  (let ((block (make-type-block :vars '(x y) :exprs '((x (true-type)) (y (false-type))))))
    (inherit-types block)
    (?equalp (block-vartype block 'x) (instance-type 'true-type))
    (?equalp (block-vartype block 'y) (instance-type 'false-type))))

(deftest applying-several-types
  (let ((block (make-type-block :vars '(x) :exprs '((x (a-type)) (x (b-type))))))
    (inherit-types block)
    (?equalp (block-vartype block 'x) (instance-type 'and-type
						     (list (instance-type 'a-type) 
							   (instance-type 'b-type))))))

(deftest and-type-simplifying
  (let ((block (make-type-block :vars '(x y z) :exprs '((x (true-type)) (x (false-type))
							(y (false-type)) (y (true-type))
							(z (true-type)) (z (true-type))))))
    (inherit-types block)
    (?equalp (block-vartype block 'x) (instance-type 'false-type))
    (?equalp (block-vartype block 'y) (instance-type 'false-type))
    (?equalp (block-vartype block 'z) (instance-type 'true-type))))

(deftest recursive-and-simplifying
  (let ((block (make-type-block :vars '(x) :exprs '((x (and-type (and-type (true-type) (true-type))
							(true-type) (true-type) 
							(and-type (true-type) (false-type))))))))
    (inherit-types block)
    (?equalp (block-vartype block 'x) (instance-type 'false-type))))

(deftest wrong-block-expressions
  (?error (make-type-block :exprs '((bla bla bla)))
	  (format nil "Wrong type expression ~a" '(bla bla bla))))

(deftest adding-expressions-to-block
  (let ((block (make-type-block :vars '(x) :exprs '((x (true-type))))))
    (block-addexpr block '(x (false-type)))
    (inherit-types block)
    (?equalp (block-vartype block 'x) (instance-type 'false-type))))

(deftest inheriting-several-types 
  (let ((block (make-type-block :vars '(x y) :exprs '((x y) (y (true-type))))))
    (inherit-types block)
    (?equalp (block-vartype block 'x) (instance-type 'true-type))
    (?equalp (block-vartype block 'y) (instance-type 'true-type))))

;adding temporal variables
;add partial evaluation
;hierarchical blocks
;adding blocks

;exressions 
#|
(defun a (b c)
  (+ b c 1))

(defun b (c d)
  (+ (a c d) 5))
|#

;blocks
#|
(block (a b)
  (block (b c)
    (= a (function tmp b c))
    (= b number)
    (= c number)
|#    



    
				
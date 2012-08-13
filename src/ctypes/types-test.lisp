(in-package #:burning-ctypes-test)

(in-case types-test)

(deftest making-type
  (let ((type (make-type '(arg1 arg2 arg3) '(rel1 t1 t2 t3) '(rel2 t12 t22))))
    (!equal (type-arguments type) '(arg1 arg2 arg3))
    (!equal (type-relations type) '((rel1 t1 t2 t3) (rel2 t12 t22)))))

(deftest symbol-type-test
  (let ((type1 (make-type '(arg) '(rel1) '(rel2 t1 t2)))
	(type2 (make-type '(arg2) ())))
    (?condition (symbol-type 'a-type)
		ctypes-undefined-type 
		(ctypes-undefined-type-name 'a-type))
    (setf (symbol-type 'a-type) type1)
    (?equalp (symbol-type 'a-type) type1)
    (setf (symbol-type 'a-type) type2)
    (?equalp (symbol-type 'a-type) type2)
    (setf (symbol-type 'a-type) nil)
    (?null (symbol-type 'a-type))
    (unbind-type 'a-type)
    (?condition (symbol-type 'a-type)
		ctypes-undefined-type 
		(ctypes-undefined-type-name 'a-type))))

(deftest local-types-test
  (let ((type1 (make-type '(arg1)))
	(type2 (make-type '(arg2))))
    (setf (symbol-type 'a-type) type1)
    (in-type-context (make-type-context)
      (setf (symbol-type 'a-type) type2)
      (?equalp (symbol-type 'a-type) type2))
    (?equalp (symbol-type 'a-type) type1)
    (unbind-type 'a-type)))

(deftest global-types-local-access
  (setf (symbol-type 'a-type) (make-type '(a) ()))
  (in-type-context (make-type-context)
    (?equalp (symbol-type 'a-type) (make-type '(a) ())))
  (unbind-type 'a-type))

(deftest saving-context
  (let ((context (make-type-context)))
    (in-type-context context
      (setf (symbol-type 'a-type) (make-type '(an-arg) ())))
    (in-type-context context
      (?equalp (symbol-type 'a-type) (make-type '(an-arg) ())))))

(deftest tlet-test
  (?eq
   (tlet ((type-a '(arg1 arg2) '(mod1) '(rel1 t1 t2) '(rel2 t3 t4))
	  (type-b '(arg3) '(mod2 mod3) '(self type-b) '(other type-a)))
     (?equalp (symbol-type 'type-a) (make-type '(arg1 arg2) '(mod1) '(rel1 t1 t2) '(rel2 t3 t4)))
     (?equalp (symbol-type 'type-b) (make-type '(arg3) '(mod2 mod3) '(self type-b) '(other type-a)))
     'ok)
   'ok))

(deftest type-reader-macro
  (let ((type (make-type '(arg1 arg2) '(mod1) '(rel1 t1 t2) '(rel2 t3 t4))))
    (setf #Ttype type)
    (?equal #Ttype type)
    (?equal (symbol-type 'type) type)
    (unbind-type 'type)))

(define-type my-type (arg1 arg2)
  (> t1 t2 t3)
  (< t4 t5 t6)
  (self my-type))

(deftest define-type-test
  (?equalp #Tmy-type (make-type '(arg1 arg2) '(> t1 t2 t3) '(< t4 t5 t6) '(self my-type))))






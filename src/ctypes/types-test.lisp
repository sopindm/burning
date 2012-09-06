(in-package #:burning-ctypes-test)
(in-readtable burning-ctypes)

;;
;; types
;;

(in-case types-test)

(deftest making-types
  (let ((type (make-type 'some-type '(arg1 arg2 arg3))))
    (?eq (type-name type) 'some-type)
    (?equal (type-args-list type) '(arg1 arg2 arg3))))

(define-equality-check type=)

(deftest type=-test
  (let ((type1 (make-type 'type1 '(arg1 arg2 arg3)))
	(type2 (make-type 'type1 '(arg1 arg2 arg3))))
    (?type= type1 type2))
  (let ((type1 (make-type 'name ()))
	(type2 (make-type 'other-name ())))
    (?not (type= type1 type2)))
  (let ((type1 (make-type 'name '(some args)))
	(type2 (make-type 'name '(some other args))))
    (?not (type= type1 type2))))

(deftest copying-types
  (let ((type (make-type 'a-type '(arg1 arg2))))
    (let ((new-type (copy-type type)))
      (?type= new-type type))
    (let ((new-type (copy-type type :new-name 'other-type)))
      (?eq (type-name new-type) 'other-type))
    (let ((new-type (copy-type type :new-args-list '(some other args))))
      (?equal (type-args-list new-type) '(some other args)))))

;;
;; type tables
;;

(in-case type-tables-test)

(defun ?have-types (table &rest types)
  (flet ((?have-type (type)
	   (?type= (if table (get-type (type-name type) table) (get-type (type-name type))) type)))
    (when table (!= (type-table-size table) (length types)))
    (mapc #'?have-type types)))

(deftest making-type-tables
  (let ((table (make-type-table))
	(type1 (make-type 'a-type '(some-args)))
	(type2 (make-type 'other-type '(other args))))
    (?error (get-type 'a-type table) 
	    "Unknown type ~a in type table ~a." 'a-type table)
    (set-type type1 table)
    (set-type type2 table)
    (?have-types table type1 type2)))

(deftest removing-types-from-table
  (let ((type1 (make-type 'type1 '()))
	(type2 (make-type 'type2 '())))
    (let ((table (make-type-table type1 type2)))
      (?have-types table type1 type2)
      (remove-type type1 table)
      (?have-types table type2)
      (remove-type 'type2 table)
      (?have-types table))))

(deftest copying-type-tables
  (let ((type1 (make-type 'a-type '(some args)))
	(type2 (make-type 'other-type '(some other args))))
    (let* ((table (make-type-table type1 type2))
	   (new-table (copy-type-table table)))
      (?have-types new-table type1 type2)
      (let ((new-type (copy-type type1 :new-args-list '(new args))))
	(set-type new-type new-table)
	(?have-types new-table new-type type2)
	(?have-types table type1 type2)))))
    
(deftest making-table-with-prototype
  (let ((type1 (make-type 'type1 ()))
	(type2 (make-type 'type2 ()))
	(type3 (make-type 'type3 ())))
    (let* ((base-table (make-type-table type1 type2))
	   (table (make-type-table type3 :prototype base-table)))
      (?have-types table type1 type2 type3))))

(deftest type-tables-in-dynamic-scope
  (let ((type1 (make-type 'type1 ()))
	(type2 (make-type 'type2 ())))
    (with-type-table (make-type-table type1 type2)
      (?type= (get-type 'type1) type1)
      (?type= (get-type 'type2) type2))))

(deftest local-type-scope
  (let ((type1 (make-type 'type1 ()))
	(new-type1 (make-type 'type1 '(some args)))
	(type2 (make-type 'type2 ()))
	(type3 (make-type 'type3 ())))
    (with-type-table (make-type-table type1 type2)
      (with-local-type-table
	(set-type type3)
	(?have-types nil type1 type2 type3)
	(set-type new-type1)
	(?type= (get-type 'type1) new-type1))
      (?have-types nil type1 type2))))

(deftest with-type-table-returns-type-table
  (let ((table (with-type-table (make-type-table))))
    (set-type (make-type 'a-type '(bla bla-bla)) table))
  (let ((table (with-local-type-table)))
    (set-type (make-type 'a-type '(bla bla-bla)) table)))

(deftest defining-types
  (with-type-table (make-type-table)
    (define-type a-type (arg1 arg2 &rest other-args) ())
    (define-type b-type (arg1 arg2) ())
    (?have-types nil (make-type 'a-type '(arg1 arg2 &rest other-args))
		 (make-type 'b-type '(arg1 arg2)))))

(deftest defining-types-in-table
  (let ((table (make-type-table)))
    (define-type my-type () () :type-table table)
    (?type= (get-type 'my-type table) (make-type 'my-type ()))))

(deftest defining-types-with-wrong-options
  (?condition (eval '(define-type a-type () () :wrong-option 'value))
	      type-error (type-error-datum :wrong-option)))

(defmacro def-type-test (name &body body)
  `(deftest ,name
     (with-type-table (make-type-table)
       ,@body)))

(def-type-test checking-types-lambda-lists 
  (define-type a-type (a b &rest c) ())
  (?error (define-type b-type (&rest a b c) ())
	  "Wrong &rest lambda list ~a." '(a b c)))
	  

;;
;; Type instances
;;

(def-type-test simple-instancing-types
  (define-type a-type () ())
  (let ((instance (make-type-instance 'a-type)))
    (?type= (instance-type instance) (get-type 'a-type))
    (?eq (instance-args instance) ())))

(def-type-test type-instance-reader-macro
  (define-type a-type (arg1 arg2) ())
  (let ((instance #T(a-type 'arg1 'arg2)))
    (?type= (instance-type instance) (get-type 'a-type))
    (?equal (instance-args instance) '(arg1 arg2))))

(def-type-test type-instancing-in-type-table
  (let ((table (make-type-table)))
    (define-type a-type () () :type-table table)
    (let ((instance #T(a-type :type-table table)))
      (?type= (instance-type instance) (get-type 'a-type table))
      (?eq (instance-args instance) ()))))

(def-type-test checking-type-instances-simple-lambda-lists
  (define-type a-type () ())
  #T(a-type)
  (?error #T(a-type 'some 'args) "Too much arguments for lambda list ~a in ~a." () '(some args))
  (define-type b-type (arg1 arg2) ())
  #T(b-type 'arg 'other-arg)
  (?error #T(b-type 'arg) "Not enought arguments for lambda list ~a in ~a." '(arg1 arg2) '(arg)))

(def-type-test checking-type-instances-lambda-lists-with-rest-and-dots
  (define-type a-type (a &rest b) ())
  #T(a-type 'arg)
  #T(a-type 'arg1 'arg2)
  (?error #T(a-type) "Not enought arguments for lambda list ~a in ~a." '(a) ()))

;;
;; Type relations
;;

(in-case type-relations-test)

(def-type-test creating-simple-relations
  (define-relation simple-relation (type1 type2) (declare (ignore type1 type2)) 'dont-know)
  (define-type type1 () ())
  (define-type type2 () ())
  (?eq (#Rsimple-relation #Ttype1 #Ttype2)
       'dont-know))

(def-type-test creating-relation-without-body
  (define-relation relation (t1 t2))
  (define-type type () ())
  (?error (#Rrelation #Ttype #Ttype)
	  "No method for relation ~a and types ~a, ~a." 'relation 'type 'type))

(def-type-test testing-unexisting-relation
  (define-type type1 () ())
  (define-type type2 () ())
  (?error (#Rsome-relation #Ttype1 #Ttype2) (format nil "Unknown type relation ~a." 'some-relation)))

(def-type-test removing-relation
  (define-type a-type () ())
  (define-relation relation (t1 t2) (declare (ignore t1 t2)) 'maybe)
  (?eq (#Rrelation #Ta-type #T a-type) 'maybe)
  (remove-relation 'relation)
  (?error (#Rrelation #Ta-type #Ta-type) "Unknown type relation ~a." 'relation))

(def-type-test relations-with-simple-specialization
  (define-type type1 () ())
  (define-type type2 () ())
  (define-relation relation (t1 t2) (list (instance-type t1) (instance-type t2)))
  (define-relation-method relation (t1 (type2)) (instance-type t1))
  (?type= (#Rrelation #Ttype1 #Ttype2) (get-type 'type1))
  (mapcheck ?type= (#Rrelation #Ttype1 #Ttype1) (list (get-type 'type1) (get-type 'type1)))
  (define-relation-method relation ((type1) t2) (instance-type t2))
  (?type= (#Rrelation #Ttype1 #Ttype2) (get-type 'type2))
  (?type= (#Rrelation #Ttype2 #Ttype2) (get-type 'type2))
  (define-relation-method relation ((type1) (type2)) 'both)
  (?equal (#Rrelation #Ttype1 #Ttype2) 'both)
  (mapcheck ?type= (#Rrelation #Ttype2 #Ttype1) (mapcar #'get-type '(type2 type1))))

(def-type-test removing-relation-methods
  (define-type type1 () ())
  (define-type type2 () ())
  (define-relation relation (t1 t2) (declare (ignore t1 t2)) 'nothing)
  (define-relation-method relation ((type1) t2) (declare (ignore t2)) 'first)
  (define-relation-method relation ((type1) (type2)) 'both)
  (?eq (#Rrelation #Ttype1 #Ttype2) 'both)
  (remove-relation-method 'relation 'type1 'type2)
  (?eq (#Rrelation #Ttype1 #Ttype2) 'first)
  (remove-relation-method 'relation 'type1 nil)
  (?eq (#Rrelation #Ttype1 #Ttype2) 'nothing))

(def-type-test removing-wrong-method-error
  (?error (remove-relation-method 'some-relation nil nil)
	  "Unknown type relation ~a." 'some-relation)
  (define-relation relation (t1 t2))
  (?error (remove-relation-method 'relation 'some-type 'other-type)
	  "Unknown method for type relation ~a and types ~a, ~a." 'relation 'some-type 'other-type))

(def-type-test relations-with-argument-binding
  (define-type type1 (a &optional b c) ())
  (define-type type2 (a &rest b) ())
  (define-relation relation (t1 t2))
  (define-relation-method relation ((type1 a &optional (b 1) (c a)) (type2 e &rest f))
    (list a b c e f))
  (?equal (#Rrelation #T(type1 -1) #T(type2 1 2 3 4)) '(-1 1 -1 1 (2 3 4))))

(def-type-test checking-relation-argument-names
  (define-type type1 (a b) ())
  (define-type type2 (c d) ())
  (define-relation relation (t1 t2))
  (?error (macroexpand '(define-relation-method relation ((type1 a b) (type2 a c)) (declare (ignore a b c)) ()))
	  "Variables ~a occurs more than once in lambda list ~a." '(a) '((a b) (a c))))

(def-type-test checking-type-lambda-list
  (define-type type1 (a b c) ())
  (define-relation relation (t1 t2))
  (?error (define-relation-method relation ((type1 a b) t2) (declare (ignore a b t2)) ())
	  (lines "Lambda list of method ~a is incompatible with that of type ~a."
		 "Method's lambda list: ~a"
		 "Type's lambda list: ~a")
	  'relation 'type1
	  '(a b)
	  '(a b c)))

(def-type-test checking-bad-lambda-lists
  (?error (define-type type (&bad a b c) ())
	  "Wrong lambda list keywords ~a." '(&bad))
  (define-type type (a b c) ())
  (?error (macroexpand '(define-relation-method relation ((type &bad a b c) t2) ()))
	  "Wrong lambda list keywords ~a." '(&bad)))
	  
(def-type-test relations-in-specified-table
  (define-type type () ())
  (define-relation relation (t1 t2) (declare (ignore t1 t2)) 'global)
  (let ((table (make-type-table)))
    (define-type type () () :type-table table)
    (define-relation (relation :type-table table) (t1 t2) (declare (ignore t1 t2)) 'local)
    (?eq (call-relation 'relation 
			(make-type-instance 'type :type-table table)
			(make-type-instance 'type :type-table table)
			table)
	 'local)
    (define-relation-method (relation :type-table table) ((type) (type)) 'inherit)
    (?eq (call-relation 'relation 
			(make-type-instance 'type :type-table table)
			(make-type-instance 'type :type-table table)
			table)
	 'inherit))
  (?eq (call-relation 'relation (make-type-instance 'type) (make-type-instance 'type))
       'global))

(def-type-test copying-relations-with-type-tables
  (define-type type1 () ())
  (define-type type2 () ())
  (define-relation relation (t1 t2) (declare (ignore t1 t2)) 'global)
  (define-relation-method relation ((type1) t2) (declare (ignore t2)) 'for-t1)
  (let ((local-table (with-local-type-table 
		       (remove-relation-method 'relation 'type1 nil)
		       (define-relation-method relation (t1 (type2)) (declare (ignore t1)) 'for-t2))))
    (?eq (#Rrelation #Ttype1 #Ttype2) 'for-t1)
    (?eq (#Rrelation #Ttype2 #Ttype2) 'global)
    (define-relation-method relation ((type1) (type2)) 'both)
    (with-type-table local-table
      (?eq (#Rrelation #Ttype1 #Ttype1) 'global)
      (?eq (#Rrelation #Ttype2 #Ttype2) 'for-t2))))

(def-type-test defining-relations-for-type
  (define-type type1 () ())
  (define-relation relation (t1 t2) (declare (ignore t1 t2)) nil)
  (define-type type2 ()
      ((relation type1)))
  (?null (#Rrelation #Ttype2 #Ttype1))
  (?t (#Rrelation #Ttype1 #Ttype2)))

(def-type-test defining-relations-for-types-with-arguments
  (define-relation relation (t1 t2) (declare (ignore t1 t2)) nil)
  (define-type type1 (a b) ())
  (define-type type2 (a b c) 
      ((relation (type1 a b))
       (relation (type1 (#'- a c)))))
  (?t (#Rrelation #T(type1 1 2) #T(type2 1 2 3)))
  (?null (#Rrelation #T(type1 1 2) #T(type2 3 2 1))))

;symmetric and symmetric-to relations

;
; (define-type name (pattern) (&body relations) &body options)
;
; (define-type array (type &rest dimensions)
;   ((inherit (sequence type))))
; 
; (define-type array (#Tint '*)
;   ((inherit int-sequence)))
;  (#Rinheirt #T(array #Tint) #(sequence #Tint)) -> nil
;  (#Rinheirt #T(array #Tint) #(int-sequence)) -> t






(in-package #:burning-ctypes-test)
(in-readtable burning-ctypes)

;;
;; types
;;

(in-case types-test)

(deftest making-types
  (let ((type (make-type 'some-type '(arg1 arg2 arg3))))
    (?eq (type-name type) 'some-type)
    (?equal (type-args-list type) '(arg1 arg2 arg3))
    (?null (imagine-type-p type)))
  (let ((type (make-type 'name 'args :imagine-type-p t)))
    (?t (imagine-type-p type))))

(define-equality-check type=)

(deftest type=-test
  (let ((type1 (make-type 'type1 '(arg1 arg2 arg3)))
	(type2 (make-type 'type1 '(arg1 arg2 arg3))))
    (?type= type1 type2))
  (let ((type1 (make-type 'type '((a (very (very))) (deep (tree)))))
	(type2 (make-type 'type '((a (very (very))) (deep (tree))))))
    (?type= type1 type2))
  (let ((type1 (make-type 'name ()))
	(type2 (make-type 'other-name ())))
    (?not (type= type1 type2)))
  (let ((type1 (make-type 'name '(some args)))
	(type2 (make-type 'name '(some other args))))
    (?not (type= type1 type2)))
  (let ((type1 (make-type 'name () :imagine-type-p t))
	(type2 (make-type 'name () :imagine-type-p nil)))
    (?not (type= type1 type2))))

(deftest copying-types
  (let ((type (make-type 'a-type '((arg1 argarg1) arg2))))
    (let ((new-type (copy-type type)))
      (?type= new-type type))
    (let ((new-type (copy-type type :new-name 'other-type)))
      (?eq (type-name new-type) 'other-type))
    (let ((new-type (copy-type type :new-args-list '(some other args))))
      (?equal (type-args-list new-type) '(some other args)))
    (let ((new-type (copy-type type :imagine-type-p t)))
      (?t (imagine-type-p new-type)))))

;inheritance

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
	    (format nil "Unknown type ~a in type table ~a." 'a-type table))
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

(deftest defining-types
  (with-type-table (make-type-table)
    (define-type a-type (arg1 arg2 &rest other-args) ())
    (define-type b-type (arg1 arg2) () 
		 :imagine-type-p t)
    (?have-types nil 
		 (make-type 'a-type '(arg1 arg2 &rest other-args))
		 (make-type 'b-type '(arg1 arg2) :imagine-type-p t))))

(deftest defining-types-in-table
  (let ((table (make-type-table)))
    (define-type my-type () () 
      :type-table table)
    (?type= (get-type 'my-type table) (make-type 'my-type ()))))

(deftest defining-types-with-wrong-options
  (?condition (eval '(define-type a-type () () :wrong-option 'value))
	      type-error (type-error-datum :wrong-option)))

(defmacro def-type-test (name &body body)
  `(deftest ,name
     (with-type-table (make-type-table)
       ,@body)))

(def-type-test type-reader-macro
  (define-type a-type () ())
  (?type= #Ta-type (get-type 'a-type)))

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

(def-type-test testing-unexisting-relation
  (define-type type1 () ())
  (define-type type2 () ())
  (?error (#Rsome-relation #Ttype1 #Ttype2) (format nil "Unknown type relation ~a." 'some-relation)))

;removing relations

(def-type-test relations-with-simple-specialization
  (define-type type1 () ())
  (define-type type2 () ())
  (define-relation relation (t1 t2) (declare (ignore t1 t2)) 'dont-know)
  (define-relation-method relation (t1 (t2 type2)) (declare (ignore t1)) (list 't2 (type-name t2)))
  (?equal (#Rrelation #Ttype1 #Ttype2) (list 't2 'type2))
  (?eq (#Rrelation #Ttype1 #Ttype1) 'dont-know)
  (define-relation-method relation ((t1 type1) t2) (declare (ignore t2)) (list 't1 (type-name t1)))
  (?equal (#Rrelation #Ttype1 #Ttype2) (list 't1 'type1))
  (?equal (#Rrelation #Ttype2 #Ttype2) (list 't2 'type2))
  (define-relation-method relation ((t1 type1) (t2 type2)) (list 'both (type-name t1) (type-name t2)))
  (?equal (#Rrelation #Ttype1 #Ttype2) (list 'both 'type1 'type2))
  (?eq (#Rrelation #Ttype2 #Ttype1) 'dont-know))

;specializaion with inheritance

;removing relation methods
;removing wrong method

;simple relations with unification

;;defining methods (using pattern matching)
;relations in table (locality, copying, etc )
;relation expanders
;expanding transitive relations
;relations in types with parameters (returns unification)
;enumerable relations
;reflecting relations







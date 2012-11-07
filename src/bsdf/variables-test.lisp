(in-package #:burning-bsdf-test)

(in-case variables-test)

(deftest making-variables
  (let ((var (make-variable "var" "123")))
    (?equal (variable-name var) "var")
    (?equal (variable-expression var) "123")
    (?equal (variable-description var) "")
    (?eq (variable-type var) t)
    (?null (variable-visible-p var)))
  (let ((var (make-variable "var2" "456" :description "sample variable")))
    (?equal (variable-description var) "sample variable"))
  (let ((var (make-variable "var3" "789" :visible-p t)))
    (?t (variable-visible-p var))))

;variable name must me string or symbol

(defmacro ?expr= ((expr type) value string &optional (test '?equal))
  (let ((var-sym (gensym)))
    `(let ((,var-sym (make-variable "var" ,expr :type ,type)))
       (?condition-safe (?equal (variable-type ,var-sym) ,type) bsdf-warning)
       (?condition-safe (,test (variable-value ,var-sym) ,value) bsdf-warning)
       (?condition-safe (?equal (variable-string ,var-sym) ,string) bsdf-warning))))

(deftest defining-string-variables
  (?expr= ("123" :string) "123" "123")
  (?expr= (123 :string) "123" "123")
  (?expr= ((path-from-string "a.file") :string) "a.file" "a.file")
  (?expr= (:variable :string) "VARIABLE" "VARIABLE"))

(defun wrong-cast-message (value type expected-type)
  (format nil "Cannot convert ~a from type ~a to ~a" value type expected-type))

(defmacro ?wrong-cast (value type expected-type)
  `(?bsdf-compilation-error (make-variable "var" ,value :type ,expected-type)
			    (lines "In definition of variable 'var':"
				   (wrong-cast-message ,value ,type ,expected-type))
			    ,value ,type ,expected-type))
				   
(deftest making-simple-integer-variables
  (?expr= (123 :int) 123 "123")
  (?expr= ("123" :int) 123 "123"))

(deftest making-wrong-integers
  (?wrong-cast :value :enum :int)
  (?wrong-cast t :bool :int)
  (?wrong-cast (path-from-string "123") :path :int)
  (?wrong-cast "123 is a wrong integer" :string :int))

(deftest making-integers-with-boundaries
  (?expr= (123 '(:int -100 123)) 123 "123")
  (?expr= (-200 '(:int -200 -200)) -200 "-200")
  (?expr= (10 '(:int 0)) 10 "10")
  (?expr= (10 '(:int * 100)) 10 "10")
  (?wrong-cast 123 :int '(:int 0 100))
  (?wrong-cast -201 :int '(:int -200 0))
  (?wrong-cast 98 :int '(:int 99))
  (?wrong-cast 101 :int '(:int * 100)))

(deftest making-bool-variables
  (?expr= (nil :bool) nil "()")
  (?expr= (t :bool) t "t")
  (?wrong-cast "nil" :string :bool)
  (?wrong-cast 123 :int :bool)
  (?wrong-cast (path-from-string "t") :path :bool)
  (?wrong-cast :value :enum :bool))

(deftest making-path-variables
  (let ((path (path-from-string "a.file")))
    (?expr= (path :path) path "a.file" path=)
    (?expr= (path :file) path "a.file" path=))
  (?expr= ("a.file" :path) (path-from-string "a.file") "a.file" path=)
  (?expr= ("a.file" :file) (path-from-string "a.file") "a.file" path=))
  
(deftest making-directory-variables
  (let ((path (path-from-string "a.dir/")))
    (?expr= (path :path) path "a.dir/" path=)
    (?expr= (path :directory) path "a.dir/" path=))
  (?expr= ("a.dir/" :directory) (path-from-string "a.dir/") "a.dir/" path=))

(deftest wrong-paths-errors
  (?wrong-cast 123 :int :path)
  (?wrong-cast :path :enum :path)
  (?wrong-cast (path-from-string "a.file") :path :directory)
  (?wrong-cast (path-from-string "a.dir/") :path :file))

(deftest making-enum-variables
  (?expr= (:var :enum) :var "VAR")
  (?expr= (:a '(:enum :a :b :c)) :a "A"))

(deftest wrong-enum-variables
  (?wrong-cast "var" :string :enum)
  (?wrong-cast :value :enum '(:enum :other-value)))

(deftest defining-list-variables
  (?expr= ('(1 2 3) :list) '(1 2 3) "(1 2 3)"))

(deftest wrong-variable-expressions-error
  (?bsdf-compilation-error (make-variable "var" "123" :type :list)
			   (lines "In definition of variable 'var':"
				  "Cannot convert ~a from type ~a to ~a")
			   "123" :string :list)
  (?bsdf-compilation-error (make-variable "var2" #(1 2 3))
			   (lines "In definition of variable 'var2':"
				  "Unknown BSDF type for ~a")
			   #(1 2 3)))

(deftest defining-list-variables-with-type
  (?expr= ('("a" 2 :c) '(:list :string)) '("a" "2" "C") "(a 2 C)")
  (?expr= ('(((1 2 "3") ("4" "5" "6")) (("7" 8 9))) '(:list (:list (:list :string))))
	  '((("1" "2" "3") ("4" "5" "6")) (("7" "8" "9"))) "(((1 2 3) (4 5 6)) ((7 8 9)))"))

(deftest list-with-type-wrong-casts
  (?wrong-cast '(1 2 :3) :list '(:list :int)))

(defmacro ?wrong-type (expr type)
  `(?bsdf-compilation-error (make-variable "var" ,expr :type ',type)
			    (lines "In definition of variable 'var':"
				   "Wrong BSDF type ~a") ',type))

(deftest wrong-variable-type-error
  (?wrong-type "123" (:string "bla"))
  (?wrong-type "456" :wrong)
  (?wrong-type '(1 2 3) (:list :wrong-type))
  (?wrong-type '(1 2 3) (:list (:list (:list :wrong-type))))
  (?wrong-type 123 (t 1 2 3))
  (?wrong-type 123 (:int 1 2 3))
  (?wrong-type "a.file" (:path 1))
  (?wrong-type :a (:enum :a :b 1 2 3))
  (?wrong-type nil (:bool 1)))

;string expressions

(deftest simple-variable-expression
  (?expr= ('(++ "abc" "def" "ghi") :string) "abcdefghi" "abcdefghi")
  (?expr= ('(substring "abcdef" 1) :string) "bcdef" "bcdef")
  (?expr= ('(substring "abcdef" 2 "4") :string) "cd" "cd")
  (?expr= ('(substring "abcdef" -2) :string) "ef" "ef")
  (?expr= ('(substring "abcdef" "-1") :string) "f" "f")
  (?expr= ('(substring "abcdef" -3 "-1") :string) "def" "def"))

(deftest wrong-operation-error
  (?bsdf-compilation-error (make-variable "var" '(wrong-symbol 1 2 3))
			   (lines "In definition of variable 'var':"
				  "Wrong BSDF operation ~a") 'wrong-symbol))

(defmacro ?wrong-expr ((expr &rest args) error &rest error-args)
  `(?bsdf-compilation-error (make-variable "var" '(,expr ,@args))
			    (lines "In definition of variable 'var':"
				   ,error)
			    ,@error-args))

(defmacro ?wrong-expr-arg ((expr &rest args) arg-name error)
  `(?wrong-expr (,expr ,@args) (lines* "In argument '~a' of '~a':" ,error) ',arg-name ',expr))
	       
(deftest wrong-arguments-error
  (?wrong-expr-arg (substring "abc" "a") first (wrong-cast-message "a" :STRING :INT))
  (?wrong-expr-arg (substring "abc" "1" "2a") last (wrong-cast-message "2a" :STRING :INT)))

(deftest wrong-substring-arguments
  (?wrong-expr (substring "abc" 10) "Bad interval [10, 3) for string 'abc'")
  (?wrong-expr (substring "abc" 0 10) "Bad interval [0, 10) for string 'abc'")
  (?wrong-expr (substring "abc" 2 1) "Bad interval [2, 1) for string 'abc'"))

(deftest +-expression-test
  (?expr= ('(+ 1 2) :int) 3 "3")
  (?expr= ('(+ 1 2 3) :int) 6 "6")
  (?expr= ('(+) :int) 0 "0")
  (?wrong-expr-arg (+ 1 "a") args (wrong-cast-message '(1 "a") :LIST '(:LIST :INT))))

(deftest --expression-test
  (?expr= ('(- 2 1) :int) 1 "1")
  (?expr= ('(- 2) :int) -2 "-2")
  (?wrong-expr-arg (- "a" 1) number (wrong-cast-message "a" :STRING :INT))
  (?wrong-expr-arg (- 1 "a") numbers (wrong-cast-message '("a") '(:LIST :STRING) '(:LIST :INT))))

(deftest *-expression-test
  (?expr= ('(* 2 3) :int) 6 "6")
  (?expr= ('(* 2 3 4) :int) 24 "24")
  (?expr= ('(*) :int) 1 "1")
  (?wrong-expr-arg (* 1 "a") args (wrong-cast-message '(1 "a") :LIST '(:LIST :INT))))

(deftest /-expression-test
  (?expr= ('(/ 1 2) :int) 0 "0")
  (?expr= ('(/ 8 2) :int) 4 "4")
  (?expr= ('(/ 32 3 2) :int) 5 "5")
  (?wrong-expr-arg (/ "a" 1) number (wrong-cast-message "a" :STRING :INT))
  (?wrong-expr-arg (/ 1 2 "a") numbers (wrong-cast-message '(2 "a") :LIST '(:LIST :INT)))
  (?wrong-expr (/ 1 0) "Division by zero in (/ 1 0)"))

(deftest mod-expression-test
  (?expr= ('(mod 2 2) :int) 0 "0")
  (?expr= ('(mod 10 4) :int) 2 "2")
  (?expr= ('(mod 7 1) :int) 0 "0")
  (?wrong-expr-arg (mod "a" 1) number (wrong-cast-message "a" :STRING :INT))
  (?wrong-expr-arg (mod 1 "a") divisor (wrong-cast-message "a" :STRING :INT))
  (?wrong-expr (mod 1 0) "Division by zero in (mod 1 0)"))

(deftest cons-expression-test
  (?expr= ('(cons 1 nil) '(:list :int)) '(1) "(1)")
  (?expr= ('(cons nil (1 2 3)) :list) '(nil 1 2 3) "(NIL 1 2 3)")
  (?wrong-expr-arg (cons 1 2) list (wrong-cast-message 2 :INT :LIST)))

(deftest wrong-arguments-count
  (?wrong-expr (cons 1 2 3) "Too much arguments for lambda list ~a in ~a." '(item list) '(1 2 3))
  (?wrong-expr (cons 1) "Not enought arguments for lambda list ~a in ~a." '(item list) '(1)))

(deftest append-test
  (?expr= ('(append (1 2) (3 4)) '(:list :int)) '(1 2 3 4) "(1 2 3 4)")
  (?expr= ('(append (1 "2") (3 4) (5 "6")) :list) '(1 "2" 3 4 5 "6") "(1 2 3 4 5 6)")
  (?expr= ('(append (:a :b :c)) '(:list (:enum :a :b :c))) '(:a :b :c) "(A B C)")
  (?expr= ('(append) :list) () "()")
  (?expr= ('(append :a :b :c (1 2 3) 4 5 (6 7)) :list) '(:a :b :c 1 2 3 4 5 6 7) "(A B C 1 2 3 4 5 6 7)"))

(deftest fifth-test
  (?expr= ('(fifth (1 "a" 2 :b 3)) :int) 3 "3")
  (?wrong-expr-arg (fifth "abc") list (wrong-cast-message "abc" :STRING :LIST)))

(deftest nth-test
  (?expr= ('(nth "2" (1 2 3)) :int) 3 "3")
  (?wrong-expr-arg (nth 0 123) list (wrong-cast-message 123 :INT :LIST))
  (?wrong-expr-arg (nth "a" (1 2 3)) index (wrong-cast-message "a" :STRING '(:INT 0)))
  (?wrong-expr-arg (nth -1 (1 2 3)) index (wrong-cast-message -1 :INT '(:INT 0))))

(deftest remove-test
  (?expr= ('(remove "123" (123 "123" :123)) :list) '(123 :123) "(123 123)")
  (?expr= ('(remove 456 (123 (cons 789 nil))) :list) '(123 (789)) "(123 (789))")
  (?expr= ('(remove (123 456) (123 456 (123 456))) :list) '(123 456) "(123 456)")
  (?wrong-expr-arg (remove 123 456) list (wrong-cast-message 456 :INT :LIST)))

(deftest remove-duplicates-test
  (?expr= ('(remove-duplicates (123 "123" (123 456) :123 (123 456) "123" :123 123)) :list)
	  '(123 "123" (123 456) :123) "(123 123 (123 456) 123)")
  (?wrong-expr-arg (remove-duplicates 123) list (wrong-cast-message 123 :INT :LIST)))

(defmacro def-path-test (name &body body)
  `(deftest ,name 
     (let ((*default-filesystem* (make-virtual-filesystem)))
       ,@body)))

(def-path-test parent-path-expression
  (?expr= ('(parent-path "/home/a.file") :string) "/home/" "/home/")
  (?wrong-expr-arg (parent-path 123) path (wrong-cast-message 123 :INT :PATH)))

(def-path-test directory-path-exression
  (?expr= ('(directory-path "/home/a.file") :string) "/home/" "/home/")
  (?expr= ('(directory-path "/home/a.dir/") :string) "/home/a.dir/" "/home/a.dir/"))

(def-path-test root-path-expression-test
  (?expr= ('(root-path "/home/a.file") :string) "/" "/")
  (?expr= ('(root-path "home/a.file") :string) "" ""))

(def-path-test path+-expression-test
  (?expr= ('(path+ "/home/" "a.file") :string) "/home/a.file" "/home/a.file")
  (?wrong-expr-arg (path+ "/home/" 123) paths (wrong-cast-message '("/home/" 123) ':LIST '(:LIST :PATH))))

(def-path-test as-absolute-test
  (?expr= ('(as-absolute "a.file") :string) "/work/a.file" "/work/a.file")
  (?wrong-expr-arg (as-absolute 123) path (wrong-cast-message 123 :INT :PATH)))

(def-path-test as-relative-test
  (?expr= ('(as-relative "/home/a.dir/a.file" "/home/b.dir/") :string) "../a.dir/a.file" "../a.dir/a.file"))

(def-path-test copy-path-test
  (?expr= ('(copy-path "/home/a.file" :new-name "other") :string) "/home/other.file" "/home/other.file")
  (?expr= ('(copy-path "/home/new.file" :new-type "dir") :string) "/home/new.dir" "/home/new.dir")
  (?expr= ('(copy-path "a.file" :new-name 123) :string) "123.file" "123.file"))
  
;bool expressions
;;and
;;or
;;not

;variables in expressions

;predicates
;;= (not for bools only)
;;???

;conditionals
;;if
;;cond???


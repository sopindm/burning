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

(defmacro ?wrong-cast (name value type expected-type)
  `(?bsdf-compilation-error (make-variable ,name ,value :type ,expected-type)
			    (lines "In definition of variable '~a':"
				   "Cannot convert ~a from type ~a to ~a")
			    ,name ,value ,type ,expected-type))
				   
(deftest making-simple-integer-variables
  (?expr= (123 :int) 123 "123")
  (?expr= ("123" :int) 123 "123"))

(deftest making-wrong-integers
  (?wrong-cast "var" :value :enum :int)
  (?wrong-cast "var" t :bool :int)
  (?wrong-cast "var" (path-from-string "123") :path :int)
  (?wrong-cast "var" "123 is a wrong integer" :string :int))

(deftest making-integers-with-boundaries
  (?expr= (123 '(:int -100 123)) 123 "123")
  (?expr= (-200 '(:int -200 -200)) -200 "-200")
  (?expr= (10 '(:int 100)) 10 "10")
  (?wrong-cast "var" 123 :int '(:int 0 100))
  (?wrong-cast "var2" -201 :int '(:int -200 0))
  (?wrong-cast "var" 100 :int '(:int 99))
  (?wrong-cast "var" -1 :int '(:int 100)))

(deftest making-bool-variables
  (?expr= (nil :bool) nil "()")
  (?expr= (t :bool) t "t")
  (?wrong-cast "var" "nil" :string :bool)
  (?wrong-cast "var" 123 :int :bool)
  (?wrong-cast "var" (path-from-string "t") :path :bool)
  (?wrong-cast "var" :value :enum :bool))

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
  (?wrong-cast "var" 123 :int :path)
  (?wrong-cast "var" :path :enum :path)
  (?wrong-cast "var" (path-from-string "a.file") :path :directory)
  (?wrong-cast "var" (path-from-string "a.dir/") :path :file))

(deftest making-enum-variables
  (?expr= (:var :enum) :var "VAR")
  (?expr= (:a '(:enum :a :b :c)) :a "A"))

(deftest wrong-enum-variables
  (?wrong-cast "var" "var" :string :enum)
  (?wrong-cast "var" :value :enum '(:enum :other-value)))

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
  (?wrong-cast "var" '(1 2 :3) :list '(:list :int)))


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
  (?expr= ('(substring "abcdef" 2 4) :string) "cd" "cd")
  (?expr= ('(substring "abcdef" -2) :string) "ef" "ef")
  (?expr= ('(substring "abcdef" -1) :string) "f" "f")
  (?expr= ('(substring "abcdef" -3 -1) :string) "def" "def"))

;wrong expression error
;wrong type in expression error
;wrong expression arguments error

;int expressions
;;+
;;-
;;*
;;/
;;mod

;list expressions
;;append
;;first - fifth
;;nth
;;rest
;;nth-cdr
;;find
;;remove
;;remove-duplicates

;path expressions
;;path+
;;path-
;;as-absolute
;;as-relative

;bool expressions
;;= (not for bools only)
;;and
;;or
;;not

;variables in expressions
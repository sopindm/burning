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
       (?condition-safe (?equal (variable-type ,var-sym) ,type))
       (?condition-safe (,test (variable-value ,var-sym) ,value))
       (?condition-safe (?equal (variable-string ,var-sym) ,string)))))

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
  (?wrong-cast "var" 123 :int '(:int 0 100))
  (?wrong-cast "var2" -201 :int '(:int -200 0)))

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
  (?expr= ('("a" 2 :c) '(:list :string)) '("a" 2 "C") "(a 2 C)"))

;list <type> wrong casts

;checking type correctness (wrong variable type error)

;complex variable expressions (accessing and evaluating)
;;for string or t variables
;;for integer variables
;;for list variables
;;for path variables
;;for enum variables???
;;for boolean variables (and equality for other types)

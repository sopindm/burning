(in-package #:bsdf-test)

(in-case expressions-test)

(defmacro ?expr= ((expr type) value &optional (test '?equal))
  (let ((expr-sym (gensym)))
    `(let ((,expr-sym (cast-type ,expr ,type)))
       (?condition-safe (,test (expression-value ,expr-sym) ,value) bsdf-warning))))

(defmacro ?cast= ((expr type) value dest-type &optional (test '?equal))
  (let ((expr-sym (gensym)))
    `(let ((,expr-sym (cast-type ,expr ,type)))
       (?condition-safe (,test (cast-type ,expr-sym ,dest-type ,type) ,value)))))

(deftest string-expressions
  (?expr= ("123" :string) "123"))

(defun wrong-cast-message (value type expected-type)
  (format nil "Cannot convert ~a from type ~a to ~a" value type expected-type))

(defmacro ?wrong-cast (value type expected-type)
  `(?bsdf-compilation-error (cast-type ,value ,expected-type ,type)
			    (lines (wrong-cast-message ,value ,type ,expected-type))
			    ,value ,type ,expected-type))

(deftest making-simple-integer-expressions
  (?expr= (123 :int) 123)
  (?expr= ("123" :int) 123))

(deftest making-wrong-integers
  (?wrong-cast :value :enum :int)
  (?wrong-cast t :bool :int)
  (?wrong-cast (path-from-string "123") :path :int)
  (?wrong-cast "123 is a wrong integer" :string :int))

(deftest making-integers-with-boundaries
  (?expr= (123 '(:int -100 123)) 123)
  (?expr= (-200 '(:int -200 -200)) -200)
  (?expr= (10 '(:int 0)) 10)
  (?expr= (10 '(:int * 100)) 10)
  (?wrong-cast "123" :string '(:int 0 100))
  (?wrong-cast 123 :int '(:int 0 100))
  (?wrong-cast -201 :int '(:int -200 0))
  (?wrong-cast 98 :int '(:int 99))
  (?wrong-cast 101 :int '(:int * 100)))

(deftest casting-integers-to-strings
  (macrolet ((test-cast (&rest numbers)
	       `(progn ,@(mapcar (lambda (number) `(?cast= (,number :int) ,(format nil "~a" number) :string))
				 numbers))))
    (test-cast 123 -100)))

(deftest making-bool-expressions
  (?expr= (nil :bool) nil)
  (?expr= (t :bool) t))

(deftest casting-bool-expressions
  (?cast= (nil :bool) "nil" :string)
  (?cast= (t :bool) "t" :string)
  (?wrong-cast 123 :int :bool)
  (?wrong-cast (path-from-string "t") :path :bool)
  (?wrong-cast :value :enum :bool))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (define-equality-check path=))

(deftest making-path-expressions
  (let ((path (path-from-string "a.file")))
    (?expr= (path :path) path ?path=)
    (?expr= (path :file) path ?path=))
  (let ((path (path-from-string "a.dir/")))
    (?expr= (path :path) path ?path=)
    (?expr= (path :directory) path ?path=)))
  
(deftest casting-path-expressions
  (?cast= ("a.file" :string) (path-from-string "a.file") :path ?path=)
  (?cast= ("a.file" :string) (path-from-string "a.file") :file ?path=)
  (?cast= ("a.dir/" :string) (path-from-string "a.dir/") :path ?path=)
  (?cast= ("a.dir/" :string) (path-from-string "a.dir/") :directory ?path=)
  (let ((path (path-from-string "a.file")))
    (?cast= (path :path) "a.file" :string)
    (?cast= (path :file) path :path)
    (?cast= (path :file) "a.file" :string))
  (let ((path (path-from-string "a.dir/")))
    (?cast= (path :path) "a.dir/" :string)
    (?cast= (path :directory) path :path)
    (?cast= (path :directory) "a.dir/" :string))
  (?wrong-cast 123 :int :path)
  (?wrong-cast :path :enum :path)
  (?wrong-cast (path-from-string "a.file") :path :directory)
  (?wrong-cast (path-from-string "a.dir/") :path :file))

(deftest making-enum-expressions
  (?expr= (:var :enum) :var)
  (?expr= (:a '(:enum :a :b :c)) :a))

(deftest casting-enum-expressions
  (?cast= (:value :enum) "VALUE" :string)
  (?cast= ("some value" :string) :|some value| :enum)
  (?wrong-cast :value :enum '(:enum :other-value)))

(deftest defining-list-expressions
  (?expr= ('(1 2 3) :list) '(1 2 3))
  (?expr= ('("a" 2 :c) '(:list :string)) '("a" "2" "C"))
  (?expr= ('(((1 2 "3") ("4" "5" "6")) (("7" 8 9))) '(:list (:list (:list :string))))
	  '((("1" "2" "3") ("4" "5" "6")) (("7" "8" "9")))))

(deftest casting-list-expressions
  (?wrong-cast '(1 2 :3) :list '(:list :int))
  (?cast= ('(1 2 3) :list) "(1;2;3)" :string)
  (?cast= ("(1 2 3)" :string) '("1 2 3") :list)
  (?cast= ("  (1   )        " :list) '("1   ") :list)
  (?cast= ("(1;2;3)" :string) '(1 2 3) '(:list :int))
  (?cast= ('((1 2) (3 4) ((5))) :list) "((1;2);(3;4);((5)))" :string)
  (?cast= ("(1;(2;3);((4));(5;(6)))" :string) '("1" "(2;3)" "((4))" "(5;(6))") :list)
  (?cast= ("(1\\;2)" :string) '("1\\;2") :list)
  (?cast= ("(1\\\\;2)" :string) '("1\\\\" "2") :list)
  (?cast= ("(\\(1;2\\))" :string) '("\\(1" "2\\)") :list)
  (?wrong-cast "1 2 3" :string :list))

(defmacro ?wrong-type (expr type)
  `(?bsdf-compilation-error (cast-type ,expr ',type)
			    (lines "Wrong BSDF type ~a") ',type))

(deftest wrong-type-error
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

(deftest simple-string-expression
  (?expr= ('(++ "abc" "def" "ghi") :string) "abcdefghi")
  (?expr= ('(substring "abcdef" 1) :string) "bcdef")
  (?expr= ('(substring "abcdef" 2 "4") :string) "cd")
  (?expr= ('(substring "abcdef" -2) :string) "ef")
  (?expr= ('(substring "abcdef" "-1") :string) "f")
  (?expr= ('(substring "abcdef" -3 "-1") :string) "def"))

(defmacro ?wrong-expr ((expr &rest args) error &rest error-args)
  `(?bsdf-compilation-error (check-expression (expand-expression '(,expr ,@args)))
			    (lines ,error)
			    ,@error-args))

(defmacro ?wrong-expr-arg ((expr &rest args) arg-name error)
  `(?wrong-expr (,expr ,@args) (lines* "In argument '~a' of '~a':" ,error) ',arg-name ',expr))

(deftest wrong-function-error
  (?bsdf-compilation-error (check-expression '(wrong-symbol 1 2 3)) 
			   (lines "Wrong BSDF function ~a") 'wrong-symbol))

(deftest wrong-arguments-error
  (?wrong-expr-arg (substring "abc" "a") first (wrong-cast-message "a" :STRING :INT))
  (?wrong-expr-arg (substring "abc" "1" "2a") last (wrong-cast-message "2a" :STRING :INT)))

(deftest wrong-substring-arguments
  (?wrong-expr (substring "abc" 10) "Bad interval [10, 3) for string 'abc'")
  (?wrong-expr (substring "abc" 0 10) "Bad interval [0, 10) for string 'abc'")
  (?wrong-expr (substring "abc" 2 1) "Bad interval [2, 1) for string 'abc'"))

(deftest +-expression-test
  (?expr= ('(+ 1 2) :int) 3)
  (?expr= ('(+ 1 2 3) :int) 6)
  (?expr= ('(+) :int) 0)
  (?wrong-expr-arg (+ 1 "a") args (wrong-cast-message '(1 "a") :LIST '(:LIST :INT))))

(deftest --expression-test
  (?expr= ('(- 2 1) :int) 1)
  (?expr= ('(- 2) :int) -2)
  (?wrong-expr-arg (- "a" 1) number (wrong-cast-message "a" :STRING :INT))
  (?wrong-expr-arg (- 1 "a") numbers (wrong-cast-message '("a") '(:LIST :STRING) '(:LIST :INT))))

(deftest *-expression-test
  (?expr= ('(* 2 3) :int) 6)
  (?expr= ('(* 2 3 4) :int) 24)
  (?expr= ('(*) :int) 1)
  (?wrong-expr-arg (* 1 "a") args (wrong-cast-message '(1 "a") :LIST '(:LIST :INT))))

(deftest /-expression-test
  (?expr= ('(/ 1 2) :int) 0)
  (?expr= ('(/ 8 2) :int) 4)
  (?expr= ('(/ 32 3 2) :int) 5)
  (?wrong-expr-arg (/ "a" 1) number (wrong-cast-message "a" :STRING :INT))
  (?wrong-expr-arg (/ 1 2 "a") numbers (wrong-cast-message '(2 "a") :LIST '(:LIST :INT)))
  (?wrong-expr (/ 1 0) "Division by zero in (/ 1 0)"))

(deftest mod-expression-test
  (?expr= ('(mod 2 2) :int) 0)
  (?expr= ('(mod 10 4) :int) 2)
  (?expr= ('(mod 7 1) :int) 0)
  (?wrong-expr-arg (mod "a" 1) number (wrong-cast-message "a" :STRING :INT))
  (?wrong-expr-arg (mod 1 "a") divisor (wrong-cast-message "a" :STRING :INT))
  (?wrong-expr (mod 1 0) "Division by zero in (mod 1 0)"))

(deftest cons-expression-test
  (?expr= ('(cons 1 nil) '(:list :int)) '(1))
  (?expr= ('(cons nil (1 2 3)) :list) '(nil 1 2 3))
  (?wrong-expr-arg (cons 1 2) list (wrong-cast-message 2 :INT :LIST)))

(deftest wrong-arguments-count
  (?wrong-expr (cons 1 2 3) "Too much arguments for lambda list ~a in ~a." '(item list) '(1 2 3))
  (?wrong-expr (cons 1) "Not enought arguments for lambda list ~a in ~a." '(item list) '(1)))

(deftest append-test
  (?expr= ('(append (1 2) (3 4)) '(:list :int)) '(1 2 3 4))
  (?expr= ('(append (1 "2") (3 4) (5 "6")) :list) '(1 "2" 3 4 5 "6"))
  (?expr= ('(append (:a :b :c)) '(:list (:enum :a :b :c))) '(:a :b :c))
  (?expr= ('(append) :list) ())
  (?expr= ('(append :a :b :c (1 2 3) 4 5 (6 7)) :list) '(:a :b :c 1 2 3 4 5 6 7)))

(deftest fifth-test
  (?expr= ('(fifth (1 "a" 2 :b 3)) :int) 3)
  (?wrong-expr-arg (fifth "abc") list (wrong-cast-message "abc" :STRING :LIST)))

(deftest nth-test
  (?expr= ('(nth "2" (1 2 3)) :int) 3)
  (?wrong-expr-arg (nth 0 123) list (wrong-cast-message 123 :INT :LIST))
  (?wrong-expr-arg (nth "a" (1 2 3)) index (wrong-cast-message "a" :STRING '(:INT 0)))
  (?wrong-expr-arg (nth -1 (1 2 3)) index (wrong-cast-message -1 :INT '(:INT 0))))

(deftest remove-test
  (?expr= ('(remove "123" (123 "123" :123)) :list) '(123 :123))
  (?expr= ('(remove 456 (123 (cons 789 nil))) :list) '(123 (789)))
  (?expr= ('(remove (123 456) (123 456 (123 456))) :list) '(123 456))
  (?wrong-expr-arg (remove 123 456) list (wrong-cast-message 456 :INT :LIST)))

(deftest remove-duplicates-test
  (?expr= ('(remove-duplicates (123 "123" (123 456) :123 (123 456) "123" :123 123)) :list)
	  '(123 "123" (123 456) :123))
  (?wrong-expr-arg (remove-duplicates 123) list (wrong-cast-message 123 :INT :LIST)))

(defmacro def-path-test (name &body body)
  `(deftest ,name 
     (let ((*default-filesystem* (make-virtual-filesystem)))
       ,@body)))

(def-path-test parent-path-expression
  (?expr= ('(parent-path "/home/a.file") :string) "/home/")
  (?wrong-expr-arg (parent-path 123) path (wrong-cast-message 123 :INT :PATH)))

(def-path-test directory-path-exression
  (?expr= ('(directory-path "/home/a.file") :string) "/home/")
  (?expr= ('(directory-path "/home/a.dir/") :string) "/home/a.dir/"))

(def-path-test root-path-expression-test
  (?expr= ('(root-path "/home/a.file") :string) "/")
  (?expr= ('(root-path "home/a.file") :string) ""))

(def-path-test path+-expression-test
  (?expr= ('(path+ "/home/" "a.file") :string) "/home/a.file")
  (?wrong-expr-arg (path+ "/home/" 123) paths (wrong-cast-message '("/home/" 123) ':LIST '(:LIST :PATH))))

(def-path-test as-absolute-test
  (?expr= ('(as-absolute "a.file") :string) "/work/a.file")
  (?wrong-expr-arg (as-absolute 123) path (wrong-cast-message 123 :INT :PATH)))

(def-path-test as-relative-test
  (?expr= ('(as-relative "/home/a.dir/a.file" "/home/b.dir/") :string) "../a.dir/a.file"))

(def-path-test copy-path-test
  (?expr= ('(copy-path "/home/a.file" :new-name "other") :string) "/home/other.file")
  (?expr= ('(copy-path "/home/new.file" :new-type "dir") :string) "/home/new.dir")
  (?expr= ('(copy-path "a.file" :new-name 123) :string) "123.file"))

(bsdf-defmacro ct-eval (operation &rest args)
  (expression-value (cons operation args)))

(bsdf-defmacro ct-! (n)
  (if (= n 0) 1
      `(* ,n (ct-! ,(1- n)))))

(deftest compile-time-evaluation-test
  (let ((expr (expand-expression '(ct-eval + 1 2 3))))
    (?equal expr 6))
  (let ((expr (expand-expression '(first (ct-eval append (1 2 3) (4 5 6))))))
    (?equal expr '(first (1 2 3 4 5 6))))
  (let ((expr (expand-expression '(ct-! 5))))
    (?equal expr '(* 5 (* 4 (* 3 (* 2 (* 1 1))))))))

;bool expressions
;;and
;;or
;;not

;predicates
;;= (not for bools only)
;;???

;conditionals
;;if
;;cond???

;;
;; Dependencies
;;

(defmacro ?depends= (expr &optional input-variables input-files output-files)
  `(progn (check-expression (expand-expression ',expr))
	  (?equal (expression-dependencies (expand-expression ',expr))
		  (list ,input-variables ,input-files ,output-files))))

(deftest empty-expression-dependencies-test
  (?depends= (+ 1 2 3)))

(defmacro def-context-test (name &body body)
  `(deftest ,name 
     (let ((*context* (copy-context)))
       ,@body)))

(def-context-test simple-expression-dependencies-test
  (defvariable var '(+ 1 2 3))
  (?depends= var (list 'var))
  (?depends= ($ var))
  (?depends= (+ var 3 var) (list 'var))
  (defvariable var2 "a.file" :type :path)
  (?depends= (++ (as-absolute var2) var2 (+ 1 (* var 2))) (list 'var2 'var)))

(def-context-test with-input-files-test
  (defvariable var 1)
  (?depends= (with-input-files (+ 1 var 3) ("a.file" "b.file")) (list 'var) (list "a.file" "b.file"))
  (?expr= ('(with-input-files (third (++ "(aa;" "bb;" "cc)")) ("a.file" "b.file")) :string) "cc"))

(def-context-test with-output-files-test
  (defvariable var 2)
  (?depends= (with-output-files (+ 1 2 var) ("a.file" "b.file")) (list 'var) () (list "a.file" "b.file"))
  (?expr= ('(with-output-files (third (++ "(aa;" "bb;" "cc)")) ("a.file" "b.file")) :string) "cc"))
    

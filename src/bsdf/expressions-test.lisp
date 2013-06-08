(in-package #:bsdf-test)

(in-case expressions-test)

(defmacro ?wrong-type (type &rest args)
  `(?bsdf-error (bsdf-check-type ',(cons type args))
		(lines "Wrong BSDF type ~a") ',(if args (cons type args) type)))

(deftest wrong-type-error
  (?wrong-type :string "bla")
  (?wrong-type :wrong)
  (?wrong-type :list :wrong-type)
  (?wrong-type :list (:list (:list :wrong-type)))
  (?wrong-type t 1 2 3)
  (?wrong-type :int 1 2 3)
  (?wrong-type :path 1)
  (?wrong-type :file 1)
  (?wrong-type :enum :a :b 1 2 3)
  (?wrong-type :bool 1))

(deftest declared-function-specs
  (?equal (bsdf-function-lambda-list '++) '(&rest args))
  (?equal (bsdf-function-type '++) :string)
  (?equal (bsdf-function-argument-types '++) '((args (:list :string)))))

#|
;string expressions

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

(defmacro ?depends= (expr &optional variables input output)
  `(progn (check-expression (expand-expression ',expr))
	  (?equal (expression-dependencies (expand-expression ',expr))
		  (list ',variables ',input ',output))))

(deftest empty-expression-dependencies-test
  (?depends= (+ 1 2 3)))

(defmacro def-context-test (name &body body)
  `(deftest ,name 
     (let ((*context* (copy-context)))
       ,@body)))

(def-context-test simple-expression-dependencies-test
  (defvariable var (+ 1 2 3))
  (?depends= var (var))
  (?depends= ($ var))
  (?depends= (+ var 3 var) (var))
  (defvariable var2 "a.file" :type :path)
  (?depends= (++ (as-absolute var2) var2 (+ 1 (* var 2))) (var2 var)))

(def-context-test simple-with-input-files-test
  (defvariable var 1)
  (?depends= (with-input-files (+ 1 var 3) ("a.file" "b.file")) (var) (("a.file" "b.file")))
  (?expr= ('(with-input-files (third (++ "(aa;" "bb;" "cc)")) ("a.file" "b.file")) :string) "cc"))

(def-context-test with-input-files-with-more-complex-expressions
  (defvariable var '("a.file" "b.file"))
  (defvariable var2 '("c.file" "d.file"))
  (?depends= (with-input-files (+ 1 2 3) (append var "c.file"))
	     (var) ((append var "c.file")))
  (?depends= (with-input-files (with-input-files (+ 1 2) var) ("a.file" "b.file"))
	     (var) (("a.file" "b.file") var))
  (?depends= (with-input-files 
		 (with-input-files 
		     (with-input-files 
			 (with-input-files (+ 1 2) var) ("e.file")) var2) ("f.file"))
	     (var var2) (("e.file" "f.file") (append var var2)))
  (defvariable var3 "e.file")
  (?depends= (with-input-files (+ 1 2) ("a.file" "b.file" var3 "d.file"))
	     (var3) (("a.file" "b.file" "d.file") (list var3)))
  (?depends= (with-input-files (with-input-files (+ 1 2) (append var)) (append var2))
	     (var var2) ((append var var2)))
  (?depends= (with-input-files (with-input-files (+ 1 2) (append var var2)) (append var2 var))
	     (var var2) ((append var var2))))
  
(def-context-test with-output-files-test
  (defvariable var 2)
  (?depends= (with-output-files (+ 1 2 var) ("a.file" "b.file")) (var) () (("a.file" "b.file")))
  (defvariable var2 '("a.file" "b.file"))
  (defvariable var3 '("c.file" "d.file"))
  (defvariable var4 "e.file")
  (?depends= (with-output-files (with-output-files (with-output-files var var2) (append "f.file" var4)) 
	       (append var3 "g.file" "h.file" "f.file"))
	     ((var var2 var4 var3) () (("f.file" "g.file" "h.file") (append var2 var4 var3))))
  (?expr= ('(with-output-files (third (++ "(aa;" "bb;" "cc)")) ("a.file" "b.file")) :string) "cc"))
|#

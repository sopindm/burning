(in-package :burning-syntax-test)

(defun !symbol= (symbol1 symbol2)
  (cond
    ((eq symbol1 ':gensym) t)
    ((eq symbol2 ':gensym) t)
    (t (!equal symbol1 symbol2))))

(defun !production= (prod1 prod2)
  (!= (length prod1) (length prod2))
  (mapc #'!symbol= prod1 prod2))


(defun !productions= (prods1 prods2)
  (!= (length prods1) (length prods2))
  (mapc #'!production= prods1 prods2))

(in-case core-test)

(deftest rule-to-productions-test
  (let ((rule (make-rule 'simple 'simple1 'simple2)))
    (!productions= (burning-syntax::rule-to-productions rule) 
		   '((simple simple1 simple2))))
  (let ((rule (make-rule 'simple '(:|| a b) '(:|| c d))))
    (!productions= (burning-syntax::rule-to-productions rule) 
		   '((simple a c) (simple a d) (simple b c) (simple b d))))
  (let ((rule (make-rule 'simple '(:* a b) '(:* c d))))
    (!productions= (burning-syntax::rule-to-productions rule) 
		   '((simple :gensym :gensym)
		     (:gensym)
		     (:gensym c d :gensym)
		     (:gensym)
		     (:gensym a b :gensym)))))


(deflexeme sample "Hi")

(deftest productions-terminals
  (!equal (burning-syntax::production-terminals '(x y z sample))
	  '(sample))
  (!equal (burning-syntax::production-terminals '(x sample y sample z sample bla-bla-bla))
	  '(sample)))

(deftest production-non-terminals
  (!equal (burning-syntax::production-non-terminals '(x sample y sample x sample u sample z))
	  '(y x u z))
  (!error (burning-syntax::production-non-terminals '(sample sample))
	  "Production's result must be non-terminal."))

(deflexeme lexeme1 (:empty))
(deflexeme lexeme2 (:empty))
(deflexeme lexeme3 (:empty))
(deflexeme lexeme4 (:empty))
(deflexeme lexeme5 (:empty))

(deftest grammar-making
  (let ((rule1 (make-rule 'rule1 'lexeme1 'lexeme2 'rule1 'lexeme1))
	(rule2 (make-rule 'rule2 '(:* rule1 lexeme1) 'lexeme3 '(:|| lexeme1 lexeme2))))
    (let ((grammar (make-grammar `(,rule1))))
      (!productions= (grammar-productions grammar) '((rule1 lexeme1 lexeme2 rule1 lexeme1)))
      (!equal (grammar-terminals grammar) '(:eps :no-symbol lexeme2 lexeme1))
      (!equal (grammar-non-terminals grammar) '(rule1)))
    (let ((grammar (make-grammar `(,rule1 ,rule2))))
      (!productions= (grammar-productions grammar)
		     '((rule1 lexeme1 lexeme2 rule1 lexeme1)
		       (rule2 :gensym lexeme3 lexeme1)
		       (rule2 :gensym lexeme3 lexeme2)
		       (:gensym)
		       (:gensym rule1 lexeme1 :gensym)))
      (?set= (grammar-terminals grammar) '(:eps :no-symbol lexeme1 lexeme2 lexeme3))
      (!production= (grammar-non-terminals grammar) '(rule2 rule1 :gensym)))))

(deflexeme a #\a)
(deflexeme b #\b)
(deflexeme c #\c)
(deflexeme d #\d)
(deflexeme e #\e)

(deftest nullable-test
  (let ((grammar (make-grammar `(,(make-rule 'rule1 'lexeme1)
				  ,(make-rule 'rule1 :eps)))))
    (!t (nullable-p 'rule1 grammar)))
  (let ((grammar (make-grammar `(,(make-rule 'x 'a)
				  ,(make-rule 'x 'y 'b)
				  ,(make-rule 'y 'c)
				  ,(make-rule 'y 'x 'd)))))
    (!eq (nullable-p 'x grammar) nil)
    (!eq (nullable-p 'y grammar) nil))
  (let ((grammar (make-grammar `(,(make-rule 'x 'y 'z)
				  ,(make-rule 'y 'x)
				  ,(make-rule 'x 'z)
				  ,(make-rule 'z :eps)))))
    (!t (nullable-p 'x grammar))
    (!t (nullable-p 'y grammar))
    (!t (nullable-p 'z grammar))))

(deftest first-test
  (let ((grammar (make-grammar `(,(make-rule 'rule1 'rule1 'lexeme1)
				  ,(make-rule 'rule1 'lexeme2)
				  ,(make-rule 'rule1 :eps)))))
    (?set= (production-first '(rule1) grammar) '(lexeme1 lexeme2)))
  (let ((grammar (make-grammar `(,(make-rule 'x 'a)
				  ,(make-rule 'x 'y 'b)
				  ,(make-rule 'y 'c)
			       ,(make-rule 'y 'x 'd)))))
    (?equal (production-first '(x) grammar) '(a c))
    (?equal (production-first '(y) grammar) '(c a)))
  (let ((grammar (make-grammar `(,(make-rule 'x 'a)
				  ,(make-rule 'x 'y 'b)
				  ,(make-rule 'y 'z 'c)
				  ,(make-rule 'y 'd)
				  ,(make-rule 'z 'y 'e)))))
    (?equal (production-first '(x) grammar) '(a d))
    (?equal (production-first '(y) grammar) '(d))
    (?equal (production-first '(z) grammar) '(d)))
  (let ((grammar (make-grammar `(,(make-rule 'x 'a)
				  ,(make-rule 'x 'y 'b)
				  ,(make-rule 'y 'z)
				  ,(make-rule 'y 'd)
				  ,(make-rule 'z 'y 'e)
				  ,(make-rule 'z :eps)))))
    (?set= (production-first '(x) grammar) '(a e d b))
    (?set= (production-first '(y) grammar) '(e d))
    (?set= (production-first '(z) grammar) '(d e))))

(defgrammar my-grammar
    ((x :eps)
     (x a)
     (x (y b))
     (y c)
     (y (x d)))
  :start x)

(deftest point-making
  (!equal (burning-syntax::make-point '(x a b c))
	  '((x a b c) (a b c) ()))
  (!equal (burning-syntax::make-points 'x my-grammar)
	  '(((x) () ()) ((x a) (a) ()) ((x y b) (y b) ()))))

(deftest point-closure-test
  (!equal (burning-syntax::point-closure (burning-syntax::make-point '(s x)) my-grammar)
	  '(x y)))

(deftest point-joining
  (!equal (burning-syntax::join-points (list (list '(x a b c) '(c) ())
					     (list '(x a b) '(b) ())
					     (list '(x a b) '(b) ())
					     (list '(x a b) '(a b) ())
					     (list '(x a b c) '(b c) ())))
	  '(((x a b) (a b) ())
	    ((x a b) (b) ())
	    ((x a b c) (b c) ())
	    ((x a b c) (c) ()))))

(deftest point-goto
  (let ((point (burning-syntax::make-point '(s x))))
    (!equal (burning-syntax::point-goto point 'x)
	    '(((s x) () ())))))

(deftest points-closure
  (let ((point1 (burning-syntax::make-point '(x)))
	(point2 (burning-syntax::make-point '(x y b))))
    (?equal (burning-syntax::points-closure `((,point1 ,point2) ()) my-grammar)
	    '((((x) () ())
	       ((x y b) (y b) ()))
	      (x y)))))

(deftest points-goto
  (let ((points '((((x) () ())
		   ((x y b) (y b) ()))
		  (x y))))
    (!equal (burning-syntax::points-goto points 'x my-grammar)
	    '((((y x d) (d) ()))
	      ()))
    (!equal (burning-syntax::points-goto points 'y my-grammar)
	    '((((x y b) (b) ()))
	      ()))
    (!equal (burning-syntax::points-goto points 'a my-grammar)
	    '((((x a) () ()))
	      ()))
    (!equal (burning-syntax::points-goto points 'c my-grammar)
	    '((((y c) () ()))
	      ()))))

(deflexeme id (:empty))
(deflexeme + (:empty))
(deflexeme * (:empty))
(deflexeme ob (:empty))
(deflexeme cb (:empty))
(deflexeme = (:empty))

(defgrammar expression-grammar
    ((x ((:|| (x + t) t)))
     (t ((:|| (t * f) f)))
     (f ((:|| ( ob x cb ) id))))
  :start x)

(defgrammar pointer-grammar
    ((s ((:|| (l = r) r)))
     (l ((:|| (* r) id)))
     (r l))
  :start s)

(defgrammar axml-grammar
    ((xml (open-tag xml-nodes closing-tag))
     (xml short-tag)
     (open-tag a)
     (closing-tag b)
     (short-tag c)
     (xml-nodes :eps)
     (xml-nodes (xml xml-nodes)))
  :start xml)

(deftest lr1-point-closure
  (let ((point '((x x + t) (x + t) ())))
    (!equal (burning-syntax::lr1-point-closure point '(:no-symbol) expression-grammar)
	    '((f (* +)) (t (* +)) (x (+)))))
  (let ((point '((s l) (l) ())))
    (!equal (burning-syntax::lr1-point-closure point '(:no-symbol) pointer-grammar)
	    '((l (:no-symbol)))))
  (let ((point '((z x) (x x) ())))
    (!equal (burning-syntax::lr1-point-closure point '(:no-symbol) my-grammar)
	    '((x (a c d :no-symbol)) (y (b)))))
  (let ((point '((xml-nodes xml xml-nodes) (xml-nodes) ())))
    (!equal (burning-syntax::lr1-point-closure point '(:no-symbol) axml-grammar)
	    '((open-tag (a b c))
	      (short-tag (a c :no-symbol))
	      (xml (a c :no-symbol))
	      (xml-nodes (:no-symbol))))))


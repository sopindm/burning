(in-package :burning-syntax-test)

(in-case language-test)

(defun !rule= (rule result &rest arguments)
  (let ((productions (remove-if #'(lambda (x) (eq (first x) ':aux)) arguments))
	(aux (remove-if-not #'(lambda (x) (eq (first x) ':aux)) arguments)))
    (!symbol= (rule-result rule) result)
    (let ((real-productions (rule-productions rule)))
      (!= (length real-productions) (length productions))
      (mapc #'(lambda (x y) (!production= x y)) real-productions productions))
    (let ((real-aux (aux-rules rule)))
      (!= (length real-aux) (length aux))
      (mapc #'(lambda (x y) (apply #'!rule= (cons x (second y)))) real-aux aux))))

(deftest simple-rule
  (!rule= (make-rule 'simple 'simple1 'simple2) 
	  'simple '(simple1 simple2))
  (!rule= (make-rule 'sample 'eps 'simple 'eps)
	  'sample '(simple))
  (!rule= (make-rule 'sample 'eps)
	  'sample '()))

(deftest or-rule
  (!rule= (make-rule 'sample '(:|| sample1 sample2) 'sample3 '(:|| sample4 sample5))
	  'sample
	  '(sample1 sample3 sample4)
	  '(sample1 sample3 sample5)
	  '(sample2 sample3 sample4)
	  '(sample2 sample3 sample5))
  (!rule= (make-rule 'sample '(:|| (:|| a b) c))
	  'sample
	  '(a)
	  '(b)
	  '(c))
  (!rule= (make-rule 'sample '(:|| (a b) c) 'd)
	  'sample
	  '(a b d)
	  '(c d)))

(deftest star-rule
  (!rule= (make-rule 'sample '(:* sample1 sample2) 'sample3)
	  'sample
	  '(:gensym sample3)
	  '(:aux (:gensym 
		  ()
		  (sample1 sample2 :gensym))))
  (!rule= (make-rule 'sample '(:* (:|| a b) c) 'd)
	  'sample
	  '(:gensym d)
	  '(:aux (:gensym
		  ()
		  (a c :gensym)
		  (b c :gensym)))))

(deftest positive-rule
  (!rule= (make-rule 'sample '(:+ (:|| a b) c) 'd)
	  'sample
	  '(:gensym d)
	  '(:aux (:gensym 
		  (:gensym :gensym)
		  (a c)
		  (b c)))))

(deftest maybe-rule
  (!rule= (make-rule 'sample '(:? (:|| a b) c) 'd)
	  'sample
	  '(a c d)
	  '(b c d)
	  '(d)))


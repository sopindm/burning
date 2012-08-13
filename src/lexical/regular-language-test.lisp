(in-package :burning-lexical-test)

(in-case regular-language-test)

;;
;; Support functions
;;

(defun char-node (char)
  (range-node char char))

(defun not-a-regular-expression-error (expression)
  (with-output-to-string (output)
    (format output "Regular expression must be a list. ~a" expression)))

(defun wrong-type-error (function-name position expected-type type)
  (with-output-to-string (output)
    (format output 
	    "~a-th argument for ~a must be ~a, not ~a." 
	    position 
	    (write-to-string function-name)
	    expected-type
	    type)))

(deftest char-test
  (!node= (make-node #\a) (char-node #\a)))

(deftest range-test
  (!node= (make-node :range #\a #\b)
	  (range-node #\a #\b)))

(deftest or-test
  (!node= (make-node :or (make-node #\a) '(:range #\b #\c))
	  (or-node (character-node #\a) (range-node #\b #\c))))

(deftest empty-test
  (!node= (make-node :empty)
	  (empty-node)))

(deftest to-range-set-test
  (!equal (to-range-set (character-node #\a))
	  '((#\a . #\a)))
  (!equal (to-range-set (range-node #\a #\e))
	  '((#\a . #\e)))
  (!equal (to-range-set (or-node (range-node #\a #\b)
				 (range-node #\d #\e)))
	  '((#\a . #\b) (#\d . #\e)))
  (!equal (to-range-set (or-node (range-node #\d #\e)
				 (range-node #\a #\b)))
	  '((#\a . #\b) (#\d . #\e)))
  (!equal (to-range-set (or-node (range-node #\a #\c)
				 (range-node #\b #\d)))
	  '((#\a . #\d)))
  (!equal (to-range-set (or-node (or-node (range-node #\a #\d)
					  (range-node #\f #\h))
				 (or-node (range-node #\b #\c)
					  (or-node (range-node #\f #\l)
						   (range-node #\o #\z)))))
	  '((#\a . #\d) (#\f . #\l) (#\o . #\z)))
  (!equal (to-range-set (or-node (range-node #\a #\b)
				 (range-node #\c #\d)))
	  '((#\a . #\d)))
  (!equal (to-range-set (empty-node))
	  '())
  (!equal (to-range-set (or-node (or-node (range-node #\a #\b)
					  (range-node #\d #\e))
				 (range-node #\b #\d)))
	  '((#\a . #\e))))

(deftest not-test
  (!node= (make-node :not (make-node :or (make-node :range #\b #\f) (make-node :range #\B #\F))
		     :full-range (make-node :or (make-node :range #\a #\z) (make-node :range #\A #\Z)))
	  (or-node (range-node #\A #\A)
		   (or-node (range-node #\G #\Z)
			    (or-node (range-node #\a #\a)
				     (range-node #\g #\z)))))
  (let ((node (make-node :or (make-node :range #\a #\d) (make-node :range #\f #\g))))
    (!node= (make-node :not (make-node :not node)) node)))

(deftest letter-test
  (!node= (make-node :letter)
	  (or-node (range-node #\a #\z)
		   (range-node #\A #\Z)))
  (!node= (make-node :letter :no-upper t)
	  (range-node #\a #\z))
  (!node= (make-node :letter :no-lower t)
	  (range-node #\A #\Z))
  (!node= (make-node :letter :languages '(en ru))
	  (or-node (or-node (range-node #\a #\z)
			    (range-node #\A #\Z))
		   (or-node (range-node #\а #\я)
			    (range-node #\А #\Я)))))

(deftest positive-test
  (!node= (make-node :positive (make-node #\a))
	  (and-node (character-node #\a)
		    (star-node (character-node #\a)))))

(deftest repeat-test
  (!node= (make-node :repeat (make-node #\a) 3 3)
	  (and-node (character-node #\a)
		    (and-node (character-node #\a)
			      (character-node #\a))))
  (!node= (make-node :repeat (make-node #\a) 0 2)
	  (and-node (or-node (empty-node) (character-node #\a))
		    (or-node (empty-node) (character-node #\a))))
  (!node= (make-node :repeat (make-node #\a) 1 2)
	  (and-node (character-node #\a)
		    (or-node (empty-node) (character-node #\a)))))

(defun my-empty-node ()
  (make-node :empty))

(let ((empty-node (make-node :empty)))
  (deflexeme bla-bla-lexeme (:and (my-empty-node) (:empty) empty-node) :minimal t))

(deftest make-lexeme-test
  (!node= (lexeme-expression bla-bla-lexeme)
	  (and-node (empty-node) (and-node (empty-node) (empty-node)))))

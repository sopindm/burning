(in-package :burning-lexical-test)

(in-case core-test)

(define-equality-check node=)

(defun irange-node (range position)
  (make-instance 'burning-lexical::integer-range-node :range range :position position))

(defun p-final-node (lexeme position)
  (make-instance 'burning-lexical::final-node 
		 :lexeme lexeme
		 :position position))

(define-equality-check oequal)

(deftest lexical-making
  (let* ((lexeme1 (make-lexeme 'sample1 (make-node "s1")))
	 (lexeme2 (make-lexeme 'sample2 (make-node "s2")))
	 (lexic (make-lexic lexeme1 lexeme2)))
    (?node= (lexic-expression lexic) 
	    (or-node (and-node (and-node (irange-node 3 0) (irange-node 0 1))
			       (p-final-node lexeme1 2))
		     (and-node (and-node (irange-node 3 3) (irange-node 1 4)) 
			       (p-final-node lexeme2 5))))
    (?equal (lexic-translation lexic) '(((#\1 . #\1) 0) ((#\2 . #\2) 1) ((#\3 . #\r) 2) ((#\s . #\s) 3)))
    (?oequal (lexic-follows lexic) #((1) (2) () (4) (5) ())) 
    (?oequal (lexic-values lexic) (make-array 6 :initial-contents `(nil nil ,lexeme1 nil nil ,lexeme2)))
    (?equalp (lexic-nexts lexic) #(3 0 nil 3 1 nil))))

(deftest nullable-test
  (?not (nullable (make-node :range #\a #\d)))
  (?t (nullable (make-node :star #\a)))
  (?t (nullable (make-node :maybe "ab")))
  (?not (nullable (make-node :and '(:star "ab") '(:maybe "cd") "ef")))
  (?t (nullable (make-node :and '(:star "ab") '(:maybe "cd") '(:or "ef" "gh" (:maybe "ij")))))
  (?not (nullable (final-node 'end))))

(deflexeme if "if")
(deflexeme then "then")
(deflexeme word (:positive (:letter :no-upper t)))
(deflexeme integer (:and (:maybe (:or #\- #\+)) (:positive (:digit))))
(deflexeme spaces (:star #\Space))
(deflexeme r-word (:positive (:letter :no-upper t :languages (ru))))

(defun get-lexeme-expression (lexeme)
  (lexic-expression (make-lexic lexeme)))

(deftest first-test
  (!equal (first-pos (get-lexeme-expression if)) '(0))
  (!equal (first-pos (get-lexeme-expression word)) '(0))
  (!equal (first-pos (get-lexeme-expression integer)) '(0 1 2))
  (!equal (first-pos (get-lexeme-expression spaces)) '(0 1)))

(deftest last-test
  (!equal (last-pos (get-lexeme-expression if)) '(2))
  (!equal (last-pos (left-node (get-lexeme-expression word)))
	  '(0 1))
  (!equal (last-pos (left-node (get-lexeme-expression integer)))
	  '(2 3)))

(deflexeme sample-lexeme (:and (:star (:range #\a #\b))
			       "abb"))

(deflexic sample-lexic sample-lexeme)

(deftest follow-test
  (!equalp (lexic-follows sample-lexic)
	   #((0 1 2) (0 1 2) (3) (4) (5) ())))

(deftest lexic-generation
  (!node= (lexic-expression sample-lexic)
	  (and-node (and-node (star-node (or-node (irange-node 0 0) 
						  (irange-node 1 1)))
			      (and-node (irange-node 0 2) 
					(and-node (irange-node 1 3) 
						  (irange-node 1 4))))
		    (p-final-node sample-lexeme 5)))
  (!equalp (lexic-follows sample-lexic) #((0 1 2) (0 1 2) (3) (4) (5) ()))
  (!oequal (lexic-values sample-lexic) 
	   (list nil nil nil nil nil sample-lexeme))
  (!equalp (lexic-nexts sample-lexic) #(0 1 0 1 1 nil)))

(deflexeme c-string (:and #\" (:star (:any)) #\") :minimal t)
(deflexic min-lexic c-string)

(deftest minimal-lexic
  (!oequal (lexic-values min-lexic)
	   (list nil nil nil nil nil nil c-string)))

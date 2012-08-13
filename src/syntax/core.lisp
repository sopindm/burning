(in-package :burning-syntax)

(defclass rule ()
  ((productions :initarg :productions :accessor rule-productions)
   (result :initarg :result :reader rule-result)
   (aux-rules :initform () :accessor aux-rules)))

(defun rule-to-productions (rule)
  (append (mapcar #'(lambda (x) (cons (rule-result rule) x)) (rule-productions rule))
	  (apply #'append (mapcar #'rule-to-productions (aux-rules rule)))))

(defun list-to-set (list)
  (cond ((null list) nil)
	(t (adjoin (first list) (list-to-set (rest list))))))

(defun lexeme-p (symbol)
  (and (boundp symbol) (typep (symbol-value symbol) 'lexeme)))

(defun production-non-terminals (production)
  (when (lexeme-p (first production))
    (error "Production's result must be non-terminal.")) 
  (list-to-set (cons (first production) 
		     (remove-if #'lexeme-p (rest production)))))

(defun production-terminals (production)
  (list-to-set (remove-if-not #'lexeme-p (rest production))))

(defclass grammar ()
  ((productions :initarg :productions :reader grammar-productions)
   (start-symbol :initarg :start-symbol :reader grammar-start)
   (terminals :initarg :terminals :reader grammar-terminals)
   (non-terminals :initarg :non-terminals :reader grammar-non-terminals)))

(defun terminal-p (symbol grammar)
  (find symbol (grammar-terminals grammar)))

(defun symbol-productions (symbol grammar)
  (remove-if-not #'(lambda (x) (eq (first x) symbol)) (grammar-productions grammar)))

(defun multi-union (lists)
  (cond 
    ((null lists) ())
    ((null (rest lists)) (first lists))
    (t (union (first lists) (multi-union (rest lists))))))

(defun make-grammar (rules &key (start-symbol))
  (let* ((productions (apply #'append (mapcar #'rule-to-productions rules)))
	 (terminals (multi-union (mapcar #'production-terminals productions)))
	 (non-terminals (multi-union (mapcar #'production-non-terminals productions))))
    (make-instance 'grammar
		   :productions productions
		   :start-symbol start-symbol
		   :terminals (append (list 'eps :no-symbol) terminals)
		   :non-terminals non-terminals)))

(defun nullable-p (symbol grammar &optional (deny-list ()))
  (cond 
    ((terminal-p symbol grammar) nil)
    ((find symbol deny-list) nil)
    (t (eval (cons 'or (mapcar #'(lambda (x) (nullable-production-p x grammar (cons symbol deny-list)))
			       (symbol-productions symbol grammar)))))))

(defun nullable-production-p (production grammar &optional (deny-list ()))
  (eval (cons 'and (mapcar #'(lambda (x) (nullable-p x grammar deny-list)) (rest production)))))
  
(defun symbol-first (symbol grammar &optional (deny-list ()))
  (cond
    ((terminal-p symbol grammar) `(,symbol))
    ((find symbol deny-list) nil)
    (t (multi-union (mapcar #'(lambda (x) (production-first (rest x) grammar (cons symbol deny-list))) 
			    (symbol-productions symbol grammar))))))

(defun production-first (production grammar &optional (deny-list ()))
  (cond
    ((null production) nil)
    ((not (nullable-p (first production) grammar)) (symbol-first (first production) grammar deny-list))
    (t (union (symbol-first (first production) grammar deny-list) 
	      (production-first (rest production) grammar deny-list)))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun defined-production (expr)
    (let ((length (length expr)))
      (when (> length 2)
	(error "Too much parameters for production definition. Only 2 expected."))
      (when (< length 2)
	(error "Not enoght parameters for production definition. 2 expected.")))
    (let ((production (second expr)))
      (if (atom production)
	  `(make-rule ',(first expr) ',production)
	  `(make-rule ',(first expr) ,@(mapcar #'(lambda (x) `',x) production))))))

(defmacro defgrammar (name (&rest productions) &key (start))
  `(defparameter ,name 
     (make-grammar (list ,@(mapcar #'(lambda (x) (defined-production x)) productions))
		   :start-symbol ',start)))

(defun make-point (production)
  (list production (rest production) ()))

(defun make-points (symbol grammar)
  (mapcar #'make-point (symbol-productions symbol grammar)))

(defun point-production (point)
  (first point))

(defun point-position (point)
  (second point))

(defun point-lookups (point)
  (third point))

(defun symbol< (symbol1 symbol2)
  (string< (symbol-name symbol1) (symbol-name symbol2)))

(defun production< (production1 production2)
  (cond
    ((and (null production1) (null production2)) nil)
    ((null production1) t)
    ((null production2) nil)
    ((symbol< (first production1) (first production2)) t)
    ((symbol< (first production2) (first production1)) nil)
    (t (production< (rest production1) (rest production2)))))

(defun point< (point1 point2)
  (cond
    ((production< (point-production point1) (point-production point2)) t)
    ((production< (point-production point2) (point-production point1)) nil)
    (t (> (length (point-position point1)) (length (point-position point2))))))

(defun symbol-closure (symbol grammar symbols-list)
  (cond
    ((terminal-p symbol grammar) symbols-list)
    ((find symbol symbols-list) symbols-list)
    (t (sort (adjoin symbol 
		     (multi-union (mapcar #'(lambda (x) (point-closure x grammar 
								       (cons symbol symbols-list))) 
					  (make-points symbol grammar))))
		   #'symbol<))))

(defun point-closure (point grammar &optional (symbols ()))
  (if (null (point-position point))
      ()
      (symbol-closure (first (point-position point)) grammar symbols)))

(defun cons-symbol-list (symbol list)
  (join-symbol-lists (list symbol) list))

(defun join-symbol-lists (list1 list2)
  (cond
    ((null list1) list2)
    ((null list2) list1)
    ((symbol< (first (first list1)) (first (first list2))) (cons (first list1) 
								 (join-symbol-lists (rest list1) list2)))
    ((symbol< (first (first list2)) (first (first list1))) (join-symbol-lists list2 list1))
    (t (cons (cons (first (first list1))
		   (list (sort (union (first (rest (first list1))) (first (rest (first list2)))) #'symbol<)))
	     (join-symbol-lists (rest list1) (rest list2))))))
    
(defun append-symbols (symbol-lists)
  (cond
    ((null symbol-lists) nil)
    ((null (rest symbol-lists)) (copy-tree (first symbol-lists)))
    (t (join-symbol-lists (copy-tree (first symbol-lists)) (append-symbols (rest symbol-lists))))))

(defun lr1-symbol-closure (symbol grammar lookups symbols)
  (cond
    ((terminal-p symbol grammar) symbols)
    ((subsetp lookups (first (rest (find symbol symbols :key #'first)))) symbols)
    (t (sort (copy-tree 
	      (append-symbols 
	       (mapcar #'(lambda (x) (lr1-point-closure x lookups grammar
							(cons-symbol-list (cons symbol (list lookups))
									  symbols)))
		       (make-points symbol grammar))))
	     #'symbol< :key #'first))))

(defun lr1-point-closure (point lookups grammar &optional (symbols ()))
  (cond
    ((null (point-position point)) symbols)
    (t (let ((symbol (first (point-position point)))
	     (rest (rest (point-position point))))
	 (sort (copy-tree (append-symbols 
			   (mapcar #'(lambda (x) 
				       (lr1-symbol-closure symbol grammar 
							   (production-first (append rest (list x)) grammar)
							   symbols))
				       lookups)))
	       #'symbol< :key #'first)))))

(defun join-points (points)
  (sort (remove-duplicates points :test #'equal) #'point<))

(defun point-goto (point symbol)
  (if (eq (first (point-position point)) symbol)
      `(,(list (point-production point) (rest (point-position point)) ()))
      ()))

(defun point-next (point)
  (point-goto point (first (point-position point))))

(defun points-closure (points grammar)
  (list (join-points (first points))
	(multi-union (append (mapcar #'(lambda (x) (point-closure x grammar)) (first points))
			     (list (second points))))))

(defun points-set (points grammar)
  (append (first points)
	  (apply #'append (mapcar #'(lambda (x) (make-points x grammar)) (second points)))))

(defun points-goto (points symbol grammar)
  (list (join-points (apply #'append 
			    (mapcar #'(lambda (x) (point-goto x symbol)) 
				    (points-set points grammar))))
	()))

(defun starting-points (grammar)
  (let ((point (make-point `(:start ,(grammar-start grammar)))))
    (list (list point) (point-closure point grammar))))

(defun null-points-p (points)
  (and (null (first points))
       (null (second points))))

(defgeneric add-state (point table))
(defgeneric state-goto (state symbol table))
(defgeneric (setf state-goto) (value state symbol table))
(defgeneric state-point (state table))

(defclass points-table ()
  (states 
   (points :reader points)))

(defmethod initialize-instance :after ((table points-table) &key)
  (setf (slot-value table 'states) (make-array 0 :adjustable t :fill-pointer 0))
  (setf (slot-value table 'points) (make-array 0 :adjustable t :fill-pointer 0)))

(defmethod add-state (point (table points-table))
  (let ((new-state (vector-push-extend () (slot-value table 'states)))
	(new-point (vector-push-extend (copy-tree point) (slot-value table 'points))))
    (assert (= new-state new-point))
    new-state))

(defun points-table-entity (state symbol table)
  (assoc symbol (aref (slot-value table 'states) state)))

(defmethod state-goto (state symbol (table points-table))
  (rest (points-table-entity state symbol table)))

(defmethod (setf state-goto) (value state symbol table)
  (let ((place (points-table-entity state symbol table)))
    (if place
	(setf (rest place) value)
	(push (cons symbol value)
	      (aref (slot-value table 'states) state)))))

(defmethod state-point (state (table points-table))
  (aref (slot-value table 'points) state))

(defun add-next-state (state symbol table added-points grammar)
  (let ((next-point (points-closure (points-goto (state-point state table) symbol grammar) grammar)))
    (unless (null-points-p next-point)
      (add-state-to-table next-point table added-points grammar)
      (let ((next-state (gethash next-point added-points)))
	(setf (state-goto state symbol table) next-state)))))

(defun add-new-state-to-table (point table added-points grammar)
  (let ((new-state (add-state point table)))
    (setf (gethash point added-points) new-state)
    (mapc #'(lambda (x) (add-next-state new-state x table added-points grammar))
	  (append (grammar-terminals grammar)
		  (grammar-non-terminals grammar)))))

;;
;; LARL symbols
;;

(defun next-state (state point table)
  (state-goto state (first (point-position point)) table))

(defun set-generated (symbol point real-point state table gen-table)
  (let ((next-point (point-next real-point))
	(next-state (next-state state real-point table)))
    (assert (not (null next-state)))
    (let ((value (assoc next-point (aref gen-table next-state))))
      (if value
	  (pushnew symbol (rest value))
	  (push (cons next-point symbol)
		(aref gen-table next-state))))))

(defun set-spreaded (point real-point state table spreaded)
  (let ((next-point (point-next real-point))
	(next-state (next-state state real-point table)))
    (assert (not (null next-state)))
    (let ((value (assoc point (aref spreaded state))))
      (push (cons point (cons next-state next-point))
	    (aref spreaded state)))))

(defun fill-larl (point base-point lookaheads state table generated spreaded grammar)
  (unless (null (point-position point))

    (mapc #'(lambda (x) (if (eq x ':no-symbol)
			    (set-spreaded base-point point state table spreaded)
			    (set-generated x base-point point state table generated))) 
	  lookaheads)))

(defun fill-point-larl (point state table generated spreaded grammar)
  (fill-larl point point '(:no-symbol) state table generated spreaded grammar)
  (let ((closure (lr1-point-closure point '(:no-symbol) grammar)))
    (dolist (symbol closure)
    (mapcar #'(lambda (x) (fill-larl x point (second symbol) state table generated spreaded grammar))
	    (make-points (first symbol) grammar)))))

(defun fill-larl-symbols (generated-symbols spreaded-symbols table grammar)
  (dotimes (i (length (points table)))
    (mapc #'(lambda (point) (fill-point-larl point i table generated-symbols spreaded-symbols grammar))
		    (first (state-point i table)))))

(defun find-point (point state table)
  (flet ((point-equal (point1 point2)
	   (and (equal (point-production point1) (point-production point2))
		(equal (point-position point1) (point-position point2)))))
    (let ((value (find point (first (state-point state table)) :test #'point-equal)))
      (assert value)
      value)))

(defun generate-larl-symbols (table generated-symbols)
  (push 'eps
	(third (find ':start (first (state-point 0 table)) :key #'(lambda (x) (first (first x))))))
  (flet ((generate-larl (point symbol state table)
	   (pushnew symbol (third (find-point point state table)))))
    (dotimes (i (length (points table)))
      (mapc #'(lambda (x) (generate-larl (first (first x)) (rest x) i table)) (aref generated-symbols i)))))

(defun spread-larl-symbols (table spreaded)
  (labels 
      ((spread-larl (point dest-point state spreaded-state)
	 (let ((spreading-point (find-point point state table))
	       (spreaded-point (find-point dest-point spreaded-state table)))
	   (if (subsetp (third spreading-point) (third spreaded-point))
	       nil
	       (progn
		 (setf (third spreaded-point) 
		       (sort (union (copy-tree (third spreading-point)) 
				    (copy-tree (third spreaded-point))) #'symbol<))
		 t)))))
  (let ((continue? nil))
    (dotimes (i (length (points table)))
      (when (eval (cons 'or 
			(mapcar #'(lambda (x) (spread-larl (first x) (third x) i (second x))) (aref spreaded i))))
	(setq continue? t)))
    (when continue? 
      (spread-larl-symbols table spreaded)))))

;;
;; LARL table generation
;;

(defun add-state-to-table (point table added-points grammar)
  (cond
    ((gethash point added-points) t)
    (t (add-new-state-to-table point table added-points grammar))))

(defun make-points-table (grammar)
  (let ((first-point (starting-points grammar))
	(added-points (make-hash-table :test 'equal))
	(table (make-instance 'points-table)))
    (add-state-to-table first-point table added-points grammar)
    (let ((generated-symbols (make-array (length (points table)) :initial-element nil))
	  (spreaded-symbols (make-array (length (points table)) :initial-element nil)))
      (fill-larl-symbols generated-symbols spreaded-symbols table grammar)
      (generate-larl-symbols table generated-symbols)
      (spread-larl-symbols table spreaded-symbols))
    table))


  
      


	       
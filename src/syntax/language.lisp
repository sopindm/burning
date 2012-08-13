(in-package :burning-syntax)

(defun append-production (production productions)
  (mapcar #'(lambda (x) (append production x)) productions))

(defgeneric parse-operation (operation arguments rule))

(defmethod parse-operation (operation arguments rule)
  (make-productions (cons operation arguments) rule))

(defmethod parse-operation ((operation (eql ':||)) arguments rule)
  (apply #'append (mapcar #'(lambda (x) (parse-symbol x rule)) arguments)))

(defmethod parse-operation ((operation (eql ':*)) arguments rule)
  (let* ((aux-name (gensym (string (rule-result rule))))
	 (aux-rule (do-make-rule aux-name arguments)))
    (setf (rule-productions aux-rule)
	  (mapcar #'(lambda (x) (append x (list aux-name))) (rule-productions aux-rule)))
    (push () (rule-productions aux-rule))
    (push aux-rule (aux-rules rule))
    (list (list aux-name))))

(defmethod parse-operation ((operation (eql ':+)) arguments rule)
  (let* ((aux-name (gensym (string (rule-result rule))))
	 (aux-rule (do-make-rule aux-name arguments)))
    (push (list aux-name aux-name) (rule-productions aux-rule))
    (push aux-rule (aux-rules rule))
    (list (list aux-name))))

(defmethod parse-operation ((operation (eql ':?)) arguments rule)
  (parse-symbol `(:|| ,arguments 'eps) rule))

(defun parse-symbol (symbol rule)
  (cond
    ((eq symbol 'eps) (list ()))
    ((listp symbol) (parse-operation (first symbol) (rest symbol) rule))
    (t (list (list symbol)))))

(defun make-productions (production rule)
  (cond 
    ((null production) ())
    ((null (rest production)) (parse-symbol (first production) rule))
    (t (let ((symbols (parse-symbol (first production) rule))
	     (productions (make-productions (rest production) rule)))
	 (apply #'append (mapcar #'(lambda (x) (append-production x productions)) symbols))))))

(defun do-make-rule (result production)
  (let ((rule (make-instance 'rule :result result)))
    (setf (slot-value rule 'productions) 
	  (make-productions production rule))
    rule))

(defun make-rule (result &rest production)
  (do-make-rule result production))
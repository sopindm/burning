(in-package :burning-syntax)

(defclass lr-table ()
  ((actions :initarg :actions)
   (gotos :initarg :gotos)))

(defun table-action (symbol state table)
  (rest (assoc symbol (aref (slot-value table 'actions) state))))

(defun table-goto (state symbol table)
  (rest (assoc symbol (aref (slot-value table 'gotos) state))))

(defun merge-action (production)
  (if (eq (first production) :start)
      (list (cons 'accept production))
      (list (cons 'merge production))))

(defun move-action (next-state)
  (if next-state
      (cons 'move next-state)))

(defun point-action (point symbol grammar)
  (labels ((do-point-action (point)
	   (if (and (null (point-position point))
		    (find symbol (point-lookups point)))
	       (merge-action (point-production point))
	       ()))
	 (do-symbol-action (symbol grammar)
	   (apply #'append (mapcar #'(lambda (x) (do-point-action (list (point-production x)
									(point-position x)
									(second symbol))))
				   (make-points (first symbol) grammar)))))
    (apply #'append 
	   (cons (do-point-action point)
		 (mapcar #'(lambda (x) (do-symbol-action x grammar)) 
			 (lr1-point-closure point (point-lookups point) grammar))))))

(defun set-action (state symbol table points-table grammar)
  (let* ((points-set (state-point state points-table))
	 (points (first points-set))
	 (merge-actions (apply #'append (mapcar #'(lambda (x) (point-action x symbol grammar)) points)))
	 (move-action (move-action (state-goto state symbol points-table)))
	 (action (if move-action move-action (first merge-actions))))
    (if (> (length merge-actions) 1)
	(warn "Merge/merge conflict between productions ~{~a ~} at symbol ~a." 
	      (mapcar #'rest merge-actions) symbol))
    (if (and merge-actions move-action)
	(warn "Merge/move conflict for production ~a and symbol ~a" (rest (first merge-actions)) symbol))
    (if action
	(push (cons symbol action) (aref (slot-value table 'actions) state)))))
		
(defun make-lr-table (grammar)
  (let* ((points-table (make-points-table grammar))
	 (lr-table (make-instance 'lr-table
				  :actions (make-array (length (points points-table)) :initial-element nil)
				  :gotos (make-array (length (points points-table)) :initial-element nil))))
    (labels ((set-goto (symbol value state table)
	       (when value
		 (push (cons symbol value) (aref (slot-value table 'gotos) state)))))
      (dotimes (i (length (points points-table)))
	(mapc #'(lambda (x) (set-goto x (state-goto i x points-table) i lr-table))
	      (grammar-non-terminals grammar))
	(mapc #'(lambda (x) (set-action i x lr-table points-table grammar)) (grammar-terminals grammar)))
      lr-table)))

(defclass lr-parser () 
  ((table :initarg :table :reader parser-table)
   (state-stack :initform '(0))
   (value-stack :initform nil)))

(defun make-lr-parser (table)
  (make-instance 'lr-parser :table table))

(defun push-state-to-parser (state value parser)
  (push state (slot-value parser 'state-stack))
  (push value (slot-value parser 'value-stack)))

(defun current-state (parser)
  (first (slot-value parser 'state-stack)))

(defun push-production-to-parser (production parser)
  (declare (optimize (speed 3) (safety 0)))
  (let ((next-state (table-goto (current-state parser) (first production) (parser-table parser))))
    (assert next-state)
    (push-state-to-parser next-state (rest production) parser)))

(defun pop-production (production parser)
  (declare (optimize (speed 3) (safety 0)))
  (labels 
      ((pop-token (name parser)
	 (pop (slot-value parser 'state-stack))
	 (cons name (pop (slot-value parser 'value-stack))))
       (do-pop-production (production parser)
	 (cond
	   ((null production) nil)
	   ((null (rest production)) (list (pop-token (first production) parser)))
	   (t (let ((rest (do-pop-production (rest production) parser)))
		(cons (pop-token (first production) parser) rest))))))
    (cons (first production) (do-pop-production (rest production) parser))))
   
(defun parser-next (token parser)
  (let ((symbol (first token))
	(value (rest token))
	(state (current-state parser)))
    (let ((action (table-action symbol state (parser-table parser))))
      (ecase (first action)
	(move (push-state-to-parser (rest action) value parser)
	       nil)
	(merge (let ((production (pop-production (rest action) parser)))
		  (push-production-to-parser production parser)
		  (parser-next token parser)))
	(accept (let ((production (pop-production (rest action) parser)))
		  (setf (slot-value parser 'value-stack) (list production))
		  t))))))

(defun parser-value (parser)
  (when (> (length (slot-value parser 'value-stack)) 1)
    (error "Tried to get value of non-complete expression."))
  (pop (slot-value parser 'value-stack)))
		 

(defun parse-input (iterator lexic-machine lr-parser)
  (loop (let ((token (get-lexeme iterator lexic-machine)))
	  (if (parser-next (cons (lexeme-name (rest token)) (first token)) lr-parser)
	      (return))))
  (parser-value lr-parser))
      
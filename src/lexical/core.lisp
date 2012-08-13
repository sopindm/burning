(in-package :burning-lexical)

(defgeneric oequal (o1 o2))
(defmethod oequal (o1 o2)
  (equalp o1 o2))

(defmethod oequal ((o1 sequence) (o2 sequence))
  (cond	((not (eq (length o1) (length o2))) nil)
	(t (every #'(lambda (x) (not (null x))) (map 'list #'oequal o1 o2)))))

;;Base node class

(defclass node () ())

(defgeneric node-childs (node))
(defmethod node-childs ((node node))
  ())

(defgeneric (setf node-childs) (value node))
(defmethod (setf node-childs) (value node)
  (when (> (length value) 0)
      (error "Node of type ~a has no childs" (type-of node))))

(defgeneric node= (node1 node2))
(defmethod node=((node1 node) (node2 node))
  nil)

(defclass positioned-node (node)
  ((position :initarg :position :initform nil :accessor node-position)))

(defmethod node= ((node1 positioned-node) (node2 positioned-node))
  (eq (node-position node1) (node-position node2)))

(defgeneric clone-node (node))
(defmethod clone-node ((node node))
  (error "Cloning for type ~a not implemented." (type-of node)))

;;Empty node

(defclass empty-node (node) ())

(defun empty-node ()
  (make-instance 'empty-node))

(defmethod node= ((node1 empty-node) (node2 empty-node))
  t)

(defmethod clone-node ((node empty-node))
  node)

;;Final node

(defclass final-node (positioned-node) 
  ((lexeme :initarg :lexeme :accessor node-lexeme)))

(defun final-node (lexeme)
  (make-instance 'final-node :lexeme lexeme))

(defmethod print-object ((node final-node) stream)
  (format stream "#s(final-node :lexeme ~a :position ~a)" (node-lexeme node) (node-position node)))

(defmethod node= ((node1 final-node) (node2 final-node))
  (and (lexeme= (node-lexeme node1) (node-lexeme node2))
       (call-next-method)))

(defmethod clone-node ((node final-node))
  (make-instance 'final-node 
		 :lexeme (node-lexeme node) 
		 :position (node-position node)))

;;Range node

(defclass range-node (node)
  ((first :initarg :first :reader range-first)
   (last :initarg :last :reader range-last)))

(defun character-node (char)
  "Node containing one character"
  (make-instance 'range-node :first char :last char))

(defun range-node (first last)
  "Node representing character range from 'first' till 'last'"
  (make-instance 'range-node :first first :last last))

(defun any-character-node ()
  (range-node (code-char 0) (code-char (1- char-code-limit))))
  
(defmethod print-object ((object range-node) stream)
  (format stream "#s(range-node :first ~a :last ~a)" (range-first object) (range-last object)))

(defmethod node= ((node1 range-node) (node2 range-node))
  (and (char= (range-first node1) (range-first node2))
       (char= (range-last node1) (range-last node2))))

(defmethod clone-node ((node range-node))
  (make-instance 'range-node :first (range-first node) :last (range-last node)))

;;Integer range node

(defclass integer-range-node (positioned-node)
  ((range :initarg :range :reader range-value)))

(defun integer-range-node (value)
  (make-instance 'integer-range-node :range value))

(defmethod print-object ((object integer-range-node) stream)
  (format stream "#s(integer-range-node :value ~a :position ~a)" (range-value object) (node-position object)))

(defmethod node= ((node1 integer-range-node) (node2 integer-range-node))
  (and (= (range-value node1) (range-value node2))
       (call-next-method)))

(defmethod clone-node ((node integer-range-node))
  (make-instance 'integer-range-node :range (range-value node) :position (node-position node)))

;;Star node

(defclass star-node (node)
  ((child :initarg :child :reader child-node)))

(defun star-node (child)
  "Node representing star expression for child"
  (make-instance 'star-node :child child))

(defmethod node-childs ((node star-node))
  (list (slot-value node 'child)))

(defmethod (setf node-childs) (value (node star-node))
  (if (/= (length value) 1)
      (error "Star node has only one child.")
      (setf (slot-value node 'child)
	    (first value))))

(defmethod node= ((node1 star-node) (node2 star-node))
  (node= (child-node node1) (child-node node2)))

(defmethod print-object ((node star-node) stream)
  (format stream "#s(star-node :child ~a)" (child-node node)))

(defmethod clone-node ((node star-node))
  (make-instance 'star-node :child (clone-node (child-node node))))

;;Binary node

(defclass binary-node (node)
  ((left :initarg :left :reader left-node)
   (right :initarg :right :reader right-node)))

(defmethod node-childs ((node binary-node))
  (list (slot-value node 'left)
	(slot-value node 'right)))

(defmethod (setf node-childs) (value (node binary-node))
  (if (/= (length value) 2)
      (error "Binary node has 2 childs.")
      (progn
	(setf (slot-value node 'left) (first value))
	(setf (slot-value node 'right) (second value)))))

(defmethod print-object ((object binary-node) stream)
  (format stream "#s(~a :left ~a :right ~a)" (type-of object) (left-node object) (right-node object)))

(defmethod node= ((node1 binary-node) (node2 binary-node))
  (and (node= (left-node node1) (left-node node2))
       (node= (right-node node1) (right-node node2))))

(defmethod clone-node ((node binary-node))
  (make-instance (class-of node) :left (clone-node (left-node node)) :right (clone-node (right-node node))))

;;And node

(defclass and-node (binary-node)())

(defun and-node (left right)
  "Node representing and expression for left and right"
  (make-instance 'and-node :left left :right right))

;;Or node

(defclass or-node (binary-node)())

(defun or-node (left right)
  "Node representing or expression for left and right"
  (make-instance 'or-node :left left :right right))

;; Adding positions to nodes

(defun integer-generator (n)
  (decf n)
  (lambda () (incf n)))

(defgeneric add-positions (lexeme number-generator))
(defmethod add-positions ((lexeme node) generator)
  (mapc #'(lambda (x) (add-positions x generator)) (node-childs lexeme)))

(defmethod add-positions :after ((lexeme positioned-node) generator)
  (setf (node-position lexeme) (funcall generator)))

;; Generating lexics

(defun merge-lexemes (lexemes)
  (cond ((null lexemes) ())
	((null (rest lexemes)) (clone-node (lexeme-to-node (first lexemes))))
	(t (or-node (clone-node (lexeme-to-node (first lexemes))) (merge-lexemes (rest lexemes))))))

(defun do-make-lexic (lexemes generator)
  (add-positions (merge-lexemes lexemes) generator))

(defstruct (lexic (:constructor %make-lexic))
  expression translation follows values nexts)

(defun make-lexic (&rest lexemes)
  (let ((lexic (%make-lexic)))
    (%initialize-lexic lexic lexemes)
    lexic))

;;
;; Char translations
;;

(defun do-split (list char)
  (let ((head (first list)))
    (cond
      ((< (rest head) char) (do-split (rest list) char))
      ((= (first head) char) t)
      (t 
       (let ((new-node (list (cons char (rest head)))))
	 (setf (rest new-node) (rest list))
	 (setf (rest list) new-node)
	 (setf (rest (first list)) (- char 1)))))))

(defun split (translation first last)
  (do-split translation (char-code first))
  (do-split translation (1+ (char-code last))))

(defgeneric expression-translation (node translation))
(defmethod expression-translation ((node node) translation)
  (mapc #'(lambda (x) (expression-translation x translation)) (node-childs node))
  translation)

(defmethod expression-translation ((node range-node) translation)
  (split translation (range-first node) (range-last node))
  translation)

(defun make-translation (lexic)
  (let ((translation (expression-translation (lexic-expression lexic) (list (cons -1 (1+ char-code-limit))))))
    (setf translation (butlast (rest translation)))
    (let ((translation-id 0))
      (setf translation (mapcar #'(lambda (range) (list (cons (code-char (first range))
							      (code-char (rest range)))
							(1- (incf translation-id))))
				translation)))
    (setf (lexic-translation lexic) translation)))

(defun translate (char translation)
  (if (null translation) nil
      (let ((range (first (first translation)))
	    (value (second (first translation))))
	(cond
	  ((char< char (first range)) nil)
	  ((char< (rest range) char) (translate char (rest translation)))
	  (t value)))))

;;
;; Splitting range nodes
;;

(defgeneric split-ranges (expr translation))
(defmethod split-ranges (expr translation)
  (setf (node-childs expr)
	(mapcar #'(lambda (x) (split-ranges x translation)) (node-childs expr)))
  expr)

(defmethod split-ranges ((expr range-node) translation)
  (range-to-tree (translate (range-first expr) translation)
		 (translate (range-last expr) translation)))

(defun range-to-tree (first last)
  (cond ((= first last) (integer-range-node first))
	(t (or-node (integer-range-node first) (range-to-tree (1+ first) last)))))

;;
;; Making lexics
;;

(defun %initialize-lexic (lexic lexemes)
  (setf (lexic-expression lexic) (merge-lexemes lexemes))
  (setf (lexic-translation lexic) (make-translation lexic))
  (setf (lexic-expression lexic) (split-ranges (lexic-expression lexic)
					 (lexic-translation lexic)))
  (let* ((generator (integer-generator 0))
	 (size (progn (add-positions (lexic-expression lexic) generator)
		      (funcall generator))))
    (setf (lexic-follows lexic) (make-array size :initial-element nil))
    (setf (lexic-values lexic) (make-array size :initial-element nil))
    (setf (lexic-nexts lexic) (make-array size :initial-element nil)))
  (fill-follow-pos (lexic-expression lexic) (lexic-follows lexic))
  (fill-values (lexic-expression lexic) (lexic-values lexic) (lexic-nexts lexic)))

(defmacro deflexic (name &rest lexemes)
  `(defparameter ,name (make-lexic ,@lexemes)))

;;
;; Nullable
;;

(defgeneric nullable (lexeme))

(defmethod nullable ((lexeme node))
  (error "No method for type ~a." (type-of lexeme)))
(defmethod nullable ((lexeme empty-node)) t)
(defmethod nullable ((lexeme range-node)) nil)
(defmethod nullable ((lexeme integer-range-node)) nil)
(defmethod nullable ((lexeme final-node)) nil)
(defmethod nullable ((lexeme star-node)) t)

(defmethod nullable ((lexeme and-node))
  (and (nullable (left-node lexeme))
       (nullable (right-node lexeme))))

(defmethod nullable ((lexeme or-node))
  (or (nullable (left-node lexeme))
      (nullable (right-node lexeme))))
  
(defun lexeme-function (name &rest args)
  (let ((lexeme (car args)))
    (cond 
      ((null (car lexeme)) (apply (get nil name) args))
      ((characterp (car lexeme)) (apply (get 'character name) args))
      ((integerp (car lexeme)) (apply (get 'character name) args))
      (t (apply (get (car lexeme) name) args)))))

(defun make-set (first second)
  (sort (copy-list (union first second)) #'<))

;;
;;First pos
;;

(defgeneric first-pos (lexeme))

(defmethod first-pos ((lexeme empty-node)) ())
(defmethod first-pos ((lexeme positioned-node)) (list (node-position lexeme)))

(defmethod first-pos ((lexeme and-node))
  (if (nullable (left-node lexeme))
      (make-set (first-pos (left-node lexeme))
		(first-pos (right-node lexeme)))
      (first-pos (left-node lexeme))))

(defmethod first-pos ((lexeme or-node))
  (make-set (first-pos (left-node lexeme))
	    (first-pos (right-node lexeme))))

(defmethod first-pos ((lexeme star-node))
  (first-pos (child-node lexeme)))

;;
;;Last pos
;;

(defgeneric last-pos (lexeme))

(defmethod last-pos ((lexeme empty-node))
  ())

(defmethod last-pos ((lexeme positioned-node))
  (list (node-position lexeme)))

(defmethod last-pos ((lexeme and-node))
  (if (nullable (right-node lexeme))
      (make-set (last-pos (left-node lexeme))
		(last-pos (right-node lexeme)))
      (last-pos (right-node lexeme))))

(defmethod last-pos ((lexeme or-node))
  (make-set (last-pos (left-node lexeme))
	    (last-pos (right-node lexeme))))

(defmethod last-pos ((lexeme star-node))
  (last-pos (child-node lexeme)))

;
;Follow-pos
;

(defgeneric fill-follow-pos (lexeme vector))

(defmethod fill-follow-pos ((lexeme node) vector)
  (mapc #'(lambda (x) (fill-follow-pos x vector)) (node-childs lexeme)))

(defun set-vector (positions values vector)
  (dolist (position positions)
    (setf (elt vector position) 
	  (make-set (elt vector position) values))))

(defmethod fill-follow-pos ((lexeme and-node) vector)
  (let ((fst (first-pos (right-node lexeme)))
	(lst (last-pos (left-node lexeme))))
    (set-vector lst fst vector))
  (call-next-method))

(defmethod fill-follow-pos ((lexeme star-node) vector)
  (let ((fst (first-pos (child-node lexeme)))
	(lst (last-pos (child-node lexeme))))
    (set-vector lst fst vector))
  (call-next-method))

(defgeneric fill-values (lexeme value-vector next-vector))

(defmethod fill-values ((lexeme node) v n)
  (mapc #'(lambda(x) (fill-values x v n)) (node-childs lexeme)))

(defmethod fill-values ((lexeme integer-range-node) v next-vector)
  (declare (ignore v))
  (setf (elt next-vector (node-position lexeme))
	(range-value lexeme)))

(defmethod fill-values ((lexeme final-node) value-vector n)
  (declare (ignore n))
  (let ((value (node-lexeme lexeme)))
    (setf (elt value-vector (node-position lexeme))
	  value)))


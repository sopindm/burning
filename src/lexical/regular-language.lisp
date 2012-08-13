(in-package :burning-lexical)

;;
;;Range sets
;;

(defun make-range (first last &key (include-first t) (include-last t))
  (cons (if include-first first (code-char (1+ (char-code first))))
	(if include-last last (code-char (1- (char-code last))))))

(defun range< (range1 range2 &optional include-previous)
  (char< (rest range1) (if include-previous (code-char (1- (char-code (first range2)))) (first range2))))

(defun char-min (char1 char2)
  (code-char (min (char-code char1) (char-code char2))))

(defun char-max (char1 char2)
  (code-char (max (char-code char1) (char-code char2))))

(defun range+ (range1 range2)
  (cons (char-min (first range1) (first range2))
	(char-max (rest range1) (rest range2))))

(defun range- (range1 range2)
  (let ((first (char-max (first range1) (first range2)))
	(last (char-min (rest range1) (rest range2))))
    (let ((first (if (char> first (first range1)) (list (make-range (first range1) first :include-last nil))))
	  (last (if (char< last (rest range1)) (list (make-range last (rest range1) :include-first nil)))))
      (append first last))))

(defun merge-sets (set1 set2)
  (cond ((null set1) set2)
	((null set2) set1)
	((range< (first set1) (first set2) t) (cons (first set1) (merge-sets (rest set1) set2)))
	((range< (first set2) (first set1) t) (cons (first set2) (merge-sets set1 (rest set2))))
	(t (merge-sets (list (range+ (first set1) (first set2)))
		       (merge-sets (rest set1) (rest set2))))))
	
(defun sub-sets (set1 set2)
  (cond ((null set1) nil)
	((null set2) set1)
	((range< (first set1) (first set2)) (cons (first set1) (sub-sets (rest set1) set2)))
	((range< (first set2) (first set1)) (sub-sets set1 (rest set2)))
	(t (sub-sets (append (range- (first set1) (first set2)) (rest set1))
		     set2))))

(defgeneric to-range-set (node))
(defmethod to-range-set (node)
  (error "Expressions containing ~a cannot be converted to range set." (type-of node)))

(defmethod to-range-set ((node range-node))
  (list (cons (range-first node) (range-last node))))

(defmethod to-range-set ((node or-node))
  (merge-sets (to-range-set (left-node node))
	      (to-range-set (right-node node))))

(defmethod to-range-set ((node empty-node))
  ())

(defun range-to-node (range)
  (range-node (first range) (rest range)))

(defun range-set-to-node (set)
  (cond 
    ((null set) (empty-node))
    ((null (rest set)) (range-to-node (first set)))
    (t (or-node (range-to-node (first set)) (range-set-to-node (rest set))))))

;;
;;Operations
;;

(defgeneric make-node (node &rest args))

(defvar *node-functions* ())

(defmacro define-node-type ((name type) (&rest args) &body body)
  `(defmethod make-node ((,name ,type) &rest generic-args)
     (labels ((self (,@args) ,@body))
       (apply #'self generic-args))))

(defmacro defnode (name (&rest args) &body body)
  `(progn
     (define-node-type (,(gensym) (eql ,name)) (,@args) ,@body)
     (pushnew ,name *node-functions*)))

(defun construct-node (arg)
  (if (listp arg) (apply #'make-node arg) (make-node arg)))

(defmacro ensure-node (&rest args)
  (flet ((ensure-expand (arg)
	   `(unless (typep ,arg 'node)
	      (setf ,arg (construct-node ,arg)))))
    `(progn ,@(mapcar #'ensure-expand args))))

(defun ensure-nodes (list)
  (flet ((ensure-first (list)
	   (unless (typep (first list) 'node)
	     (setf (first list) (construct-node (first list))))))
    (mapl #'ensure-first list)))

(define-node-type (char character) ()
  (character-node char))

(define-node-type (string string) ()
  (apply #'make-node :and (map 'list #'identity string)))

(defnode :range (first last) 
  (when (char> first last)
    (error "First character in range is ~a, that is greater then ~a." first last))
  (range-node first last))

(defnode :or (&rest nodes)
  (ensure-nodes nodes)
  (cond ((null nodes) (empty-node))
	((null (rest nodes)) (first nodes))
	(t (or-node (first nodes) (apply #'self (rest nodes))))))

(defnode :empty ()
  (empty-node))

(defnode :not (node &key (full-range (any-character-node)))
  (ensure-node node)
  (range-set-to-node
   (sub-sets (to-range-set full-range) 
	     (to-range-set node))))

#|
(defun :all ()
  (any-character-node))
|#

(defnode :any ()
  (make-node :not #\Newline))

(defgeneric upper-letters (language))
(defgeneric lower-letters (language))

(defmethod upper-letters (language)
  (error "Sorry, language ~a not supported yet. Your help in this field would be usefull." language))

(defmethod lower-letters (language)
  (error "Sorry, language ~a not supported yet. Your help in this field would be usefull." language))

(defun letters (language no-upper no-lower)
  (cond (no-upper (lower-letters language))
	(no-lower (upper-letters language))
	(t (make-node :or (lower-letters language) (upper-letters language)))))

(defnode :letter (&key (no-upper nil) (no-lower nil) (languages '(en)))
  (cond ((null languages) (empty-node))
	((null (rest languages)) (letters (first languages) no-upper no-lower))
	(t (make-node :or (letters (first languages) no-upper no-lower) 
		      (make-node :letter :no-upper no-upper
				 :no-lower no-lower
				 :languages (rest languages))))))

(defmethod upper-letters ((language (eql 'en)))
  (make-node :range #\A #\Z))

(defmethod lower-letters ((language (eql 'en)))
  (make-node :range #\a #\z))

(defmethod upper-letters ((language (eql 'ru)))
  (make-node :range #\А #\Я))

(defmethod lower-letters ((language (eql 'ru)))
  (make-node :range #\а #\я))

(defnode :digit ()
  (make-node :range #\0 #\9))

#|
(defun :hex-digit ()
  (:or (:range #\0 #\9)
       (:or (:range #\a #\f)
	    (:range #\A #\F))))

(defun :blank ()
  (:or (:char #\Space) (:char #\Tab)))
|#

(defnode :space ()
  (make-node :or #\Space #\Tab #\Newline))

(defnode :and (&rest nodes)
  (ensure-nodes nodes)
  (cond ((null nodes) (empty-node))
	((null (rest nodes)) (first nodes))
	(t (and-node (first nodes) (apply #'self (rest nodes))))))

(defnode :star (expr)
  (ensure-node expr)
  (star-node expr))

(defnode :positive (expr)
  (make-node :and expr (make-node :star expr)))

(defun repeat (expr times)
  (cond ((> times 1) (make-node :and expr (repeat expr (1- times))))
	((= times 1) expr)
	(t (error "Wrong argument ~a for repeat" times))))

(defun ?repeat (expr times)
  (repeat (make-node :or (make-node :empty) expr) times))

(defnode :repeat (expr min-times max-times)
  (cond ((and (= min-times 0) (= max-times 0)) (make-node :empty))
	((= min-times 0) (?repeat expr max-times))
	((= max-times min-times) (repeat expr min-times))
	((< max-times min-times) (error "~a is less than ~a in repeat" max-times min-times))
	(t (make-node :and (repeat expr min-times) (?repeat expr (- max-times min-times))))))

(defnode :maybe (expr)
  (ensure-node expr)
  (make-node :or :empty expr))

(defstruct (lexeme (:constructor %make-lexeme))
  name
  expression
  minimal-p
  skipped-p
  conversion)

(defun lexeme-to-node (lexeme)
  (make-node :and (lexeme-expression lexeme) (final-node lexeme)))

(defun make-lexeme (name expression &key (minimal nil) (skipped nil) (conversion #'identity))
  (%make-lexeme :name name 
		:expression expression
		:minimal-p minimal
		:skipped-p skipped
		:conversion conversion))

(defun lexeme= (lexeme1 lexeme2)
  (and (eq (lexeme-name lexeme1) (lexeme-name lexeme2))
       (node= (lexeme-expression lexeme1) (lexeme-expression lexeme2))
       (eq (lexeme-minimal-p lexeme1) (lexeme-minimal-p lexeme2))
       (eq (lexeme-skipped-p lexeme1) (lexeme-skipped-p lexeme2))))

(defmethod oequal ((o1 lexeme) (o2 lexeme))
  (lexeme= o1 o2))

(defun mk-list (arg)
  (if (listp arg) (mapcar #'(lambda (arg) (if (and (listp arg) (member (first arg) *node-functions*)) `',arg arg)) 
			  arg)
      (list arg)))

(defmacro deflexeme (name expression &key (minimal nil) (skipped nil) (conversion '#'identity))
  `(defparameter ,name (make-lexeme ',name (make-node ,@(mk-list expression)) 
				    :minimal ,minimal 
				    :skipped ,skipped
				    :conversion ,conversion)))

(in-package #:bsdf-expressions)

;;
;; BSDF primitives
;;

(defun bsdf-symbol-p (expr)
  (and (symbolp expr) 
       (not (typep expr 'boolean))
       (not (eq (symbol-package expr) (find-package "KEYWORD")))))

(defun bsdf-function-p (expr)
  (if (listp expr) (bsdf-symbol-p (first expr))))

(defun bsdf-list-p (expr)
  (and (listp expr) (not (bsdf-variable-p (first expr)))))

(defun bsdf-kind-of (value)
  (cond ((bsdf-function-p value) :function)
	((bsdf-list-p value) :list)
	((bsdf-symbol-p value) :symbol)
	(t :value)))

(defun bsdf-value-p (expr)
  (eq (bsdf-kind-of expr) :value))

(defun bsdf-constant-p (expr)
  (ecase (bsdf-kind-of expr)
    (:function nil)
    (:list (every #'bsdf-constant-p expr))
    (:symbol nil)
    (:value t)))

;;
;; Macroexpansion
;;

(defgeneric bsdf-macroexpand (name args))
(defmethod bsdf-macroexpand (name args)  
  (values (cons name (mapcar #'expand-expression args)) t))

(defun expand-expression (expr)
  (if (bsdf-function-p expr)
      (multiple-value-bind (value last-p) (bsdf-macroexpand (first expr) (rest expr))
	(if last-p 
	    value
	    (expand-expression value)))
      expr))

;;
;; Functions generic and macro's
;;

(defgeneric bsdf-function-dependencies (func args))
(defmethod bsdf-function-dependencies (func args)
  (declare (ignore func))
  (list-dependencies args))

(defgeneric bsdf-atom-dependencies (atom))
(defmethod bsdf-atom-dependencies (atom)
  (declare (ignore atom))
  (list () () ()))

;;
;; Expressions
;;

(defun check-expression (expr)
  t)

(defun dependencies+ (&rest deps)
  (labels ((do-append (expr1 expr2)
	     (let ((list1 (if (and (listp expr1) (eq (first expr1) 'append)) (rest expr1) (list expr1)))
		   (list2 (if (and (listp expr2) (eq (first expr2) 'append)) (rest expr2) (list expr2))))
	       `(append ,@(remove-duplicates (append list1 list2) :test #'equal :from-end t))))
	   (files+ (f1 f2)
	     (let ((const1 (if (bsdf-constant-p (first f1)) (first f1)))
		   (rest1 (if (bsdf-constant-p (first f1)) (second f1) (first f1)))
		   (const2 (if (bsdf-constant-p (first f2)) (first f2)))
		   (rest2 (if (bsdf-constant-p (first f2)) (second f2) (first f2))))
	       (append (aif (remove-duplicates (append const1 const2) :test #'equal) (list it))
		       (cond ((and (null rest1) (null rest2)) nil)
			     ((null rest1) (list rest2))
			     ((null rest2) (list rest1))
			     (t (list (do-append rest1 rest2)))))))
	   (do+ (dep1 dep2)
	     (list (remove-duplicates (append (first dep1) (first dep2)) :test #'equal :from-end t)
		   (remove-duplicates (files+ (second dep1) (second dep2)) :test #'equal :from-end t)
		   (remove-duplicates (files+ (third dep1) (third dep2)) :test #'equal :from-end t))))
    (reduce #'do+ deps)))

(defun list-dependencies (list)
  (reduce #'dependencies+ (mapcar #'expression-dependencies list)))

(defun expression-dependencies (expr)
  (if (listp expr)
      (if (bsdf-function-p expr)
	  (bsdf-function-dependencies (first expr) (rest expr))
	  (list-dependencies expr))
      (bsdf-atom-dependencies expr)))

;;
;; Functions
;;

(defun try-bind (lambda-list args)
  (handler-case (bind-lambda-list lambda-list args)
    (simple-error (err)
      (error 'bsdf-compilation-error
	     :format-control (simple-condition-format-control err)
	     :format-arguments (simple-condition-format-arguments err)))))

(defstruct %bsdf-function 
  lambda-list
  type
  argument-types)

(defvar *bsdf-functions* (make-hash-table :test #'eq))

(defun set-bsdf-function (name value)
  (setf (gethash name *bsdf-functions*) value))

(defmacro bsdf-declfun (name args &body type-specs)
  `(set-bsdf-function ',name
		      (make-%bsdf-function :lambda-list ',args
					   :type ',(first type-specs)
					   :argument-types ',(rest type-specs))))

(defmacro bsdf-defmacro (name args &body body)
  (let ((args-sym (gensym)))
    `(progn (defmethod bsdf-macroexpand ((function (eql ',name)) ,args-sym)
	      (try-bind ',args ,args-sym)
	      (dbind ,args ,args-sym
		,@body)))))

;;
;; String functions
;;

(bsdf-declfun ++ (&rest args)
  :string (args (:list :string)))

(bsdf-declfun substring (string first &optional last))
  :string (string :string) (first :int) (last :int))

(bsdf-declfun length (seq)
  :int)

;;
;; Int functions
;;

(bsdf-declfun + (&rest args)
  :int (args (:list :int)))

(bsdf-declfun - (number &rest numbers)
  :int (number :int) (numbers (:list :int)))

;;
;; List functions
;; 

(bsdf-declfun cons (item list)
  :list (list :list))

(bsdf-declfun list (&rest args)
  :list)

(bsdf-declfun append (&rest lists)
  :list (list (:list :list)))

(defmacro def-nth (n)
  (let ((name (intern (string-upcase (format nil "~:r" n)))))
    `(bsdf-declfun ,name (list)
	 t (list :list))))

(defmacro def-nths (max)
  `(progn ,@(do ((n 1 (1+ n))
		 (specs ()))
		((> n max) (reverse specs))
		(push `(def-nth ,n) specs))))

(def-nths 10)

(bsdf-declfun nth (index list)
  t (index (:int 0)) (list :list))

(bsdf-declfun remove (item list)
  :list (list :list))

(bsdf-declfun remove-duplicates (list)
  :list (list :list))

;;
;; Path type
;;

(bsdf-declfun parent-path (path)
  :path (path :path))

(bsdf-declfun directory-path (path)
  :path (path :path))

(bsdf-declfun root-path (path)
  :path (path :path))

(bsdf-declfun path+ (&rest paths)
  :path (paths (:list :path)))

(bsdf-declfun as-absolute (path)
  :path (path :path))

(bsdf-declfun as-relative (path base-path)
  :path (path :path) (base-path :path))

(bsdf-declfun copy-path (path &key new-name new-type)
  :path (path :path) (new-name :string) (new-type :string))

;; Aux functions

(bsdf-declfun with-input-files (expr files)
  t (files (:list :path)))

(defun %file-list-as-dependencies (list)
  (if (bsdf-list-p list)
      (append (aif (remove-if-not #'bsdf-constant-p list) (list it))
	      (aif (remove-if #'bsdf-constant-p list) (list `(append ,@it))))
      (list list)))

(defmethod bsdf-function-dependencies ((func (eql 'with-input-files)) args)
  (dependencies+ (expression-dependencies (first args))
		 (expression-dependencies (second args))
		 (list () (%file-list-as-dependencies (second args)) ())))

(bsdf-declfun with-output-files (expr files)
  t (files (:list :path)))

(defmethod bsdf-function-dependencies ((func (eql 'with-output-files)) args)
  (dependencies+ (expression-dependencies (first args))
		 (expression-dependencies (second args))
		 (list () () (%file-list-as-dependencies (second args)))))


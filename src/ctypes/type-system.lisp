(in-package #:burning-ctypes)

(defstruct (type-block (:constructor %make-type-block))
  context
  vars
  exprs)

(defstruct type-instance type args)
  
(defgeneric initialize-type-instance (type instance))
(defmethod initialize-type-instance (type instance)
  (declare (ignore type instance))
  t)

(defun instance-type (type &optional args)
  (let ((instance (make-type-instance :type type :args args)))
    (initialize-type-instance type instance)
    instance))

(defstruct (typed-variable (:constructor %make-typed-variable)) name type depended)

(defun make-typed-variable (name &optional (type (instance-type 'undefined-type)))
  (%make-typed-variable :name name :type type))

(defun make-type-block (&key context vars exprs)
  (mapc #'(lambda (expr) (unless (= (length expr) 2) (error "Wrong type expression ~a" expr))) exprs)
  (%make-type-block :context context
		    :vars (mapcar #'make-typed-variable vars)
		    :exprs exprs))

(defun block-addvar (block name)
  (pushnew (make-typed-variable name) (type-block-vars block) :key #'typed-variable-name))

(defun block-addexpr (block expr)
  (unless (= (length expr) 2) (error "Wrong type expression ~a" expr))
  (setf (type-block-exprs block) (append (type-block-exprs block) (list expr)))) 

(defun block-var (block name)
  (find name (type-block-vars block) :key #'typed-variable-name))

(defun block-vartype (block name)
  (typed-variable-type (block-var block name)))

(defun (setf block-vartype) (value block name)
  (setf (typed-variable-type (block-var block name)) value))

(defun inherit-types (block)
  (labels ((form-to-type (form)
	     (if (atom form)
		 (instance-type 'variable-type (list (block-var block form)))
		 (instance-type (first form) (rest form))))
	   (set-type (expr)
	     (let ((name (first expr))
		   (type (form-to-type (second expr))))
	       (setf (block-vartype block name)
		     (if (equalp (block-vartype block name) (instance-type 'undefined-type))
			 type
			 (instance-type 'and-type (list (block-vartype block name) type))))
	       (mapc #'(lambda (var) (pushnew (block-var block name)
					      (typed-variable-depended var) :test #'equalp))
		     (type-depends-on (block-vartype block name))))))
    (mapc #'set-type (type-block-exprs block))
    (mapc #'simplify-vartype (type-block-vars block))))

(defun type= (type1 type2)
  (cond ((and (symbolp type1) (symbolp type2)) (eq type1 type2))
	((symbolp type1) (eq type1 (type-instance-type type2)))
	((symbolp type2) (eq type2 (type-instance-type type1)))
	(t (equalp type1 type2))))

(defun simplify-vartype (var)
  (setf (typed-variable-type var) (simplify-type (typed-variable-type var))))

(defun simplify-type (type)
  (aif (simplify (type-instance-type type) (type-instance-args type))
       (values it t)
       (values type nil)))

(defgeneric simplify (type args))
(defmethod simplify (type args) 
  (declare (ignore type args)))

(defmethod initialize-type-instance ((type (eql 'and-type)) instance)
  (setf (type-instance-args instance)
	(mapcar #'(lambda (arg) (if (type-instance-p arg) arg (instance-type (first arg) (rest arg))))
		(type-instance-args instance))))

(defmethod simplify ((type (eql 'and-type)) args)
  (flet ((simplify-and (type1 type2)
	   (destructuring-bind ((type1 type1-new) (type2 type2-new)) (list type1 type2)
	     (cond ((type= type1 'false-type) type1)
		   ((type= type2 'false-type) type2)
		   ((type= type1 type2) type1)
		   ((or type1-new type2-new) (instance-type 'and-type (list type1 type2)))))))
    (cond ((null args) (instance-type 'true-type))
	  ((null (rest args)) (simplify-type (first args)))
	  (t (simplify-and (multiple-value-list (simplify-type (first args)))
			   (multiple-value-list (simplify-type (if (rest (rest args)) 
								   (instance-type 'and-type (rest args))
								   (first (rest args))))))))))

(defmethod simplify ((type (eql 'variable-type)) args)
  (let ((type (typed-variable-type (first args))))
    (if (not (type-depends-on type))
	type)))

(defun type-depends-on (type)
  (%type-depends-on (type-instance-type type) (type-instance-args type)))

(defgeneric %type-depends-on (type args))
(defmethod %type-depends-on (type args)
  (declare (ignore type args))
  ())

(defmethod %type-depends-on ((type (eql 'and-type)) args)
  (let ((result ()))
    (dolist (arg args)
      (setf result (union result (type-depends-on arg) :test #'equalp)))
    result))

(defmethod %type-depends-on ((type (eql 'variable-type)) args)
  (list (first args)))
  
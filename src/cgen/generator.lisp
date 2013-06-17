(in-package #:burning-cgen)

;;
;; Generator
;;

(defstruct (generator (:conc-name %generator-) (:copier nil))
  (sources ()))

(defparameter *generator* (make-generator))

(defun generator-sources ()
  (%generator-sources *generator*))

(defun (setf generator-sources) (value)
  (setf (%generator-sources *generator*) value))

(defun generator-add-source (value)
  (push value (generator-sources)))

(defmacro cgen-let ((&rest bindings) &body body)
  (flet ((make-binding (binding)
	   `(,(first binding) (make-instance 'variable-expression :name ',(first binding))))
	 (define-type (binding)
	   `(setf (symbol-type (make-cgen-symbol ',(first binding) :variable)) ,(second binding))))
    `(let (,@(mapcar #'make-binding bindings)
	   (*type-table* (copy-type-table)))
       ,@(mapcar #'define-type bindings)
       (make-block ,@body))))
     
;;
;; Basic generation
;;

(defun generate-code ()
  (generate-statments (mapcar #'funcall (reverse (generator-sources)))))

(defun make-block (&rest forms)
  (make-instance 'block :type-table *type-table* :forms forms))

(defmacro burning-cgen-source:defun (name (&rest typed-lambda-list) &body body)
  (labels ((untyped-lambda-list (list)
	     (every-nth list 2))
	   (make-arguments-list (list)
	     (group list 2))
	   (make-bind-list (list)
	     (mapcar (lambda (arg) `(,(first arg) ',(second arg))) (make-arguments-list list))))
    (let ((lambda-list (untyped-lambda-list typed-lambda-list)))
      `(progn (defun ,name (,@lambda-list)
		(make-instance 'funcall-expression 
			       :name (make-cgen-symbol ',name :function)
			       :args-list (list ,@lambda-list)))
	      (generator-add-source (named-lambda ,(make-symbol (symbol-name name)) ()
				      (make-instance 'defun-statment
						     :name (make-cgen-symbol ',name :function)
						     :arglist ',(make-arguments-list typed-lambda-list)
						     :body (cgen-let (,@(make-bind-list typed-lambda-list))
							     ,@body))))))))

(defun burning-cgen-source:setf (place value)
  (make-instance 'setf-statment :place place :value value))

(defun burning-cgen-source:if (expr then-form &optional (else-form nil else-form-p))
  (make-instance 'if-statment
		 :expr expr
		 :then-form (make-block then-form)
		 :else-form (if else-form-p (make-block else-form))
		 :else-form-p else-form-p))

(defmacro burning-cgen-source:defvar (name value)
  (let ((value-sym (gensym)))
    `(progn (defparameter ,name (make-instance 'variable-expression :name ',name))
	    (let ((,value-sym ,value))
	      (generator-add-source (named-lambda ,(make-symbol (symbol-name name)) ()
				      (make-instance 'defvar-statment 
						     :name (make-cgen-symbol ',name :variable)
						     :value ,value-sym
						     :type (expression-type ,value-sym))))))))

(defun burning-cgen-source:cast (expr type)
  (make-instance 'cast-expression :expr expr :type type))

(defun burning-cgen-source:+ (num &rest more-nums)
  (make-instance '+-expression :num num :nums more-nums))

(defun burning-cgen-source:- (num &rest more-nums)
  (make-instance '--expression :num num :nums more-nums))

(defun burning-cgen-source:* (num &rest more-nums)
  (make-instance '*-expression :num num :nums more-nums))

(defun burning-cgen-source:/ (num &rest more-nums)
  (make-instance '/-expression :num num :nums more-nums))

(defmacro burning-cgen-source:let ((&rest bindings) &body body)
  (flet ((make-binding (binding)
	   (let ((arg (first binding)))
	     `(,arg (make-instance 'variable-expression :name ',arg))))
	 (make-initializator (binding)
	   (let ((arg (first binding))
		 (value (second binding)))
	     `(burning-cgen-source:setf ,arg ,value))))
    `(let (,@(mapcar #'make-binding bindings))
       (make-block ,@(mapcar #'make-initializator bindings)
		       ,@body))))


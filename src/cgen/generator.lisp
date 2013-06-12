(in-package #:burning-cgen)

;;
;; Generator
;;

(defstruct (generator (:conc-name %generator-))
  (sources ())
  (type-table (make-hash-table :test #'equal)))

(defparameter *generator* (make-generator))

(defun generator-sources ()
  (%generator-sources *generator*))

(defun (setf generator-sources) (value)
  (setf (%generator-sources *generator*) value))

(defun generator-add-source (value)
  (push value (generator-sources)))

(defun generator-symbol-type (symbol)
  (let ((table (%generator-type-table *generator*)))
    (gethash (cons (cgen-symbol-symbol symbol) (cgen-symbol-namespace symbol)) table)))

(defun (setf generator-symbol-type) (value symbol)
  (let ((table (%generator-type-table *generator*)))
    (setf (gethash (cons (cgen-symbol-symbol symbol) (cgen-symbol-namespace symbol)) table) value)))

;;
;; Basic generation
;;

(defun generate-code ()
  (generate-statments (mapcar #'funcall (reverse (generator-sources)))))

(defun make-block (&rest forms)
  (make-instance 'block-statment :forms forms))

(defmacro burning-cgen-source:defun (name (&rest typed-lambda-list) &body body)
  (flet ((bind-argument (arg)
	   `(,arg (make-instance 'variable-expression :name ',arg)))
	 (untyped-lambda-list (list)
	   (every-nth list 2))
	 (make-arguments-list (list)
	   (group list 2)))
    (let ((lambda-list (untyped-lambda-list typed-lambda-list)))
      `(progn (defun ,name (,@lambda-list)
		(make-instance 'funcall-expression 
			       :name (make-cgen-symbol ',name :function)
			       :args-list (list ,@lambda-list)))
	      (generator-add-source (named-lambda ,(make-symbol (symbol-name name)) ()
				      (make-instance 'defun-statment
						     :name (make-cgen-symbol ',name :function)
						     :arglist ',(make-arguments-list typed-lambda-list)
						     :body (let (,@(mapcar #'bind-argument lambda-list))
							     (make-block ,@body)))))))))

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

(defun burning-cgen-source:cast (value type)
  (make-instance 'cast-expression :value value :type type))

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


(in-package :burning-ffi)

(defmacro foreign-lambda-wrapper (return-value args)
  (let ((arg-names (mapcar #'(lambda (x) (gensym (symbol-name x))) args)))
    `#'(lambda (x)
	 #'(lambda ,arg-names
	     (cffi:foreign-funcall-pointer x () ,@(reduce #'append (mapcar #'list args arg-names)) ,return-value)))))

(defmacro lambda-callback-wrapper (return-value args)
  `#'(lambda (x)
       (cffi:get-callback (generate-callback x ',return-value ',args))))

(defun generate-callback (function return-type args)
  (let ((argument-names (mapcar #'(lambda (x) (gensym (symbol-name x))) args)))
    (eval `(cffi:defcallback ,(gensym "CALLBACK") ,return-type ,(mapcar #'list argument-names args)
	     (funcall ,function ,@argument-names)))))

(defun make-functional-type (return-type args)
  (let ((callback-wrapper (eval `(lambda-callback-wrapper ,return-type ,args)))
	(foreign-wrapper (eval `(foreign-lambda-wrapper ,return-type ,args))))
    (eval `(cffi:defctype ,(gensym "FUNCTIONAL") 
	       (:wrapper :pointer 
			 :from-c ,foreign-wrapper
			 :to-c ,callback-wrapper)))))

(defmethod get-type ((name (eql :function)) &rest args)
  (if (%get-type args)
      (%get-type args)
      (progn 
	(%add-type args (funcall #'make-functional-type (first args) (second args)))
	(%get-type args))))

(defun ffi-type (argument)
  (let ((type-name (if (atom argument) argument (first argument)))
	(arguments (if (atom argument) nil (rest argument))))
    (let ((value (apply #'get-type type-name arguments)))
      (if value 
	  value
	  (error "Wrong ffi type - ~a." argument)))))

(defaction function (name-and-type arguments)
  (let ((args-count (length name-and-type)))
    (when (or (> args-count 3) (< args-count 2)) (error "Wrong parameters count in call to :function"))
    (flet ((parse-argument (argument)
	     (let ((last (ffi-type (first (last argument)))))
	       (mapcar #'(lambda (x) (list x last)) (butlast argument)))))
      (let ((c-name (if (= args-count 2) (first name-and-type) (second name-and-type)))
	    (lisp-name (if (= args-count 3) (first name-and-type) nil))
	    (type (first (last name-and-type))))
	(eval `(cffi:defcfun ,(if lisp-name (list c-name lisp-name) c-name) ,(ffi-type type)
		 ,@(reduce #'append (mapcar #'parse-argument arguments))))))))

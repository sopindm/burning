(in-package #:burning-cgen)

;;
;; Generator
;;

(defun make-generator ()
  ())

(defparameter *generator* (make-generator))

; sources

(defun generator-sources ()
  *generator*)

(defun (setf generator-sources) (value)
  (setf *generator* value))

(defun generator-add-source (value)
  (push value (generator-sources)))

;;
;; Basic generation functions
;;

(defun generate-code ()
  (format nil "~{~a~^~%~}" (mapcar #'generate-statment (mapcar #'funcall (reverse (generator-sources))))))

#|
(defun generate-call (name &rest args)
  (format nil "~a~a" (generate-name name) "()"))

(defun generate-reference (name)
  (format nil "~a" (generate-name name)))
|#

;;
;; Basic generation macro's
;;

(defun generate-block (&rest forms)
  (make-instance 'block-statment :forms forms))

(defmacro burning-cgen-source:defun (name (&rest args) &body body)
  (flet ((bind-argument (arg)
	   `(,arg (make-instance 'variable-expression :name ',arg))))
    `(progn (defun ,name (,@args)
	      (make-instance 'funcall-expression :name ',name :args-list (list ,@args)))
	    (generator-add-source (named-lambda ,(make-symbol (symbol-name name)) ()
				    (make-instance 'defun-statment
						   :name (make-cgen-symbol ',name :function)
						   :arglist ',args
						   :body (let (,@(mapcar #'bind-argument args))
							   (generate-block ,@body))))))))

(defmacro burning-cgen-source:setf (place value)
  `(make-instance 'setf-statment :place ,place :value ,value))

(defmacro burning-cgen-source:if (expr then-form &optional (else-form nil else-form-p))
  (flet ((make-then-form (form)
	   `(generate-block ,form))
	 (make-else-form (form)
	   `(lines* ""
		    "else"
		    (generate-block ,form))))
    `(format nil "if( ~a )~%~a~a" 
	     ,expr 
	     ,(make-then-form then-form)
	     ,(if else-form-p (make-else-form else-form) ""))))

(defmacro burning-cgen-source:defvar (name value)
  `(progn (defparameter ,name (make-instance 'variable-expression :name ',name))
	  (generator-add-source (named-lambda ,(make-symbol (symbol-name name)) ()
				  (make-instance 'defvar-statment :name ,name :value ,value)))))

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
       (generate-block ,@(mapcar #'make-initializator bindings)
		       ,@body))))


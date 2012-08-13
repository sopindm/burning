(in-package #:burning-ctypes)

(defstruct (ctype (:conc-name type-) (:constructor %make-type))
  arguments
  relations)

(defun make-type (arguments &rest relations)
  (%make-type :arguments arguments :relations relations))

(defstruct (type-context (:constructor %make-type-context))
  table
  base)

(defun make-type-context (&optional (base *type-context*))
  (%make-type-context :table (make-hash-table) :base base))

(defvar *type-context* (make-type-context nil))

(defun %symbol-type (symbol context)
  (unless context
    (error 'ctypes-undefined-type :name symbol))
  (multiple-value-bind (value set-p) (gethash symbol (type-context-table context))
    (if set-p value
	(%symbol-type symbol (type-context-base context)))))

(defun symbol-type (symbol)
  (%symbol-type symbol *type-context*))

(defun unbind-type (symbol)
  (remhash symbol (type-context-table *type-context*)))

(defun (setf symbol-type) (value symbol)
  (setf (gethash symbol (type-context-table *type-context*)) value))

(define-condition ctypes-undefined-type (error)
  ((name :initarg :name :reader ctypes-undefined-type-name)))

(defmacro in-type-context (context &body body)
  `(let ((*type-context* ,context))
     ,@body))

(defmacro tlet ((&rest bindings) &body body)
  (flet ((parse-type (form)
	   `(setf (symbol-type ',(first form)) (make-type ,@(rest form)))))
    `(in-type-context (make-type-context)
       ,@(mapcar #'parse-type bindings)
       ,@body)))

(set-dispatch-macro-character #\# #\T 
			      #'(lambda (stream c1 c2)
				  (declare (ignore c1 c2))
				  `(symbol-type ',(read stream t nil t))))

(defmacro define-type (name (&rest arguments) &body relations)
  `(setf (symbol-type ',name) (make-type '(,@arguments) ,@(mapcar #'(lambda (rel) `',rel) relations))))



  
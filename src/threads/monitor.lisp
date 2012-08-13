(in-package :burning-threads)

(defmacro defmonitor (name-and-options &body slots)
  (labels ((condition-p (slot)
	     (and (not (atom slot))
		  (find ':condition slot)))
	   (to-slot (condition)
	     (first condition))
	   (intern-constructor ()
	     (intern (concatenate 'string
				  "MAKE-"
				  (symbol-name (if (atom name-and-options)
						   name-and-options
						   (first name-and-options))))))
	   (constructor-name (options)
	     (if (atom options)
		 (intern-constructor)
		 (let ((constructor (find ':constructor options :key #'(lambda (x) (if (atom x) x (first x))))))
		   (if constructor
		       (second constructor)
		       (intern-constructor)))))
	   (condition-constructor (name lock-name)
	     `(,(intern (symbol-name name) "KEYWORD") (make-condition-variable ,lock-name))))
    (let ((options name-and-options)
	  (conditions (mapcar #'to-slot (remove-if-not #'condition-p slots)))
	  (no-conditions (remove-if #'condition-p slots))
	  (lock-sym (gensym)))
      (let* ((constructor-name (constructor-name options))
	     (gen-constructor-name (gensym))
	     (options (cons (if (atom options) options (first options))
			    `((:constructor ,gen-constructor-name)))))
	`(progn 
	   (defstruct ,options
	     %lock
	     ,@conditions
	     ,@no-conditions)
	   (defun ,constructor-name (&rest args)
	     (let ((,lock-sym (make-mutex)))
	       (apply ',gen-constructor-name 
		      ':%lock ,lock-sym 
		      ,@(reduce #'append (mapcar #'(lambda (x) (condition-constructor x lock-sym)) conditions))
		      args))))))))

(defmacro defmfun (name (monitor &rest args) &body body)
  `(defun ,name (,monitor ,@args)
     (with-mutex (slot-value ,monitor '%lock)
       ,@body)))
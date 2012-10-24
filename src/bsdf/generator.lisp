(in-package #:burning-bsdf-generator)

(defvar *bsdf-generator* 'bsc)

;;
;; Generator interface macro's
;;

(defmacro define-generator (name)
  `(define-command-generator ,name))

(defmacro define-generator-generic (name (&rest args) &body body)
  `(progn (defgeneric ,name (generator ,@args))
	  ,(when body
		 `(defmethod ,name (generator ,@args) 
		    (declare (ignorable generator))
		    ,@body))))

(defmacro define-generator-method (name (generator &rest args) &body body)
  (let ((generator-name (if (listp generator) (first generator) (gensym)))
	(generator-specializer `(eql ',(if (listp generator) (second generator) generator))))
    `(defmethod ,name ((,generator-name ,generator-specializer) ,@args)
       ,@body)))

;;
;; Generator interface
;;

(define-generator-generic generator-make-context (path)
  (cons path (make-double-list nil)))

(define-generator-generic generator-write-context (context))

(define-generator-generic generator-close-context (context)
  (declare (ignore context))
  nil)

(define-generator-generic generator-context-add-target (context target)
  (let ((native (generate-command generator target)))
    (mapc (lambda (target) (double-list-push target (rest context))) native)))

(defun generate-command (generator target)
  (let ((*bsdf-generator* generator)
	(command (target-command target)))
    (funcall-command command target)))

(defun generate-file (&key (generator *bsdf-generator*)
		      (path (path-from-string "")))
  (let ((context (generator-make-context generator path)))
    (unwind-protect 
	 (progn 
	   (mapc (lambda (target) (generator-context-add-target generator context target))
		 (get-targets))
	   (generator-write-context generator context))
      (generator-close-context generator context))))

(defun generate-from-file (path &key (generator nil generator-p)  (output-path nil output-p))
  (let ((*targets* (copy-targets-table))
	(*package* (find-package '#:burning-bsdf-user)))
    (let ((exprs (make-double-list nil)))
      (with-open-file (input path :direction :input)
	(do ((expr #1=(read input nil :eof) #1#))
	    ((eq expr :eof) nil)
	  (double-list-push expr exprs)))
      (eval (cons 'progn (double-list-head exprs))))
    (apply #'generate-file (append (when generator-p (list :generator generator))
				   (when output-p (list :path output-path))))))
	

;;
;; Generator commands basics
;;

(defvar *commands* (make-hash-table))

(defmacro define-command-generator (name)
  `(setf (gethash ',name *commands*) (make-hash-table)))

(defun funcall-command (command target)
  (if command 
      (funcall command target)
      (funcall (get-command nil) target)))

(defun get-command (name &optional (generator *bsdf-generator*))
  (let ((command (gethash name (gethash generator *commands*))))
    (unless (functionp command)
      (error "Unknown command ~a in generator ~a" name generator))
    command))

(defun (setf get-command) (value name &optional (generator *bsdf-generator*))
  (setf (gethash name (gethash generator *commands*)) value))

(defmacro define-command-generic (name (&rest args))
  (let ((target-sym (gensym)))
    `(defun ,name (,@args)
       (lambda (,target-sym)
	 (funcall (get-command ',name) ,target-sym ,@(lambda-list-arguments args))))))

(defmacro define-command-method (generator name (target &rest args) &body body)
  `(setf (get-command ',name ',generator) 
	 (lambda (,target ,@(lambda-list-arguments args)) ,@body)))

;;
;; Common generator functions
;;

(define-command-generic echo (control &rest args))


  
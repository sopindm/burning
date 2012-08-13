(in-package #:burning-cgen)

(defstruct (macrotable (:copier nil))
  (macros (make-hash-table :test #'equal))
  base-table)

(defun copy-macrotable (table)
  (make-macrotable :base-table table))

(defvar *cgen-macros* (make-macrotable))

(defun macro-name (name symbol-p)
  (if symbol-p (cons :symbol name) name))

(defun cg-macro (name &key symbol-p)
  (labels ((macro-from-table (table)
	     (gethash (list *cgen-language* (macro-name name symbol-p)) table))
	   (macro-from-macrotable (table)
	     (or (macro-from-table (macrotable-macros table)) 
		 (aif (macrotable-base-table table) (macro-from-macrotable it)))))
    (macro-from-macrotable *cgen-macros*)))

(defun (setf cg-macro) (value name &key symbol-p)
  (setf (gethash (list *cgen-language* (macro-name name symbol-p)) (macrotable-macros *cgen-macros*)) value))

(defmacro with-local-macrotable (&body body)
  `(let ((*cgen-macros* (copy-macrotable *cgen-macros*)))
     ,@body))

(defun define-macro (name args body &key symbol-p)
  (setf (cg-macro name :symbol-p symbol-p) (eval `(lambda (,@args) ,@body))))

(defmacro def-cg-macro (name (&rest args) &body body)
  `(define-macro ',name ',args ',body))

(defun expander (name)
  (aif (%expander name *cgen-language*)
       (lambda (&rest args)
	 (let ((expanded (apply it args)))
	   (if (listp expanded)
	       (aif (eq (first expanded) :expand)
		    (do-macroexpand (second expanded))
		    (mapcar #'(lambda (arg) (if (and (listp arg) (eq (first arg) :expand))
						(do-macroexpand (second arg))
						arg))
			    expanded))
	       expanded)))))

(defgeneric %expander (name language))
(defmethod %expander (name language)
  (declare (ignore name language))
  nil)

(defmacro %defexpander (language class (&rest args) &body body)
  (with-gensyms (expander-name language-name)
    `(defmethod %expander ((,expander-name (eql ',class)) 
			   ,(if language `(,language-name (eql ',language)) language-name))
       (declare (ignore ,expander-name ,language-name))
       (named-lambda ,(symbolicate class "-EXPANDER") (,@args)
	 ,@body))))

(defmacro def-universal-expander (class (&rest args) &body body)
  `(%defexpander nil ,class ,args ,@body))

(defmacro defexpander (class (&rest args) &body body)
  `(%defexpander ,*cgen-language* ,class ,args ,@body))

(defun expand-progn (body)
  (if (> (length body) 1) (cons 'progn (mapcar #'do-macroexpand body)) (do-macroexpand (first body))))

(def-universal-expander macrolet (forms &rest body)
  (with-local-macrotable
    (mapc #'(lambda (form) (destructuring-bind (name args &rest body) form
			     (define-macro name args body))) forms)
    (expand-progn body)))

(def-universal-expander symbol-macrolet (forms &rest body)
  (with-local-macrotable
    (mapc #'(lambda (form) (destructuring-bind (name &rest body) form
			     (define-macro name () body :symbol-p t))) forms)
    (expand-progn body)))

(defun expand-list (name args)
  (acond ((cg-macro name) (do-macroexpand (apply it args)))
	 ((expander name) (apply it args))
	 (t (cons name (mapcar #'do-macroexpand args)))))

(defun do-macroexpand (args)
  (if (listp args) (expand-list (first args) (rest args))
      (aif (cg-macro args :symbol-p t)
	   (do-macroexpand (funcall it))
	   args)))

(defun macroexpand (args)
  (handler-case (do-macroexpand args)
    (error (err) (error 'cgen-macroexpansion-error :object err)))) 



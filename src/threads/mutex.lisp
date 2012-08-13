(in-package :burning-threads)

(defun make-mutex (&optional (name "mutex"))
  #+clisp
  (mt:make-mutex :name name)
  #+sbcl
  (sb-thread:make-mutex :name name)
  #+ccl
  (ccl:make-lock name)
  #-(or clisp sbcl ccl)
  (no-threads-error))

(defun grab-mutex (mutex)
  #+clisp
  (mt:mutex-lock mutex)
  #+sbcl
  (sb-thread:grab-mutex mutex)
  #+ccl
  (ccl:grab-lock mutex)
  #-(or clisp sbcl ccl)
  (no-threads-error))

(defun release-mutex (mutex)
  #+clisp
  (mt:mutex-unlock mutex)
  #+sbcl
  (sb-thread:release-mutex mutex)
  #+ccl
  (ccl:release-lock mutex)
  #-(or clisp sbcl ccl)
  (no-threads-error))

(defmacro with-mutex (mutex &body body)
  #+clisp
  `(mt:with-mutex-lock (,mutex)
     ,@body)
  #+sbcl
  `(sb-thread:with-mutex (,mutex)
     ,@body)
  #+ccl
  `(ccl:with-lock-grabbed (,mutex)
     ,@body)
  #-(or clisp sbcl ccl)
  `(no-threads-error))

(defstruct (shared-variable (:constructor %make-sv))
  value
  lock)

(defun make-shared-variable (value &optional (lock (make-mutex)))
  (%make-sv :value value :lock lock))

(defmacro with-shared-variable (variable &body body)
  (let ((variable-sym (gensym))
	(variable-name (if (atom variable) variable (first variable)))
	(variable-value (if (listp variable) (second variable) variable)))
    `(with-mutex (shared-variable-lock ,variable-value) 
       (let ((,variable-sym ,variable-value))
	 (symbol-macrolet ((,variable-name ,`(shared-variable-value ,variable-sym)))
	   ,@body)))))

(defmacro do-with-shared-variables ((&rest variables) &body body)
  (cond
    ((null variables) `(progn ,@body))
    ((null (rest variables)) `(with-shared-variable ,(first variables) ,@body))
    (t `(with-shared-variable ,(first variables)
	  (do-with-shared-variables ,(rest variables) ,@body)))))

(defmacro with-shared-variables ((&rest variables) &body body)
  (flet ((symbol< (s1 s2)
	   (string< (symbol-name s1) (symbol-name s2))))
    (let ((sorted-variables (sort (copy-tree variables) #'symbol<)))
      `(do-with-shared-variables ,sorted-variables ,@body))))
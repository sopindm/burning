(in-package #:burning-ctypes)

;;
;; types
;;

(defclass ctype ()
  ((name :initarg :name :reader type-name)
   (args-list :initarg :args-list :reader type-args-list)))

(defun make-type (name args-list)
  (check-lambda-list args-list :allowed-keywords '(&optional &rest))
  (make-instance 'ctype :name name :args-list args-list))

(defun copy-type (type &key (new-name nil new-name-p) (new-args-list nil new-args-list-p))
  (make-type (if new-name-p new-name (type-name type))
	     (if new-args-list-p new-args-list (copy-list (type-args-list type)))))

(defun type= (type1 type2)
  (and (eq (type-name type1) (type-name type2))
       (equal (type-args-list type1) (type-args-list type2))))

(defmethod print-object ((type ctype) stream)
  (with-slots (name args-list) type
    (format stream "#<CTYPE :name ~a :args-list ~a>" name args-list)))

(defun check-type-lambda-list (args type)
  (if (bind-lambda-list (type-args-list type) args)
      t))

;;
;; Type table type
;;

(defstruct (type-table (:constructor %make-type-table) (:copier %copy-type-table))
  (types (make-hash-table :test #'eq))
  (relations (make-hash-table :test #'eq)))

(defun make-type-table (&rest types-and-prototype)
  (let ((table (aif (find-keyword :prototype types-and-prototype)
		    (copy-type-table it)
		    (%make-type-table))))
    (let ((types (remove-keywords types-and-prototype)))
      (mapc (lambda (type) (set-type type table)) types))
    table))

(defun copy-type-table (table)
  (flet ((copy-types ()
	   (copy-hash-table (type-table-types table)))
	 (copy-relations ()
	   (copy-hash-table (type-table-relations table) 
			    :key (lambda (x) (copy-type-generic x 2)))))
    (let ((table (%copy-type-table table)))
      (setf (type-table-types table) (copy-types))
      (setf (type-table-relations table) (copy-relations))
      table)))

(defun type-table-size (table)
  (hash-table-count (type-table-types table)))

(defvar *types* (make-type-table))

(defun get-type (name &optional (table *types*))
  (aif (gethash name (type-table-types table))
       it
       (error "Unknown type ~a in type table ~a." name table)))

(defun set-type (type &optional (table *types*))
  (setf (gethash (type-name type) (type-table-types table)) type))

(defun remove-type (type table)
  (let ((type-name (etypecase type
		     (ctype (type-name type))
		     (symbol type))))
    (remhash type-name (type-table-types table))))

;;
;; Type instances
;;

(defstruct (type-instance (:constructor %make-type-instance) (:conc-name instance-))
  type
  args)

(defun make-type-instance (name &rest args-and-options)
  (let ((args (remove-keyword :type-table args-and-options))
	(table (or (find-keyword :type-table args-and-options) *types*)))
    (let ((type (get-type name table)))
      (check-type-lambda-list args type)
      (%make-type-instance :type type :args args))))


;;
;; Type methods
;;

(defun make-type-generic (args-count &optional method)
  (flet ((make-table (nil-value) 
	   (let ((table (make-hash-table :test #'eq)))
	     (when nil-value (setf (gethash nil table) nil-value))
	     table)))
    (if (= args-count 1) 
	(make-table method) 
	(make-table (make-type-generic (1- args-count) method)))))

(defun copy-type-generic (generic args-count)
  (copy-hash-table generic 
		   :key (if (= args-count 1) #'identity (lambda (x) (copy-type-generic x (1- args-count))))))

(defun type-method (generic &rest types)
  (flet ((method (type) (or (gethash type generic) (gethash nil generic))))
    (if (null (rest types))
	(method (first types))
	(apply #'type-method (method (first types)) (rest types)))))

(defun (setf type-method) (value generic &rest types)
  (labels ((method (generic type types)
	     (or (gethash type generic)
		 (setf (gethash type generic) (make-type-generic (length types)))))
	   (do-set (generic types)
	     (if (null (rest types))
		 (setf (gethash (first types) generic) value)
		 (do-set (method generic (first types) (rest types)) (rest types)))))
    (do-set generic types)))

(define-condition type-method-error (error)
  ((generic :initarg :generic :reader type-method-error-generic)
   (type :initarg :type :reader type-method-error-type)))

(defun remove-type-method (generic &rest types)
  (if (null (rest types))
      (if (gethash (first types) generic)
	  (remhash (first types) generic)
	  (error 'type-method-error :generic generic :type (first types)))
      (let ((method (type-method generic (first types))))
	(apply #'remove-type-method method (rest types))
	(when (= (hash-table-count method) 0)
	  (remhash (first types) generic)))))

;;
;; Type relations
;;

(defun types-relation (name &optional (table *types*))
  (aif (gethash name (type-table-relations table))
       it
       (error "Unknown type relation ~a." name)))

(defun (setf types-relation) (value name &optional (table *types*))
  (setf (gethash name (type-table-relations table)) value))

(defun remove-relation (name &optional (table *types*))
  (remhash name (type-table-relations table)))

(defun call-relation (name arg1 arg2 &optional (table *types*))
  (let ((name1 (type-name (instance-type arg1)))
	(name2 (type-name (instance-type arg2))))
    (let ((method (type-method (types-relation name table) name1 name2)))
      (unless method (error "No method for relation ~a and types ~a, ~a." name name1 name2))
      (funcall method arg1 arg2))))

;;
;; Type table macro's
;;
  
(defmacro define-type (name (&rest arguments) (&rest relations) &body options)
  (check-keywords '(:type-table) options)
  (labels ((set-relation-method (relation type)
	     `(define-relation-method ,relation (,(aif (listp type) type (list type)) (,name ,@arguments)) t))
	   (set-relation (spec)
	     (mapcar (lambda (type) (set-relation-method (first spec) type)) (rest spec))))
    `(progn (set-type (make-type ',name ',arguments) ,@(aif (find-keyword :type-table options) (list it)))
	    ,@(mapcan #'set-relation relations))))

(defmacro with-type-table (init-form &body body)
  `(let ((*types* ,init-form))
     ,@body
     *types*))

(defmacro with-local-type-table (&body body)
  `(let ((*types* (copy-type-table *types*)))
     ,@body
     *types*))
  
(defmacro define-relation (name-and-options (arg1 arg2) &body body)
  (let ((name (if (listp name-and-options) (first name-and-options) name-and-options))
	(options (if (listp name-and-options) (rest name-and-options) nil)))
    `(setf (types-relation ',name ,@(aif (find-keyword :type-table options) (list it)))
	   (make-type-generic 2 ,@(if body `((lambda (,arg1 ,arg2) ,@body)))))))

(defun %argument-to-type (arg)
  (if (listp arg) (first arg)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun argument-lambda-list (arg)
    (if (listp arg)
	(rest arg)
	(list arg))))

(defun %check-type-lambda-list (method type list)
  (unless (lambda-list= (type-args-list type) list :macro-p t)
    (error (lines "Lambda list of method ~a is incompatible with that of type ~a."
		  "Method's lambda list: ~a"
		  "Type's lambda list: ~a")
	   method (type-name type) list (type-args-list type)))
  t)

(defmacro define-relation-method (name-and-options (arg1 arg2) &body body)
  (let ((name (if (listp name-and-options) (first name-and-options) name-and-options))
	(options (if (listp name-and-options) (rest name-and-options) nil))
	(arg1-sym (gensym))
	(arg2-sym (gensym))
	(list1 (argument-lambda-list arg1))
	(list2 (argument-lambda-list arg2)))
    (check-lambda-list list1 :macro-p t)
    (check-lambda-list list2 :macro-p t)
    (awhen (intersection (lambda-list-arguments list1) (lambda-list-arguments list2))
      (error "Variables ~a occurs more than once in lambda list ~a." it (list list1 list2)))
    (let ((type-table (find-keyword :type-table options)))
      `(progn
	 ,@(aif (%argument-to-type arg1) 
		`((%check-type-lambda-list ',name (get-type ',it ,@(aif type-table (list it))) ',list1)))
	 ,@(aif (%argument-to-type arg2) 
		`((%check-type-lambda-list ',name (get-type ',it ,@(aif type-table (list it))) ',list2)))
	 (setf (type-method (types-relation ',name ,@(aif (find-keyword :type-table options) (list it)))
			    (%argument-to-type ',arg1)
			    (%argument-to-type ',arg2))
	       (lambda (,arg1-sym ,arg2-sym)
		 (destructuring-bind ,(list list1 list2)
		     (list ,(if (symbolp arg1) `(list ,arg1-sym) `(instance-args ,arg1-sym))
			   ,(if (symbolp arg2) `(list ,arg2-sym) `(instance-args ,arg2-sym)))
		   ,@body)))))))

(defun remove-relation-method (name type1 type2 &optional (table *types*))
  (handler-case
      (remove-type-method (types-relation name table) type1 type2)
    (type-method-error ()
      (error "Unknown method for type relation ~a and types ~a, ~a." name type1 type2))))


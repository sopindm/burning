(in-package #:burning-ctypes)

;;
;; types
;;

(defclass ctype ()
  ((name :initarg :name :reader type-name)
   (args-list :initarg :args-list :reader type-args-list)
   (imagine-type-p :initarg :imagine-type-p :initform nil :reader imagine-type-p)))

(defun make-type (name args-list &key imagine-type-p)
  (make-instance 'ctype :name name :args-list args-list :imagine-type-p imagine-type-p))

(defgeneric copy-type (type &key new-name new-args-list imagine-type-p))

(defmethod copy-type ((type ctype) &key
		      (new-name nil new-name-p)
		      (new-args-list nil new-args-list-p)
		      (imagine-type-p nil imagine-type-p-set-p))
  (make-type (if new-name-p new-name (type-name type))
	     (if new-args-list-p new-args-list (copy-tree (type-args-list type)))
	     :imagine-type-p (if imagine-type-p-set-p imagine-type-p (imagine-type-p type))))

(defgeneric type= (type1 type2))

(defmethod type= ((type1 ctype) (type2 ctype))
  (and (eq (type-name type1) (type-name type2))
       (equal (type-args-list type1) (type-args-list type2))
       (eq (imagine-type-p type1) (imagine-type-p type2))))

(defun check-type-lambda-list (args type)
  (let ((list (type-args-list type)))
    (unless (and (typep args 'list) (= (length args) (length list)))
      (error "Wrong arguments list ~a for type ~a with lambda list ~a." args (type-name type) list))
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
	   (copy-hash-table (type-table-relations table))))
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

(defun call-type-method (generic &rest args)
  (apply (apply #'type-method generic 
		(mapcar (lambda (arg) (type-name (instance-type arg))) args)) args))

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
  (call-type-method (types-relation name table) arg1 arg2))

;;
;; Type table macro's
;;
  
(defmacro define-type (name (&rest arguments) (&rest relations) &body options)
  (declare (ignore relations))
  (check-keywords '(:imagine-type-p :type-table) options)
  `(set-type (make-type ',name ',arguments ,@(aif (find-keyword :imagine-type-p options)
						  `(:imagine-type-p ,it)))
	     ,@(aif (find-keyword :type-table options) (list it))))

(defmacro with-type-table (init-form &body body)
  `(let ((*types* ,init-form))
     ,@body))

(defmacro with-local-type-table (&body body)
  `(let ((*types* (copy-type-table *types*)))
     ,@body))
  
(defmacro define-relation (name-and-options (arg1 arg2) &body body)
  (let ((name (if (listp name-and-options) (first name-and-options) name-and-options))
	(options (if (listp name-and-options) (rest name-and-options) nil)))
    `(setf (types-relation ',name ,@(aif (find-keyword :type-table options) (list it)))
	   (make-type-generic 2 (lambda (,arg1 ,arg2) ,@body)))))

(defun %argument-to-type (arg)
  (if (listp arg) (first arg)))

(defmacro define-relation-method (name-and-options (arg1 arg2) &body body)
  (let ((name (if (listp name-and-options) (first name-and-options) name-and-options))
	(options (if (listp name-and-options) (rest name-and-options) nil))
	(arg1-sym (gensym))
	(arg2-sym (gensym)))
    (flet ((argument-bindings (arg var)
	     (if (listp arg) 
		 nil
		 `((,arg ,var)))))
      `(setf (type-method (types-relation ',name ,@(aif (find-keyword :type-table options) (list it)))
			  (%argument-to-type ',arg1)
			  (%argument-to-type ',arg2))
	     (lambda (,arg1-sym ,arg2-sym)
	       (declare (ignorable ,arg1-sym ,arg2-sym))
	       (let (,@(argument-bindings arg1 arg1-sym)
		     ,@(argument-bindings arg2 arg2-sym))
		 ,@body))))))


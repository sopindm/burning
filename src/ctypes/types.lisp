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
;; Type relations
;;

(defun make-types-relation (method)
  (flet ((make-table (default-key default-value)
	   (let ((table (make-hash-table :test #'eq)))
	     (setf (gethash default-key table) default-value)
	     table)))
    (make-table nil (make-table nil method))))

(defun types-relation (name &optional (table *types*))
  (aif (gethash name (type-table-relations table))
       it
       (error "Unknown type relation ~a." name)))

(defun (setf types-relation) (value name &optional (table *types*))
  (setf (gethash name (type-table-relations table)) value))

(defun remove-relation (name &optional (table *types*))
  (remhash name (type-table-relations table)))

(defun types-relation-method (relation type1 type2) 
  (flet ((table-relation (table type)
	   (or (gethash type table) (gethash nil table))))
    (table-relation (table-relation relation type1) type2)))

(defun (setf types-relation-method) (value relation type1 type2)
  (unless (gethash type1 relation)
    (setf (gethash type1 relation) (make-hash-table :test #'eq)))
  (let ((table (gethash type1 relation)))
    (unless table 
      (setf table (setf (gethash type1 relation) (make-hash-table :test #'eq))))
    (setf (gethash type2 table) value)))

(defun remove-types-relation-method (relation type1 type2)
  (let ((table (gethash type1 relation)))
    (unless table (error "No method for relation ~a and types ~a and ~a exists." relation type1 type2))
    (if (gethash type2 relation)
	(remhash type2 relation)
	(error "No method for relation ~a and types ~a and ~a exists." relation type1 type2))))

(defun relation-call (relation arg1 arg2)
  (funcall (types-relation-method relation (type-name arg1) (type-name arg2)) arg1 arg2))

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
  
(defmacro define-relation (name (arg1 arg2 &rest options) &body body)
  `(setf (types-relation ',name ,@(aif (find-keyword :type-table options) (list it)))
	 (make-types-relation (lambda (,arg1 ,arg2) ,@body))))

(defun %argument-to-type (arg)
  (if (and (listp arg) (= (length arg) 2)) (second arg)))

(defmacro define-relation-method (name (arg1 arg2) &body body)
  (flet ((argument-name (arg)
	   (if (listp arg) (first arg) arg)))
    `(setf (types-relation-method (types-relation ',name)
				  (%argument-to-type ',arg1)
				  (%argument-to-type ',arg2))
	   (lambda (,(argument-name arg1) ,(argument-name arg2)) ,@body))))

(defun have-relation-p (name arg1 arg2 &optional (table *types*))
  (relation-call (types-relation name table) arg1 arg2))
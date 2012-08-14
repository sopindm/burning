(in-package #:burning-ctypes)

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

(defun make-type-table (&rest types-and-prototype)
  (let ((table (aif (find-keyword :prototype types-and-prototype)
		    (copy-type-table it)
		    (make-hash-table :test #'eq))))
    (let ((types (remove-keywords types-and-prototype)))
      (mapc (lambda (type) (set-type type table)) types))
    table))

(defun copy-type-table (table)
  (copy-hash-table table))

(defun type-table-size (table)
  (hash-table-count table))

(defvar *types* (make-type-table))

(defun get-type (name &optional (table *types*))
  (aif (gethash name table)
       it
       (error "Unknown type ~a in type table ~a." name table)))

(defun set-type (type &optional (table *types*))
  (setf (gethash (type-name type) table) type))

(defmacro define-type (name (&rest arguments) (&rest relations) &body options)
  (declare (ignore relations))
  (check-keywords '(:imagine-type-p :type-table) options)
  `(set-type (make-type ',name ',arguments ,@(aif (find-keyword :imagine-type-p options)
						  `(:imagine-type-p ,it)))
	     ,@(aif (find-keyword :type-table options) (list it))))
    

(defun remove-type (type table)
  (let ((type-name (etypecase type
		     (ctype (type-name type))
		     (symbol type))))
    (remhash type-name table)))
  
(defmacro with-type-table (init-form &body body)
  `(let ((*types* ,init-form))
     ,@body))

(defmacro with-local-type-table (&body body)
  `(let ((*types* (copy-type-table *types*)))
     ,@body))
  
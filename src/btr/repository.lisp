(in-package #:burning-btr)

(defparameter *repository-api-version* "0.1")

(define-condition btr-error (error) ())
(defgeneric btr-error-string (err))

(defmacro define-error-string ((variable class) &body body)
  `(defmethod btr-error-string ((,variable ,class))
     ,@body))

;;
;; Entity
;;

(defclass entity () 
  ((name :initarg :name :reader entity-name)))

;;
;; Entities group
;;

(defclass entities-group ()
  ((entities :initarg :entities :accessor entities)))

(define-condition entity-with-same-name-already-exists (btr-error)
  ((name :initarg :name :reader entity-with-same-name-already-exists-name)))

(define-error-string (err entity-with-same-name-already-exists)
  (format nil "File ~a already in repository.~%" (entity-with-same-name-already-exists-name err)))

(defun add-entity (entity list)
  (when (find (entity-name entity) (entities list) :test #'equal :key #'entity-name)
    (error 'entity-with-same-name-already-exists :name (entity-name entity)))
  (setf (entities list) (nconc (entities list) (list entity))))

(defun remove-entity (name list)
  (setf (entities list) (remove name (entities list) :test #'equal :key #'entity-name)))


;;
;; Repository
;;

(defclass repository (entities-group)
  ((version :initarg :version :reader repository-version)))

(defparameter *repository-class* 'repository)

(defun make-repository (&key (version *repository-api-version*) entities)
  (make-instance *repository-class* :entities entities :version version))

;;
;; Units
;;

(defclass unit (entity)
  ((files :initarg :files :accessor unit-files)))

(defun unit-file (unit)
  (first (unit-files unit)))

(defparameter *unit-class* 'unit)

(defun make-unit (name &key files)
  (make-instance *unit-class* :name name :files files))

;;
;; Groups
;;

(defclass group (entity entities-group) ())

(defun make-group (name)
  (make-instance 'group :name name :entities ()))

;;
;; Xml attributes
;;

(defstruct attribute
  name
  name-string
  default-value)

(define-condition missed-xml-attribute-warning (warning)
  ((node-name :initarg :node-name :reader missed-xml-attribute-warning-node-name)
   (name :initarg :name :reader missed-xml-attribute-warning-name)))
  
(defun attribute-value (attribute node)
  (let ((name (attribute-name-string attribute)))
    (if (xml-attribute-p node name)
	(xml-attribute node name)
	(progn
	  (warn 'missed-xml-attribute-warning :node-name (xml-node-name node) :name name)
	  (attribute-default-value attribute)))))

(defun set-attribute (obj attribute node)
  (setf (slot-value obj (attribute-name attribute)) (attribute-value attribute node)))

(defun set-xml-attribute (obj attribute node)
  (setf (xml-attribute node (attribute-name-string attribute)) (slot-value obj (attribute-name attribute))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *attributes* (make-hash-table)))

(defmacro define-class-attributes (class &body attributes)
  (flet ((attribute-form (attribute)
	   `(make-attribute :name ',(first attribute) 
			    :name-string ,(second attribute) 
			    :default-value ,(third attribute))))
    `(setf (gethash ',class *attributes*) (list ,@(mapcar #'attribute-form attributes)))))

(define-class-attributes repository (version "version" *repository-api-version*))
(define-class-attributes unit (name "name"))
(define-class-attributes group (name "name"))

;;
;; Writing
;;

(defun attributes-to-xml (node obj class)
  (mapc #'(lambda (attr) (set-xml-attribute obj attr node)) (gethash class *attributes*)))

(defgeneric entity-to-xml (entity))

(defmethod entity-to-xml ((unit unit))
  (flet ((file-to-xml (file)
	   (make-xml-node "file" (list `("name" ,file)))))
    (let ((node (make-xml-node "unit" () (mapcar #'file-to-xml (unit-files unit)))))
      (attributes-to-xml node unit *unit-class*)
      node)))

(defmethod entity-to-xml ((group group))
  (let ((node (make-xml-node "group" () (mapcar #'entity-to-xml (entities group)))))
    (attributes-to-xml node group 'group)
    node))

(defun repository-to-xml (repo)
  (let ((node (make-xml-node "repository" () (mapcar #'entity-to-xml (entities repo)))))
    (attributes-to-xml node repo *repository-class*)
    node))

(defun print-repository (repo stream)
  (flet ((do-print (stream) 
	   (xml-print (repository-to-xml repo) stream)))
    (if stream
	(do-print stream)
	(with-output-to-string (stream)
	  (do-print stream)))))

;;
;; Reading
;;

(define-condition wrong-xml-node-name (btr-error)
  ((expected :initarg :expected :reader wrong-xml-node-name-expected)
   (got :initarg :got :reader wrong-xml-node-name-got)))

(define-error-string (err wrong-xml-node-name)
  (format nil "Wrong XML node name - ~a, expected ~a.~%" 
	  (wrong-xml-node-name-got err)
	  (wrong-xml-node-name-expected err)))

(defun check-node-name (name node)
  (unless (equal (xml-node-name node) name)
    (error 'wrong-xml-node-name :expected name :got (xml-node-name node)))
  t)

(defgeneric parse-entity-from-xml (type node))

(defmethod parse-entity-from-xml ((type (eql 'unit)) node)
  (flet ((file-from-xml (node)
	   (check-node-name "file" node)
	   (attribute-value (make-attribute :name-string "name") node)))
    (let ((unit (make-instance *unit-class* :files (mapcar #'file-from-xml (xml-childs node)))))
      unit)))

(defmethod parse-entity-from-xml ((type (eql 'group)) node)
  (let ((group (make-instance 'group :entities ())))
    (mapc #'(lambda (node) (add-entity (entity-from-xml node) group)) (xml-childs node))
    group))

(defun initialize-entity-attributes (obj type node)
  (mapc #'(lambda (attr) (set-attribute obj attr node)) (gethash type *attributes*)))

(defun entity-from-xml (node)
  (flet ((node-type ()
	   (cond ((equal (xml-node-name node) "unit") 'unit)
		 ((equal (xml-node-name node) "group") 'group)
		 (t (error 'wrong-xml-node-name :got (xml-node-name node) :expected '("unit" "group"))))))
    (let* ((type (node-type))
	   (obj (parse-entity-from-xml type node)))
      (initialize-entity-attributes obj (type-of obj) node)
      obj)))

(defun read-repository (stream)
  (let ((xml (parse-xml stream)))
    (check-node-name "repository" xml)
    (let ((repository (make-repository :entities (mapcar #'entity-from-xml (xml-childs xml)))))
      (initialize-entity-attributes repository *repository-class* xml)
      repository)))

;;
;; Extending default classes
;;

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun key-argument-value (name list)
    (cond
      ((null list) (values nil nil))
      ((eq (first list) name) (values (second list) t))
      (t (key-argument-value name (rest list)))))
  (defun remove-key-argument (name list)
    (cond 
      ((null list) nil)
      ((eq (first list) name) (remove-key-argument name (rest (rest list))))
      (t (cons (first list) (remove-key-argument name (rest list))))))
  (defun remove-key-arguments (list &rest names)
    (if (null names) list
	(apply #'remove-key-arguments (remove-key-argument (first names) list) (rest names)))))

(defmacro define-btr-class (name base-class &body slots)
  (labels ((name (slot)
	     (first slot))
	   (xml-name (slot)
	     (if (key-argument-value :xml-name slot) 
		 (key-argument-value :xml-name slot)  
		 (string-downcase (symbol-name (name slot)))))
	   (default-value (slot)
	     (key-argument-value :initform slot))
	   (make-attribute-definition (slot)
	     (list (name slot) (xml-name slot) (default-value slot))))
    (let ((class-slots (mapcar #'(lambda (slot) (remove-key-argument :xml-name slot)) slots)))
      `(progn
	 (defclass ,name (,base-class)
	   ,class-slots)
	 (define-class-attributes ,name ,@(mapcar #'make-attribute-definition slots))
	 (setf (gethash ',name *attributes*) (nconc (gethash ',name *attributes*) 
						    (gethash ',base-class *attributes*)))))))

(defgeneric method-name (name))

(defgeneric class-argument-position (name))
(defmethod class-argument-position (name) 0)

(defmacro define-repository-method (name outer-name (&rest args))
  `(progn (defgeneric ,name (,@args))
	  (defmethod ,name (,@args)
	    nil)
	  (defmethod method-name ((name (eql ,outer-name)))
	    ',name)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (define-repository-method initialize-repository :create (repository args))
  (define-repository-method initialize-unit :add (unit))
  (define-repository-method release-unit :remove (unit)))

(defmacro define-repository-function (class name (&rest args) &body body)
  (flet ((method-args (name args)
	   (let ((class-arg (class-argument-position name))
		 (args (copy-list args)))
	     (setf (nth class-arg args) `(,(nth class-arg args) ,class))
	     args)))
    `(defmethod ,(method-name name) ,(method-args name args)
       ,@body)))

(in-package #:burning-bsdf-variables)

;;
;; Aux structure macro's
;;

(defmacro defreader (type slot)
  `(defun ,(symbolicate type "-" slot) (,type)
     (,(symbolicate "%" type "-" slot) ,type)))

(defmacro defaccessor (type slot)
  (with-gensyms (val-sym)
    `(progn (defreader ,type ,slot)
	    (defun (setf ,(symbolicate type "-" slot)) (,val-sym ,type)
	      (setf (,(symbolicate "%" type "-" slot) ,type) ,val-sym)))))

(defmacro defaccessors (type slots read-only-slots)
  `(progn ,@(mapcar (lambda (slot) `(defaccessor ,type ,slot)) slots)
	  ,@(mapcar (lambda (slot) `(defreader ,type ,slot)) read-only-slots)))

;;
;; Type generics and macro's
;;

(defgeneric bsdf-type-p (type &rest args))

(defmethod bsdf-type-p (type &rest args)
  (declare (ignore type args))
  nil)

(defmacro def-type-p (type (args-var) &body body)
  `(defmethod bsdf-type-p ((type (eql ',type)) &rest ,args-var)
     ,@body))

(defgeneric bsdf-type-of (value))

(defmethod bsdf-type-of (value)
  (bsdf-compilation-error "Unknown BSDF type for ~a" value))

(defmacro def-type-of (type inner-type)
  `(defmethod bsdf-type-of ((value ,type))
     ,inner-type))

(defun cast-type (value dest-type &optional (type (bsdf-type-of value)))
  (cond ((eq dest-type t) value)
	((equal dest-type type) value)
	((eq type t) (cast-type value dest-type))
	(t (let ((type (if (listp type) (first type) type))
		 (type-args (if (listp type) (rest type)))
		 (dest-type (if (listp dest-type) (first dest-type) dest-type))
		 (dest-type-args (if (listp dest-type) (rest dest-type))))
	     (%cast-type value type dest-type type-args dest-type-args)))))

(defgeneric %cast-type (value type dest-type type-args dest-type-args))

(defmethod %cast-type (value type dest-type type-args dest-type-args)
  (flet ((type-name (type args)
	   (if args (cons type args) type)))
    (bsdf-compilation-error "Cannot convert ~a from type ~a to ~a" value 
			    (type-name type type-args)
			    (type-name dest-type dest-type-args))))

(defmethod %cast-type (value type (dest-type (eql t)) type-args dest-type-args)
  (declare (ignore type type-args dest-type-args))
  value)

(defmacro defcast (type-and-args dest-type-and-args (value) &body body)
  (with-gensyms (type-sym dest-sym)
    (let ((type (if (listp type-and-args) (first type-and-args) type-and-args))
	  (type-args (if (listp type-and-args) (second type-and-args) (gensym)))
	  (dest-type (if (listp dest-type-and-args) (first dest-type-and-args) dest-type-and-args))
	  (dest-type-args (if (listp dest-type-and-args) (second dest-type-and-args) (gensym))))
      `(defmethod %cast-type (,value (,type-sym (eql ,type)) (,dest-sym (eql ,dest-type)) ,type-args ,dest-type-args)
	 (declare (ignorable ,type-args ,dest-type-args))
	 ,@body))))

;;
;; Types
;;

(defmethod bsdf-type-of ((value list))
  (let ((types (mapcar #'bsdf-type-of value)))
    (if (every (lambda (type) (equal type (first types))) (rest types))
	`(:list ,(first types))
	:list)))

(defmethod bsdf-type-of ((value (eql t)))
  :bool)

(def-type-p t (args)
  (null args))

(def-type-of string :string)
(def-type-of integer :int)
(def-type-of burning-filesystem:path :path)
(def-type-of keyword :enum)

(def-type-p :string (args)
  (null args))

(defcast :int :string (value)
  (format nil "~d" value))

(def-type-p :int (args)
  (< (length args) 3))

(defcast :path :string (value)
  (path-to-string value))

(defcast :bool :string (value)
  (if value "t" "nil"))

(defcast :enum :string (value)
  (symbol-name value))

(def-type-p :list (args)
  (and (<= (length args) 1)
       (or (not args)
	   (apply #'bsdf-type-p (if (listp (first args)) (first args) (list (first args)))))))

(defcast (:list args) (:list dest-args) (value)
  (if dest-args
      (restart-case 
	  (handler-bind ((bsdf-compilation-error (lambda (err) (invoke-restart 'list-error err))))
	    (mapcar (lambda (arg) (cast-type arg (first dest-args))) value))
	(list-error (err) (declare (ignore err)) (call-next-method)))
      value))

(defcast (:list args) :string (value)
  (format nil "(~{~a~^ ~})" value))

(defcast :string :int (value)
  (multiple-value-bind (int pos) (parse-integer value :junk-allowed t)
    (if (= pos (length value)) int (call-next-method))))

(defcast (:int args) (:int dest-args) (value)
  (declare (ignore args))
  (if (and dest-args
	   (dbind (max &optional (min 0)) (reverse dest-args)
	     (or (< value min) (> value max))))
      (call-next-method)
      value))

(defcast :list :bool (value)
  (if (null value) nil t))

(defcast :path :file (value)
  (if (file-path-p value) value (call-next-method)))

(defcast :path :directory (value)
  (if (directory-path-p value) value (call-next-method)))

(defcast :string :path (value)
  (path-from-string value))

(defcast :string :file (value)
  (cast-type (cast-type value :path :string) :file :path))

(defcast :string :directory (value)
  (cast-type (cast-type value :path :string) :directory :path))

(def-type-p :path (args)
  (null args))

(def-type-p :file (args)
  (apply #'bsdf-type-p :path args))

(def-type-p :directory (args)
  (apply #'bsdf-type-p :path args))

(def-type-p :bool (args)
  (null args))

(defcast :enum (:enum dest-args) (value)
  (if (or (not dest-args) (member value dest-args)) value
      (call-next-method)))

(def-type-p :enum (args)
  (every (lambda (x) (and (symbolp x) (eq (symbol-package x) (find-package "KEYWORD"))))
	 args))

;;
;; Variables
;;

(defstruct (variable (:constructor %make-variable) (:conc-name %variable-))
  name expression type description visible-p)

(defaccessors variable () (name expression type description visible-p))

(defun make-variable (name expression &key (type t) (description "") (visible-p nil))
  (handler-bind ((bsdf-compilation-error (lambda (err) 
					   (setf (bsdf-condition-format-control err)
						 (lines* "In definition of variable '~a':"
							 (bsdf-condition-format-control err)))
					   (setf (bsdf-condition-format-args err)
						 (cons name (bsdf-condition-format-args err))))))
    (unless (apply #'bsdf-type-p (if (listp type) type (list type)))
      (bsdf-compilation-error "Wrong BSDF type ~a" type))
    (let ((real-type (bsdf-type-of expression)))
      (unless (equal real-type type) 
	(setf expression (cast-type expression type real-type))))
    (%make-variable :name name
		    :type type
		    :expression expression
		    :description description
		    :visible-p visible-p)))

(defun variable-value (var) (variable-expression var))
(defun variable-string (var) (cast-type (variable-value var) :string))


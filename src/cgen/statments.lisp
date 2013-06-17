(in-package #:burning-cgen)

;;
;; Type table
;;



(defstruct (type-table (:copier nil))
  (table (make-hash-table :test #'equal))
  (parent nil))

(defvar *type-table* (make-type-table))

(defun symbol-type (symbol &optional (table *type-table*))
  (flet ((get1 (table)
	   (gethash (cons (cgen-symbol-symbol symbol) (cgen-symbol-namespace symbol)) (type-table-table table))))
    (or (get1 table)
	(aif (type-table-parent table) (symbol-type symbol it)))))

(defun (setf symbol-type) (value symbol &optional (table *type-table*))
  (setf (gethash (cons (cgen-symbol-symbol symbol) (cgen-symbol-namespace symbol)) (type-table-table table))
	value))

(defun copy-type-table ()
  (let ((table *type-table*))
    (make-type-table :table (make-hash-table :test #'equal)
		     :parent table)))

;;
;; Symbols
;;

(defstruct (cgen-symbol (:constructor make-cgen-symbol (symbol namespace)))
  symbol
  namespace)

(defun generate-symbol (symbol)
  (let ((name (symbol-name (cgen-symbol-symbol symbol))))
    (search-and-replace-all (string-downcase name) "-" "_")))

;;
;; Statments
;;

(defun generate-statments (list)
  (flet ((need-newline ()
	   (and list (rest list)
		(or (not (eq (type-of (first list)) (type-of (second list))))
		    (typep (first list) 'defun-statment)
		    (typep (second list) 'defun-statment)))))
    (if list
	(format nil "~a~a~a" 
		(generate-statment (first list))
		(if (need-newline) #\Newline "")
		(generate-statments (rest list)))
	"")))

(defgeneric generate-statment (statment))

;;
;; Expressions
;;

(defclass expression () ())

(defgeneric expression-type (expr))

;;
;; Boolean
;;

(defmethod expression-type ((value (eql nil)))
  'bool)

(defmethod generate-statment ((value (eql nil)))
  "false")

(defmethod expression-type ((value (eql t)))
  'bool)

(defmethod generate-statment ((value (eql t)))
  "true")

;;
;; Numbers
;;

(defmethod generate-statment ((value number))
  (format nil "~a" value))

(defmethod expression-type ((value integer))
  'int)

(defmethod expression-type ((value float))
  'float)

;;
;; Cast
;;

(defclass cast-expression (expression)
  ((expr :initarg :expr)
   (type :initarg :type)))

(defmethod expression-type ((expr cast-expression))
  (slot-value expr 'type))

(defmethod generate-statment ((expr cast-expression))
  (with-slots (expr type) expr
    (format nil "type_cast<~a>( ~a )" (generate-symbol (make-cgen-symbol type :type)) (generate-statment expr))))

;;
;; Defun statment
;;

(defclass defun-statment ()
  ((name :initarg :name)
   (arglist :initarg :arglist)
   (body :initarg :body)))

(defmethod generate-statment ((statment defun-statment))
  (labels ((generate-argument (arg)
	     (format nil "~a ~a" 
		     (generate-symbol (make-cgen-symbol (second arg) :type))
		     (generate-symbol (make-cgen-symbol (first arg) :variable))))
	   (generate-arglist (args)
	     (format nil "~{~a~^, ~}" (mapcar #'generate-argument args))))
    (with-slots (name arglist body) statment
      (format nil "~a (~a)~%~a~%" (generate-symbol name) (generate-arglist arglist) (generate-statment body)))))

;;
;; Funcall expression
;;

(defclass funcall-expression ()
  ((name :initarg :name)
   (args-list :initarg :args-list)))

(defmethod generate-statment ((expr funcall-expression))
  (with-slots (name args-list) expr
    (format nil "~a(~{~a~^, ~})" (generate-symbol name) (mapcar #'generate-statment args-list))))

;;
;; Variable expression
;;

(defclass variable-expression ()
  ((name :initarg :name)))

(defmethod expression-type ((expr variable-expression))
  (symbol-type (make-cgen-symbol (slot-value expr 'name) :variable)))

(defmethod generate-statment ((expr variable-expression))
  (generate-symbol (make-cgen-symbol (slot-value expr 'name) :variable)))
  

;;
;; Defvar statment
;;

(defclass defvar-statment ()
  ((name :initarg :name)
   (value :initarg :value)
   (type :initarg :type)))

(defmethod initialize-instance :after ((st defvar-statment) &key &allow-other-keys)
  (with-slots (name type) st
    (setf (symbol-type name) type)))

(defmethod generate-statment ((statment defvar-statment))
  (with-slots (name value type) statment
    (format nil "~a ~a = ~a~%" 
	    (generate-symbol (make-cgen-symbol type :type))
	    (generate-symbol name)
	    (generate-statment value))))

;;
;; Setf statment
;;

(defclass setf-statment ()
  ((place :initarg :place)
   (value :initarg :value)))

(defmethod generate-statment ((statment setf-statment))
  (with-slots (place value) statment
      (format nil "~a = ~a" (generate-statment place) (generate-statment value))))

;;
;; If statment
;;

(defclass if-statment ()
  ((expr :initarg :expr)
   (then-form :initarg :then-form)
   (else-form :initarg :else-form)
   (else-form-p :initarg :else-form-p)))

(defmethod generate-statment ((statment if-statment))
  (with-slots (expr then-form else-form else-form-p) statment
    (format nil "if( ~a )~%~a~a" 
	    (generate-statment expr)
	    (generate-statment then-form)
	    (if else-form-p (lines* "" "else" (generate-statment else-form)) ""))))

;;
;; Block statment
;;

(defclass block ()
  ((type-table :initarg :type-table)
   (forms :initarg :forms)))

(defmethod generate-statment ((statment block))
  (with-slots (forms type-table) statment
    (let ((*type-table* type-table))
      (flet ((generate-line (line)
	       (search-and-replace-all (format nil "  ~a" (generate-statment line)) 
				       (format nil "~%")
				       (format nil "~%  "))))
	(format nil "{~a~{~a~^~%~}~%}" 
		(if forms #\Newline "")
		(mapcar #'generate-line forms))))))

;;
;; Ariphmetic
;;

(defclass ariphmetic-expression ()
  ((num :initarg :num)
   (nums :initarg :nums)))

(defun generate-ariphmetic-statment (name expr)
  (let ((format-string (format nil "~~a~~{ ~a ~~a~~}" name)))
    (format nil format-string
	    (generate-statment (slot-value expr 'num))
	    (mapcar #'generate-statment (slot-value expr 'nums)))))

(defmethod expression-type ((expr ariphmetic-expression))
  (with-slots (num nums) expr
    (let ((types (cons (expression-type num) (mapcar #'expression-type nums))))
      (if (some (lambda (x) (eq x 'float)) types) 'float 'int))))

(defclass +-expression (ariphmetic-expression) ())

(defmethod generate-statment ((statment +-expression))
  (generate-ariphmetic-statment "+" statment))

(defclass --expression (ariphmetic-expression) ())

(defmethod generate-statment ((statment --expression))
  (generate-ariphmetic-statment "-" statment))

(defclass *-expression (ariphmetic-expression) ())

(defmethod generate-statment ((statment *-expression))
  (generate-ariphmetic-statment "*" statment))

(defclass /-expression (ariphmetic-expression) ())

(defmethod generate-statment ((statment /-expression))
  (with-slots (num nums) statment
    (if (eq (expression-type num) 'float)
	(generate-ariphmetic-statment "/" statment)
	(generate-ariphmetic-statment "/" (make-instance 'ariphmetic-expression 
							 :num (make-instance 'cast-expression
									     :expr num
									     :type 'float)
							 :nums nums)))))

(defmethod expression-type ((expr /-expression))
  'float)
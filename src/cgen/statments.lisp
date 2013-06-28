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

(defun check-symbol (symbol)
  (labels ((check-commons (symbol)
	     (let ((name (symbol-name symbol)))
	       (flet ((check-first-char (char)
			(when (and (char>= char #\0) (char<= char #\9))
			  (error "Symbol ~a starts with digit ~a" symbol char)))
		      (check-char (char)
			(unless (or (and (char>= char #\a) (char<= char #\z))
				    (and (char>= char #\A) (char<= char #\Z))
				    (and (char>= char #\0) (char<= char #\9))
				    (member char '(#\- #\* #\% #\$ #\! #\?) :test #'char=))
			  (error "Symbol ~a contains wrong character ~a" symbol char))))
		 (check-first-char (char name 0))
		 (map 'list #'check-char name))))
	   (check-function (symbol)
	     (when (symbol-type symbol)
	       (error "Function with name ~a already defined." (cgen-symbol-symbol symbol)))
	     (when (symbol-type (make-cgen-symbol (cgen-symbol-symbol symbol) :variable))
	       (error "~a already a variable's name." symbol))))
    (check-commons (cgen-symbol-symbol symbol))	   
    (ecase (cgen-symbol-namespace symbol)
      (:function (check-function symbol)))
    t))

;;
;; Statments
;;

(defun generate-statments (list)
  (flet ((need-newline ()
	   (and list (rest list)
		(or (not (eq (type-of (first list)) (type-of (second list))))
		    (typep (first list) 'defun)
		    (typep (second list) 'defun)))))
    (if list
	(format nil "~a~a~a" 
		(generate-statment (first list))
		(if (need-newline) #\Newline "")
		(generate-statments (rest list)))
	"")))

(defgeneric generate-statment (statment))

(defgeneric return-type (statment))

(defmethod return-type (statment)
  (expression-type statment))

;;
;; Return
;;

(defclass return ()
  ((value :initarg :value)))

(defmethod generate-statment ((statment return))
  (if (slot-boundp statment 'value)
      (format nil "return ~a" (generate-statment (slot-value statment 'value)))
      "return"))

(defgeneric make-return-form (form))

(defmethod make-return-form (form)
  (make-instance 'return :value form))

(defmethod return-type ((obj return))
  (if (slot-boundp obj 'value) 
      (return-type (slot-value obj 'value))
      'void))

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

(defclass cast (expression)
  ((expr :initarg :expr)
   (type :initarg :type)))

(defmethod expression-type ((expr cast))
  (slot-value expr 'type))

(defmethod generate-statment ((expr cast))
  (with-slots (expr type) expr
    (format nil "type_cast<~a>( ~a )" (generate-symbol (make-cgen-symbol type :type)) (generate-statment expr))))

;;
;; Defun statment
;;

(defclass defun ()
  ((name :initarg :name)
   (arglist :initarg :arglist)
   (body :initarg :body)))

(defmethod initialize-instance :after ((obj defun) &key &allow-other-keys)
  (with-slots (name body) obj
    (check-symbol name)
    (setf (symbol-type name) (return-type body))
    (setf body (make-return-form body))))

(defmethod generate-statment ((statment defun))
  (with-slots (name arglist body) statment
    (labels ((generate-argument (arg)
	       (format nil "~a ~a" 
		       (generate-symbol (make-cgen-symbol (second arg) :type))
		       (generate-symbol (make-cgen-symbol (first arg) :variable))))
	     (generate-arglist (args)
	       (format nil "~{~a~^, ~}" (mapcar #'generate-argument args))))
      (format nil "~a ~a (~a)~%~a~%" 
	      (generate-symbol (make-cgen-symbol (return-type body) :type))
	      (generate-symbol name)
	      (generate-arglist arglist)
	      (generate-statment body)))))

;;
;; Funcall expression
;;

(defclass funcall (expression)
  ((name :initarg :name)
   (args-list :initarg :args-list)))

(defmethod generate-statment ((expr funcall))
  (with-slots (name args-list) expr
    (format nil "~a(~{~a~^, ~})" (generate-symbol name) (mapcar #'generate-statment args-list))))

(defmethod expression-type ((expr funcall))
  (symbol-type (slot-value expr 'name)))

;;
;; Variable expression
;;

(defclass variable (expression)
  ((name :initarg :name)))

(defmethod expression-type ((expr variable))
  (symbol-type (make-cgen-symbol (slot-value expr 'name) :variable)))

(defmethod generate-statment ((expr variable))
  (generate-symbol (make-cgen-symbol (slot-value expr 'name) :variable)))
  

;;
;; Defvar statment
;;

(defclass defvar ()
  ((name :initarg :name)
   (value :initarg :value)
   (type :initarg :type)))

(defmethod initialize-instance :after ((st defvar) &key &allow-other-keys)
  (with-slots (name type) st
    (setf (symbol-type name) type)))

(defmethod generate-statment ((statment defvar))
  (with-slots (name value type) statment
    (format nil "~a ~a = ~a~%" 
	    (generate-symbol (make-cgen-symbol type :type))
	    (generate-symbol name)
	    (generate-statment value))))

;;
;; Setf statment
;;

(defclass setf ()
  ((place :initarg :place)
   (value :initarg :value)))

(defmethod generate-statment ((statment setf))
  (with-slots (place value) statment
      (format nil "~a = ~a" (generate-statment place) (generate-statment value))))

;;
;; Def-local-var statment
;;

(defclass def-local-var ()
  ((name :initarg :name)
   (value :initarg :value)
   (type :initarg :type)))

(defmethod generate-statment ((statment def-local-var))
  (with-slots (name value type) statment
    (format nil "~a ~a = ~a" 
	    (generate-symbol (make-cgen-symbol (aif type it (expression-type value)) :type))
	    (generate-statment name)
	    (generate-statment value))))

;;
;; If statment
;;

(defclass if ()
  ((expr :initarg :expr)
   (then-form :initarg :then-form)
   (else-form :initarg :else-form)
   (else-form-p :initarg :else-form-p)))

(defmethod return-type ((statment if))
  (with-slots (expr then-form else-form else-form-p) statment
      (let ((type1 (return-type then-form))
	    (type2 (if else-form-p (return-type else-form))))
	(if type2 type1 'void))))

(defmethod generate-statment ((statment if))
  (with-slots (expr then-form else-form else-form-p) statment
    (format nil "if( ~a )~%~a~a" 
	    (generate-statment expr)
	    (generate-statment then-form)
	    (if else-form-p (lines* "" "else" (generate-statment else-form)) ""))))

(defmethod make-return-form ((form if))
  (with-slots (then-form else-form) form
    (when else-form
      (setf then-form (make-return-form then-form))
      (setf else-form (make-return-form else-form))))
  form)
	

;;
;; Block statment
;;

(defclass block ()
  ((type-table :initarg :type-table)
   (forms :initarg :forms)))

(defmethod generate-statment ((statment block))
  (with-slots (forms type-table) statment
    (let ((*type-table* type-table))
      (flet ((generate-line (value)
	       (let ((statment (generate-statment value)))
		 (search-and-replace-all (format nil "  ~a" statment)
					 (format nil "~%")
					 (format nil "~%  ")))))
	(format nil "{~a~{~a~^~%~}~%}" 
		(if forms #\Newline "")
		(mapcar #'generate-line forms))))))

(defmethod return-type ((statment block))
  (let ((*type-table* (slot-value statment 'type-table))
	(forms (slot-value statment 'forms)))
    (if forms (return-type (first (last forms))) 'void)))

(defmethod make-return-form ((form block))
  (with-slots (type-table forms) form
    (make-instance 'block 
		   :type-table type-table
		   :forms (if forms (append (butlast forms) (list (make-return-form (first (last forms)))))))))

;;
;; Let statment
;;

(defclass let ()
  ((body :initarg :body)))

(defmethod initialize-instance :after ((obj let) &key args values types body &allow-other-keys)
  (flet ((make-setup (arg value type)
	   (make-instance 'def-local-var :name arg :value value :type type)))
    (setf (slot-value obj 'body)
	  (apply #'make-block (append (mapcar #'make-setup args values types) body)))))

(defmethod generate-statment ((statment let))
  (generate-statment (slot-value statment 'body)))

(defmethod return-type ((statment let))
  (return-type (slot-value statment 'body)))

(defmethod make-return-form ((form let))
  (setf (slot-value form 'body) (make-return-form (slot-value form 'body))))

;;
;; Ariphmetic
;;

(defclass ariphmetic-expression (expression)
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

(defclass + (ariphmetic-expression) ())

(defmethod generate-statment ((statment +))
  (generate-ariphmetic-statment "+" statment))

(defclass - (ariphmetic-expression) ())

(defmethod generate-statment ((statment -))
  (generate-ariphmetic-statment "-" statment))

(defclass * (ariphmetic-expression) ())

(defmethod generate-statment ((statment *))
  (generate-ariphmetic-statment "*" statment))

(defclass / (ariphmetic-expression) ())

(defmethod generate-statment ((statment /))
  (with-slots (num nums) statment
    (if (eq (expression-type num) 'float)
	(generate-ariphmetic-statment "/" statment)
	(generate-ariphmetic-statment "/" (make-instance 'ariphmetic-expression 
							 :num (make-instance 'cast
									     :expr num
									     :type 'float)
							 :nums nums)))))

(defmethod expression-type ((expr /))
  'float)


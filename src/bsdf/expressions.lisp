(in-package #:bsdf-expressions)

;;
;; Operation generic and macro's
;;

(defvar *operations* (make-hash-table :test #'eq))

(defun bsdf-operation-p (symbol-or-expr)
  (let ((symbol (if (listp symbol-or-expr) (first symbol-or-expr) symbol-or-expr)))
    (and (symbolp symbol) (not (typep symbol 'boolean)) 
	 (not (eq (symbol-package symbol) (find-package "KEYWORD"))))))

(defun set-operation (name)
  (setf (gethash name *operations*) t))

(defgeneric bsdf-operation-expansion (operation args))
(defmethod bsdf-operation-expansion (operation args)
  (values (cons operation (mapcar #'expand-expression args)) t))

(defun expand-expression (expr)
  (if (and expr (listp expr))
      (multiple-value-bind (value last-p) (bsdf-operation-expansion (first expr) (rest expr))
	(if last-p value
	    (expand-expression value)))
      expr))

(defgeneric bsdf-operation-type (operation args))

(defmethod bsdf-operation-type (operation args)
  (declare (ignore args))
  (bsdf-compilation-error "Wrong BSDF operation ~a" operation))

(defgeneric bsdf-operation-value (operation args))

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
	((bsdf-operation-p value) `(cast ,value ,dest-type))
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
  (let ((type-sym (gensym)) (dest-sym (gensym)))
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
  (if (bsdf-operation-p (first value)) (bsdf-operation-type (first value) (rest value))
      (let ((types (mapcar #'bsdf-type-of value)))
	(if (every (lambda (type) (equal type (first types))) (rest types))
	    `(:list ,(first types))
	    :list))))

(defmethod bsdf-type-of ((value (eql nil)))
  :list)

(defmethod bsdf-type-of ((value (eql t)))
  :bool)

(def-type-p t (args)
  (null args))

(def-type-of string :string)
(def-type-of integer :int)
(def-type-of burning-filesystem:path :path)
(def-type-of symbol :enum)

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
	   (dbind (min &optional (max '*)) dest-args
	     (or (and (not (eq min '*)) (< value min)) 
		 (and (not (eq max '*)) (> value max)))))
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
;; Expressions
;;

(defun expression-value (expr)
  (if (listp expr)
      (if (bsdf-operation-p expr)
	  (bsdf-operation-value (first expr) (mapcar #'expression-value (rest expr)))
	  (mapcar #'expression-value expr))
      expr))

(defun expression-string (expr)
  (cast-type (expression-value expr) :string))

(defun expression-type (expr)
  (bsdf-type-of expr))

;;
;; Operations
;;

(defun try-bind (lambda-list args)
  (handler-case (bind-lambda-list lambda-list args)
    (simple-error (err)
      (error 'bsdf-compilation-error
	     :format-control (simple-condition-format-control err)
	     :format-arguments (simple-condition-format-arguments err)))))

(defmacro defoperation (name args type-specs &body body)
  (let ((args-sym (gensym)))
    (labels ((var-handler (var)
	       `(lambda (err)
		  (setf (bsdf-condition-format-control err)
			(lines* (format nil "In argument '~a' of '~a':" ',var ',name)
				(bsdf-condition-format-control err)))
		  err))
	     (cast-expr (spec)
	       (dbind (var type) spec
		 `(,var (handler-bind ((bsdf-compilation-error ,(var-handler var)))
			  (cast-type ,var ',type))))))
      `(progn (set-operation ',name)
	      (defmethod bsdf-operation-type ((operation (eql ',name)) ,args-sym)
		(try-bind ',args ,args-sym)
		(dbind ,args ,args-sym
		  (declare (ignorable ,@(lambda-list-arguments args)))
		  ,(first type-specs)))
	      (defmethod bsdf-operation-value ((operation (eql ',name)) ,args-sym)
		(try-bind ',args ,args-sym)
		(dbind ,args ,args-sym
		  (let (,@(mapcar #'cast-expr (rest type-specs)))
		    (cast-type (progn ,@body) (bsdf-operation-type ',name ,args-sym)))))))))

(defmacro defoperation-macro (name args &body body)
  (let ((args-sym (gensym)))
    `(progn (set-operation ',name)
	    (defmethod bsdf-operation-expansion ((operation (eql ',name)) ,args-sym)
	      (try-bind ',args ,args-sym)
	      (dbind ,args ,args-sym
		,@body)))))

(defoperation cast (value type)
    (type)
  (cast-type value type))

(defoperation quote (item)
    (t)
  item)

(defoperation ++ (&rest args)
    (:string (args (:list :string)))
  (apply #'concatenate 'string args))

(defoperation substring (string first &optional (last (length string)))
    (:string (string :string) (first :int) (last :int))
  (when (< first 0) (setf first (+ (length string) first)))
  (when (< last 0) (setf last (+ (length string) last 1)))
  (when (or (> last (length string)) (> first last))
    (bsdf-compilation-error "Bad interval [~a, ~a) for string '~a'" first last string))
  (subseq string first last))

;;
;; Int type
;;

(defoperation + (&rest args)
    (:int (args (:list :int)))
  (apply #'+ args))

(defoperation - (number &rest numbers)
    (:int (number :int) (numbers (:list :int)))
  (apply #'- number numbers))

(defoperation * (&rest args)
    (:int (args (:list :int)))
  (apply #'* args))

(defoperation / (number &rest numbers)
    (:int (number :int) (numbers (:list :int)))
  (when (some (lambda (x) (= x 0)) numbers)
    (bsdf-compilation-error "Division by zero in (/ ~a ~{~a~^ ~})" number numbers))
  (floor (apply #'/ number numbers)))

(defoperation mod (number divisor)
    (:int (number :int) (divisor :int))
  (when (= 0 divisor)
    (bsdf-compilation-error "Division by zero in (mod ~a ~a)" number divisor))
  (mod number divisor))

;;
;; List type
;; 

(defoperation cons (item list)
    (:list (list :list))
  (cons item list))

(defoperation append (&rest lists)
    (:list)
  (apply #'append 
	 (mapcar (lambda (x) (if (listp x) x (list x))) lists)))

(defmacro def-nth (n)
  (let ((name (intern (string-upcase (format nil "~:r" n)))))
    `(defoperation ,name (list)
	 (t (list :list))
       (,name list))))

(defmacro def-nths (max)
  `(progn ,@(do ((n 1 (1+ n))
		 (specs ()))
		((> n max) (reverse specs))
		(push `(def-nth ,n) specs))))

(def-nths 10)

(defoperation nth (index list)
    (t (index (:int 0)) (list :list))
  (nth index list))

(defoperation remove (item list)
    (:list (list :list))
  (remove item list :test #'equal))

(defoperation remove-duplicates (list)
    (:list (list :list))
  (labels ((do-remove (list)
	     (if (null list) nil
		 (cons (first list)
		       (do-remove (remove (first list) (rest list) :test #'equal))))))
    (do-remove list)))

;;
;; Path type
;;

(defoperation parent-path (path)
    (:path (path :path))
  (parent-path path))

(defoperation directory-path (path)
    (:path (path :path))
  (copy-path path :new-filename nil))

(defoperation root-path (path)
    (:path (path :path))
  (root-path path))

(defoperation path+ (&rest paths)
    (:path (paths (:list :path)))
  (apply #'path+ paths))

(defoperation as-absolute (path)
    (:path (path :path))
  (as-absolute-path path))

(defoperation as-relative (path base-path)
    (:path (path :path) (base-path :path))
  (as-relative-path path base-path))

(defoperation copy-path (path 
			 &key (new-name (path-name path) new-name-p) (new-type (path-type path) new-type-p))
    (:path (path :path) (new-name :string) (new-type :string))
  (apply #'copy-path path
	 (append (when new-name-p (list :new-name new-name))
		 (when new-type-p (list :new-type new-type)))))
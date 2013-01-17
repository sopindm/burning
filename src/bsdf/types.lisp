(in-package #:bsdf-expressions)

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
  (unless (apply #'bsdf-type-p (if (listp type) type (list type)))
    (bsdf-compilation-error "Wrong BSDF type ~a" type))
  (unless (apply #'bsdf-type-p (if (listp dest-type) dest-type (list dest-type)))
    (bsdf-compilation-error "Wrong BSDF type ~a" dest-type))
  (cond ((eq dest-type t) value)
	((equal dest-type type) value)
	((or (bsdf-function-p value) (bsdf-variable-p value)) `(cast ,value ,dest-type))
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
  (if (bsdf-function-p value) (bsdf-function-type (first value) (rest value))
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

(def-type-p :string (args)
  (null args))

(defcast :int :string (value)
  (format nil "~d" value))

(def-type-p :int (args)
  (< (length args) 3))

(defcast :path :string (value)
  (path-to-string value))

(defcast :file :string (value)
  (cast-type (cast-type value :path :file) :string))

(defcast :directory :string (value)
  (cast-type (cast-type value :path :directory) :string))

(defcast :bool :string (value)
  (if value "t" "nil"))

(defcast :string :bool (value)
  (cond ((string= value "t") t)
	((string= value "nil") nil)
	(t (call-next-method))))

(defcast :enum :string (value)
  (symbol-name value))

(defcast :string :enum (value)
  (intern value (find-package "KEYWORD")))

(def-type-p :list (args)
  (and (<= (length args) 1)
       (or (not args)
	   (apply #'bsdf-type-p (if (listp (first args)) (first args) (list (first args)))))))

(defcast (:list args) (:list dest-args) (value)
  (if dest-args
      (handler-case (mapcar (lambda (arg) (cast-type arg (first dest-args))) value)
	(bsdf-compilation-error () (call-next-method)))
      value))

(defcast (:list args) :string (value)
  (format nil "(~{~a~^;~})" (mapcar (lambda (val) (cast-type val :string)) value)))

(defcast :string (:list args) (value)
  (labels ((unescaped-p (string pos)
	     (or (= pos 0)
		 (char/= (char string (1- pos)) #\\)
		 (not (unescaped-p string (1- pos)))))
	   (unescaped-position (char string &key (start 0))
	     (aif (position char string :start start)
		  (if (unescaped-p string it)
		      it
		      (unescaped-position char string :start (1+ it)))))
	   (do-cast (string &optional (position 0))
	     (aif (unescaped-position #\; string :start position)
		  (let ((ob-pos (or (unescaped-position #\( string) (length string)))
			(cb-pos (or (unescaped-position #\) string) (length string))))
		    (if (or (< it ob-pos) (> it cb-pos))
			(cons (subseq string 0 it) (do-cast (subseq string (1+ it))))
			(do-cast string (1+ cb-pos))))
		  (list string))))
    (setf value (string-trim '(#\Space #\Tab #\Newline) value))
    (unless (char= (char value 0) #\()
      (call-next-method))
    (unless (char= (char value (1- (length value))) #\))
      (call-next-method))
    (cast-type (do-cast (subseq value 1 (1- (length value)))) `(:list ,@args))))

(defcast :string (:int args) (value)
  (multiple-value-bind (int pos) (parse-integer value :junk-allowed t)
    (if (= pos (length value)) 
	(handler-case (cast-type int `(:int ,@args))
	  (bsdf-compilation-error () (call-next-method)))
	(call-next-method))))

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

(defcast :file :path (value)
  value)

(defcast :directory :path (value)
  value)

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

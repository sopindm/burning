(in-package #:bsdf-expressions)

;;
;; Functions generic and macro's
;;

(defvar *functions* (make-hash-table :test #'eq))

(defun bsdf-function-p (expr)
  (aif (and (listp expr) (first expr))
       (and (symbolp it) (not (typep it 'boolean))
	    (not (eq (symbol-package it) (find-package "KEYWORD"))))))

(defun set-function (name)
  (setf (gethash name *functions*) t))

(defgeneric bsdf-macroexpand (name args))
(defmethod bsdf-macroexpand (name args)  
  (values (cons name (mapcar #'expand-expression args)) t))

(defun expand-expression (expr)
  (if (and expr (listp expr))
      (multiple-value-bind (value last-p) (bsdf-macroexpand (first expr) (rest expr))
	(if last-p 
	    value
	    (expand-expression value)))
      expr))

(defgeneric bsdf-function-type (func args))

(defmethod bsdf-function-type (func args)
  (declare (ignore args))
  (bsdf-compilation-error "Wrong BSDF function ~a" func))

(defgeneric bsdf-function-value (func args))
(defgeneric bsdf-atom-value (atom))
(defmethod bsdf-atom-value (atom) atom)

(defgeneric bsdf-function-dependencies (func args))
(defmethod bsdf-function-dependencies (func args)
  (declare (ignore func))
  (list-dependencies args))

(defgeneric bsdf-atom-dependencies (atom))
(defmethod bsdf-atom-dependencies (atom)
  (declare (ignore atom))
  (list () () ()))

;;
;; Expressions
;;

(defun expression-value (expr)
  (if (listp expr)
      (if (bsdf-function-p expr)
	  (bsdf-function-value (first expr) (mapcar #'expression-value (rest expr)))
	  (mapcar #'expression-value expr))
      (bsdf-atom-value expr)))

(defun expression-type (expr)
  (bsdf-type-of expr))

(defun check-expression (expr)
  (and (expression-type expr) (expression-value expr) t))

(defun list-dependencies (list)
  (let ((mapped (mapcar #'expression-dependencies list)))
    (list (remove-duplicates (mapcan #'first mapped) :test #'bsdf=)
	  (remove-duplicates (mapcan #'second mapped) :test #'bsdf=)
	  (remove-duplicates (mapcan #'third mapped) :test #'bsdf=))))

(defun expression-dependencies (expr)
  (if (listp expr)
      (if (bsdf-function-p expr)
	  (bsdf-function-dependencies (first expr) (rest expr))
	  (list-dependencies expr))
      (bsdf-atom-dependencies expr)))

;;
;; Functions
;;

(defun try-bind (lambda-list args)
  (handler-case (bind-lambda-list lambda-list args)
    (simple-error (err)
      (error 'bsdf-compilation-error
	     :format-control (simple-condition-format-control err)
	     :format-arguments (simple-condition-format-arguments err)))))

(defmacro bsdf-defun (name args type-specs &body body)
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
      `(progn (set-function ',name)
	      (defmethod bsdf-function-type ((function (eql ',name)) ,args-sym)
		(try-bind ',args ,args-sym)
		(dbind ,args ,args-sym
		  (declare (ignorable ,@(lambda-list-arguments args)))
		  ,(first type-specs)))
	      (defmethod bsdf-function-value ((function (eql ',name)) ,args-sym)
		(try-bind ',args ,args-sym)
		(dbind ,args ,args-sym
		  (let (,@(mapcar #'cast-expr (rest type-specs)))
		    (cast-type (progn ,@body) (bsdf-function-type ',name ,args-sym)))))))))

(defmacro bsdf-defmacro (name args &body body)
  (let ((args-sym (gensym)))
    `(progn (set-function ',name)
	    (defmethod bsdf-macroexpand ((function (eql ',name)) ,args-sym)
	      (try-bind ',args ,args-sym)
	      (dbind ,args ,args-sym
		,@body)))))

(bsdf-defun cast (value type)
    (type)
  (cast-type value type))

(bsdf-defun quote (item)
    (t)
  item)

(bsdf-defun ++ (&rest args)
    (:string (args (:list :string)))
  (apply #'concatenate 'string args))

(bsdf-defun substring (string first &optional (last (length string)))
    (:string (string :string) (first :int) (last :int))
  (when (< first 0) (setf first (+ (length string) first)))
  (when (< last 0) (setf last (+ (length string) last 1)))
  (when (or (> last (length string)) (> first last))
    (bsdf-compilation-error "Bad interval [~a, ~a) for string '~a'" first last string))
  (subseq string first last))

;;
;; Int type
;;

(bsdf-defun + (&rest args)
    (:int (args (:list :int)))
  (apply #'+ args))

(bsdf-defun - (number &rest numbers)
    (:int (number :int) (numbers (:list :int)))
  (apply #'- number numbers))

(bsdf-defun * (&rest args)
    (:int (args (:list :int)))
  (apply #'* args))

(bsdf-defun / (number &rest numbers)
    (:int (number :int) (numbers (:list :int)))
  (when (some (lambda (x) (= x 0)) numbers)
    (bsdf-compilation-error "Division by zero in (/ ~a ~{~a~^ ~})" number numbers))
  (floor (apply #'/ number numbers)))

(bsdf-defun mod (number divisor)
    (:int (number :int) (divisor :int))
  (when (= 0 divisor)
    (bsdf-compilation-error "Division by zero in (mod ~a ~a)" number divisor))
  (mod number divisor))

;;
;; List type
;; 

(bsdf-defun cons (item list)
    (:list (list :list))
  (cons item list))

(bsdf-defun append (&rest lists)
    (:list)
  (apply #'append 
	 (mapcar (lambda (x) (if (listp x) x (list x))) lists)))

(defmacro def-nth (n)
  (let ((name (intern (string-upcase (format nil "~:r" n)))))
    `(bsdf-defun ,name (list)
	 (t (list :list))
       (,name list))))

(defmacro def-nths (max)
  `(progn ,@(do ((n 1 (1+ n))
		 (specs ()))
		((> n max) (reverse specs))
		(push `(def-nth ,n) specs))))

(def-nths 10)

(bsdf-defun nth (index list)
    (t (index (:int 0)) (list :list))
  (nth index list))

(bsdf-defun remove (item list)
    (:list (list :list))
  (remove item list :test #'equal))

(bsdf-defun remove-duplicates (list)
    (:list (list :list))
  (labels ((do-remove (list)
	     (if (null list) nil
		 (cons (first list)
		       (do-remove (remove (first list) (rest list) :test #'equal))))))
    (do-remove list)))

;;
;; Path type
;;

(bsdf-defun parent-path (path)
    (:path (path :path))
  (parent-path path))

(bsdf-defun directory-path (path)
    (:path (path :path))
  (copy-path path :new-filename nil))

(bsdf-defun root-path (path)
    (:path (path :path))
  (root-path path))

(bsdf-defun path+ (&rest paths)
    (:path (paths (:list :path)))
  (apply #'path+ paths))

(bsdf-defun as-absolute (path)
    (:path (path :path))
  (as-absolute-path path))

(bsdf-defun as-relative (path base-path)
    (:path (path :path) (base-path :path))
  (as-relative-path path base-path))

(bsdf-defun copy-path (path 
			 &key (new-name (path-name path) new-name-p) (new-type (path-type path) new-type-p))
    (:path (path :path) (new-name :string) (new-type :string))
  (apply #'copy-path path
	 (append (when new-name-p (list :new-name new-name))
		 (when new-type-p (list :new-type new-type)))))
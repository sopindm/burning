(in-package #:bsdf-expressions)

(defun bsdf-check-type (type)
  (if (listp type)
      (apply #'%check-type (first type) (rest type))
      (%check-type type)))

(defgeneric %check-type (type &rest args)
  (:method-combination or)
  (:method or (type &rest args) (bsdf-error "Wrong BSDF type ~a" (if args (cons type args) type))))

(defmacro def-type-check (type (args-var) &body body)
  `(defmethod %check-type or ((type (eql ',type)) &rest ,args-var)
     (handler-case (progn ,@body)
       (bsdf-error () nil))))

;;
;; Type checks
;;

(def-type-check t (args)
  (null args))

(def-type-check :string (args)
  (null args))

(def-type-check :int (args)
  (and (< (length args) 3) (every (lambda (x) (or (integerp x) (eq x '*))) args)))

(def-type-check :list (args)
  (and (<= (length args) 1)
       (or (not args)
	   (bsdf-check-type (first args)))))

(def-type-check :path (args)
  (null args))

(def-type-check :file (args)
  (bsdf-check-type (cons :path args)))

(def-type-check :directory (args)
  (bsdf-check-type (cons :path args)))

(def-type-check :bool (args)
  (null args))

(def-type-check :enum (args)
  (every (lambda (x) (and (symbolp x) (eq (symbol-package x) (find-package "KEYWORD"))))
	 args))

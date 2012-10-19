(in-package #:burning-bsdf-test)

(defmacro ?bsdf-condition (expr message &optional (condition 'bsdf-condition))
  `(?condition ,expr, condition
	       (bsdf-condition-message ,message :test equal)))

(defmacro define-?bsdf-condition (name type)
  `(defmacro ,name (expr message &rest args)
     `(?bsdf-condition ,expr (format nil ,message ,@args) ,',type)))

(defmacro define-?bsdf-conditions (&body forms)
  `(progn ,@(mapcar (lambda (form) `(define-?bsdf-condition ,(first form) ,(second form))) forms)))

(define-?bsdf-conditions 
  (?bsdf-error bsdf-error)
  (?bsdf-warning bsdf-warning)
  (?bsdf-compilation-error bsdf-compilation-error)
  (?bsdf-compilation-warning bsdf-compilation-warning))


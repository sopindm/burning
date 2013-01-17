(in-package #:bsdf-variables)

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
;; Variables
;;

(defstruct (variable (:constructor %make-variable) (:conc-name %variable-))
  name expression type)

(defaccessors variable () (name expression type))

(defun make-variable (name expression &key (type t))
  (unless (symbolp name) (bsdf-compilation-error "Wrong variable name '~a'" name))
  (handler-bind ((bsdf-compilation-error (lambda (err) 
					   (setf (bsdf-condition-format-control err)
						 (lines* "In definition of variable '~a':"
							 (bsdf-condition-format-control err)))
					   (setf (bsdf-condition-format-args err)
						 (cons name (bsdf-condition-format-args err))))))
    (setf expression (expand-expression expression))
    (let ((real-type (expression-type expression)))
      (unless (equal real-type type) 
	(setf expression (cast-type expression type real-type)))
      (check-expression expression))
    (let ((var (%make-variable :name name
			       :type type
			       :expression expression)))
      var)))




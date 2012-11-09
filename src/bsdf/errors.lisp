(in-package #:bsdf-errors)

(define-condition bsdf-condition ()
  ((control :initarg :control :accessor bsdf-condition-format-control)
   (args :initarg :args :accessor bsdf-condition-format-args)))

(defmethod simple-condition-format-control ((condition bsdf-condition))
  (bsdf-condition-format-control condition))

(defmethod simple-condition-format-arguments ((condition bsdf-condition))
  (bsdf-condition-format-args condition)) 

(define-condition bsdf-error (bsdf-condition simple-error)
  ())

(define-condition bsdf-warning (bsdf-condition warning)
  ())

(define-condition bsdf-compilation-error (bsdf-error)
  ())

(define-condition bsdf-compilation-warning (bsdf-warning)
  ())

(defun bsdf-condition-message (cond)
  (lines (apply #'format nil (bsdf-condition-format-control cond) (bsdf-condition-format-args cond))))

(defun %bsdf-error (error message args)
  (error error :control message :args args))

(defun bsdf-error (message &rest args)
  (%bsdf-error 'bsdf-error message args))

(defun bsdf-compilation-error (message &rest args)
  (%bsdf-error 'bsdf-compilation-error message args))

(defun bsdf-compilation-warn (message &rest args)
  (warn 'bsdf-compilation-warning :control message :args args))
(in-package #:bsdf-errors)

(define-condition bsdf-condition (simple-condition) ())

(defun bsdf-condition-format-control (cond)
  (simple-condition-format-control cond))

(defun bsdf-condition-format-args (cond)
  (simple-condition-format-arguments cond))

(defun (setf bsdf-condition-format-control) (value cond)
  (reinitialize-instance cond :format-control value :format-arguments (bsdf-condition-format-args cond)))

(defun (setf bsdf-condition-format-args) (value cond)
  (reinitialize-instance cond :format-control (bsdf-condition-format-control cond) :format-arguments value))

(define-condition bsdf-error (bsdf-condition simple-error)
  ())

(define-condition bsdf-warning (bsdf-condition warning)
  ())

(define-condition bsdf-compilation-error (bsdf-error)
  ())

(define-condition bsdf-compilation-warning (bsdf-warning)
  ())

(defun bsdf-condition-message (cond)
  (let ((message (bsdf-condition-format-control cond))
	(args (bsdf-condition-format-args cond)))
    (apply #'format nil (lines message) args)))

(defun %bsdf-error (error message args)
  (error error :format-control message :format-arguments args))

(defun bsdf-error (message &rest args)
  (%bsdf-error 'bsdf-error message args))

(defun bsdf-compilation-error (message &rest args)
  (%bsdf-error 'bsdf-compilation-error message args))

(defun bsdf-compilation-warn (message &rest args)
  (warn 'bsdf-compilation-warning :format-control message :format-arguments args))
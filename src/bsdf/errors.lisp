(in-package #:burning-bsdf)

(define-condition bsdf-condition (simple-condition)
  ())

(define-condition bsdf-error (bsdf-condition simple-error)
  ())

(define-condition bsdf-warning (bsdf-condition simple-warning)
  ())

(define-condition bsdf-compilation-error (bsdf-error)
  ())

(define-condition bsdf-compilation-warning (bsdf-warning)
  ())

(defun bsdf-condition-message (cond)
  (lines (apply #'format nil (simple-condition-format-control cond) (simple-condition-format-arguments cond))))

(defun bsdf-compilation-error (message &rest args)
  (error 'bsdf-compilation-error :format-control message :format-arguments args))

(defun bsdf-compilation-warn (message &rest args)
  (warn 'bsdf-compilation-warning :format-control message :format-arguments args))
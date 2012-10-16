(in-package #:burning-bsdf-test)

(defmacro ?bsdf-condition (expr message &optional (condition 'bsdf-condition))
  `(?condition ,expr, condition
	       (bsdf-condition-message ,message :test equal)))

(defmacro ?bsdf-error (expr message)
  `(?bsdf-condition ,expr ,message bsdf-error))

(defmacro ?bsdf-warning (expr message)
  `(?bsdf-condition ,expr ,message bsdf-warning))

(defmacro ?bsdf-compilation-error (expr message)
  `(?bsdf-condition ,expr ,message bsdf-compilation-error))

(defmacro ?bsdf-compilation-warning (expr message)
  `(?bsdf-condition ,expr ,message bsdf-compilation-warning))
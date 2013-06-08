(in-package #:burning-cgen)

;;
;; Generator
;;

(defun make-generator ()
  ())

(defparameter *generator* (make-generator))

; sources

(defun generator-sources ()
  *generator*)

(defun (setf generator-sources) (value)
  (setf *generator* value))

(defun generator-add-source (value)
  (push value (generator-sources)))

;;
;; Basic generation functions
;;

(defun generate-code ()
  (reduce #'string+ (mapcar #'funcall (reverse (generator-sources))) :initial-value ""))

(defun generate-call (name namespace)
  (format nil "~a()" (generate-name name)))

(defun generate-name (name)
  (setf name (symbol-name name))
  (search-and-replace-all (string-downcase name) "-" "_"))

;;
;; Basic generation macro's
;;

(defmacro burning-cgen-source:defun (name ())
  `(progn (defun ,name ()
	    (generate-call ',name :function))
	  (generator-add-source (flet ((,name ()
					 (lines (format nil "~a ()" (generate-name ',name))
						"{"
						"}")))
				  #',name))))
		

(defmacro burning-cgen-source:defvar (name value)
  `(progn (defparameter ,name (generate-call ',name :variable))
	  (generator-add-source (flet ((,name ()
					 (lines (format nil "~a = ~a;" (generate-name ',name) ,value))))
				  #',name))))
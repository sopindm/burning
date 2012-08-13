(in-package #:burning-cgen)

(define-condition cgen-error (error) 
  ((position :initform nil :accessor cgen-error-position)
   (object :initarg :object :reader cgen-error-object)))

(defmethod initialize-instance :after ((error cgen-error) &key source)
  (when source
    (setf (slot-value error 'position) (cgen-source-position source))))

(defgeneric cgen-error-type (error))
(defmethod cgen-error-type ((error cgen-error))
  "")

(defun cgen-error-message (error)
  (with-output-to-string (stream)
    (let ((object (cgen-error-object error)))
      (format stream "CGen~a error: ~a~%" (cgen-error-type error)
	      (if (typep object 'simple-error) 
		  (apply #'format nil (simple-condition-format-control object)
			 (simple-condition-format-arguments object))
		  (type-of object))))
    (awhen (cgen-error-position error)
      (format stream "in lines ~a to ~a~%"
	      (stream-position-line (first (cgen-error-position error)))
	      (stream-position-line (rest (cgen-error-position error)))))))

(define-condition cgen-macroexpansion-error (cgen-error) ())
(defmethod cgen-error-type ((error cgen-macroexpansion-error))
  " macroexpansion")

;; add generation error test with vfs
  
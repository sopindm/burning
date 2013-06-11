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
  (format nil "~{~a~^~%~}" (mapcar #'funcall (reverse (generator-sources)))))

(defun generate-call (name &rest args)
  (format nil "~a~a" (generate-name name) "()"))

(defun generate-reference (name)
  (format nil "~a" (generate-name name)))

(defun generate-name (name)
  (setf name (symbol-name name))
  (search-and-replace-all (string-downcase name) "-" "_"))

;;
;; Basic generation macro's
;;

(defun generate-arguments-list (args)
  (format nil "~{~a~^, ~}" (mapcar #'generate-name args)))

(defun generate-block (&rest forms)
  (flet ((generate-line (line)
	   (search-and-replace-all (format nil "  ~a" line) (format nil "~%") (format nil "~%  "))))
    (format nil "{~a~{~a~^~%~}~%}" 
	    (if forms #\Newline "")
	    (mapcar #'generate-line forms))))

(defmacro burning-cgen-source:defun (name (&rest args) &body body)
  (flet ((bind-argument (arg)
	   `(,arg (generate-reference ',arg))))
    `(progn (defun ,name (,@args)
	      (generate-call ',name ,@args))
	    (generator-add-source (named-lambda ,(make-symbol (symbol-name name)) ()
				    (format nil "~a (~a)~%~a~%" 
					    (generate-name ',name)
					    (generate-arguments-list ',args)
					    (let (,@(mapcar #'bind-argument args))
					      (generate-block ,@body))))))))

(defmacro burning-cgen-source:setf (place value)
  `(format nil "~a = ~a" (generate-reference ',place) ,value))

(defmacro burning-cgen-source:if (expr then-form &optional (else-form nil else-form-p))
  (flet ((make-then-form (form)
	   `(generate-block ,form))
	 (make-else-form (form)
	   `(lines* ""
		    "else"
		    (generate-block ,form))))
    `(format nil "if( ~a )~%~a~a" 
	     ,expr 
	     ,(make-then-form then-form)
	     ,(if else-form-p (make-else-form else-form) ""))))

(defmacro burning-cgen-source:defvar (name value)
  `(progn (defparameter ,name (generate-reference ',name))
	  (generator-add-source (named-lambda ,(make-symbol (symbol-name name)) ()
				  (lines (burning-cgen-source:setf ,name ,value))))))

(defun burning-cgen-source:+ (arg1 arg2)
  (format nil "~a + ~a" arg1 arg2))

(defun burning-cgen-source:- (arg1 arg2)
  (format nil "~a - ~a" arg1 arg2))

(defun burning-cgen-source:* (arg1 arg2)
  (format nil "~a * ~a" arg1 arg2))

(defun burning-cgen-source:/ (arg1 arg2)
  (format nil "~a / ~a" arg1 arg2))

(defmacro burning-cgen-source:let ((&rest bindings) &body body)
  (flet ((make-binding (binding)
	   (let ((arg (first binding)))
	     `(,arg (generate-reference ',arg))))
	 (make-initializator (binding)
	   (let ((arg (first binding))
		 (value (second binding)))
	     `(burning-cgen-source:setf ,arg ,value))))
    `(let (,@(mapcar #'make-binding bindings))
       (generate-block ,@(mapcar #'make-initializator bindings)
		       ,@body))))


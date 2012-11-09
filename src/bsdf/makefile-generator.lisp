(in-package #:bsdf-generator)

;;
;; Makefile target structure
;;

(defstruct (makefile-target (:constructor make-makefile-target (input output &optional command)))
  input
  output
  command)

(defun print-makefile-target (target stream)
  (flet ((print-makefile-list (list)
	   (unless (listp list) (setf list (list list)))
	   (format nil "~{~a~^ ~}" list)))
    (format stream "~a: ~a~%" 
	    (print-makefile-list (makefile-target-output target))
	    (print-makefile-list (makefile-target-input target)))
    (awhen (makefile-target-command target)
      (mapc (lambda (line) (format stream "~c~a~%" #\Tab line)) (if (listp it) it (list it))))))

;;
;; Makefile generation
;;

(defun to-makefile-targets (target command)
  (flet ((to-lists (target)
	   (with-accessors ((input target-input) 
			    (output target-output)
			    (name target-name)
			    (depends-on target-depends-on)) target
	     (cond ((and name output) (list (list input output command)
					    (if depends-on (list depends-on output))
					    (list name ".PHONY")
					    (list output name)))
		   (name (list (list name ".PHONY")
			       (list input name command)
			       (if depends-on (list depends-on name))))
		   (output (list (list input output command)))
		   (t (error "Cannot generate makefile entity for target ~a" target))))))
    (mapcar (lambda (list) (apply #'make-makefile-target list))
	    (remove-if #'null (to-lists target)))))

(define-generator makefile)

(define-generator-method generator-write-context (makefile context)
  (let ((path (first context))
	(targets (double-list-head (rest context))))
    (with-open-file (stream (path+ (copy-path path :new-filename nil) (path-from-string "Makefile")) 
			    :direction :output :if-exists :supersede)
      (mapl (lambda (targets) 
	      (let ((target (first targets)))
		(print-makefile-target target stream)
		(when (and (rest targets) (makefile-target-command target)) (format stream "~%"))))
	    targets)
      (format stream "~%"))))
      
;;
;; Makefile common commands
;;  

(defmacro def-simple-makefile-command (name (&rest args) &body body)
  (let ((target-sym (gensym)))
    `(define-command-method makefile ,name (,target-sym ,@args)
       (flet ((self ()
		,@body))
	 (to-makefile-targets ,target-sym (funcall #'self))))))

(def-simple-makefile-command nil ()
  nil)

(def-simple-makefile-command echo (format &rest args)
  (apply #'format nil (format nil "@echo '~a'" format) args))

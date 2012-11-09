(in-package #:bsdf-generator)

;;
;; Makefile target structure
;;

(defun escape-makefile-string (string)
  (labels ((replace-char (char seq string)
	     (aif (position char string)
		  (string+ (subseq string 0 it)
			   seq
			   (replace-char char seq (subseq string (1+ it))))
		  string)))
    (replace-char #\Space "\\ " string)))

(defstruct (makefile-target (:constructor %make-makefile-target))
  input
  output
  command)

(defun make-makefile-target (input output &optional command)
  (%make-makefile-target :input (mapcar #'escape-makefile-string (if (listp input) input (list input)))
			 :output (mapcar #'escape-makefile-string (if (listp output) output (list output)))
			 :command command))

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
  (let ((input (target-input target))
	(output (target-output target))
	(name (target-name target))
	(depends-on (target-depends-on target)))
    (when (and output (rest output) (not name))
      (setf name (gen-tmp-name (format nil "~{~a~^ ~}" output))))
    (append (if name (list (make-makefile-target name ".PHONY")
			   (make-makefile-target input name command))
		(list (make-makefile-target input output command)))
	    (when (and name output) (list (make-makefile-target name output)))
	    (when depends-on (list (make-makefile-target depends-on (or output name)))))))

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

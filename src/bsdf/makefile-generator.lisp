(in-package #:bsdf-generator)

;;
;; Makefile target structure
;;

(defun replace-sequences (sequences string)
  (labels ((replace-sequence (seq string)
	     (dbind (source dest) seq
	       (aif (search source string)
		    (string+ (subseq string 0 it)
			     dest
			     (replace-sequence seq (subseq string (+ it (length source)))))
		    string)))
	   (do-replace (sequences)
	     (if sequences
		 (replace-sequences (rest sequences) (replace-sequence (first sequences) string))
		 string)))
    (do-replace sequences)))

(defun escape-makefile-name (name)
  (when (and name (symbolp name)) (setf name (symbol-name name)))
  (labels ((find-wildcards (wildcards string &optional (index 0))
	     (if (< index (length wildcards))
		 (or (find (char wildcards index) string :test #'char=)
		     (find-wildcards wildcards string (1+ index)))))
	   (find-wildcard (string)
	     (find-wildcards "*?%" string)))
    (when (find-wildcard name)
      (bsdf-warn "Makefile entity's name '~a' contains wildcard characters" name))
    (replace-sequences '((" " "\\ ")
			 ("$" "$$"))
		       name)))

(defstruct (makefile-target (:constructor %make-makefile-target))
  input
  output
  command)

(defun make-makefile-target (input output &optional command)
  (%make-makefile-target :input (if (listp input) input (list input))
			 :output (if (listp output) output (list output))
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
;; Makefile variables
;;

(defun print-makefile-value (value type)
  (when (eq type t)
    (setf type (expression-type value)))
  (let ((type (if (listp type) (first type) type))
	(args (if (listp type) (rest type))))
    (%print-makefile-value value type args)))

(defgeneric %print-makefile-value (value type type-args))

(defmethod %print-makefile-value (value type type-args)
  (declare (ignore type type-args))
  (cast-type (expression-value value) :string))

(defmethod %print-makefile-value (value (type (eql :bool)) type-args)
  (declare (ignore type-args))
  (if value "true" "false"))

(defmethod %print-makefile-value (value (type (eql :list)) type-args)
  (declare (ignore type-args))
  (labels ((replace-all (seq dest string)
	     (aif (search seq string)
		  (string+ (subseq string 0 it)
			   dest
			   (replace-all seq dest (subseq string (+ it (length seq)))))
		  string))
	   (quote-list (string)
	     (replace-all "__NEXT" "__NEXTT" string))
	   (print-elem (val)
	     (let ((string (print-makefile-value val (expression-type val))))
	       (if (listp val)
		   (quote-list string)
		   string))))
    (format nil "__(__NEXT!~{~a__NEXT!~}__)" (mapcar #'print-elem value))))

(defun print-makefile-variable (var stream)
  (format stream "~a = ~a~%" 
	  (escape-makefile-name (variable-name var))
	  (print-makefile-value (expression-value (variable-expression var)) (variable-type var))))

;;
;; Makefile generation
;;

(defun to-makefile-targets (target command)
  (let ((input (target-input target))
	(output (target-output target))
	(name (target-name target))
	(depends-on (target-depends-on target)))
    (when (and output (rest output) (not name))
      (setf name (format nil "__~{~a~^ ~}" output)))
    (setf name (escape-makefile-name name))
    (setf input (mapcar #'escape-makefile-name input))
    (setf output (mapcar #'escape-makefile-name output))
    (setf depends-on (mapcar #'escape-makefile-name depends-on))
    (append (when name (list (make-makefile-target name ".PHONY")))
	    (when (and name output) (list (make-makefile-target name output)))
	    (when depends-on (list (make-makefile-target depends-on (or output name))))
	    (if name 
		(list (make-makefile-target input name command))
		(list (make-makefile-target input output command))))))

(define-generator makefile)

(define-generator-method generator-write-context (makefile context)
  (let ((path (generator-context-path context))
	(targets (double-list-head (generator-context-targets context)))
	(vars (double-list-head (generator-context-variables context))))
    (with-open-file (stream (path+ (copy-path path :new-filename nil) (path-from-string "Makefile")) 
			    :direction :output :if-exists :supersede)
      (mapc (lambda (var) (print-makefile-variable var stream)) vars)
      (mapl (lambda (targets) 
	      (let ((target (first targets)))
		(print-makefile-target target stream)
		(when (and (rest targets) (makefile-target-command target))
		  (format stream "~%"))))
	    targets))))
      
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

(in-package #:burning-command-line)

;;
;; Argument base class
;;

(defclass argument ()
  ((name :initarg :name :reader argument-name)
   (short-name :initarg :short-name :initform nil :reader argument-short-name)
   (description :initarg :description :reader argument-description)
   (group :initarg :group :reader argument-group)))

(defgeneric make-argument (class name &key short-name description &allow-other-keys))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defgeneric parse-argument-spec (class spec))
  (defmethod parse-argument-spec (class spec)
    `(make-argument ,class ,@spec)))

;;
;; Flag class 
;;

(defclass flag (argument) ())

(defmethod make-argument ((class (eql :flag)) name &key short-name description)
  (make-instance 'flag 
		 :name name
		 :short-name short-name
		 :description description))

;;
;; Key class
;;

(defclass key (argument) 
  ((type :initarg :type :reader key-argument-type)))

(defmethod make-argument ((class (eql :key)) name &key type short-name description)
  (make-instance 'key
		 :name name
		 :type type
		 :short-name short-name
		 :description description))

;;
;; Arguments list
;;

(defclass arguments-list () 
  ((arguments :accessor %arguments-list-arguments)
   (parent :initform nil :reader arguments-list-parent)
   (help :initform nil :accessor arguments-list-help)))

(defun arguments-list-arguments (list &key (include-positionals t))
  (let ((args (%arguments-list-arguments list)))
    (unless include-positionals
      (setf args (remove-if #'positional-group-p args)))
    (apply #'append
	   (remove-if #'group-p args)
	   (mapcar #'%arguments-list-arguments (remove-if-not #'group-p args)))))

(defgeneric arguments-list-name (list))

(defun do-check-arguments-list (list)
  (labels ((have-same-names (args)
	     (cond
	       ((null args) nil)
	       ((have-same-name (first args) (rest args)))
	       (t (have-same-names (rest args))))))
    (let ((duplicate (have-same-names list)))
      (if duplicate (error duplicate) t))))

(defun check-arguments-list (list)
  (do-check-arguments-list (arguments-list-arguments list :include-positionals nil)))

(defun do-add-argument (arg list &optional (group-name "ARGS"))
  (do-check-arguments-list (append (if (group-p arg) 
				       (arguments-list-arguments arg :include-positionals nil)
				       (list arg))
				   (arguments-list-arguments list :include-positionals nil)))
  (if (group-p arg)
      (setf (%arguments-list-arguments list)
	    (nconc (%arguments-list-arguments list) (list arg)))
      (let ((group (find group-name (groups list) :test #'equal :key #'arguments-list-name)))
	(setf (%arguments-list-arguments group)
	      (nconc (%arguments-list-arguments group) (list arg)))
	(setf (slot-value arg 'group) group)
	(when (typep arg 'action)
	  (setf (slot-value arg 'parent) list)))))

(defmacro add-argument (arg list &key (group nil))
  `(do-add-argument ,arg ,list ,@(if group (list group) ())))

(defun update-argument (arg list &optional (group-name "ARGS"))
  (if (group-p arg)
      (let ((position (position (arguments-list-name arg) (%arguments-list-arguments list) 
				:test #'equal 
				:key #'arguments-list-name)))
	(if position 
	    (setf (nth position (%arguments-list-arguments list)) arg)
	    (add-argument arg list :group group-name)))
      (let* ((group (find group-name (groups list) :test #'equal :key #'arguments-list-name))
	     (position (position (argument-name arg) (%arguments-list-arguments group) 
				 :test #'equal 
				 :key #'argument-name)))
	(if (and group position)
	    (progn
	      (setf (nth position (%arguments-list-arguments  group)) arg)
	      (setf (slot-value arg 'group) group)
	      (when (typep arg 'action)
		(setf (slot-value arg 'parent) list)))
	    (add-argument arg list :group group-name)))))

(defmacro argument-from-spec (spec)
  (parse-argument-spec (first spec) (rest spec)))

(set-dispatch-macro-character #\# #\A 
			      #'(lambda (stream c1 c2)
				  (let ((spec (read stream t nil t)))
				    `(argument-from-spec ,spec))))

(defun argument (name list)
  (find name (arguments-list-arguments list) :test #'equal :key #'argument-name))

(defun have-argument-p (name list)
  (or (some  #'(lambda (action) (have-argument-p name action)) (actions list))
      (if (argument name list) t nil)))

(defun string+ (&rest strings)
  (apply #'concatenate 'string strings))

(defgeneric arguments-list-description (list))

(defun help-message (list)
  (labels ((group-names (list)
	     (let ((group-names (mapcar #'(lambda (group) (format nil "~a" (group-name group)))
					(groups list))))
	       (reduce #'string+ group-names)))
	   (usage-message (list)
	     (string+ (if (string/= (arguments-list-description list) "")
			  (format nil "~a~%~%" (arguments-list-description list)) "")
		      (format nil "Usage:~%  ~a~a~%~%" (arguments-list-name list) (group-names list))))
	   (argument-name-message (arg)
	     (string+ (format nil "    --~a" (argument-name arg))
		      (if (argument-short-name arg) (format nil ",-~a" (argument-short-name arg)) "")
		      (if (typep arg 'key) (format nil " ~a" (type-value-name (key-argument-type arg))) "")))
	   (argument-description-message (arg)
	     (if (argument-description arg) (format nil "~a" (argument-description arg)) ""))
	   (positional-message (arg)
	     (assert (= (length (arguments-list-arguments arg)) 1))
	     (format nil "  Where ~a is~va~a~%~%" (arguments-list-name arg) 10 "" 
		     (argument-description-message (first (arguments-list-arguments arg)))))
	   (arguments-message (list)
	       (string+ (format nil "  Where ~a are:~%" (arguments-list-name list))
			(if (> (length (arguments-list-arguments list)) 0)
			    (let ((names (mapcar #'argument-name-message (arguments-list-arguments list)))
				  (descs (mapcar #'argument-description-message (arguments-list-arguments list))))
			      (let ((space (+ 10 (apply #'max (mapcar #'length names)))))
				(apply #'string+ 
				       (mapcar #'(lambda (name desc) 
						   (format nil "~a~va~a~%" name (- space (length name)) "" desc))
					       names descs))))
			    "")
			(format nil "~%"))))
    (string+ (usage-message list)
	     (reduce #'string+ (mapcar #'positional-message (remove-if-not #'positional-group-p (groups list))))
	     (reduce #'string+ (mapcar #'arguments-message (remove-if #'positional-group-p (groups list)))))))

(defmethod initialize-instance :after ((obj arguments-list) &key (help nil help-set-p) arguments)
  (let ((default-group-arguments (remove-if #'group-p arguments)))
    (let ((help (if help-set-p help (make-argument :flag "help" :description "Products this help message"))))
      (when help 
	(push help default-group-arguments)
	(setf (arguments-list-help obj) help)))
    (when (and (not (group-p obj)) (> (length default-group-arguments) 0))
      (setf default-group-arguments `(,(make-instance 'group 
						      :arguments default-group-arguments
						      :name "ARGS"
						      :help nil))))
    (setf (%arguments-list-arguments obj) (append default-group-arguments (remove-if-not #'group-p arguments))))
  (mapc #'(lambda (action) (setf (slot-value action 'parent) obj)) (actions obj))
  (check-arguments-list obj))

(defmacro make-arguments-list (class name-and-options argument-specs)
  (labels ((parse-option (spec)
	     (let ((name (if (listp spec) (first spec) spec)))
	       (case name
		 (:help `(:help (make-argument :flag ,@(rest spec))))
		 (:no-help `(:help nil))
		 (otherwise (list spec)))))
	   (parse-options (opts)
	     (if (stringp opts)
		 (list :name opts)
		 `(:name ,(first opts) ,@(reduce #'append (mapcar #'parse-option (rest opts)))))))
    `(make-instance ,class
		    ,@(parse-options name-and-options)
		    :arguments ,(parse-arguments-option argument-specs))))

;;
;; Action class
;;

(defclass action (argument arguments-list) ())

(defmethod make-argument ((class (eql :action)) name &key short-name description arguments)
  (make-instance 'action 
		 :name name
		 :short-name short-name
		 :description description
		 :arguments arguments))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun parse-arguments-option (args)
    `(list ,@(mapcar #'(lambda (spec) (parse-argument-spec (first spec) (rest spec))) args))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmethod parse-argument-spec ((class (eql :action)) spec)
    (let ((args (find-keyword :arguments spec))
	  (spec (remove-keyword :arguments spec)))
      (unless (find-keyword :description spec)
	(setf spec (append spec '(:description ""))))
      `(make-arguments-list 'action ,spec ,args))))

(defmethod arguments-list-description ((list action))
  (argument-description list))

(defmethod arguments-list-name ((list action))
  (string+ (if (arguments-list-parent list) 
	       (format nil "~a " (arguments-list-name (arguments-list-parent list))) "")
   (format nil "--~a" (argument-name list))
	   (if (argument-short-name list) (format nil ",-~a" (argument-short-name list)) "")))

(defun action-p (arg)
  (typep arg 'action))

(defun actions (arg-list)
  (remove-if-not #'action-p (arguments-list-arguments arg-list)))

(defun groups (arg-list)
  (remove-if-not #'group-p (%arguments-list-arguments arg-list)))

;;
;; Group class
;;

(defclass group (arguments-list)
  ((name :initarg :name :reader arguments-list-name)
   (args-min :initarg :args-min :initform 0 :reader group-args-min)
   (args-max :initarg :args-max :initform :infinity :reader group-args-max)))

(defmethod initialize-instance :after ((obj group) &key)
  (when (not (null (groups obj)))
    (error "Nested groups not allowed"))
  (mapc #'(lambda (arg) (setf (slot-value arg 'group) obj)) (%arguments-list-arguments obj)))

(defmethod argument-name ((obj group))
  (arguments-list-name obj))

(defun group-name (group)
  (let ((name (arguments-list-name group)))
    (flet ((optional-name ()
	     (format nil " [~a~@[ ...~]]" name (eq (group-args-max group) :infinity))))
      (string+ (if (> (group-args-min group) 0) (format nil " ~a" name) "")
	       (if (not (eq (group-args-min group) (group-args-max group)))
		   (optional-name) "")))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmethod parse-argument-spec ((class (eql :group)) spec)
    (flet ((restriction-p (arg)
	     (member arg '(:one-max :one-only :one-min)))
	   (restriction-arg (arg)
	     (ecase arg
	       ((nil) nil)
	       (:one-max '(:args-max 1))
	       (:one-min '(:args-min 1))
	       (:one-only '(:args-min 1 :args-max 1)))))
      (let ((args (find-keyword :arguments spec))
	    (spec (remove-keyword :arguments spec)))
	(let ((restrictions (remove-if-not #'restriction-p spec))
	      (spec (remove-if #'restriction-p spec)))
	  (when (> (length restrictions) 1) (error "Too much restrictions for group."))
	  `(make-arguments-list 'group ,(append spec (restriction-arg (first restrictions)) '(:no-help))
				,args))))))

(defun group-p (arg)
  (typep arg 'group))

;;
;; Positionals
;;

(defclass positional (group) ())

(defun positional-p (arg)
  (typep (argument-group arg) 'positional))

(defun positional-group-p (group)
  (typep group 'positional))

(defmethod initialize-instance :after ((obj positional) &key)
  (unless (= (length (arguments-list-arguments obj)) 1)
    (error "Positional argument's group must have 1 argument"))
  (unless (every #'(lambda (arg) (typep arg 'key)) (arguments-list-arguments obj))
    (error "Positional argument must be a key")))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmethod parse-argument-spec ((class (eql :positional)) spec)
    (when (member :short-name spec)
      (error "Positional arguments can't have short names"))
    (let ((optional-p (member :optional spec))
	  (spec (remove :optional spec)))
      `(make-arguments-list 'positional (,(first spec) :no-help :args-min ,(if optional-p 0 1) :args-max 1)
			    ((:key ,@spec))))))
;;
;; Arguments specification
;;

(defclass arguments-spec (arguments-list) 
  ((name :initarg :name :reader arguments-list-name)
   (description :initarg :description :initform "" :reader arguments-list-description)))

(define-condition argument-already-exists-error (error)
  ((name :initarg :name :reader argument-already-exists-error-name)))

(define-condition short-name-already-exists-error (error)
  ((char :initarg :char :reader short-name-already-exists-error-char)))

(defun have-same-name (arg args)
  (flet ((find-name (arg args)
	   (find (argument-name arg) args :test #'equal :key #'argument-name))
	 (find-short-name (arg args)
	   (if (argument-short-name arg)
	       (find (argument-short-name arg) args :key #'argument-short-name))))
    (cond 
      ((null args) nil)
      ((find-name arg args) 
       (make-condition 'argument-already-exists-error :name (argument-name arg)))
      ((find-short-name arg args) 
       (make-condition 'short-name-already-exists-error :char (argument-short-name arg))))))

(defmacro make-arguments-spec (&optional (name-and-options "") &body argument-specs)
  `(make-arguments-list 'arguments-spec ,name-and-options ,argument-specs))

;; Parsing


(define-condition wrong-argument-error (cmd-parsing-error) ())
(defmethod cmd-parsing-error-message ((err wrong-argument-error))
  (format nil "Wrong argument name: ~a" (cmd-parsing-error-argument err)))

(define-condition wrong-short-argument-error (cmd-parsing-error) ())
(defmethod cmd-parsing-error-message ((err wrong-short-argument-error))
  (format nil "Wrong argument short name: ~a" (cmd-parsing-error-argument err)))

(defun arguments-to-list (args)
  (flet ((argument-to-list (arg)
	   (assert (stringp arg))
	   (cond 
	     ((and (>= (length arg) 2) (equal (subseq arg 0 2) "--")) `((:arg ,(subseq arg 2))))
	     ((and (>= (length arg) 2) (equal (char arg 0) #\-) (not (or (digit-char-p (char arg 1)) 
									 (eq (char arg 1) #\.))))
	      (map 'list #'(lambda (char) `(:short ,char)) (subseq arg 1)))
	     (t `((:param ,arg))))))
    (apply #'append (mapcar #'argument-to-list args))))

(defstruct list-iterator
  list)

(defun iterator-current (iter)
  (first (list-iterator-list iter)))

(defun iterator-next (iter)
  (setf (list-iterator-list iter) (rest (list-iterator-list iter))))

(defgeneric parse-argument (arg rest-args))

(defmethod parse-argument (arg rest)
  nil)

(defmethod parse-argument ((arg key) rest)
  (let ((type-spec (key-argument-type arg)))
    (destructuring-bind (type type-args) (if (listp type-spec) 
					     (list (first type-spec) (rest type-spec))
					     (list type-spec nil))
      (handler-bind ((cmd-parsing-error (lambda (err) 
					  (setf (slot-value err 'argument) (argument-name arg)))))
	(apply #'parse-type rest type type-args)))))

(defmacro aif (if-clause then-form &optional else-form)
  `(let ((it ,if-clause))
     (if it ,then-form ,else-form)))

(defun %find-argument (spec test env &optional (recursive-p t))
  (or (and recursive-p (find-if #'(lambda (action) (%find-argument action test env)) (actions spec)))
      (find-if #'(lambda (arg) (funcall test arg (argument-set-p (argument-name arg) env))) 
	       (arguments-list-arguments spec))))

(defun %parse-argument (test rest spec env)
  (labels ((parse-in-action (action env)
	     (aif (and (argument-set-p (argument-name action) env)
		       (%parse-argument test rest action (argument-value (argument-name action) env)))
		  (setf (argument-value (argument-name action) env) it))))
    (if (find-if #'(lambda (action) (parse-in-action action env)) (actions spec))
	env
	(aif (%find-argument spec test env nil)
	     (cons (cons it (parse-argument it rest)) env)))))

(defmacro null-cond (var null-value &body t-value)
  `(cond 
     ((null ,var) ,null-value)
     (t ,@t-value)))

(define-condition help-set-signal (condition)
  ((action :initarg :action :reader help-set-signal-action)))

(defun check-set-help (action env)
  (if (argument-set-p (argument-name (arguments-list-help action)) env)
      (signal 'help-set-signal :action action)
      (mapc #'(lambda (action) 
		(if (argument-set-p (argument-name action) env)
		    (check-set-help action (argument-value (argument-name action) env))
		    nil))
	    (actions action))))

(defun parse-arguments (args spec)
  (labels ((parse-positional (iter spec env)
	     (%parse-argument (positional-test) iter spec env))
	   (parse-argument (test iter spec env)
	     (iterator-next iter)
	     (%parse-argument test iter spec env))
	   (argument-name-test (name key)
	     (lambda (arg set-p)
	       (declare (ignore set-p))
	       (equal (funcall key arg) name)))
	   (positional-test ()
	     (lambda (arg set-p)
	       (and (not set-p) (positional-p arg))))
	   (parse-cmd-argument (args env)
	     (let ((value (iterator-current args)))
	       (ecase (first value)
		 (:arg (aif (parse-argument (argument-name-test (second value) #'argument-name)
					    args spec env)
			    it (error 'wrong-argument-error :argument (second value))))
		 (:short (aif (parse-argument (argument-name-test (second value) #'argument-short-name)
					      args spec env)
			      it (error 'wrong-short-argument-error :argument (second value))))
		 (:param (aif (parse-positional args spec env)
			      it (error 'wrong-argument-error :argument (second value)))))))
	   (do-parse-arguments (iter &optional env)
	     (null-cond (iterator-current iter) 
		 (reverse (mapcar #'(lambda (arg) (cons (first arg) (if (action-p (first arg))
									(reverse (rest arg))
									(rest arg))))
				  env))
	       (let ((env (parse-cmd-argument iter env)))
		 (do-parse-arguments iter env)))))
    (let ((env (do-parse-arguments (make-list-iterator :list (arguments-to-list args)))))
      (check-set-help spec env)
      (check-groups spec env)
      env)))

(defun command-line-arguments ()
  #+sbcl
  (rest sb-ext:*posix-argv*)
  #+ccl
  ccl:*unprocessed-command-line-arguments*
  #-(or sbcl ccl)
  (error "Cannot get command line arguments for this implementation."))

(defmacro parse-command-line (spec &optional (arguments '(command-line-arguments)) 
			      &key (handle-errors t) (handle-help t))
  (let ((parse-line `(parse-arguments ,arguments ,spec)))
    (if (not (or handle-errors handle-help))
	parse-line
	`(handler-case
	     ,parse-line
	   ,@(when handle-errors
		   `((cmd-parsing-error (err)
		       (format t "~a~%~%" (cmd-parsing-error-message err))
		       (format t (help-message ,spec))
		       (quit))))
	   ,@(when handle-help
		   `((help-set-signal (help)
		        (format t (help-message (help-set-signal-action help)))
			(quit))))))))

(define-condition too-few-arguments-in-group-set (cmd-parsing-error) ())

(defmethod cmd-parsing-error-message ((err too-few-arguments-in-group-set))
  (format nil "Too few arguments in group ~a set" (cmd-parsing-error-argument err)))

(define-condition too-much-arguments-in-group-set (cmd-parsing-error)
  ((arguments :initarg :arguments :reader too-much-arguments-in-group-set-arguments)))

(defmethod cmd-parsing-error-message ((err too-much-arguments-in-group-set))
  (format nil "Too much arguments in group ~a set:~{ ~a~}" (cmd-parsing-error-argument err)
	  (too-much-arguments-in-group-set-arguments err)))

(defun check-groups (spec env)
  (flet ((check-group (group)
	   (let ((set-arguments (remove-if-not #'(lambda (arg) (argument-set-p (argument-name arg) env))
					       (arguments-list-arguments group))))
	     (unless (eq (group-args-max group) :infinity)
	       (when (> (length set-arguments) (group-args-max group))
		 (error 'too-much-arguments-in-group-set 
			:argument (arguments-list-name group)
			:arguments (mapcar #'argument-name set-arguments))))
	     (when (< (length set-arguments) (group-args-min group))
	       (error 'too-few-arguments-in-group-set
		      :argument (arguments-list-name group))))))
    (mapc #'check-group (groups spec))
    (let ((actions (remove-if-not #'(lambda (act) (argument-set-p (argument-name act) env)) (actions spec))))
      (mapc #'check-groups actions
	    (mapcar #'(lambda (action) (argument-value (argument-name action) env)) actions)))
    env))

(defun find-argument-in-list (name list)
  (find name list :test #'equal :key #'(lambda (arg) (argument-name (first arg)))))

(defun argument-set-p (name args-list)
  (if (find-argument-in-list name args-list)  t nil))

(defun (setf argument-value) (value name args-list)
  (aif (find-argument-in-list name args-list)
       (setf (rest it) value)))
    
(defun argument-value (name args-list)
  (let ((arg (find-argument-in-list name args-list)))
    (if arg (values (rest arg) t) (values nil nil))))

(defun group-value (name args-list)
  (remove-if-not #'(lambda (x) (equal x name)) args-list
		 :key #'(lambda (x) (argument-name (argument-group (first x))))))
  
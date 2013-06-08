(in-package #:bsdf-targets)

;;
;; Target structure
;;

(defstruct (target (:constructor %make-target) (:conc-name %target-))
  name
  command 
  input 
  output
  depends-on)

(defun target-name (target)
  (%target-name target))

(defun target-command (target)
  (%target-command target))

(defun target-input (target)
  (double-list-head (%target-input target)))

(defun target-output (target)
  (double-list-head (%target-output target)))

(defun target-depends-on (target)
  (double-list-head (%target-depends-on target)))

(defun %target-print-name (name output)
  (let ((name (or name output)))
    (if (listp name) (format nil "~{~a~^ ~}" name) name)))

(defun target-print-name (target)
  (%target-print-name (target-name target) (target-output target)))

(defun make-target (command &key name input output depends-on)
  (flet ((to-list (arg type) 
	   (unless (or (stringp arg) (and (listp arg) (every #'stringp arg)))
	     (bsdf-compilation-error "Wrong ~a '~a' in target '~a'" type arg (%target-print-name name output)))
	   (copy-list (if (listp arg) arg (list arg))))
	 (to-name (arg) 
	   (unless (or (not name) (stringp name)) (bsdf-compilation-error "Wrong target name '~a'" name))
	   arg)
	 (to-command (arg) 
	   (when (and arg (not (functionp arg)))
	     (bsdf-compilation-error "Wrong command ~a in target '~a'" 
				     command (%target-print-name name output)))
	   arg))
    (setf output (to-list output "output"))
    (when (and (not name) (not output))
      (bsdf-compilation-error "No output file or name in target"))
    (%make-target :name (to-name name)
		  :command (to-command command)
		  :input (make-double-list (to-list input "input"))
		  :output (make-double-list output)
		  :depends-on (make-double-list (to-list depends-on "dependencies list")))))

(defmacro deftarget (name command input output &optional depends-on)
  (flet ((to-list (expr)
	   (if (listp expr) (cons 'list expr) expr)))
    (let ((input (to-list input))
	  (output (to-list output))
	  (depends-on (to-list depends-on)))
      `(set-target (make-target ,command 
				:name ,name
				:input ,input
				:output ,output
				:depends-on ,depends-on)))))

;;
;; Files
;;

(defstruct (file (:constructor %make-file) (:conc-name %file-))
  target
  depends)

(defun file-target (file)
  (%file-target file))

(defun file-depends (file)
  (double-list-head (%file-depends file)))

(defun file-add-dependency (file entity)
  (double-list-push entity (%file-depends file)))

(defun make-file (&optional target depends)
  (%make-file :target target :depends (make-double-list depends)))

;;
;; Context table
;;

(defstruct (context (:copier %copy-context))
  (targets-list (make-double-list nil))
  (targets (make-hash-table :test #'equal))
  (files (make-hash-table :test #'equal))
  (variables-list (make-double-list nil))
  (variables (make-hash-table :test #'equal))
  (tmp-names (make-hash-table :test #'equal)))

(defvar *context* (make-context))

(defun get-context () *context*)

(defun copy-context (&optional (context (get-context)))
  (make-context :targets-list (copy-double-list (context-targets-list context))
		:targets (copy-hash-table (context-targets context) :test #'equal)
		:files (copy-hash-table (context-files context) :test #'equal)
		:variables (copy-hash-table (context-variables context) :test #'equal)
		:variables-list (copy-double-list (context-variables-list context))
		:tmp-names (copy-hash-table (context-tmp-names context) :test #'equal)))

(defun get-file (name)
  (gethash name (context-files (get-context))))

(defun (setf get-file) (value name)
  (setf (gethash name (context-files (get-context))) value))

(defun get-target (name)
  (gethash name (context-targets (get-context))))

(defun (setf get-target) (value name)
  (setf (gethash name (context-targets (get-context))) value))

(defun get-variable (name)
  (gethash name (context-variables (get-context))))

(defun get-targets ()
  (double-list-head (context-targets-list (get-context))))

(defun get-variables ()
  (double-list-head (context-variables-list (get-context))))

(defun get-target-depends (target)
  (append (target-input target)
	  (target-depends-on target)))

(defun get-file-depends (file)
  (append (aif (file-target file) (list (target-key-name it)))
	  (file-depends file)))

(defun get-depends (entity)
  (typecase entity
    (target (get-target-depends entity))
    (null nil)
    (otherwise (acond ((get-target entity) (get-target-depends it))
		      ((get-file entity) (get-file-depends it))))))

(defun %map-depends (mapper reducer function entity recursive 
		     &optional (visited (make-hash-table :test #'equal)) denied)
  (when (target-p entity)
    (setf entity (target-key-name entity)))
  (push entity denied)
  (let ((depends (get-depends entity)))
    (mapc (lambda (entity) (when (find entity denied :test #'equal)
			     (bsdf-error "Circular dependency for target '~a' found" entity)))
	  depends)
    (when recursive (setf depends (remove-if (lambda (x) (gethash x visited)) depends)))
    (let ((mapped (funcall mapper function depends)))
      (setf (gethash entity visited) t)
      (when (and recursive depends)
	(setf mapped (apply reducer mapped
			    (mapcan (lambda (entity) 
				      (list (%map-depends mapper reducer function entity t visited denied)))
				    depends))))
      mapped)))

(defun map-depends (function entity &key recursive)
  (%map-depends #'mapcar #'append function entity recursive))

(defun mapc-depends (function entity &key recursive)
  (%map-depends #'mapc (lambda (&rest args) (first args)) function entity recursive))

;;
;; Table names
;;

(defun entity-type (entity)
  (typecase entity
    (target :target)
    (file :file)
    (variable :variable)
    (otherwise :empty)))

(defun type-name (type)
  (string-downcase (symbol-name type)))

(defun get-entity (name)
  (or (get-target name) (get-file name) (get-variable name)))

(defun name-error (name type entity-type)
  (let ((type-name (string-capitalize (type-name type))))
    (if (eq type entity-type)
	(bsdf-compilation-error "~a with name '~a' already exists" type-name name)
	(bsdf-compilation-error "~a name '~a' is already a ~a name" type-name name (type-name entity-type)))))

(defun check-name (name type)
  (awhen (get-entity name)
    (unless (and (eq type :file) (file-p it))
      (name-error name type (entity-type it))))
  (awhen (and (stringp name) (get-tmp-name name))
    (awhen (rest it) 
      (funcall it (gen-tmp-name (first (parse-tmp-string name)) it))))
  name)

(defun next-tmp-number (number)
  (if number (1+ number) 2))

(defun update-tmp-number (name func)
  (let ((value (gethash name (context-tmp-names (get-context)))))
    (dbind (number new-value) (funcall func value)
      (setf (gethash name (context-tmp-names (get-context))) new-value)
      number)))

(defun push-number (list callback &aux value)
  (labels ((do-push (list next)
	       (cond ((null list) (setf value nil) (list (cons nil callback)))
		     ((not (eql (first (first list)) next)) (setf value next) (cons (cons next callback) list))
		     (t (cons (first list) (do-push (rest list) (next-tmp-number (first (first list)))))))))
    (let ((new-list (do-push list (next-tmp-number nil))))
      (list value new-list))))

(defun gen-tmp-name (name &optional callback)
  (flet ((tmp-string (number)
	   (string+ (format nil "__~a" name)
		    (if number (format nil "_~a" number) ""))))
    (let ((tmp (tmp-string (update-tmp-number name (lambda (x) (push-number x callback))))))
      (if (get-entity tmp)
	  (gen-tmp-name name)
	  tmp))))

(defun parse-tmp-string (string) 
  (flet ((parse-number (string)
	   (multiple-value-bind (value length) (parse-integer string :junk-allowed t)
	     (if (= length (length string)) value))))
    (let ((prefix (subseq string 0 2))
	  (suffix (subseq string 2)))
      (if (not (equal prefix "__")) nil
	  (let ((index (or (search "_" suffix :from-end t) (length suffix))))
	    (let ((name (subseq suffix 0 index))
		  (number (if (< index (length suffix)) (parse-number (subseq suffix (1+ index))))))
	      (list name number)))))))

(defun get-tmp-name (name)
  (let ((value nil)
	(tmp-spec (parse-tmp-string name)))
    (when tmp-spec
      (dbind (name number) tmp-spec
	(update-tmp-number name (lambda (x) (awhen (find number x :key #'first) (setf value it)) (list nil x)))))
    value))

(defun free-tmp-name (name)
  (dbind (name number) (parse-tmp-string name)
    (update-tmp-number name 
		       (lambda (x) (list nil (remove number x :key #'first))))))

(defmacro with-tmp-name ((var name &optional callback) &body body)
  `(with-tmp-names ((,var ,name ,@(aif callback (list it)))) ,@body))

(defmacro with-tmp-names ((&rest specs) &body body)
  (flet ((setup-form (spec)
	   (dbind (var name &optional callback) spec
	     `(,var (gen-tmp-name ,name ,@(aif callback (list it))))))
	 (cleanup-form (spec)
	   `(free-tmp-name ,(first spec))))
    `(let (,@(mapcar #'setup-form specs))
       (unwind-protect (progn ,@body)
	 ,@(mapcar #'cleanup-form specs)))))

;;
;; Pushing to table
;;

(defun target-key-name (target)
  (or (target-name target)
      (if (target-command target) (copy-list (target-output target)))))

(defun push-target-to-list (target)
  (double-list-push target (context-targets-list (get-context))))

(defun push-target-to-table (target)
  (when-let (name (target-key-name target))
    (check-name name :target)
    (setf (get-target name) target)))

(defun push-file-to-table (name target)
  (check-name name :file)
  (let* ((file (or (get-file name) (make-file)))
	 (file-target (file-target file))
	 (depends (file-depends file)))
    (when (and target (target-command target) file-target)
      (bsdf-compilation-error (lines* "Found two targets generating '~a':"
				      "'~a', '~a'")
			      name (target-print-name file-target) (target-print-name target)))
    (when target
      (if (target-command target) (setf file-target target)
	  (setf depends (append depends (target-input target) (target-depends-on target)))))
    (setf (get-file name) (make-file file-target depends))))

(defun set-target (target)
  (push-target-to-table target)
  (mapc (lambda (file) (push-file-to-table file target)) (target-output target))
  (mapc (lambda (file) (push-file-to-table file nil)) (target-input target))
  (when (and (null (target-command target))
	     (or (null (target-input target))
		 (null (target-output target))))
    (bsdf-compilation-warn "Target '~a' is empty" (target-print-name target)))
  (push-target-to-list target)
  target)

(defun push-var-to-list (var)
  (double-list-push var (context-variables-list (get-context))))

(defun push-var-to-table (var)
  (let ((name (variable-name var)))
    (check-name name :variable)
    (setf (gethash name (context-variables (get-context))) var)))

(defun set-variable (var)
  (push-var-to-table var)
  (push-var-to-list var)
  var)

(defmacro defvariable (name expression &key (type t))
  `(set-variable (make-variable ',name ,expression :type ',type)))

(defun get-variable-expression (name)
  (unless (symbolp name)
    (bsdf-compilation-error "Wrong variable name '~a'" name))
  (let ((variable (get-variable name)))
    (unless variable (bsdf-compilation-error "Variable '~a' does not exists" name))
    (variable-expression variable)))
      
(defmethod bsdf-expressions::bsdf-type-of ((value symbol))
  (if (bsdf-expressions::bsdf-variable-p value)
      (aif (get-variable value)
	   (variable-type it)
	   t)
      :enum))

(defmethod bsdf-expressions::bsdf-atom-value ((atom symbol))
  (if (bsdf-expressions::bsdf-variable-p atom)
      (expression-value (get-variable-expression atom))
      (call-next-method)))

(defmethod bsdf-expressions::bsdf-atom-dependencies ((atom symbol))
  (if (bsdf-expressions::bsdf-variable-p atom)
      (list (list atom) () ())
      (call-next-method)))

(bsdf-defmacro $ (name)
  (get-variable-expression name))

(defun add-dependency (target dep)
  (unless (target-p target) (setf target (get-target target)))
  (when target (double-list-push dep (%target-depends-on target))))

(defun add-input (target input)
  (unless (target-p target) (setf target (get-target target)))
  (when target
    (double-list-push input (%target-input target))
    (unless (target-key-name target)
      (mapc (lambda (file) (file-add-dependency (get-file file) input)) (target-output target)))))

(defun add-output (target output)
  (unless (target-p target) (setf target (get-target target)))
  (when target
    (let ((name (target-key-name target)))
      (double-list-push output (%target-output target))
      (unless (equal (target-key-name target) name)
	(remhash name (context-targets (get-context)))
	(setf (gethash (target-key-name target) (context-targets (get-context))) target))
      (push-file-to-table output target))))


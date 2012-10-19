(in-package #:burning-bsdf-targets)

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
;; Target table
;;

(defstruct targets
  (list (make-double-list nil))
  (table (make-hash-table :test #'equal))
  (files (make-hash-table :test #'equal)))

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

(defvar *targets* (make-targets))

(defun copy-targets-table (&optional (table *targets*))
  (make-targets :list (copy-double-list (targets-list table))
		:table (copy-hash-table (targets-table table) :test #'equal)
		:files (copy-hash-table (targets-files table) :test #'equal)))

(defun get-file (name)
  (gethash name (targets-files *targets*)))

(defun (setf get-file) (value name)
  (setf (gethash name (targets-files *targets*)) value))

(defun get-target (name)
  (gethash name (targets-table *targets*)))

(defun (setf get-target) (value name)
  (setf (gethash name (targets-table *targets*)) value))

(defun get-targets ()
  (double-list-head (targets-list *targets*)))

(defun get-file-targets ()
  (let ((targets ()))
    (maphash (lambda (key value)
	       (awhen (file-depends value)
		 (push (make-target nil :input it :output key) targets)))
	     (targets-files *targets*))
    targets))

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
;; Pushing targets to table
;;

(defun target-key-name (target)
  (or (target-name target)
      (if (target-command target) (copy-list (target-output target)))))

(defun push-to-list (target)
  (double-list-push target (targets-list *targets*)))

(defun push-to-table (target)
  (when-let (name (target-key-name target))
    (when (get-target name)
      (bsdf-compilation-error "Target with name '~a' already exists" (target-print-name target)))
    (when (get-file name)
      (bsdf-compilation-error "Target name '~a' is already a file name" name))
    (setf (get-target name) target)))

(defun push-file-to-table (name target)
  (when (get-target name)
    (bsdf-compilation-error "File name '~a' is already a target name" name))
  (let* ((file (or (get-file name) (make-file)))
	 (file-target (file-target file))
	 (depends (file-depends file)))
    (when (and (target-command target) file-target)
      (bsdf-compilation-error (lines* "Found two targets generating '~a':"
				      "'~a', '~a'")
			      name (target-print-name file-target) (target-print-name target)))
    (cond ((target-command target) (setf file-target target))
	  ((or (target-input target) (target-depends-on target)) 
	   (setf depends (append depends (target-input target) (target-depends-on target)))))
    (setf (get-file name) (make-file file-target depends))))

(defun set-target (target)
  (push-to-list target)
  (push-to-table target)
  (mapc (lambda (file) (push-file-to-table file target)) (target-output target))
  (when (and (null (target-command target))
	     (or (null (target-input target))
		 (null (target-output target))))
    (bsdf-compilation-warn "Target '~a' is empty" (target-print-name target)))
  target)

;;
;; Removing targets from table
;;

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
	(remhash name (targets-table *targets*))
	(setf (gethash (target-key-name target) (targets-table *targets*)) target))
      (push-file-to-table output target))))


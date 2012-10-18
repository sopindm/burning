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
  (%target-input target))

(defun target-output (target)
  (%target-output target))

(defun target-depends-on (target)
  (%target-depends-on target))

(defun (setf target-depends-on) (value target)
  (setf (%target-depends-on target) value))

(defun target-print-name (target)
  (let ((name (or (target-name target) (target-output target))))
    (if (listp name)
	(format nil "~{~a~^ ~}" name)
	name)))

(defun make-target (command &key name input output depends-on)
  (when (not (listp output))
    (setf output (list output)))
  (when (and (not name) (not output))
    (bsdf-compilation-error "No output file or name in target"))
  (let ((target (%make-target :name name
			      :command command 
			      :input (if (listp input) input (list input))
			      :output output
			      :depends-on (if (listp depends-on) depends-on (list depends-on)))))
    (when (and command (not (functionp command)))
      (bsdf-compilation-error "Wrong command ~a in target '~a'" command (target-print-name target)))
    target))

;;
;; Target table
;;

(defstruct targets
  (list (make-double-list))
  (table (make-hash-table :test #'equal))
  (files (make-hash-table :test #'equal)))

(defstruct file
  target
  depends)

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

(defun get-target-depends (target)
  (append (target-input target)
	  (target-depends-on target)))

(defun get-file-depends (file)
  (append (aif (file-target file) (list (target-key-name it)))
	  (file-depends file)))

(defun get-depends (entity)
  (acond ((get-target entity) (get-target-depends it))
	 ((get-file entity) (get-file-depends it))))

(defun %map-depends (mapper reducer function entity recursive 
		     &optional (visited (make-hash-table :test #'equal)) denied)
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
      (if (target-command target) (target-output target))))

(defun push-to-list (target)
  (when (target-key-name target)
    (double-list-push target (targets-list *targets*))))

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
    (setf (get-file name) (make-file :target file-target :depends depends))))

(defun set-target (target)
  (push-to-list target)
  (push-to-table target)
  (mapc (lambda (file) (push-file-to-table file target)) (target-output target))
  (when (and (null (target-command target))
	     (or (null (target-input target))
		 (null (target-output target))))
    (bsdf-compilation-warn "Target '~a' is empty" (target-print-name target)))
  target)



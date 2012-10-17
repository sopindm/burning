(in-package #:burning-bsdf)

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

;;
;; Pushing targets to table
;;

(defun push-to-list (target)
  (double-list-push target (targets-list *targets*)))

(defun push-to-table (target)
  (let ((name (or (target-name target)
		  (if (not (target-command target)) (gensym))
		  (target-output target))))
    (when (get-target name)
      (bsdf-compilation-error "Target with name '~a' already exists" (target-print-name target)))
    (when (get-file name)
      (bsdf-compilation-error "Target name '~a' is already a file name" name))
    (setf (get-target name) target)))

(defun push-file-to-table (name target)
  (when (get-target name)
    (bsdf-compilation-error "File name '~a' is already a target name" name))
  (let* ((file (get-file name))
	 (file-target (first file))
	 (depends (rest file)))
    (when (and (target-command target) file-target)
      (bsdf-compilation-error (lines* "Found two targets generating '~a':"
				      "'~a', '~a'")
			      name (target-print-name file-target) (target-print-name target)))
    (cond ((target-command target) (setf file-target target))
	  ((target-input target) (setf depends (append depends (target-input target)))))
    (setf (get-file name) (cons file-target depends))))

(defun set-target (target)
  (push-to-list target)
  (push-to-table target)
  (mapc (lambda (file) (push-file-to-table file target)) (target-output target))
  (when (and (null (target-command target))
	     (or (null (target-input target))
		 (null (target-output target))))
    (bsdf-compilation-warn "Target '~a' is empty" (target-print-name target)))
  target)



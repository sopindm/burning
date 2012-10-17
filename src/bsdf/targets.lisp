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

(defun target-key-name (target)
  (or (target-name target) (if (> (length (target-output target)) 1) 
			       (target-output target) 
			       (first (target-output target)))))

(defun target-print-name (target)
  (let ((name (target-key-name target)))
    (if (listp name)
	(format nil "~{~a~^ ~}" name)
	name)))

(defun make-target (command &key name input output depends-on)
  (when (not (listp output))
    (setf output (list output)))
  (when (and (not name) (not output))
    (bsdf-compilation-error "No output file or name for target"))
  (let ((target (%make-target :name name
			      :command command 
			      :input (if (listp input) input (list input))
			      :output output
			      :depends-on (if (listp depends-on) depends-on (list depends-on)))))
    (when (and command (not (functionp command)))
      (bsdf-compilation-error "Wrong command ~a for target '~a'" command (target-print-name target)))
    target))
    

;;
;; Target table
;;

(defvar *targets* ())

(defun copy-targets-table (&optional (table *targets*))
  (copy-tree table))

(defun find-output (file)
  (labels ((find1 (target)
	     (and (target-command target)
		  (find file (target-output target) :test #'equal)))
	   (do-find (targets)
	     (cond ((null targets) nil)
		   ((find1 (rest (first targets))) (rest (first targets)))
		   (t (do-find (rest targets))))))
    (do-find *targets*)))

(defun set-target (target)
  (let ((name (target-key-name target)))
    (awhen (get-target name)
      (when (or (target-name target) 
		(target-name it)
		(and (target-command target) (target-command it)))
	(bsdf-compilation-error "Target with name '~a' already exists" (target-print-name target))))
    (flet ((check-output (file)
	     (awhen (and (target-command target) (find-output file))
	       (bsdf-compilation-error (lines* "Found two targets generating '~a':"
					       "'~a', '~a'")
				       file (target-print-name it) (target-print-name target)))))
      (mapc #'check-output (target-output target)))
    (setf *targets* (acons (if (or (target-command target) (target-name target)) name (gensym)) target *targets*)))
  target)

(defun get-target (name)
  (rest (assoc name *targets* :test #'equal)))

(defun get-targets ()
  (let ((targets (mapcar #'rest *targets*)))
    (reverse targets)))

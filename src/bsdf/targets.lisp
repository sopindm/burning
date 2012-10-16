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

(defun make-target (command &key name input output depends-on)
  (when (not (listp output))
    (setf output (list output)))
  (when (and (not name) (not output))
    (bsdf-compilation-error "No output file or name for target"))
  (when (and command (not (functionp command)))
    (bsdf-compilation-error "Wrong command ~a for target '~a'" command (or name (first output))))
  (%make-target :name name
		:command command 
		:input (if (listp input) input (list input))
		:output output
		:depends-on (if (listp depends-on) depends-on (list depends-on))))

;;
;; Target table
;;

(defvar *targets* ())

(defun copy-targets-table (&optional (table *targets*))
  (copy-tree table))

(defun set-target (target)
  (setf *targets* (acons (target-key-name target) target *targets*))
  target)

(defun get-target (name)
  (rest (assoc name *targets* :test #'equal)))

(defun get-targets ()
  (let ((targets (mapcar #'rest *targets*)))
    (reverse targets)))

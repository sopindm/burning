(in-package :burning-threads)

(defstruct cv
  lock
  value)

#+ccl
(defstruct ccl-cv
  waiters
  wait-semaphore
  accept-semaphore)

(defun do-make-cv ()
  #+clisp (mt:make-exemption)
  #+sbcl (sb-thread:make-waitqueue)
  #+ccl
  (make-ccl-cv :waiters 0
	       :wait-semaphore (make-semaphore 0)
	       :accept-semaphore (make-semaphore 0)))

(defun make-condition-variable (&optional (lock (make-mutex)))
  (make-cv :lock lock
	   :value (do-make-cv)))

(defmacro with-condition-variable (variable &body body)
  `(progn 
     (grab-mutex (cv-lock ,variable))
     ,@body
     (release-mutex (cv-lock ,variable))))

(defun do-wait-condition (variable mutex)
  #+clisp
  (mt:exemption-wait variable mutex)
  #+sbcl
  (sb-thread:condition-wait variable mutex)
  #+ccl
  (progn
    (incf (ccl-cv-waiters variable))
    (release-mutex mutex)
    (wait-semaphore (ccl-cv-wait-semaphore variable))
    (signal-semaphore (ccl-cv-accept-semaphore variable))
    (grab-mutex mutex)))

(defmacro wait-condition (condition variable)
  `(loop (if (funcall ,condition)
	     (return)
	     (do-wait-condition (cv-value ,variable) (cv-lock ,variable)))))

(defun signal-condition (variable)
  #+clisp
  (mt:exemption-signal (cv-value variable))
  #+sbcl
  (sb-thread:condition-notify (cv-value variable))
  #+ccl
  (let ((variable (cv-value variable)))
    (when (> (ccl-cv-waiters variable) 0)
      (decf (ccl-cv-waiters variable))
      (signal-semaphore (ccl-cv-wait-semaphore variable))
      (wait-semaphore (ccl-cv-accept-semaphore variable)))))
    
(defun broadcast-condition (variable)
  #+clisp
  (mt:exemption-broadcast (cv-value variable))
  #+sbcl
  (sb-thread:condition-broadcast (cv-value variable))
  #+ccl
  (let ((variable (cv-value variable))
	(waiters (ccl-cv-waiters variable)))
    (dotimes (i waiters)
      (signal-semaphore (ccl-cv-wait-semaphore variable))
      (decf (ccl-cv-waiters variable)))
    (dotimes (i waiters)
      (wait-semaphore (ccl-cv-accept-semaphore variable)))))
    
    
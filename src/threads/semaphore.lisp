(in-package :burning-threads)

(defun no-semaphores-error ()
  (error "No semaphores implementation for ~a." (lisp-implementation-type)))

(defun make-semaphore (value)
  #+sbcl
  (sb-thread:make-semaphore :count value)
  #+ccl
  (let ((semaphore (ccl:make-semaphore)))
    (dotimes (i value)
      (ccl:signal-semaphore semaphore))
    semaphore)
  #-(or sbcl ccl)
  (no-semaphores-error))

(defun wait-semaphore (semaphore)
  #+sbcl
  (sb-thread:wait-on-semaphore semaphore)
  #+ccl
  (ccl:wait-on-semaphore semaphore)
  #-(or sbcl ccl)
  (no-semaphores-error))

(defun signal-semaphore (semaphore)
  #+sbcl
  (sb-thread:signal-semaphore semaphore)
  #+ccl
  (ccl:signal-semaphore semaphore)
  #-(or sbcl ccl)
  (no-semaphores-error))
;;;
;;; Base threading routines
;;;

(in-package :burning-threads)

(defun no-threads-error ()
  (error "No threads implementation for ~a." (lisp-implementation-type)))

(defun current-thread ()
  #+clisp
  (mt:current-thread)
  #+sbcl
  sb-thread:*current-thread*
  #+ccl
  ccl:*current-process*)

;; Makes new thread 

(defun make-thread (function &key (name "unnamed thread") (arguments ()))
  #+clisp
  (mt:make-thread #'(lambda () (apply function arguments)) :name name)
  #+sbcl
  (sb-thread:make-thread function :name name :arguments arguments)
  #+ccl
  (let ((process (ccl:make-process name)))
    (apply #'ccl:process-preset process function arguments)
    (ccl:process-enable process)
    process)
  #-(or clisp sbcl ccl)
  (no-threads-error))

(defun spawn-thread (function &rest args)
  (make-thread function :arguments args))

;;Waits for 'thread'

(defun wait-thread (thread)
  #+clisp
  (loop
     (if (mt:thread-active-p thread)
	 (mt::thread-yield)
	 (return)))
  #+sbcl
  (sb-thread:join-thread thread)
  #+ccl
  (ccl:join-process thread)
  #-(or clisp sbcl ccl)
  (no-threads-error))

(defun wait-threads (&rest threads)
  (dolist (thread threads)
    (wait-thread thread)))

(defun thread-yield ()
  #+clisp
  (mt:thread-yield)
  #+sbcl
  (sb-thread:thread-yield)
  #+ccl
  (ccl:process-allow-schedule)
  #-(or clisp sbcl ccl)
  (no-threads-error))

(defmacro with-timeout ((seconds &body at-timeout) &body body)
  #+clisp
  `(mt:with-timeout (,seconds ,@at-timeout) ,@body)
  #+sbcl
  (let ((escape-sym (gensym))
	(timer-sym (gensym)))
    `(block ,escape-sym
       (let ((,timer-sym (sb-ext:make-timer #'(lambda () (return-from ,escape-sym (progn ,@at-timeout))))))
	 (sb-ext:schedule-timer ,timer-sym ,seconds)
	 (unwind-protect (progn ,@body)
	   (sb-ext:unschedule-timer ,timer-sym)))))
  #+ccl
  (let ((escape-sym (gensym))
        (semaphore-sym (gensym)))
    `(let ((,semaphore-sym (ccl:make-semaphore)))
       (catch ',escape-sym
         (ccl:process-run-function 
             "WITH-TIMEOUT timer"
           #'(lambda (process semaphore seconds)
               (unless (ccl:timed-wait-on-semaphore semaphore seconds)
                 (ccl:process-interrupt
                  process
                  #'(lambda ()
                      (ignore-errors
                       (throw ',escape-sym
                         (progn ,@at-timeout)))))))
           ccl:*current-process*
           ,semaphore-sym
           ,seconds)
         (unwind-protect (progn ,@body)
           (ccl:signal-semaphore ,semaphore-sym)))))  
  #-(or clisp sbcl ccl)
  (no-threads-error))

(defun thread-random ()
  (case (random 3)
    (0 (thread-yield))
    (1 (sleep 0.01))
    (2 ())))

(in-package :burning-threads-test)

(defcase condition-test)

(deftest condition-test simple-condition-test ()
  (let ((empty-condition (make-condition-variable))
	(threads ())
	(work-queue ())
	(read 0)
	(written 0))
    (flet ((consumer ()
	     (dotimes (i 100)
	       (with-condition-variable empty-condition
		 (wait-condition #'(lambda () (not (null work-queue))) empty-condition)
		 (incf read (first work-queue))
		 (setf work-queue (rest work-queue)))))
	   (producer ()
	     (dotimes (i 100)
	       (with-condition-variable empty-condition
		 (let ((value (random 100)))
		   (incf written value)
		   (push value work-queue)
		   (signal-condition empty-condition))))))
      (dotimes (i 10)
	(push (spawn-thread #'consumer) threads)
	(push (spawn-thread #'producer) threads))
      (apply #'wait-threads threads)
      (!null work-queue)
      (!= read  written))))

(deftest condition-test forward-test ()
  (let ((condition (make-condition-variable))
	(semaphore nil)
	(result ""))
    (flet ((consumer ()
	       (with-timeout (0.1 (setf result "deadlock"))
		 (with-condition-variable condition
		   (wait-condition #'(lambda () semaphore) condition)
		   (setf result "ok"))))
	   (producter ()
	     (with-timeout (0.1)
	       (with-condition-variable condition
		 (signal-condition condition)
		 (setf semaphore t)
		 (wait-condition #'(lambda () nil) condition)))))
      (wait-threads (spawn-thread #'consumer) (spawn-thread #'producter))
      (!equal result "ok"))))
      



	       
    

(in-package :burning-threads-test)

(defcase monitor-test)

(defmonitor simple-monitor
    field)

(defmonitor (incf-monitor (:constructor mk-monitor))
    (value 0))

(defmfun mincf (monitor value)
  (incf (slot-value monitor 'value) value))

(defmfun mvalue (monitor)
  (slot-value monitor 'value))

(deftest monitor-test incf-monitor-test ()
  (let ((monitor (mk-monitor)))
    (flet ((monitor-client ()
	     (dotimes (i 10)
	       (thread-random)
	       (mincf monitor 2))))
      (let ((threads ()))
	(dotimes (i 500)
	  (push (spawn-thread #'monitor-client) threads))
	(apply #'wait-threads threads)))
    (!= (mvalue monitor) 10000)))

(defmonitor (con-queue (:constructor mk-queue))
  (queue (make-array 10))
  (length 0)
  (queue-not-empty :condition)
  (queue-not-full :condition))

(defmfun queue-push (queue value)
  (with-slots (queue length queue-not-empty queue-not-full) queue
    (wait-condition #'(lambda () (< length 10)) queue-not-full)
    (setf (aref queue length) value)
    (signal-condition queue-not-empty)
    (incf length)))

(defmfun queue-pop (queue)
  (with-slots (queue length queue-not-empty queue-not-full) queue
    (wait-condition #'(lambda () (> length 0)) queue-not-empty)
    (signal-condition queue-not-full)
    (decf length)
    (aref queue length)))

(deftest monitor-test queue-test ()
  (let ((queue (mk-queue))
	(read 0)
	(written 0)
	(read-lock (make-mutex))
	(write-lock (make-mutex)))
    (flet ((consumer ()
	     (dotimes (i 100)
	       (let ((value (queue-pop queue)))
		 (with-mutex read-lock
		   (incf read value)))))
	   (producer ()
	     (dotimes (i 100)
	       (let ((value (random 100)))
		 (with-mutex write-lock
		   (incf written value))
		 (queue-push queue value)))))
      (let ((threads ()))
	(dotimes (i 10)
	  (push (spawn-thread #'consumer) threads)
	  (push (spawn-thread #'producer) threads))
	(apply #'wait-threads threads)))
    (!= read written)))
      





  
  
	   
    
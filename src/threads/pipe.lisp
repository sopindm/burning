(in-package :burning-threads)

(defmonitor (pipe (:constructor make-pipe))
  (head ())
  (tail ())
  (queue-not-empty :condition))

(defmfun write-pipe (pipe value)
  (with-slots (head tail queue-not-empty) pipe
    (if (not head)
	(progn (setf head (cons value nil))
	       (setf tail head))
	(progn (setf (rest tail) (cons value nil))
	       (setf tail (rest tail))))
    (signal-condition queue-not-empty)))

(defmfun read-pipe (pipe)
  (wait-condition #'(lambda () (not (null (pipe-head pipe)))) (pipe-queue-not-empty pipe))
  (pop (pipe-head pipe)))

(defun close-pipe (pipe)
  (write-pipe pipe 'close-signal))

(defmacro with-pipe (name &body body)
  (let ((pipe-expr (if (atom name) '(make-pipe) (second name)))
	(pipe-name (if (atom name) name (first name))))
    `(progn
       (let ((,pipe-name ,pipe-expr))
	 (unwind-protect
	      (progn ,@body)
	   (close-pipe ,name))))))

(defmacro while-pipe ((variable pipe) &body body)
  (let ((read-sym (gensym))
	(pipe-sym (gensym)))
    `(let ((,pipe-sym ,pipe))
       (loop 
	  (let ((,read-sym (read-pipe ,pipe-sym)))
	    (if (eq ,read-sym 'close-signal)
		(return)
		(let ((,variable ,read-sym))
		  ,@body)))))))

	

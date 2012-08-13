(in-package :burning-threads-test)

(defcase base-test)

(defun range (first last &key (accending t))
  (cond
    ((= first last) (list first))
    (accending (cons first (range (1+ first) last :accending t)))
    (t (cons last (range first (1- last) :accending nil)))))

(defun set= (set1 set2)
  (let ((intersection (intersection set1 set2)))
    (and (= (length set1) (length intersection)) 
	 (= (length set2) (length intersection)))))

(defmacro !set= (set1 set2)
  `(equal-check ,set1 ,set2 set=))

(deftest base-test thread-creating-test ()
  (let ((threaded-queue ()))
    (let ((thread (make-thread #'(lambda () (push 'thread threaded-queue)))))
      (wait-thread thread)
      (!equal threaded-queue '(thread)))))

(deftest base-test threads-with-sleep-test ()
  (let ((result-queue ()))
    (let* ((thread1 (spawn-thread #'(lambda () (sleep 0.01) (push 0 result-queue))))
	   (thread2 (spawn-thread #'(lambda () (push 1 result-queue)))))
      (wait-threads thread1 thread2)
      (!equal result-queue '(0 1)))))

(deftest base-test timeout-test ()
  (let ((result 0))
    (with-timeout (0.1 (setf result 1))
      (sleep 0.2)
      (setf result 2))
    (!= result 1)))
    
(in-package #:cgen-example)

(defmacro compare (a b then-close &optional else-close)
  `(if (> ,a ,b) ,then-close ,@(if else-close (list else-close) nil)))

(defmacro swap (a-addr b-addr)
  `(let ((tmp (deref ,a-addr)))
     (setf (deref ,a-addr) (deref ,b-addr))
     (setf (deref ,b-addr) tmp)))

(defmacro compare-and-swap (a b)
  (with-gensyms (a-addr b-addr)
    `(let ((,a-addr (addr ,a))
	   (,b-addr (addr ,b)))
       (compare (deref ,a-addr) (deref ,b-addr)
		(swap ,a-addr ,b-addr)))))

(defun sort (array (array int) length int)
  (for ((i (1- length) 1 :step -1)
	(j 0 i))
     (compare-and-swap (aref array j) (aref array (1+ j)))))

(defun main ()
  (let ((array #(7 3 5 2 6 3 5 7)))
    (sort array)
    (foreach (i array)
      (format "~a" i))
    (format "~%")))

(foreach (i array)
  (incf sum i))

(do* ((#:index 0 (incf #:index))
      (i (aref array #:index)))
     (< #:index (length-of array))
   (incf sum i))
        
  

    

      
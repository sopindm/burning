(in-package #:burning-cgen-test)
(in-language :c)

#G
(include "stdio.h")

(def-cg-macro compare (a b then-close &optional else-close)
  `(if (> ,a ,b) ,then-close ,@(if else-close (list else-close) nil)))

(def-cg-macro swap (a-addr b-addr)
  `(let ((tmp (deref ,a-addr)))
     (setf (deref ,a-addr) (deref ,b-addr))
     (setf (deref ,b-addr) tmp)))

(def-cg-macro compare-and-swap (a b)
  (with-gensyms (a-addr b-addr)
    `(let ((,a-addr (addr ,a))
	   (,b-addr (addr ,b)))
       (compare (deref ,a-addr) (deref ,b-addr)
		(swap ,a-addr ,b-addr)))))

#G 
(defun sort (array length)
  (declare (type (array int) array)
	   (type int length))
  (for ((i :from (1- length) :downto 1))
    (for ((j :from 0 :lessto i))
      (compare-and-swap (aref array j) (aref array (1+ j))))))

#G
(defun main ()
  (let ((array (make-array :element-type int :initial-contents (7 3 5 2 6 3 5 7))))
    (sort array (length array))
    (for ((i :from 0 :lessto (length array)))
      (format "~a" (aref array i)))
    (format "~%")))
    

      
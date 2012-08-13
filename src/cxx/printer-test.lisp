(in-package :burning-cxx-test)

(defcase printer-test)

(defmacro defptest (name (&rest body) result)
  (let ((base-stream (gensym)))
    `(deftest printer-test ,name ()
       (let* ((,base-stream (make-string-output-stream))
	      (stream (make-cxx-stream :stream ,base-stream)))
	 ,@body
	 (!equal (get-output-stream-string ,base-stream) ,result)))))

(defptest simple-write
  ((stream-write stream "bla-bla"))
  "bla-bla")

(defun lines (&rest args)
  (cond 
    ((null args) t)
    ((null (rest args)) (format nil "~a" (first args)))
    (t (concatenate 'string
		    (format nil "~a~%" (first args))
		    (apply #'lines (rest args))))))

(defptest newline-test
    ((stream-write stream "bla-bla")
     (stream-newline stream)
     (stream-write stream "bla-bla"))
  (lines "bla-bla" "bla-bla"))

(defptest indentation-test
    ((stream-write stream "bla")
     (stream-indent stream 2)
     (stream-write stream "bla")
     (stream-newline stream)
     (stream-write stream "bla")
     (stream-newline stream)
     (stream-indent stream 2)
     (stream-write stream "bla")
     (stream-indent-by-position stream 2)
     (stream-write stream "bla")
     (stream-newline stream)
     (stream-write stream "bla"))
  (lines "blabla"
	 "  bla"
	 "    bla  bla"
	 "         bla"))

(defptest unindent-test
    ((stream-write stream "{")
     (stream-indent stream 2)
     (stream-newline stream)
     (stream-write stream "some line")
     (stream-unindent stream)
     (stream-newline stream)
     (stream-write stream "}"))
  (lines "{ "
	 "  some line"
	 "}"))

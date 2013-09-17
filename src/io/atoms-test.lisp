(in-package #:burning-io-test)

(in-case atom-tests)

(defmacro ?read= ((&rest inputs) (&rest values) &optional type)
  (labels ((?read1 (input value)
	       (flet ((make-input (input) 
			(if (listp input) input (list input)))
		      (make-args (type)
			(if type `(,type))))
		 `(?eql (stream-read (make-in-memory-input-stream ',(make-input input)) ,@(make-args type))
			,value))))
    `(progn ,@(mapcar #'?read1 inputs values))))

(defmacro ?write= ((&rest values) (&rest outputs) &optional type)
  (flet ((?write1 (value output)
	   (flet ((make-output (output) 
		    (if (listp output) output (list output))))
	     `(?equal (with-output-to-sequence (stream :as-list t)
			(stream-write stream ,value ,type))
		      ',(make-output output)))))
    `(progn ,@(mapcar #'?write1 values outputs))))

(deftest int-reading
  (?read= (0 1 2 3 255 254 253) (0 1 2 3 -1 -2 -3) :int8)
  (?read= (0 1 2 3 255 254 245) (0 1 2 3  255 254 245) :uint8)
  (?read= ((2 0) (4 0) (255 255) (127 255) (128 0)) (512 1024 -1 32767 -32768) :int16)
  (?read= ((2 0) (4 0) (255 255) (127 255) (128 0)) (512 1024 65535 32767 32768) :uint16))

(deftest int-writing
  (?write= (1 2 3 0 -1) (1 2 3 0 255) :int8)
  (?write= (1 2 3 0 255) (1 2 3 0 255) :uint8))
  
  

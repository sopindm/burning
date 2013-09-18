(in-package #:burning-protobuf-test)

(in-case atoms-test)

(defmacro ?write= ((value tag type) result)
  (let ((output-var (gensym)))
    `(?equal (with-output-to-sequence (,output-var :as-list t)
	       (protobuf-write ,output-var ,value ,tag ,type))
	     ,result)))

(defmacro ?read= (value tag result)
  (let ((input-var (gensym))
	(value-var (gensym))
	(tag-var (gensym)))
    `(multiple-value-bind (,value-var ,tag-var) (with-input-from-sequence (,input-var ,value)
						  (protobuf-read ,input-var))
       (?equal ,value-var ,result)
       (?equal ,tag-var ,tag))))

(deftest varint-writing
  (?write= (123 1 :varint) '(8 123))
  (?write= (1024 5 :varint) '(40 128 8))
  (?write= (150 18 :varint) '(144 150 1))
  (?write= (-1 1 :varint) '(8 255 255 255 255 255 255 255 255 255 1)))

(deftest varint-reading
  (?read= '(8 123) 1 123)
  (?read= '(40 128 8) 5 1024)
  (?read= '(144 150 1) 18 150)
  (?read= '(8 255 255 255 255 255 255 255 255 255 1) 1 -1))
  
(deftest fixnum-writing
  (?write= (123 7 :fixnum32) '(61 0 0 0 123))
  (?write= (-1 2 :fixnum32) '(21 255 255 255 255))
  (?write= (1025 1 :fixnum64) '(9 0 0 0 0 0 0 4 1))
  (?write= (-5 3 :fixnum64) '(25 255 255 255 255 255 255 255 251)))

(deftest fixnum-reading
  (?read= '(61 0 0 0 123) 7 123)
  (?read= '(21 255 255 255 255) 2 -1)
  (?read= '(9 0 0 0 0 0 0 4 1) 1 1025)
  (?read= '(25 255 255 255 255 255 255 255 251) 3 -5))

(deftest length-delimited-writing
  (?write= (#(1 2 3 4 5) 3 :length-delimited) '(26 5 1 2 3 4 5)))

(deftest length-delimited-reading
  (?read= '(42 8 1 7 2 3 4 3 2 4) 5 '(1 7 2 3 4 3 2 4)))


  

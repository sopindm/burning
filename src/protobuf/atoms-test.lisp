(in-package #:burning-protobuf-test)

(in-case atoms-test)

(defmacro ?write= ((value tag type) result)
  (let ((output-var (gensym)))
	`(?equal (with-output-to-sequence (,output-var :as-list t)
			   (protobuf-write ,output-var ,value ,tag ,type))
			 ,result)))

(deftest varint-writing
  (?write= (123 1 :int) '(8 123))
  (?write= (1024 5 :int) '(40 128 8))
  (?write= (150 18 :int) '(144 150 1))
  (?write= (-1 1 :int) '(8 255 255 255 255 255 255 255 255 255 1)))
  


  

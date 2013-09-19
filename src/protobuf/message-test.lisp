(in-package #:burning-protobuf-test)

(in-case message-tests)

(defmessage simple-message ()
  (a 1 :int32)
  (b 2 :float32)
  (c 3 :int64 :optional t))

(deftest making-message
  (let ((message (make-instance 'simple-message 
								:a 12
								:b 32.12)))
	(?t (typep message 'message))
	(?equal (type-of message) 'simple-message)
	(?equal (simple-message-a message) 12)
	(?equal (simple-message-b message) 32.12)
	(?null (slot-boundp message 'c))
	(?equal (message-slot-tag 'simple-message 'a) 1)
	(?equal (message-slot-tag 'simple-message 'b) 2)
	(?equal (message-slot-tag 'simple-message 'c) 3)
	(?equal (message-tag-slot 'simple-message 1) 'a)
	(?equal (message-tag-slot 'simple-message 2) 'b)
	(?equal (message-tag-slot 'simple-message 3) 'c)
	(?null (message-slot-optional-p 'simple-message 'a))
	(?null (message-slot-optional-p 'simple-message 'b))
	(?t (message-slot-optional-p 'simple-message 'c))
	(?equal (message-slot-type 'simple-message 'a) :int32)
	(?equal (message-slot-type 'simple-message 'b) :float32)
	(?equal (message-slot-type 'simple-message 'c) :int64)))

(defmacro ?message-write= (value result)
  `(?equal (with-output-to-sequence (output :as-list t)
			 (protobuf-write-message output ,value))
		   ,result))

(deftest writing-message
  (let ((message (make-instance 'simple-message :a 42 :b -2 :c 15)))
	(?message-write= message '(9 8 42 21 192 0 0 0 24 15)))
  (let ((message (make-instance 'simple-message :a 42 :b -2)))
	(?message-write= message '(7 8 42 21 192 0 0 0))))

(deftest reading-message
  (with-input-from-sequence (input '(9 8 42 21 192 0 0 0 24 15))
	(let ((message (protobuf-read-message input 'simple-message)))
	  (?equal (simple-message-a message) 42)
	  (?equal (simple-message-b message) -2.0)
	  (?equal (simple-message-c message) 15)))
  (with-input-from-sequence (input '(8 8 128 1 21 192 0 0 0))
	(let ((message (protobuf-read-message input 'simple-message)))
	  (?equal (simple-message-a message) 128)
	  (?equal (simple-message-b message) -2.0)
	  (?null (slot-boundp message 'c)))))

(deftest writing-message-without-required-fields
  (let ((message (make-instance 'simple-message :c -2)))
	(?error (protobuf-write-message (make-in-memory-output-stream) message)
			"Wrong message ~a. Missed required fields ~a, ~a" message 'a 'b)))

(deftest reading-message-without-required-fields
  (with-input-from-sequence (input '(7 21 192 0 0 0 24 15))
	(?condition (protobuf-read-message input 'simple-message) simple-error)))

(defmessage message-with-enum ()
  (a 1 :int32)
  (:enum e (value1 1) (value2 129) (value3 4))
  (b 2 e))

(deftest enum-fields-in-message
  (?message-write= (make-instance 'message-with-enum :a 123 :b 'value2)
				   '(5 8 123 16 129 1))
  (with-input-from-sequence (input '(5 8 123 16 129 1))
	(let ((message (protobuf-read-message input 'message-with-enum)))
	  (?equal (message-with-enum-b message) 'value2))))

(defmessage complex-message ()
  (a 1 :int32)
  (b 2 simple-message)
  (c 3 simple-message :optional t))

(deftest complex-messages-test
  (let ((message (make-instance 'complex-message
								:a 123
								:b (make-instance 'simple-message :a 1 :b -2 :c 3))))
	(?message-write= message
					 '(13 8 123 18 9 8 1 21 192 0 0 0 24 3)))
  (let ((message (with-input-from-sequence (input '(13 8 123 18 9 8 1 21 192 0 0 0 24 3))
				   (protobuf-read-message input 'complex-message))))
	(?equal (complex-message-a message) 123)
	(let ((submessage (complex-message-b message)))
	  (?equal (simple-message-a submessage) 1)
	  (?equal (simple-message-b submessage) -2.0)
	  (?equal (simple-message-c submessage) 3))))

;defining protocols




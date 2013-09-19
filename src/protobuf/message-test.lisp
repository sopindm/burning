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
	(?equal (simple-message-c message) 0)
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
	(?message-write= message '(8 42 21 192 0 0 0 24 15))))

;reading messages

;writing message without requered fields
;reading message without requered fields

;making message with wrong typed fields
;setting message fields to wrong typed values

;enum fields in message
;message fields in message

;defining protocols




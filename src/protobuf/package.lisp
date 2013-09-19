(defpackage #:burning-protobuf
  (:use #:burning-lisp #:ieee-floats)
  (:export protobuf-write
		   protobuf-read

		   protobuf-write-message
		   protobuf-read-message

		   message
		   defmessage

		   message-slot-tag
		   message-tag-slot
		   message-slot-optional-p
		   message-slot-type))

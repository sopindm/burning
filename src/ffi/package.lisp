(defpackage #:burning-ffi
  (:use #:burning-lisp)
  (:export load-ffi

	   generate-uuid-file
	   generate-uuids-from-file

	   load-uuid
	   delete-object

	   with-object
	   with-objects
	   with-objects*

	   ffi-file
	   uuid-file))

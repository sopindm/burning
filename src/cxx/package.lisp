(defpackage burning-cxx
  (:modern t)
  (:use common-lisp)
  (:export make-cxx-stream
	   stream-write
	   stream-newline
	   stream-indent
	   stream-indent-by-position
	   stream-unindent))

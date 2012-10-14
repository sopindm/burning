(in-package #:burning-bsdf)

(defstruct target
  name
  command 
  input 
  output
  depends-on)

(defun add-target (name command &key input output depends-on)
  (make-target :name name 
	       :command command 
	       :input (if (listp input) input (list input))
	       :output (if (listp output) output (list output))
	       :depends-on (if (listp depends-on) depends-on (list depends-on))))
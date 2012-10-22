(in-package #:burning-bsdf-generator)

(defvar *bsdf-generator* 'bsc)

(defgeneric generate-target (generator target stream))
(defgeneric generator-filename (generator))

(defun generate (&key (generator *bsdf-generator*)
		 (path (path-from-string (generator-filename generator))))
  (with-open-file (stream path :direction :output :if-exists :supersede)
    (mapc (lambda (target) 
	    (generate-target generator target stream))
	  (get-targets)))
  path)

  
  
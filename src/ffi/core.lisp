(in-package :burning-ffi)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun intern-keyword (symbol)
    (intern (symbol-name symbol) "KEYWORD")))

(pushnew "~/burning/lib/" cffi:*foreign-library-directories* :test #'equal)

(cffi:define-foreign-library ffi-internal
  (t (:default "libburning_ffi_internal")))

(cffi:use-foreign-library ffi-internal)

(cffi:defcfun ("generate_uuid" %generate-uuid) :pointer)

(defun %uuid-list-to-int (uuid)
  (cond 
    ((null uuid) 0)
    ((null (rest uuid)) (first uuid))
    (t (+ (first uuid) (* 256 (%uuid-list-to-int (rest uuid)))))))

(defun %uuid-int-to-list (uuid &optional (length 16))
  (cond
    ((= length 0) ())
    ((= length 1) (list uuid))
    (t (append (%uuid-int-to-list (floor (/ uuid 256)) (1- length)) (list (mod uuid 256))))))

(defun get-uuid-from-c (c-uuid)
  (let ((uuid ()))
    (dotimes (i 16)
      (push (cffi:mem-aref c-uuid :uchar i) uuid))
    (%uuid-list-to-int uuid)))

(defun generate-uuid ()
  (labels ((hex-from-char (char)
	     (cond
	       ((and (char>= char #\0) (char<= char #\9)) (- (char-code char) (char-code #\0)))
	       ((and (char>= char #\a) (char<= char #\f)) (+ 10 (- (char-code char) (char-code #\a))))
	       ((and (char>= char #\A) (char<= char #\F)) (+ 10 (- (char-code char) (char-code #\A))))
	       (t (error "Wrong hex character ~a." char))))
	   (hex-list-to-integer (list)
	     (cond 
	       ((null list) 0)
	       ((null (rest list)) (first list))
	       (t (+ (first list) (* 16 (hex-list-to-integer (rest list))))))))
    (let* ((pointer (%generate-uuid))
	   (string (cffi:foreign-string-to-lisp pointer)))
      (cffi:foreign-free pointer)
      (hex-list-to-integer (map 'list #'hex-from-char (remove-if #'(lambda (x) (char= x #\-)) string))))))

(defgeneric do-ffi-action (action &rest arguments))

(defmacro defaction (name (&rest arguments) &body body)
  (let ((action-sym (gensym))
	(arguments-sym (gensym)))
    `(defmethod do-ffi-action ((,action-sym (eql ',(intern-keyword name))) &rest ,arguments-sym)
       (declare (ignore ,action-sym))
       (labels ((this ,arguments
		,@body))
	 (apply #'this ,arguments-sym)))))

(defaction library (name &key (path nil))
  (when path
    (pushnew path cffi:*foreign-library-directories* :test #'equal))
  (let ((library (intern name)))
    (eval
     `(progn
	(cffi:define-foreign-library ,library
	  (t (:default ,(concatenate 'string "lib" name))))
	(cffi:use-foreign-library ,library)))))

(defun load-ffi-actions (&rest actions)
  (mapc #'(lambda (x) (apply #'do-ffi-action x)) actions))

(defun load-ffi (filename)
  (cl:with-open-file (file filename :direction :input)
    (do ((action (read file nil) (read file nil)))
	((null action) t)
      (apply #'do-ffi-action action))))

(defaction package (name)
  (eval `(in-package ,name)))
(in-package #:burning-command-line)

(define-condition cmd-parsing-error (error) 
  ((argument :initarg :argument :reader cmd-parsing-error-argument)))

(defgeneric cmd-parsing-error-message (err))
(defmethod cmd-parsing-error-message ((err cmd-parsing-error))
  (format nil "Error parsing argument ~a" (cmd-parsing-error-argument err)))

(defgeneric parse-type (values type &rest type-args))
(defgeneric parse-type-value (value type &rest type-args))

(defgeneric type-value-name (type &rest type-args))

(defmethod type-value-name (type &rest type-args)
  (declare (ignore type type-args))
  (symbol-name type))

(defmethod type-value-name ((type cons) &rest type-args)
  (declare (ignore type-args))
  (apply #'type-value-name (first type) (rest type)))

;;
;; General parsing methods
;;

(define-condition missed-key-value-error (cmd-parsing-error)
  ((type :initarg :type :reader missed-key-value-error-type)))

(defmethod cmd-parsing-error-message ((err missed-key-value-error))
  (format nil "Missed value for key ~a of type ~a" (cmd-parsing-error-argument err)
	  (missed-key-value-error-type err)))

(define-condition wrong-key-value-error (cmd-parsing-error)
  ((value :initarg :value :reader wrong-key-value-error-value)
   (type :initarg :type :reader wrong-key-value-error-type)))

(defmethod cmd-parsing-error-message ((err wrong-key-value-error))
  (format nil "Wrong value for key ~a of type ~a: ~a" (cmd-parsing-error-argument err)
	  (wrong-key-value-error-type err)
	  (wrong-key-value-error-value err)))

(defmethod parse-type (value type &rest type-args)
  (let ((arg (iterator-current value)))
    (assert (listp arg))
    (unless (eq (first arg) :param)
      (error 'missed-key-value-error :type (if type-args (cons type type-args) type)))
    (assert (eq (length arg) 2))
    (handler-case (let ((result (apply #'parse-type-value (second arg) type type-args)))
		    (iterator-next value)
		    result)
      (wrong-key-value-error (err)
	(unless (slot-boundp err 'value)
	  (setf (slot-value err 'value) (second arg)))
	(unless (slot-boundp err 'type)
	  (setf (slot-value err 'type) (if type-args (cons type type-args) type)))
	(error err))
      (error (err)
	(error 'wrong-key-value-error :type (if type-args (cons type type-args) type) :value (second arg))))))

;;
;; Parsers for common types
;;

(defmethod parse-type-value (value (type (eql 'string)) &rest type-args)
  (when (and type-args (not (member value type-args :test #'equal)))
    (error 'wrong-key-value-error))
  value)

(defmethod type-value-name ((type (eql 'string)) &rest type-args)
  (if type-args
      (format nil "{~{~a~^, ~}}" type-args)
      "string"))

(define-condition argument-value-too-low-error (wrong-key-value-error)
  ((min-value :initarg :min-value :reader argument-value-too-low-error-min-value)))

(defmethod cmd-parsing-error-message ((err argument-value-too-low-error))
  (format nil "Value for argument ~a of type ~a - ~a, that is less then ~a" (cmd-parsing-error-argument err)
	  (wrong-key-value-error-type err)
	  (wrong-key-value-error-value err)
	  (argument-value-too-low-error-min-value err)))

(define-condition argument-value-too-high-error (wrong-key-value-error)
  ((max-value :initarg :max-value :reader argument-value-too-high-error-max-value)))

(defmethod cmd-parsing-error-message ((err argument-value-too-high-error))
  (format nil "Value for argument ~a of type ~a - ~a, that is more then ~a" (cmd-parsing-error-argument err)
	  (wrong-key-value-error-type err)
	  (wrong-key-value-error-value err)
	  (argument-value-too-high-error-max-value err)))

(defmethod parse-type-value (value (type (eql 'integer)) &rest type-args)
  (flet ((check-lower-bound (int bound)
	   (when (< int bound)
	     (error 'argument-value-too-low-error :min-value bound :value int)))
	 (check-higher-bound (int bound)
	   (when (> int bound)
	     (error 'argument-value-too-high-error :max-value bound :value int))))
    (assert (<= (length type-args) 2))
    (let ((value (handler-case (parse-integer value) (error () (error 'wrong-key-value-error)))))
      (when (>= (length type-args) 1) (check-lower-bound value (first type-args)))
      (when (>= (length type-args) 2) (check-higher-bound value (second type-args)))
      value)))

(defmethod parse-type-value (value (type (eql 'float)) &rest type-args)
  (assert (null type-args))
  (parse-real-number value))

;;
;; Parser for list type
;;

(defun parameter-p (arg)
  (and (listp arg) (= (length arg) 2) (eq (first arg) :param)))

(defmethod type-value-name ((type (eql 'list)) &rest type-args)
  (format nil "[~a ...]" (type-value-name (first type-args))))

(defmethod parse-type (value (type (eql 'list)) &rest type-args)
  (assert (= (length type-args) 1))
  (if (parameter-p (iterator-current value))
      (cons (apply #'parse-type value type-args) (apply #'parse-type value type type-args))
      nil))

;;
;; Parser for tuples  
;;

(defmethod type-value-name ((type (eql 'tuple)) &rest type-args)
  (flet ((argument-name (arg)
	   (format nil " ~a" arg)))
    (subseq (apply #'string+ (mapcar #'argument-name type-args)) 1)))

(defmethod parse-type (value (type (eql 'tuple)) &rest type-args)
  (flet ((as-list (value)
	   (if (atom value)
	       (list value)
	       value)))
    (if type-args
	(cons (apply #'parse-type value (as-list (first type-args)))
	      (apply #'parse-type value 'tuple (rest type-args))))))

;;
;; Filesystem types
;;

(define-condition wrong-path-argument-error (wrong-key-value-error) ())

(defmethod cmd-parsing-error-message ((err wrong-path-argument-error))
  (format nil "Error parsing argument ~a: ~a isn't correct path" (cmd-parsing-error-argument err)
	  (wrong-key-value-error-value err)))

(define-condition wrong-file-path-argument-error (wrong-key-value-error) ())

(defmethod cmd-parsing-error-message ((err wrong-file-path-argument-error))
  (format nil "Error parsing argument ~a: ~a isn't correct file path" (cmd-parsing-error-argument err)
	  (wrong-key-value-error-value err)))

(define-condition wrong-directory-path-argument-error (wrong-key-value-error) ())

(defmethod cmd-parsing-error-message ((err wrong-directory-path-argument-error))
  (format nil "Error parsing argument ~a: ~a isn't correct directory path" (cmd-parsing-error-argument err)
	  (wrong-key-value-error-value err)))

(defmethod parse-type-value (value (type (eql 'path)) &rest type-args)
  (assert (null type-args))
  (handler-case (path-from-string value)
    (wrong-filename-error ()
      (error 'wrong-path-argument-error))))

(defmethod parse-type-value (value (type (eql 'file-path)) &rest type-args)
  (assert (null type-args))
  (handler-case (path-from-string value :type :file) 
    (wrong-filename-error () 
      (error 'wrong-file-path-argument-error))))

(defmethod parse-type-value (value (type (eql 'directory-path)) &rest type-args)
  (assert (null type-args))
  (path-from-string value :type :directory))

(defmacro define-path-type (type base-type check check-error)
  `(defmethod parse-type-value (value (type (eql ',type)) &rest type-args)
     (assert (null type-args))
     (let ((path (parse-type-value value ',base-type)))
       (unless (,check path)
	 (error ',check-error))
       path)))

(define-path-type existing-file-path file-path path-exists-p wrong-file-path-argument-error)
(define-path-type existing-directory-path directory-path path-exists-p wrong-directory-path-argument-error)

(defmethod parse-type-value (value (type (eql 'existing-path)) &rest type-args)
  (assert (null type-args))
  (let ((path (parse-type-value value 'path)))
    (cond
      ((path-exists-p path) path)
      ((and (file-path-p path) (path-exists-p (path-as-directory path))) (path-as-directory path))
      (t (error 'wrong-path-argument-error)))))

(define-path-type creatable-path path correct-path-p wrong-path-argument-error)
(define-path-type creatable-file-path file-path correct-path-p wrong-file-path-argument-error)
(define-path-type creatable-directory-path directory-path correct-path-p wrong-directory-path-argument-error)
  

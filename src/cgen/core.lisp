(in-package #:burning-cgen)

(defvar *cgen-language* :none)

(defmacro in-language (name)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (setf *cgen-language* ,name)
     (setf *cgen-generator* (make-instance (or (code-generator-class) 'code-generator)))))

(defclass code-generator () ())

(defgeneric generator-add-source (generator source))
(defmethod generator-add-source ((generator code-generator) source) (declare (ignore source)) ())

(defvar *cgen-generator-classes* (make-hash-table))
(defvar *cgen-generator* (make-instance 'code-generator))

(defun code-generator-class ()
  (gethash *cgen-language* *cgen-generator-classes*))

(defun (setf code-generator-class) (value)
  (setf (gethash *cgen-language* *cgen-generator-classes*) value))

(defmacro defgenerator (name (&rest base-classes) &body slots)
  `(progn (defclass ,name (code-generator ,@base-classes) (,@slots))
	  (setf (code-generator-class) ',name)))

(defstruct (cgen-source (:constructor %make-cgen-source))
  code
  position)

(defun read-spaces (stream)
  (do ((char (read-char stream) (read-char stream nil nil)))
      ((or (not char) (and (not (char= char #\Space)) (not (char= char #\Tab)) (not (char= char #\Newline))))
       (unread-char char stream))))

(defun make-cgen-source (stream)
  (read-spaces stream)
  (let* ((start-position (counting-stream-position stream))
	 (source (read stream t nil t))
	 (end-position (counting-stream-position stream)))
    (%make-cgen-source :code source :position (cons start-position end-position))))

(defun add-code-to-generator (source)
  (handler-case
      (setf (cgen-source-code source) (macroexpand (cgen-source-code source)))
    (cgen-error (err) (setf (cgen-error-position err) (cgen-source-position source)) (error err)))
  (generator-add-source *cgen-generator* source))

(defun source-generator (stream c1 c2)
  (declare (ignore c1 c2))
  `(add-code-to-generator ,(make-cgen-source stream)))

(defun generate-source (filename)
  (let ((*readtable* (copy-readtable))
	(*cgen-language* *cgen-language*)
	(*cgen-generator* (make-instance 'code-generator))
	(*package* *package*))
    (set-dispatch-macro-character #\# #\G #'source-generator)
    (with-open-file (stream (path-from-string filename))
      (let ((stream (make-counting-stream stream)))
	(awhile (read stream nil)
	  (eval it))))))

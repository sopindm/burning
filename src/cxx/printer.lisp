(in-package :burning-cxx)

;;
;; Stream level API
;;

(defstruct cxx-stream
  (stream t)
  (indentation '(0))
  (position 0))

(defun stream-write (stream value)
  (let ((value (format nil "~a" value)))
    (incf (cxx-stream-position stream) (length value))
    (format (cxx-stream-stream stream) value)))

(defun stream-newline (stream)
  (setf (cxx-stream-position stream) 0)
  (format (cxx-stream-stream stream) "~%~va" (first (cxx-stream-indentation stream)) ""))

(defun stream-indent (stream length)
  (push (+ (first (cxx-stream-indentation stream)) length) 
	(cxx-stream-indentation stream))
  (format (cxx-stream-stream stream) "~va" (max 0 (- length (cxx-stream-position stream))) "")
  (setf (cxx-stream-position stream) 0))

(defun stream-indent-by-position (stream &optional (length 0))
  (stream-indent stream (+ (cxx-stream-position stream) length)))

(defun stream-unindent (stream)
  (pop (cxx-stream-indentation stream)))

;;
;; Generation API
;;

(in-package :burning-xml)

(defclass xml-node ()
  ((name :initarg :name :accessor xml-node-name)
   (attributes :initarg :attributes :reader xml-attributes)
   (childs :initarg :childs :reader xml-childs)))

(defun make-xml-node (name &optional (attributes nil) (childs nil))
  (make-instance 'xml-node 
		 :name name
		 :attributes (mapcar #'(lambda (x) (cons (first x) (second x))) attributes)
		 :childs childs))

(defun xml-attribute (node name)
  (let ((attribute (find name (xml-attributes node) :key #'first :test #'equal)))
    (if attribute
	(values (rest attribute) t)
	(values nil nil))))

(defun (setf xml-attribute) (value node name)
  (let ((attribute (find name (xml-attributes node) :key #'first)))
    (if attribute
	(setf (rest attribute) value)
	(push (cons name value) (slot-value node 'attributes)))))

(defun xml-remove-attribute (node name)
  (setf (slot-value node 'attributes) (remove name (xml-attributes node) :key #'first)))

(defun xml-attribute-p (node name)
  (multiple-value-bind (_ attribute-p) (xml-attribute node name)
    (declare (ignore _))
    attribute-p))

(defun xml-print (node &optional (stream t) (indentation 0))
  (labels 
      ((begin-line ()
	 (dotimes (i (* 2 indentation))
	   (format stream " ")))
       (print-argument-value (value)
	 (if (stringp value)
	     (format stream "\"~a\"" value)
	     (format stream "~a" value))))
    (begin-line)
    (format stream "<~a" (xml-node-name node))
    (mapc #'(lambda (x) (format stream " ~a=" (first x)) (print-argument-value (rest x))) (xml-attributes node))
    (if (null (xml-childs node))
	(format stream "/>~%")
	(progn (format stream ">~%")
	       (mapc #'(lambda (x) (xml-print x stream (1+ indentation))) (xml-childs node))
	       (begin-line)
	       (format stream "</~a>~%" (xml-node-name node))))))

(defun xml-find-childs (node name)
  (remove-if-not #'(lambda (x) (equal (xml-node-name x) name)) (xml-childs node)))

(defun xml-remove-child (node child)
  (setf (slot-value node 'childs) (remove child (xml-childs node))))

(defun xml-add-child (node child)
  (push child (slot-value node 'childs)))

(defun decode-xml (value)
  (labels
      ((decode-attribute-value (value)
	 (ecase (first value)
	   (linteger (parse-integer (rest value)))
	   (lfloat (parse-real-number (rest value)))
	   (lstring (subseq (rest value) 1 (1- (length (rest value)))))))
       (decode-attribute (name value)
	 (cons (rest name) (decode-attribute-value (second value))))
       (decode-attributes (value)
	 (if (= (length value) 1)
	     ()
	     (cons (decode-attribute (second value) (fourth value))
		   (decode-attributes (fifth value)))))
       (decode-tag (value)
	 (make-instance 'xml-node 
			:name (rest (third value)) 
			:attributes (decode-attributes (fourth value))
			:childs ()))
       (decode-nodes (value)
	 (if (second value)
	     (cons (do-decode-xml (second value))
		   (decode-nodes (third value)))
	     ()))
       (do-decode-xml (value)
	 (let ((node (decode-tag (second value))))
	   (when (eq (first (second value)) 'open-tag)
	       (setf (slot-value node 'childs) (decode-nodes (third value))))
	   node)))
    (do-decode-xml (second value))))

(defun parse-xml (stream)
  (let ((iterator (make-iterator stream)))
    (decode-xml (parse-input iterator xml-machine xml-parser))))

(defun test-xml ()
  (let ((output-file (open "/home/sopindm/models/f010.rtm.parsed" :direction :output :if-exists :overwrite
			   :if-does-not-exist :create)))
    (with-open-file (stream "/home/sopindm/models/f010.rtm")
      (xml-print (parse-xml stream) output-file))
    (close output-file)))


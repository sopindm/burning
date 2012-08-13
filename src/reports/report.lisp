(in-package :burning-reports)

;;
;; Value
;;

(defclass value ()
  ((name :initarg :name :reader name)
   (value :initarg :value :reader value)
   (measure :initarg :measure :reader measure)))

(defun parse-value (node)
  (let ((name (xml-attribute node "name"))
	(value (xml-attribute node "value"))
	(measure (xml-attribute node "measure")))
    (make-instance 'value :name name :value value :measure measure)))

;;
;; Iteration
;;

(defclass iteration ()
  ((name :initarg :name :reader name)
   (values :initarg :values :reader report-values)
   (phases :initarg :phases :reader phases)))

(defun make-iteration (&key (name nil) (values nil) (phases nil))
  (make-instance 'iteration :name name :values values :phases phases))

(defun parse-iteration (node)
  (make-iteration :name (xml-attribute node "name")
		  :values (mapcar #'parse-value (xml-find-childs node "value"))
		  :phases (append (mapcar #'parse-phase (xml-find-childs node "phase"))
				  (mapcar #'parse-loop (xml-find-childs node "loop")))))

(defun iteration-time (iteration)
  (value (find "time" (report-values iteration) :test #'equal :key #'name)))

(defun iteration-time-measure (iteration)
  (measure (find "time" (report-values iteration) :test #'equal :key #'name)))

;; 
;; Phase
;;

(defclass report-phase ()
  ((name :initarg :name :reader name)
   (iterations :initarg :iterations :reader iterations)))

(defun make-phase (&key (name nil) (iterations nil))
  (make-instance 'report-phase :name name :iterations iterations)) 

(defun phase-time (phase)
  (apply #'+ (mapcar #'iteration-time (iterations phase))))

(defun phase-time-measure (phase)
  (iteration-time-measure (first (iterations phase))))

(defun parse-phase (node)
  (let ((iteration (parse-iteration node)))
    (setf (slot-value iteration 'name) nil)
    (make-phase :name (xml-attribute node "name") :iterations `(,iteration))))

(defun parse-loop (node)
  (make-phase :name (xml-attribute node "name") 
	      :iterations (mapcar #'parse-iteration (xml-find-childs node "iteration"))))

(defun parse-from-xml (xml)
  (parse-phase xml))

(defun parse-from-stream (stream)
  (parse-phase (parse-xml stream)))

(defun print-value (value)
  (format t "~a: ~a (~a)~%" (name value) (value value) (measure value)))

(defun print-iteration (iteration)
  (format t "Iteration: ~a~%" (name iteration))
  (mapc #'print-value (report-values iteration))
  (mapc #'print-phase (phases iteration)))

(defun print-phase (phase)
  (format t "Phase: ~a~%" (name phase))
  (mapc #'print-iteration (iterations phase)))

(defun check-value (x y)
  (and (equal (name x) (name y))
       (equal (measure x) (measure y))))

(defun check-iteration (iteration master)
  (and (eval (cons 'and
		   (mapcar #'check-value (report-values iteration) (report-values master))))
       (eval (cons 'and
		   (mapcar #'(lambda (x y) (equal (name x) (name y))) 
			   (phases iteration) (phases master))))))

(defun check-phase (phase)
  (eval (cons 'and
	      (mapcar #'(lambda (x) (check-iteration x (first (iterations phase)))) 
		      (iterations phase)))))

(defun iteration-header (iteration)
  (labels ((full-name (name measure)
	     (if measure
		 (format nil "~a (~a)" name measure)
		 (format nil "~a" name))))
  (concatenate 'vector
	       (map 'vector #'(lambda (x) (full-name (name x) (measure x)))
		    (report-values iteration))
	       (map 'vector  #'(lambda (x) (full-name (concatenate 'string (name x) "-time") 
							   (phase-time-measure x)))
		    (phases iteration)))))

(defun iteration-line (iteration)
  (concatenate 'vector
	       (map 'vector #'value (report-values iteration))
	       (map 'vector #'phase-time (phases iteration))))

(defclass table ()
  ((path :initarg :path :reader path)
   (header :initarg :header :reader header)
   (lines :initarg :lines :reader lines)))

(defun cons-last (list atom)
  (if atom
      (append list (list atom))
      list))

(defun merge-tables (&rest tables)
  (labels ((merge-paths (path1 path2)
	     (cond
	       ((null path1) nil)
	       ((null path2) nil)
	       ((equal (first path1) (first path2)) (cons (first path1) (merge-paths (rest path1) (rest path2))))
	       ((member (first path1) path2 :test #'equal) (merge-paths path1 (rest path2)))
	       ((member (first path2) path1 :test #'equal) (merge-paths (rest path1) path2))
	       (t (merge-paths (rest path1) (rest path2)))))
	   (do-merge (table1 table2)
	     (when (not (equalp (header table1) (header table2)))
	       (error "Cannot merge tables with different headers:~%~a and ~a~%" (header table1) (header table2)))
	     (make-instance 'table
			    :path (merge-paths (path table1) (path table2))
			    :header (header table1)
			    :lines (concatenate 'vector 
						(lines table1)
						(lines table2)))))
    (reduce #'do-merge tables)))

(defgeneric get-table (object &optional name-prefix))

(defmethod get-table ((iteration iteration) &optional (name-prefix ()))
  (make-instance 'table
		 :path (cons-last name-prefix (name iteration))
		 :header (iteration-header iteration)
		 :lines `(,(iteration-line iteration))))

(defmethod get-table ((phase report-phase) &optional (name-prefix ()))
  (apply #'merge-tables (mapcar #'(lambda (x) (get-table x (cons-last name-prefix (name phase))))
				(iterations phase))))

;;Printer

(defclass printer ()
  ((stream :initarg :stream :initform t :accessor printer-stream)))

(defgeneric begin-printing-table (printer name))
(defgeneric end-printing-table (printer name))
(defgeneric begin-printing-header (printer))
(defgeneric end-printing-header (printer))
(defgeneric begin-printing-line (printer))
(defgeneric end-printing-line (printer))
(defgeneric print-cell (printer value padding))

;;Plain printer

(defclass plain-printer (printer) ())

(defmethod begin-printing-table ((printer plain-printer) name)
  (format (printer-stream printer) "~a:~%" name)
  nil)

(defmethod end-printing-table ((printer plain-printer) name)
  (declare (ignore name))
  nil)

(defmethod begin-printing-header ((printer plain-printer))
  nil)

(defmethod end-printing-header ((printer plain-printer))
  (format (printer-stream printer) "~%"))

(defmethod begin-printing-line ((printer plain-printer))
  nil)

(defmethod end-printing-line ((printer plain-printer))
  (format (printer-stream printer) "~%"))

(defmethod print-cell ((printer plain-printer) value padding)
  (print-with-padding value padding (printer-stream printer)))

;;Html printer

(defclass html-printer (printer) 
  ((header-p :initform nil :accessor header-p)))

(defmethod begin-printing-table ((printer html-printer) name)
  (format (printer-stream printer) "<p>~a:</p>~%" name)
  (format (printer-stream printer) "<table border=1>~%"))

(defmethod end-printing-table ((printer html-printer) name)
  (declare (ignore name))
  (format (printer-stream printer) "</table>~%"))

(defmethod begin-printing-header ((printer html-printer))
  (format (printer-stream printer) "<tr> ")
  (setf (header-p printer) t))

(defmethod end-printing-header ((printer html-printer))
  (format (printer-stream printer) "</tr>~%")
  (setf (header-p printer) nil))

(defmethod begin-printing-line ((printer html-printer))
  (format (printer-stream printer) "<tr> "))

(defmethod end-printing-line ((printer html-printer))
  (format (printer-stream printer) "</tr>~%"))

(defmethod print-cell ((printer html-printer) value padding)
  (declare (ignore padding))
  (format (printer-stream printer) "~:[<td>~;<th>~]~a~2:*~:[</td>~;</th>~] " (header-p printer) value))

;gplot printer

(defclass gplot-printer (plain-printer) ())

(defmethod begin-printing-table ((printer gplot-printer) name)
  (format (printer-stream printer) "# ")
  (call-next-method))

(defmethod end-printing-table ((printer gplot-printer) name)
  (format (printer-stream printer) "~%~%"))

(defmethod begin-printing-header ((printer gplot-printer))
  (format (printer-stream printer) "# "))

;;Table printing

(defun print-with-padding (string size &optional (stream t))
  (let ((string (format nil "~a" string)))
    (let* ((before-padding (floor (/ (- size (length string)) 2)))
	   (after-padding (- size before-padding)))
      (format stream "~va~va" before-padding "" after-padding string))))

(defun print-table-header (table)
  (let ((printer (make-instance 'plain-printer :stream t)))
    (begin-printing-header printer)
    (map 'vector #'(lambda (x) (print-cell printer x (+ (length x) 2))) (header table))))

(defun print-table (table printer)
  (begin-printing-table printer (path table))
  (labels ((element-length (array i)
	     (length (format nil "~a" (aref array i))))
	   (do-print-line (line paddings)
	     (map 'vector #'(lambda (x y) (print-cell printer x y))
		  line paddings))
	   (print-header (line paddings)
	     (begin-printing-header printer)
	     (do-print-line line paddings)
	     (end-printing-header printer))
	   (print-line (line paddings)
	     (begin-printing-line printer)
	     (do-print-line line paddings)
	     (end-printing-line printer)))
    (let ((column-paddings (make-array (length (header table)))))
      (dotimes (i (length (header table)))
	(setf (aref column-paddings i) (+ 2 (apply #'max
						   (cons (element-length (header table) i)
							 (map 'list #'(lambda (x) (element-length x i))
							      (lines table)))))))
      (print-header (header table) column-paddings)
      (map 'vector #'(lambda (x) (print-line x column-paddings)) (lines table))
      t))
  (end-printing-table printer (path table)))

(defun phase-goto (phase next)
  (if (and (= (length (iterations phase)) 1)
	   (null (name (first (iterations phase)))))
      (iteration-goto (first (iterations phase)) next)
      (if (null next)
	  phase
	  (let ((next-iteration (find (first next) (iterations phase) :test #'equal :key #'name)))
	    (assert next-iteration)
	    (iteration-goto next-iteration (rest next))))))

(defun iteration-goto (iteration next)
  (if (null next)
      iteration
      (let ((next-phase (find (first next) (phases iteration) :test #'equal :key #'name)))
	(assert next-phase)
	(phase-goto next-phase (rest next)))))

(defgeneric ls-object (object))

(defun print-name (value name-function)
  (format t "~a~%" (funcall name-function value)))

(defmethod ls-object ((phase report-phase))
  (progn
    (mapc #'(lambda (x) (print-name x #'name)) (iterations phase))
    t))

(defmethod ls-object ((iteration iteration))
  (progn
    (mapc #'(lambda (x) (print-name x #'name)) (phases iteration))
    t))

(defvar *report* nil)
(defvar *path* ())
(defvar *printer* (make-instance 'plain-printer))

(defun ls ()
  (ls-object (phase-goto *report* *path*)))

(defun path-table ()
  (get-table (phase-goto *report* *path*)))

(defun rprint (table)
  (print-table table *printer*))

(defun goto (path)
  (if (atom path)
      (cons-last *path* path)
      (append *path* path)))

(defun open-report (filename &optional (output-stream t) &rest options)
  (with-open-file (stream filename)
    (setq *report* (parse-from-stream stream)))
  (setq *path* ())
  (if (member :html options)
      (setq *printer* (make-instance 'html-printer :stream output-stream))
      (setq *printer* (make-instance 'plain-printer :stream output-stream))))

(defmacro with-report ((filename &rest options) &rest body)
  (labels ((inner-block (output-stream)
	     `(progn 
		(let ((*report* nil)
		      (*path* ())
		      (*printer* (make-instance ',(cond
						   ((member :html options) 'html-printer)
						   ((member :gplot options) 'gplot-printer)
						   (t 'plain-printer))
					       :stream ,output-stream)))
		  (with-open-file (stream ,filename)
		    (setq *report* (parse-from-stream stream)))
		  ,@body)))
	   (filename (base-name options)
	     (let ((extension (if (member ':html options) ".html" ".txt")))
	       (concatenate 'string base-name extension))))
    (let ((output-stream (gensym)))
      `(with-open-file (,output-stream ,(filename filename options) 
				       :direction :output 
				       :if-exists :supersede
				       :if-does-not-exist :create)
	 ,(inner-block output-stream)))))

(defmacro with-path (path &body body)
  `(let ((*path* (goto ,path)))
     ,@body))

(defun rename (name table)
  (make-instance 'table 
		 :path name
		 :header (header table)
		 :lines (lines table)))

(defun row-selector (name)
  (labels 
      ((index-of (name line &optional (index 0))
	 (cond
	   ((= index (length line)) nil)
	   ((equal name (aref line index)) index)
	   (t (index-of name line (1+ index)))))
       (selector (index)
	 (lambda (line)
	   (aref line index))))
    (lambda (table)
      (selector (index-of name (header table))))))

(defun image-selector (name)
  (let ((row-selector (row-selector name)))
    (lambda (table)
      (let ((row-selector (funcall row-selector table)))
	(lambda (line)
	  (let ((path (funcall row-selector line)))
	    (concatenate 'string
			 "<a href=\""
			 path
			 ".bmp\"> <img src=\""
			 path
			 ".bmp\" width=256 height=256/></a>")))))))

(defun select-rows (table selectors)
  (labels ((rows (line selectors)
	     (map 'vector #'(lambda (x) (funcall x line)) selectors)))
    (let ((selectors (mapcar #'(lambda (x) (funcall x table)) selectors)))
      (let ((header (rows (header table) selectors))
	    (lines (map 'vector #'(lambda (x) (rows x selectors)) (lines table))))
	(make-instance 'table
		       :path (path table)
		       :header header
		       :lines lines)))))

(defmacro select (name rows-list table)
  (labels 
      ((make-selector (selector)
	 (cond
	   ((stringp selector) (row-selector selector))
	   ((eq (first selector) 'image) (image-selector (second selector)))
	   (t (error "Unknown selection expression."))))
       (make-selectors (selectors table)
	 (cond 
	   ((eq selectors '*) table)
	   (t `(select-rows ,table ',(mapcar #'make-selector selectors))))))
    (let ((selector (make-selectors rows-list table)))
      `(rename ,name ,selector))))



(in-package :burning-reports-test)

(defcase report-test)

(defun !value= (value1 value2)
  (!equal (burning-reports::name value1) (burning-reports::name value2))
  (!equal (burning-reports::value value1) (burning-reports::value value2))
  (!equal (burning-reports::measure value1) (burning-reports::measure value2)))

(defun !iteration= (iteration1 iteration2)
  (mapc #'!value= (burning-reports::report-values iteration1) (burning-reports::report-values iteration2)))

(defun !phase= (phase1 phase2)
  (mapc #'!iteration= 
	(burning-reports::iterations phase1)
	(burning-reports::iterations phase2)))

(deftest report-test simple-phase ()
  (let ((xml (make-xml-node "simple-phase")))
    (!phase= (parse-phase xml)
	     (make-phase :iterations `(,(make-iteration))))))

(deftest report-test simple-loop ()
  (let ((xml (make-xml-node 
	      "simple-loop" () 
	      (list (make-xml-node "iteration")
		    (make-xml-node "iteration")
		    (make-xml-node "iteration")))))
    (!phase= (parse-phase xml)
	     (make-phase :iterations (list (make-iteration)
					   (make-iteration)
					   (make-iteration))))))

(defun make-value (name value measure)
  (make-instance 'burning-reports::value :name name :value value :measure measure))

(deftest report-test value-parse ()
  (let ((xml (make-xml-node "iteration" ()
			    (list (make-xml-node "value" 
						 '(("name" "value1") ("measure" "ms") ("value" 123)))
				  (make-xml-node "value" 
						 '(("name" "value2") ("measure" "m2") ("value" "bla-bla")))))))
    (!iteration= (burning-reports::parse-iteration xml)
		 (make-iteration 
		  :values (list (make-value "value1" 123 "ms")
				(make-value "value2" "bla-bla" "m2"))))))

(deftest report-test time-test ()
  (let ((phase (make-phase :iterations (list (make-iteration :values `(,(make-value "time" 12 "ms")))
					     (make-iteration :values `(,(make-value "time" 13 "ms")))))))
    (!= (burning-reports::phase-time phase) 25)
    (!equal (burning-reports::phase-time-measure phase) "ms")))

(deftest report-test phase-goto-test ()
  (let ((subphase1 (make-phase :name "subphase1"))
	(subphase2 (make-phase :name "subphase2")))
    (let ((iteration1 (make-iteration :name "iteration1" 
				      :phases `(,subphase1)))
	  (iteration2 (make-iteration :name "iteration2"
				      :phases `(,subphase2))))
      (let ((phase (make-phase :iterations `(,iteration1 ,iteration2))))
	(!iteration= (phase-goto phase '("iteration1")) iteration1)
	(!iteration= (phase-goto phase '("iteration2")) iteration2)
	(!phase= (phase-goto phase '("iteration1" "subphase1")) subphase1)
	(!phase= (phase-goto phase '("iteration2" "subphase2")) subphase2)))))

(defun make-table (path header &rest lines)
  (make-instance 'burning-reports::table
		 :path path
		 :header (coerce header 'vector)
		 :lines (map 'vector #'(lambda (x) (coerce x 'vector)) lines)))

(defun !table= (table1 table2)
  (!equal (burning-reports::path table1) (burning-reports::path table2))
  (!equalp (burning-reports::header table1) (burning-reports::header table2))
  (!equalp (burning-reports::lines table1) (burning-reports::lines table2)))

(deftest report-test phase-table-test ()
  (let ((iteration1 (make-iteration :name "iteration1"
				    :values `(,(make-value "value1" 1 "m")
					       ,(make-value "value2" 2 "m2"))))
	(iteration2 (make-iteration :name "iteration2"
				    :values `(,(make-value "value1" "10" "m")
					       ,(make-value "value2" "20" "m2")))))
    (let ((phase (make-phase :name "phase" :iterations `(,iteration1 ,iteration2))))
      (!table= (get-table phase)
	       (make-table '("phase")
			   '("value1 (m)" "value2 (m2)")
			   '(1 2) '("10" "20"))))))

(defun multiline (&rest lines)
  (format nil "~{~a~%~}" lines))

(deftest report-test print-table-test ()
  (let ((table (make-table "sample table"
			   '("bla" "bla-bla" "bla-bla-bla")
			   '("11111" "22222" "33333")
			   '("1" "222222222" "333"))))
    (with-output-to-string (stream)
      (burning-reports::print-table table (make-instance 'burning-reports::plain-printer :stream stream))
      (!equal (get-output-stream-string stream)
	      (multiline "sample table:"
			 "  bla    bla-bla   bla-bla-bla "
			 " 11111    22222       33333    "
			 "   1    222222222      333     ")))
    (with-output-to-string (stream)
      (burning-reports::print-table table (make-instance 'burning-reports::html-printer :stream stream))
      (!equal (get-output-stream-string stream)
	      (multiline "<p>sample table:</p>"
			 "<table border=1>"
			 "<tr> <th>bla</th> <th>bla-bla</th> <th>bla-bla-bla</th> </tr>"
			 "<tr> <td>11111</td> <td>22222</td> <td>33333</td> </tr>"
			 "<tr> <td>1</td> <td>222222222</td> <td>333</td> </tr>"
			 "</table>")))))
      
			   

									      

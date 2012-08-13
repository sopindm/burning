(in-package :burning-xml-test)

(in-case core-test)

(deftest attribute-test
  (let ((node (make-instance 'xml-node :attributes '((a . 0) (b . 1)))))
    (!equal (multiple-value-list (xml-attribute node 'a)) '(0 t))
    (!t (xml-attribute-p node 'a))
    (!equal (multiple-value-list (xml-attribute node 'b)) '(1 t))
    (!t (xml-attribute-p node 'b))
    (!equal (multiple-value-list (xml-attribute node 'c)) '(nil nil))
    (!null (xml-attribute-p node 'c)))
  (let ((node (make-instance 'xml-node :attributes '(("a" . 0)("b" . 1)))))
    (!equal (xml-attribute node "a") 0)
    (!equal (xml-attribute node "b") 1)))

(deftest set-attribute-test
  (let ((node (make-instance 'xml-node :attributes '((a . 0)))))
    (setf (xml-attribute node 'a) 1)
    (!= (xml-attribute node 'a) 1)
    (setf (xml-attribute node 'b) 2)
    (!= (xml-attribute node 'b) 2)))

(deftest remove-attribute-tes
  (let ((node (make-instance 'xml-node :attributes '((a . 0)(b . 1)))))
    (xml-remove-attribute node 'a)
    (!null (xml-attribute-p node 'a))
    (xml-remove-attribute node 'b)
    (!null (xml-attribute-p node 'b))
    (xml-remove-attribute node 'c)
    (!null (xml-attribute-p node 'c))))

(deftest node-print-test
  (let ((stream (make-string-output-stream)))
    (xml-print (make-xml-node "sample") stream)
    (!equal (get-output-stream-string stream) (format nil "<sample/>~%"))
    (xml-print (make-xml-node "sample" '(("x" 0)("y" 1)("z" 2))) stream)
    (!equal (get-output-stream-string stream) (format nil "<sample x=0 y=1 z=2/>~%"))
    (xml-print (make-xml-node "sample" '(("string" "bla bla"))) stream)
    (!equal (get-output-stream-string stream) (format nil "<sample string=\"bla bla\"/>~%"))))

(deftest childs-test
  (let ((child1 (make-xml-node "child1"))
	(child2 (make-xml-node "child1"))
	(child3 (make-xml-node "child2")))
    (let ((node (make-xml-node "node" () (list child1 child2 child3))))
      (!equal (xml-find-childs node "child1") (list child1 child2))
      (!equal (xml-find-childs node "child2") (list child3))
      (xml-remove-child node child2)
      (!equal (xml-find-childs node "child1") (list child1))
      (xml-remove-child node child3)
      (!equal (xml-find-childs node "child2") ()))))

(defun multiformat (&rest lines)
  (apply #'concatenate (cons 'string
			     (mapcar #'(lambda (x) (format nil (concatenate 'string x "~%"))) lines))))

(deftest print-childs-test
  (let ((node (make-xml-node "node" () (list (make-xml-node "child"))))
	(stream (make-string-output-stream)))
    (xml-print node stream)
    (!equal (get-output-stream-string stream)
	    (multiformat "<node>"
			 "  <child/>"
			 "</node>")))
  (let ((node (make-xml-node "node" '(("x" 0)("y" 1)("z" 2)) 
			     (list (make-xml-node "child1" '(("x" 3)("y" 4)))
				   (make-xml-node "child2" () (list (make-xml-node "subchild1")
								    (make-xml-node "subchild2" '(("x" 100)))))
				   (make-xml-node "child3" '(("x" 1)("y" 100))))))
	(stream (make-string-output-stream)))
    (xml-print node stream)
    (!equal (get-output-stream-string stream)
	    (multiformat "<node x=0 y=1 z=2>"
			 "  <child1 x=3 y=4/>"
			 "  <child2>"
			 "    <subchild1/>"
			 "    <subchild2 x=100/>"
			 "  </child2>"
			 "  <child3 x=1 y=100/>"
			 "</node>"))))

(defun !xml-node= (node1 node2)
  (!equal (xml-node-name node1) (xml-node-name node2))
  (!equal (xml-attributes node1) (xml-attributes node2))
  (!= (length (xml-childs node1)) (length (xml-childs node2)))
  (eval (cons 'and 
	      (mapcar #'!xml-node= (xml-childs node1) (xml-childs node2)))))

(deftest xml-parsing
  (let ((node (parse-xml (make-string-input-stream "<node/>"))))
    (!equal (xml-node-name node) "node")
    (!equal (xml-attributes node) ())
    (!equal (xml-childs node) ()))
  (let ((node (parse-xml (make-string-input-stream "<node x=0 y=1 z=2/>"))))
    (!equal (xml-node-name node) "node")
    (!equal (xml-attributes node) '(("x" . 0)("y" . 1)("z" . 2)))
    (!equal (xml-childs node) ()))
  (let ((node (parse-xml (make-string-input-stream "<node string=\"bla bla\"/>"))))
    (!equal (xml-node-name node) "node")
    (!equal (xml-attributes node) '(("string" . "bla bla")))
    (!equal (xml-childs node) ()))
  (let ((node (parse-xml (make-string-input-stream "<floatnode float=123.4/>"))))
    (!equal (xml-node-name node) "floatnode")
    (!equal (xml-attributes node) '(("float" . 123.4)))
    (!equal (xml-childs node) ()))
  (let ((node (parse-xml (make-string-input-stream 
			  (multiformat "<parent id=0>"
				       "  <child id=1/>"
				       "  <child id=2>"
				       "    <subchild id=3/>"
				       "    <subchild id=4/>"
				       "  </child>"
				       "  <child id=5/>"
				       "</parent>")))))
	   (!xml-node= node
		(make-xml-node "parent" '(("id" 0))
			       (list (make-xml-node "child" '(("id" 1)))
				     (make-xml-node "child" '(("id" 2)) 
						    (list (make-xml-node "subchild" '(("id" 3)))
							  (make-xml-node "subchild" '(("id" 4)))))
				     (make-xml-node "child" '(("id" 5))))))))
			 


    
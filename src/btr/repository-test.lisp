(in-package #:burning-btr-test)

(defun print-repository (repo &optional stream)
  (burning-btr::print-repository repo stream))

(in-case repository-test)

;;
;;Simple repository
;;

(deftest creating-repository
  (let ((repository (make-repository)))
    (!eq (entities repository) ())
    (!equal (repository-version repository) "0.1")))

(defun echo (&rest lines)
  (make-string-input-stream 
   (with-output-to-string (stream)
     (mapc #'(lambda (line) (write-line line stream)) lines))))

(deftest saving-repository
  (!equal (print-repository (make-repository))
	  (lines "<repository version=\"0.1\"/>")))

(defun read-repository (stream)
  (burning-btr::read-repository stream))

(deftest loading-repository
  (let ((repository (read-repository (echo "<repository version=\"a.version\"/>"))))
    (!eq (entities repository) ())
    (!equal (repository-version repository) "a.version")))

(deftest loading-xml-with-wrong-name
  (!condition (read-repository (echo "<repo/>"))
	      wrong-xml-node-name
	      (wrong-xml-node-name-expected "repository")
	      (wrong-xml-node-name-got "repo")))

(deftest loading-xml-without-version
  (!condition (read-repository (echo "<repository/>"))
	      missed-xml-attribute-warning
	      (missed-xml-attribute-warning-name "version")
	      (missed-xml-attribute-warning-node-name "repository")))

;;
;; Repository with units
;;

(deftest adding-unit
  (let ((unit (make-unit "unit" :files '("name1" "name.2" "third name"))))
    (!equal (entity-name unit) "unit")
    (!equal (unit-files unit) '("name1" "name.2" "third name"))
    (let ((repository (make-repository)))
      (add-entity unit repository)
      (!equal (entities repository) `(,unit)))))

(deftest removing-unit
  (let ((unit (make-unit "unit"))
	(repo (make-repository)))
    (add-entity unit repo)
    (remove-entity "unit" repo)
    (!eq (entities repo) ())))

(deftest units-with-same-name
  (let ((repo (make-repository))
	(unit (make-unit "unit")))
    (add-entity unit repo)
    (!condition (add-entity unit repo)
		entity-with-same-name-already-exists
		(entity-with-same-name-already-exists-name "unit"))))

(deftest working-with-several-units
  (let ((unit1 (make-unit "unit1"))
	(unit2 (make-unit "unit2"))
	(unit3 (make-unit "unit3"))
	(repo (make-repository)))
    (add-entity unit1 repo)
    (add-entity unit2 repo)
    (add-entity unit3 repo)
    (!equal (entities repo) (list unit1 unit2 unit3))
    (remove-entity "unit2" repo)
    (!equal (entities repo) (list unit1 unit3))
    (remove-entity "unit3" repo)
    (!equal (entities repo) (list unit1))))

(deftest print-units
  (let ((unit1 (make-unit "unit1" :files '("file1" "file2")))
	(unit2 (make-unit "unit2" :files '("f1")))
	(unit3 (make-unit "unit3" :files '("file-one" "file-two" "file-three")))
	(repo (make-repository)))
    (add-entity unit1 repo)
    (add-entity unit2 repo)
    (add-entity unit3 repo)
    (!equal (print-repository repo)
	    (lines "<repository version=\"0.1\">"
		   "  <unit name=\"unit1\">"
		   "    <file name=\"file1\"/>"
		   "    <file name=\"file2\"/>"
		   "  </unit>"
		   "  <unit name=\"unit2\">"
		   "    <file name=\"f1\"/>"
		   "  </unit>"
		   "  <unit name=\"unit3\">"
		   "    <file name=\"file-one\"/>"
		   "    <file name=\"file-two\"/>"
		   "    <file name=\"file-three\"/>"
		   "  </unit>"
		   "</repository>"))))

(defun !unit= (unit1 unit2)
  (!equal (entity-name unit1) (entity-name unit2))
  (!equal (unit-files unit1) (unit-files unit2))) 

(deftest parse-units
  (let ((repo (read-repository (echo "<repository version=\"0.1\">"
				     "  <unit name=\"unit1\">"
				     "    <file name=\"file1\"/>"
				     "    <file name=\"file2\"/>"
				     "  </unit>"
				     "  <unit name=\"unit2\">"
				     "    <file name=\"f1\"/>"
				     "  </unit>"
				     "  <unit name=\"unit3\">"
				     "    <file name=\"one\"/>"
				     "    <file name=\"two\"/>"
				     "    <file name=\"three\"/>"
				     "  </unit>"
				     "</repository>"))))
    (!equal (repository-version repo) "0.1")
    (mapc #'!unit= (entities repo) (list (make-unit "unit1" :files '("file1" "file2"))
					 (make-unit "unit2" :files '("f1"))
					 (make-unit "unit3" :files '("one" "two" "three"))))))

;;
;; Groups
;;

(defun !group= (g1 g2)
  (!equal (entity-name g1) (entity-name g2))
  (mapc #'!entity= (entities g1) (entities g2)))

(defun !entity= (e1 e2)
  (!eq (type-of e1) (type-of e2))
  (when (eq (type-of e1) (type-of e2))
    (cond 
      ((typep e1 'unit) (!unit= e1 e2))
      ((typep e1 'group) (!group= e1 e2)))))

(deftest making-group
  (let ((group (make-group "group")))
    (!equal (entity-name group) "group")
    (!eq (entities group) ())))

(deftest group-with-files
  (let ((group (make-group "group"))
	(unit (make-unit "unit")))
    (add-entity unit group)
    (mapc #'!entity= (entities group) (list unit))))

(deftest group-with-multiple-entities
  (let ((group (make-group "group"))
	(unit1 (make-unit "unit1"))
	(subgroup (make-group "subgroup"))
	(unit2 (make-unit "unit2"))
	(unit3 (make-unit "unit3")))
    (add-entity unit1 group)
    (add-entity subgroup group)
    (add-entity unit2 subgroup)
    (add-entity unit3 group)
    (mapc #'!entity= (entities group) (list unit1 subgroup unit3))
    (remove-entity "unit1" group)
    (mapc #'!entity= (entities group) (list subgroup unit3))
    (remove-entity "subgroup" group)
    (mapc #'!entity= (entities group) (list unit3))))

(deftest print-groups
  (let ((repo (make-repository))
	(g1 (make-group "g1"))
	(g2 (make-group "g2"))
	(u1 (make-unit "u1"))
	(u2 (make-unit "u2"))
	(u3 (make-unit "u3")))
    (add-entity g1 repo)
    (add-entity u1 g1)
    (add-entity g2 g1)
    (add-entity u2 g2)
    (add-entity u3 repo)
    (!equal (print-repository repo)
	    (lines "<repository version=\"0.1\">"
		   "  <group name=\"g1\">"
		   "    <unit name=\"u1\"/>"
		   "    <group name=\"g2\">"
		   "      <unit name=\"u2\"/>"
		   "    </group>"
		   "  </group>"
		   "  <unit name=\"u3\"/>"
		   "</repository>"))))

(deftest parsing-groups
  (let ((repo (read-repository (echo "<repository version=\"0.1\">"
				     "  <group name=\"g1\">"
				     "    <unit name=\"u1\"/>"
				     "  </group>"
				     "  <group name=\"g2\"/>"
				     "</repository>")))
	(g1 (make-group "g1"))
	(g2 (make-group "g2")))
    (add-entity (make-unit "u1") g1)
    (mapc #'!entity= (entities repo) (list g1 g2))))






			 
			 
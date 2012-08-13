(in-package #:burning-btr-test)

(in-case extension-test)

(define-btr-class my-repository repository
  (status :initform "unspecified")
  (name :xml-name "a_name" :initform 1 :accessor mr-name))

(define-btr-class my-unit unit
  (version :initform 0 :accessor mu-version)
  (owner :initform "unknown" :accessor mu-owner))

(defmacro def-extension-test (name &body body)
  `(deftest ,name
     (let ((*repository-class* 'my-repository)
	   (*unit-class* 'my-unit)
	   (*default-filesystem* (make-virtual-filesystem)))
       ,@body)))

(def-extension-test creating-repository
  (let ((repo (make-repository)))
    (!eq (mr-name repo) 1)
    (!equal (slot-value repo 'status) "unspecified")
    (!equal (print-repository repo)
	    (lines "<repository version=\"0.1\" a_name=1 status=\"unspecified\"/>"))))

(def-extension-test parsing-repository 
  (let ((repo (read-repository (echo "<repository version=\"blabla\" a_name=101 status=\"a status\"/>"))))
    (!equal (repository-version repo) "blabla")
    (!eq (mr-name repo) 101)
    (!equal (slot-value repo 'status) "a status")))

(def-extension-test creating-units 
  (let ((repo (make-repository))
	(unit1 (make-unit "u1"))
	(unit2 (make-unit "u2"))
	(group (make-group "g")))
    (!eq (mu-version unit1) 0)
    (!equal (mu-owner unit1) "unknown")
    (add-entity unit1 repo)
    (add-entity group repo)
    (add-entity unit2 group)
    (!equal (print-repository repo)
	    (lines "<repository version=\"0.1\" a_name=1 status=\"unspecified\">"
		   "  <unit name=\"u1\" owner=\"unknown\" version=0/>"
		   "  <group name=\"g\">"
		   "    <unit name=\"u2\" owner=\"unknown\" version=0/>"
		   "  </group>"
		   "</repository>"))))

(def-extension-test parsing-units
  (let ((repo (read-repository (echo "<repository version=0 a_name=42 status=\"\">"
				     "  <unit name=\"u\" owner=\"burning\" version=101/>"
				     "</resository>"))))
    (!eq (length (entities repo)) 1)
    (let ((unit (first (entities repo))))
      (!equal (entity-name unit) "u")
      (!equal (mu-owner unit) "burning")
      (!eq (mu-version unit) 101))))

(def-extension-test lising-extended-units
  (init-repository)
  (write-file (path-from-string ".btr/repository.conf") 
	      (lines "<repository version=0 a_name=123 status=\"no\">"
		     "  <unit name=\"u\" owner=\"burning\" version=101/>"
		     "  <group name=\"g\">"
		     "    <unit name=\"u2\" owner=\"me\" version=202/>"
		     "  </group>"
		     "</repository>"))
  (!equal (with-standard-output-to-string (btr-run '("--ls" "-r")))
	  (lines " Path  version  owner   "
		 ""
		 " g/u2  202      me      "
		 " u     101      burning "))
  (!equal (with-standard-output-to-string (btr-run '("--ls")))
	  (lines " Path  version  owner   "
		 ""
		 " g/   "
		 " u     101      burning ")))

(define-repository-function my-repository :create (repository args)
  (setf (mr-name repository) "created repository"))

(def-extension-test initialize-function-extension 
  (btr-run '("--create"))
  (!equal (read-file (path-from-string ".btr/repository.conf"))
	  (lines "<repository version=\"0.1\" a_name=\"created repository\" status=\"unspecified\"/>")))

(define-repository-function my-unit :add (unit)
  (setf (mu-version unit) (file-length (first (unit-files unit))))
  (setf (mu-owner unit) "itself"))

(def-extension-test adding-units
  (init-repository "file")
  (!equal (with-standard-output-to-string (btr-run '("--ls")))
	  (lines " Path  version  owner  "
		 ""
		 " file  0        itself ")))

(define-repository-function my-unit :remove (unit)
  (remove-file (first (unit-files unit))))

(def-extension-test removing-extended-units
  (init-repository "file")
  (btr-run '("--rm" "file"))
  (!null (path-exists-p (path-from-string "file"))))

(def-extension-test updating-attributes
  (init-repository "file")
  (write-file (path-from-string "file") "a file")
  (btr-run '("--update"))
  (!equal (with-standard-output-to-string (btr-run '("--ls")))
	  (lines " Path  version  owner  "
		 ""
		 " file  6        itself ")))

(define-run-action "myRun"
    "Runs my repository")

(define-run-function "myRun" :run (unit context)
  (format t "Unit ~a was run.~%" (entity-name unit)))

(def-extension-test simple-run
  (init-repository "file")
  (!equal (with-standard-output-to-string (btr-run '("--run" "--myRun")))
	  (lines "Unit file was run.")))

(def-extension-test run-from-subdirectory
  (init-repository "file" "dir/file")
  (vfs-cd (path-from-string "dir/"))
  (!equal (with-standard-output-to-string (btr-run '("--run" "--myRun")))
	  (lines "Unit file was run."
		 "Unit file was run.")))

(define-run-function "myRun" :filter (unit args)
  (if (find #\. (entity-name unit)) nil t))

(def-extension-test run-with-filtering
  (init-repository "file" "a.file")
  (!equal (with-standard-output-to-string (btr-run '("--run" "--myRun")))
	  (lines "Unit file was run.")))

(define-run-class mega-runner
  (units :initform () :accessor mr-units))

(define-run-action ("other run" mega-runner) 
    "The best repository runner"
  (:key "skip" :type 'string :description "units to skip"))

(define-run-function "other run" :setup (context args)
  (setf (mr-units context) '("start"))
  (format t "Set up~%"))

(define-run-function "other run" :tearDown (context args)
  (format t "Tear down context ~{~a ~}~%" (mr-units context)))

(define-run-function "other run" :run (unit context)
  (push (entity-name unit) (mr-units context)))

(define-run-function "other run" :filter (unit args)
  (if (argument-set-p "skip" args)
      (not (string= (entity-name unit) (argument-value "skip" args)))
      t))

(def-extension-test setup-and-teardown
  (init-repository)
  (!equal (with-standard-output-to-string (btr-run '("--run" "--other run")))
	  (lines "Set up"
		 "Tear down context start ")))

(def-extension-test complex-run-test
  (init-repository "file1"
		   "file2"
		   "dir/file3"
		   "file4")
  (!equal (with-standard-output-to-string (btr-run '("--run" "--other run" "--skip" "file2")))
	  (lines "Set up"
		 "Tear down context file4 file3 file1 start ")))

(def-extension-test run-without-action-error
  (init-repository)
  (!condition (btr-run '("--run"))
	      no-action-specified-error))

(def-extension-test run-with-several-actions-error
  (init-repository)
  (!condition (btr-run '("--run" "--myRun" "--other run"))
	      too-much-actions-specified-error
	      (too-much-actions-specified-error-actions '("other run" "myRun"))))

(define-run-action "wrong run" 
    "Runs repository wrong")

(def-extension-test run-without-run
  (init-repository "file")
  (!condition (btr-run '("--run" "--wrong run"))
	      no-run-function-error
	      (no-run-function-error-action "wrong run")))

(define-run-class base-runner)

(define-run-action ("base run" base-runner) "a base runner"
  (:flag "a base flag"))

(define-run-function "base run" :setup (context args) 
  (format t "base setup called~%")
  (when (argument-set-p "a base flag" args)
    (format t "base flag set~%")))

(define-run-function "base run" :filter (unit args)
  (format t "base filter on ~a called~%" (entity-name unit))
  t)

(define-run-function "base run" :run (unit context)
  (format t "base run on ~a called~%" (entity-name unit)))

(define-run-function "base run" :tearDown (context args)
  (format t "base teardown called~%"))

(define-run-action ("simple run" :inherit "base run") "a runner")

(def-extension-test running-simple-run
  (init-repository "file1" "file2")
  (?lines= (with-standard-output-to-string (btr-run '("--run" "--simple run")))
	   (lines "base setup called"
		  "base filter on file1 called"
		  "base run on file1 called"
		  "base filter on file2 called"
		  "base run on file2 called"
		  "base teardown called"))
  (?lines= (with-standard-output-to-string (btr-run '("--run" "--simple run" "--a base flag")))
	   (lines "base setup called"
		  "base flag set"
		  "base filter on file1 called"
		  "base run on file1 called"
		  "base filter on file2 called"
		  "base run on file2 called"
		  "base teardown called")))

(define-run-action ("complex run" :inherit "simple run") "another runner"
  (:flag "complex flag"))

(define-run-function "complex run" :tearDown (context args)
  (format t "complex teardown called~%")
  (when (argument-set-p "complex flag" args) (format t "complex flag set~%")))

(def-extension-test running-complex-run
  (init-repository "file")
  (?lines= (with-standard-output-to-string (btr-run '("--run" "--complex run" "--complex flag" "--a base flag")))
	   (lines "base setup called"
		  "base flag set"
		  "base filter on file called"
		  "base run on file called"
		  "complex teardown called"
		  "complex flag set")))

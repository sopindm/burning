(in-package #:burning-command-line-test)

(in-case base-test)

(defmacro !parse ((&rest args) spec &body values)
  (let ((args-sym (gensym)))
    (flet ((value-check (value)
	     (destructuring-bind (test name value) value
	       `(,test (argument-value ,name ,args-sym) ,value))))
      `(let ((,args-sym (parse-arguments ',args ,spec)))
	 (declare (ignorable ,args-sym))
	 ,@(mapcar #'value-check values)))))

;;
;; Flags test
;; 

(deftest simple-list
  (let ((args (make-arguments-spec "" 
		(:flag "arg1") (:flag "arg2"))))
    (!t (have-argument-p "arg1" args))
    (!t (have-argument-p "arg2" args))))

(deftest non-existing-argument
  (let ((spec (make-arguments-spec "")))
    (!null (have-argument-p "some flag" spec))))

(deftest parse-simple-list
  (let ((spec (make-arguments-spec "" (:flag "flag")  (:flag "flag2"))))
    (let ((args (parse-arguments '("--flag") spec)))
      (!t (argument-set-p "flag" args))
      (!null (argument-set-p "flag2" args)))))

(deftest parsing-wrong-argument
  (let ((args (make-arguments-spec "args" (:flag "flag"))))
    (!condition (parse-arguments '("a-flag") args) 
		wrong-argument-error
		(cmd-parsing-error-argument "a-flag"))))

(deftest argument-with-description
  (let ((spec (make-arguments-spec "" (:flag "flag" :description "a test flag"))))
    (!t (have-argument-p "flag" spec))
    (!equal (argument-description (argument "flag" spec)) "a test flag")))

(deftest argument-with-same-names
  (!condition (make-arguments-spec "" (:flag "flag") (:flag "flag"))
	      argument-already-exists-error
	      (argument-already-exists-error-name "flag")))

(deftest adding-arguments
  (let ((spec (make-arguments-spec "" (:flag "flag1"))))
    (add-argument (make-argument :flag "flag2") spec)
    (!t (have-argument-p "flag1" spec))
    (!t (have-argument-p "flag2" spec))))
  
(deftest adding-argument-twice
  (let ((spec (make-arguments-spec "" (:flag "flag"))))
    (!condition (add-argument (make-argument :flag "flag") spec)
		argument-already-exists-error
		(argument-already-exists-error-name "flag"))))

(deftest added-arguments-help
  (let ((spec (make-arguments-spec "")))
    (add-argument (make-argument :flag "flag" :description "a flag") spec)
    (!equal (help-message spec)
	    (lines "Usage:"
		   "   [ARGS ...]"
		   ""
		   "  Where ARGS are:"
		   '("    --help" 10 "Products this help message")
		   '("    --flag" 10 "a flag")
		   ""))))

;;
;; Base help messages test
;;

(deftest help-message-test
  (!equal (help-message (make-arguments-spec ""))
	  (lines "Usage:"
		 "   [ARGS ...]"
		 ""
		 "  Where ARGS are:"
		 '("    --help" 10 "Products this help message")
		 "")))

(deftest help-with-flag
  (!equal (help-message (make-arguments-spec "SPEC" (:flag "flag" :description "some flag")))
	  (lines "Usage:"
		 "  SPEC [ARGS ...]"
		 ""
		 "  Where ARGS are:"
		 '("    --help" 10 "Products this help message")
		 '("    --flag" 10 "some flag")
		 "")))

(deftest custom-help-flag
  (let ((spec (make-arguments-spec ("" (:help "my-help" :short-name #\m :description "my own help")))))
    (!t (have-argument-p "my-help" spec))
    (!null (have-argument-p "help" spec))
    (!eq (argument-short-name (argument "my-help" spec)) #\m)
    (!equal (argument-description (argument "my-help" spec)) "my own help")))

(deftest no-help-flag
  (let ((spec (make-arguments-spec ("" :no-help))))
    (!null (have-argument-p "help" spec))))

(deftest help-flag-parsing
  (let ((spec (make-arguments-spec)))
    (let ((args (parse-arguments '("--help") spec)))
      (!t (argument-set-p "help" args)))))

(deftest help-flag-signal
  (let ((spec (make-arguments-spec)))
    (!condition (parse-arguments '("--help") spec)
		help-set-signal
		(help-set-signal-action spec))))

;; Short names test

(deftest making-with-short-names
  (let ((spec (make-arguments-spec "" (:flag "flag" :short-name #\a))))
    (!eq (argument-short-name (argument "flag" spec)) #\a)))

(deftest making-arguments-with-same-short-names
  (!condition (make-arguments-spec "" (:flag "f1" :short-name #\f) (:flag "f2" :short-name #\f))
	      short-name-already-exists-error
	      (short-name-already-exists-error-char #\f)))

(deftest parsing-short-names
  (let ((spec (make-arguments-spec "" 
		(:flag "f1" :short-name #\a)
		(:flag "f2" :short-name #\b))))
    (let ((args (parse-arguments '("-a") spec)))
      (!t (argument-set-p "f1" args))
      (!null (argument-set-p "f2" args)))))
				   
(deftest non-existing-short-name
  (let ((spec (make-arguments-spec "" (:flag "f1" :short-name #\a))))
    (!condition (parse-arguments '("-b") spec) 
		wrong-short-argument-error
		(cmd-parsing-error-argument #\b))))

(deftest multiple-short-names-parsing
  (let ((spec (make-arguments-spec ""
		(:flag "fa" :short-name #\a)
		(:flag "fb" :short-name #\b)
		(:flag "fc" :short-name #\c))))
    (let ((args (parse-arguments '("-ac") spec)))
      (!t (argument-set-p "fa" args))
      (!t (argument-set-p "fc" args))
      (!null (argument-set-p "fb" args)))))

(deftest print-short-names
  (let ((spec (make-arguments-spec "" (:flag "flag" :short-name #\f :description "some flag"))))
    (!equalp (help-message spec)
	     (lines "Usage:"
		    "   [ARGS ...]"
		    ""
		    "  Where ARGS are:"
		    '("    --help" 13 "Products this help message")
		    '("    --flag,-f" 10 "some flag")
		    ""))))

;;
;; Key tests
;;

(deftest simple-key-test
  (let ((spec (make-arguments-spec "" (:key "key" :type 'integer :description "some key"))))
    (!t (have-argument-p "key" spec))
    (let ((args (parse-arguments '("--key" "123") spec)))
      (!t (argument-set-p "key" args))
      (!= (argument-value "key" args) 123))))

(deftest wrong-value-for-key
  (let ((spec (make-arguments-spec "" (:key "key" :type 'integer))))
    (!condition (parse-arguments '("--key" "blabla") spec)
		wrong-key-value-error
		(wrong-key-value-error-value "blabla")
		(wrong-key-value-error-type 'integer))))

(deftest missed-key-value-test
  (let ((spec (make-arguments-spec "" (:key "key" :type 'integer))))
    (!condition (parse-arguments '("--key") spec)
		missed-key-value-error
		(cmd-parsing-error-argument "key")
		(missed-key-value-error-type 'integer))
    (!condition (parse-arguments '("--key" "--key") spec)
		missed-key-value-error
		(cmd-parsing-error-argument "key")
		(missed-key-value-error-type 'integer))))

(deftest key-help
  (let ((spec (make-arguments-spec "Spec" (:key "key" :type 'integer :short-name #\k :description "some key"))))
    (!equal (help-message spec)
	    (lines "Usage:"
		   "  Spec [ARGS ...]"
		   ""
		   "  Where ARGS are:"
		   '("    --help" 20 "Products this help message")
		   '("    --key,-k INTEGER" 10 "some key")
		   ""))))

(deftest list-key-test
  (let ((spec (make-arguments-spec "" (:key "key" :type '(list integer)))))
    (let ((args (parse-arguments '("--key" "123" "456" "789") spec)))
      (!t (argument-set-p "key" args))
      (!equal (argument-value "key" args) '(123 456 789)))))

(deftest key-after-list-test
  (let ((spec (make-arguments-spec "" (:key "key" :type '(list integer)) (:key "key2" :type 'string))))
    (let ((args (parse-arguments '("--key" "123" "456" "--key2" "blabla") spec)))
      (!t (argument-set-p "key" args))
      (!t (argument-set-p "key2" args))
      (!equal (argument-value "key" args) '(123 456))
      (!equal (argument-value "key2" args) "blabla"))))

(deftest list-key-help
  (let ((args (make-arguments-spec "SP" (:key "key" :type '(list integer) :description "some key"))))
    (!equal (help-message args)
	    (lines "Usage:"
		   "  SP [ARGS ...]"
		   ""
		   "  Where ARGS are:"
		   '("    --help" 23 "Products this help message")
		   '("    --key [INTEGER ...]" 10 "some key")
		   ""))))

(deftest action-argument-test
  (let ((spec (make-arguments-spec "Spec" (:action "act" :arguments ((:flag "flag"))))))
    (!t (have-argument-p "act" spec))
    (!t (have-argument-p "flag" spec))))

(deftest action-help-test
  (let ((spec (make-arguments-spec "Spec" 
				   (:action "act" :short-name #\a
					    :description "destructs universe"
					    :arguments ((:flag "flag")))
				   (:flag "flag" :description "some flag"))))
				   
    (!equal (help-message (argument "act" spec))
	    (lines "destructs universe"
		   ""
		   "Usage:"
		   "  Spec --act,-a [ARGS ...]"
		   ""
		   "  Where ARGS are:"
		   '("    --help" 10 "Products this help message")
		   '("    --flag" 10 "")
		   ""))))

(deftest spec-with-description-help
  (let ((spec (make-arguments-spec ("spec" :description "destructs universe"))))
    (!equal (help-message spec)
	    (lines "destructs universe"
		   ""
		   "Usage:"
		   "  spec [ARGS ...]"
		   ""
		   "  Where ARGS are:"
		   '("    --help" 10 "Products this help message")
		   ""))))

(deftest parsing-actions-help
  (let ((spec (make-arguments-spec "spec" (:action "act"))))
    (let ((args (parse-arguments '("--help") spec)))
      (?t (argument-set-p "help" args))
      (?null (argument-set-p "act" args)))
    (let ((args (parse-arguments '("--act" "--help") spec)))
      (!t (argument-set-p "act" args))
      (?null (argument-set-p "help" args))
      (?t (argument-set-p "help" (argument-value "act" args))))
    (?condition (parse-arguments '("--act" "--help") spec)
		help-set-signal
		(help-set-signal-action (argument "act" spec)))))
		

(deftest parsing-action-test
  (let ((spec (make-arguments-spec ("spec") (:action "act" :arguments ((:flag "flag"))))))
    (let ((args (parse-arguments '("--act" "--flag") spec)))
      (!t (argument-set-p "act" args))
      (!t (argument-set-p "flag" (argument-value "act" args))))))

(deftest argument-for-non-set-action
  (let ((spec (make-arguments-spec "" (:action "act" :arguments ((:flag "flag"))))))
    (!condition (parse-arguments '("--flag") spec)
		wrong-argument-error
		(cmd-parsing-error-argument "flag"))))

(deftest parsing-subactions-test
  (let ((spec (make-arguments-spec "" (:action "act" :arguments ((:action "act2"))))))
    (let ((args (parse-arguments '("--act" "--act2") spec)))
      (!t (argument-set-p "act" args))
      (!null (argument-set-p "act2" args))
      (!t (argument-set-p "act2" (argument-value "act" args))))))

(deftest subactions-same-arguments
  (let ((spec (make-arguments-spec "" (:flag "flag") (:action "act" :arguments ((:flag "flag"))))))
    (let ((args (parse-arguments '("--act" "--flag") spec)))
      (!null (argument-set-p "flag" args))
      (!t (argument-set-p "act" args))
      (!t (argument-set-p "flag" (argument-value "act" args))))))

(deftest arguments-for-non-set-subactoin
  (let ((spec (make-arguments-spec "" (:action "a1" :arguments ((:action "a2" :arguments ((:flag "f"))))))))
    (!condition (parse-arguments '("--a1" "--f") spec)
		wrong-argument-error
		(cmd-parsing-error-argument "f"))))

(deftest simple-group
  (let ((spec (make-arguments-spec "" (:group "group" :arguments ((:flag "flag1") (:flag "flag2"))))))
    (let ((args (parse-arguments '("--flag2") spec)))
      (!t (argument-set-p "flag2" args))
      (!null (argument-set-p "flag1" args)))))

(deftest group-with-max-one
  (let ((spec (make-arguments-spec "" (:group "group" :one-max :arguments ((:flag "f1") (:flag "f2"))))))
    (!condition (parse-arguments '("--f1" "--f2") spec)
		too-much-arguments-in-group-set
		(cmd-parsing-error-argument "group")
		(too-much-arguments-in-group-set-arguments '("f1" "f2")))))

(deftest groups-in-action-check
  (let ((spec (make-arguments-spec "" (:action "act" 
					       :arguments ((:group "group" :one-max :arguments ((:flag "f1")
												(:flag "f2"))))))))
    (!condition (parse-arguments '("--act" "--f1" "--f2") spec)
		too-much-arguments-in-group-set
		(cmd-parsing-error-argument "group")
		(too-much-arguments-in-group-set-arguments '("f1" "f2")))))

(deftest nested-groups-error-test
  (!error (make-arguments-spec "" (:group "g1" :arguments ((:group "g2"))))
	  "Nested groups not allowed"))

(deftest groups-with-one-min
  (let ((spec (make-arguments-spec "" (:group "g" :one-min :arguments ((:flag "f1"))))))
    (!condition (parse-arguments () spec)
		too-few-arguments-in-group-set
		(cmd-parsing-error-argument "g"))))

(deftest groups-with-one-only
  (let ((spec (make-arguments-spec "" (:group "g" :one-only :arguments ((:flag "f1") (:flag "f2"))))))
    (!condition (parse-arguments () spec)
		too-few-arguments-in-group-set
		(cmd-parsing-error-argument "g"))
    (!condition (parse-arguments '("--f1" "--f2") spec)
		too-much-arguments-in-group-set
		(cmd-parsing-error-argument "g")
		(too-much-arguments-in-group-set-arguments '("f1" "f2")))))

(deftest empty-spec-help
  (!equal (help-message (make-arguments-spec ("Empty spec" :no-help)))
	  (lines "Usage:"
		 "  Empty spec"
		 "")))

(deftest groups-printing
  (let ((spec (make-arguments-spec "My spec" (:group "g1" :arguments ((:flag "f1" :description "flag1") 
								      (:flag "f2" :description "flag2"))))))
    (!equal (help-message spec)
	    (lines "Usage:"
		   "  My spec [ARGS ...] [g1 ...]"
		   ""
		   "  Where ARGS are:"
		   '("    --help" 10 "Products this help message")
		   ""
		   "  Where g1 are:"
		   '("    --f1" 10 "flag1")
		   '("    --f2" 10 "flag2")
		   ""))))

(deftest restricted-groups-printing
  (let ((spec (make-arguments-spec ("My spec" :no-help)
		(:group "g1" :one-max)
		(:group "g2" :one-min)
		(:group "g3" :one-only))))
    (!equal (help-message spec)
	    (lines "Usage:"
		   "  My spec [g1] g2 [g2 ...] g3"
		   ""
		   "  Where g1 are:"
		   ""
		   "  Where g2 are:"
		   ""
		   "  Where g3 are:"
		   ""))))

(deftest group-name-same-as-arguments
  (make-arguments-spec "" (:group "arg" :arguments ((:flag "arg")))))

(deftest adding-argument-to-group
  (let ((spec (make-arguments-spec ("" :no-help) (:group "group"))))
    (add-argument (make-argument :flag "flag" :description "a flag") spec :group "group")
    (!t (have-argument-p "flag" spec))
    (!equal (help-message spec)
	    (lines "Usage:"
		   "   [group ...]"
		   ""
		   "  Where group are:"
		   '("    --flag" 10 "a flag")
		   ""))))

(deftest empty-action-help
  (let ((spec (make-arguments-spec ("" :no-help) (:action "act" :no-help))))
    (!equal (help-message spec)
	    (lines "Usage:"
		   "   [ARGS ...]"
		   ""
		   "  Where ARGS are:"
		   '("    --act" 10 "")
		   ""))))

(deftest added-arguments-parent
  (let ((spec (make-arguments-spec "spec"))
	(action #A(:action "act" :no-help)))
    (add-argument action spec)
    (!equal (help-message action)
	    (lines "Usage:"
		   "  spec --act"
		   ""))))

(deftest adding-group
  (let ((spec (make-arguments-spec ("" :no-help))))
    (add-argument #A(:group "group" :no-help :arguments ((:flag "flag"))) spec)
    (!equal (help-message spec)
	    (lines "Usage:"
		   "   [group ...]"
		   ""
		   "  Where group are:"
		   '("    --flag" 10 "")
		   ""))))

(deftest adding-positional
  (let ((spec (make-arguments-spec "")))
    (add-argument #A(:positional "bla" :type 'string) spec)
    (let ((args (parse-arguments '("blabla") spec)))
      (!equal (argument-value "bla" args) "blabla"))))

(deftest adding-argument-twice
  (let ((spec (make-arguments-spec "" (:flag "flag"))))
    (!condition (add-argument #A(:group "group" :arguments ((:flag "flag"))) spec)
		argument-already-exists-error)))

(deftest updating-argument
  (let ((spec (make-arguments-spec "" (:flag "flag"))))
    (update-argument #A(:key "flag" :type 'integer) spec)
    (!parse ("--flag" "123") spec
      (!= "flag" 123))))

(deftest updating-positional
  (let ((spec (make-arguments-spec "" (:positional "p" :type 'integer))))
    (update-argument #A(:positional "p" :type 'string) spec)
    (!parse ("bla") spec
      (!equal "p" "bla"))))

(deftest defining-positional
  (let ((spec (make-arguments-spec "" (:positional "key" :type 'integer))))
    (!t (have-argument-p "key" spec))))

(deftest parsing-positionals
  (let ((spec (make-arguments-spec "" (:positional "key" :type 'integer ))))
    (let ((args (parse-arguments '("123") spec)))
      (!t (argument-set-p "key" args))
      (!= (argument-value "key" args) 123))))

(deftest positionals-order-test
  (let ((spec (make-arguments-spec "" (:positional "key1" :type 'integer) 
				   (:positional "key2" :type 'integer))))
    (let ((args (parse-arguments '("123" "456") spec)))
      (!= (argument-value "key1" args) 123)
      (!= (argument-value "key2" args) 456))))

(deftest positonal-help-test
  (let ((spec (make-arguments-spec "" (:positional "key" :description "a number of universe")
				   (:positional "key2" :description "REAL number of universe"))))
    (!equal (help-message spec)
	    (lines "Usage:"
		   "   [ARGS ...] key key2"
		   ""
		   '("  Where key is" 10 "a number of universe")
		   ""
		   '("  Where key2 is" 10 "REAL number of universe")
		   ""
		   "  Where ARGS are:"
		   '("    --help" 10 "Products this help message")
		   ""))))
		     
(deftest positional-short-name-error
  (!error (macroexpand '(make-arguments-spec "" (:positional "arg" :short-name #\a)))
	  "Positional arguments can't have short names"))

(deftest positonal-name-eq-common-name-test
  (make-arguments-spec "" (:positional "arg") (:flag "arg")))

(deftest optional-positional-test
  (let ((spec (make-arguments-spec "" (:positional "p1" :type 'integer)
				   (:positional "p2" :type 'integer :optional))))
    (let ((args (parse-arguments '("123") spec)))
      (!= (argument-value "p1" args) 123)
      (!not (argument-set-p "p2" args)))))

(deftest optional-positinals-help
  (let ((spec (make-arguments-spec ("" :no-help) (:positional "p" :optional))))
    (!equal (help-message spec)
	    (lines "Usage:"
		   "   [p]"
		   ""
		   '("  Where p is" 10 "")
		   ""))))




(in-package #:burning-btr-test)

(in-case action-test)

(defmacro def-action-test (name &body body)
  `(deftest ,name
     (let ((*default-filesystem* (make-virtual-filesystem)))
       ,@body)))

(def-action-test running-without-action
  (!condition (btr-run '())
	      no-action-specified-error))

(defun make-files-from-string (&rest names)
  (mapc #'(lambda (name) (make-file (path-from-string name) :recursive t)) names))

(defun init-repository (&rest file-names)
  (btr-run '("--create"))
  (apply #'make-files-from-string file-names)
  (when file-names
    (btr-run (cons "--add" file-names))))

(def-action-test creation-test 
  (init-repository)
  (!t (path-exists-p (path-from-string "/work/.btr/repository.conf")))
  (!equal (read-file (path-from-string "/work/.btr/repository.conf"))
	  (lines "<repository version=\"0.1\"/>")))

(def-action-test creating-in-specified-directory
  (btr-run '("--create" "-R" "/home"))
  (!null (path-exists-p (path-from-string "/work/.btr/"))))

(def-action-test creating-with-existing-directory
  (write-repository (make-repository) (current-directory))
  (!condition (btr-run '("--create" "-R" "/work/bla"))
	      repository-already-exists-error
	      (repository-already-exists-error-path (current-directory) :test path=)))

(def-action-test adding-unit
  (init-repository "file")
  (!equal (read-file (path-from-string ".btr/repository.conf"))
	  (lines "<repository version=\"0.1\">"
		 "  <unit name=\"file\">"
		 "    <file name=\"file\"/>"
		 "  </unit>"
		 "</repository>")))

(def-action-test adding-several-units
  (init-repository "file" "file2" "file3")
  (!equal (read-file (path-from-string ".btr/repository.conf"))
	  (lines "<repository version=\"0.1\">"
		 "  <unit name=\"file\">"
		 "    <file name=\"file\"/>"
		 "  </unit>"
		 "  <unit name=\"file2\">"
		 "    <file name=\"file2\"/>"
		 "  </unit>"
		 "  <unit name=\"file3\">"
		 "    <file name=\"file3\"/>"
		 "  </unit>"
		 "</repository>")))

(def-action-test adding-units-without-repository 
  (make-file (path-from-string "file"))
  (!condition (btr-run '("--add" "file"))
	      repository-does-not-exist-error
	      (repository-does-not-exist-error-path (path-from-string "") :test path=)))

(def-action-test running-with-several-actions
  (make-file (path-from-string "file"))
  (!condition (btr-run '("--create" "--add" "file"))
	      too-much-actions-specified-error
	      (too-much-actions-specified-error-actions '("add" "create"))))

(def-action-test adding-groups
  (init-repository "dir/file1" "dir/file2" "dir2/file")
  (!equal (read-file (path-from-string ".btr/repository.conf"))
	  (lines "<repository version=\"0.1\">"
		 "  <group name=\"dir\">"
		 "    <unit name=\"file1\">"
		 "      <file name=\"dir/file1\"/>"
		 "    </unit>"
		 "    <unit name=\"file2\">"
		 "      <file name=\"dir/file2\"/>"
		 "    </unit>"
		 "  </group>"
		 "  <group name=\"dir2\">"
		 "    <unit name=\"file\">"
		 "      <file name=\"dir2/file\"/>"
		 "    </unit>"
		 "  </group>"
		 "</repository>")))

(def-action-test adding-wrong-files
  (make-file (path-from-string "/home/file"))
  (btr-run '("--create"))
  (!condition (btr-run '("--add" "/home/file"))
	      path-is-not-in-repository-error
	      (path-is-not-in-repository-error-path (path-from-string "/home/file") :test path=)
	      (path-is-not-in-repository-error-repository-path (path-from-string "") :test path=)))

(defmacro with-standard-output-to-string (&body body)
  `(let ((*standard-output* (make-string-output-stream)))
     ,@body
     (get-output-stream-string *standard-output*)))

(def-action-test simple-listing
  (init-repository "file1" "a-very-long-file-name" "file3")
  (!equal (with-standard-output-to-string (btr-run '("--ls")))
	  (lines " Path                  " 
		 "" 
		 " file1                 " 
		 " a-very-long-file-name " 
		 " file3                 ")))

(def-action-test listing-subdirectory 
  (init-repository "dir/file1" "dir/file2")
  (?lines= (with-standard-output-to-string (btr-run '("--ls" "dir")))
	   (lines " Path      "
		  ""
		  " dir/file1 "
		  " dir/file2 ")))

(def-action-test listing-groups
  (init-repository "file1" "dir/file2")
  (!equal (with-standard-output-to-string (btr-run '("--ls")))
	  (lines " Path  "
		 ""
		 " dir/  "
		 " file1 ")))

(def-action-test listing-recursive 
  (init-repository "file1" "dir1/file2" "dir1/file3" "dir2/file4")
  (!equal (with-standard-output-to-string (btr-run '("--ls" "-r")))
	  (lines " Path       "
		 ""
		 " dir1/file2 "
		 " dir1/file3 "
		 " dir2/file4 "
		 " file1      ")))

(def-action-test listing-from-other-directory
  (init-repository "file1" "dir/file2")
  (vfs-cd (path-from-string "dir/"))
  (!equal (with-standard-output-to-string (btr-run '("--ls" "-r")))
	  (lines " Path     "
		 ""
		 " file2    "
		 " ../file1 ")))

(def-action-test listing-from-outside 
  (init-repository "file1" "dir/file2")
  (vfs-cd (path-from-string "/home/"))
  (!equal (with-standard-output-to-string (btr-run '("--ls" "-r" "-R" "/work")))
	  (lines " Path            "
		 ""
		 " /work/dir/file2 "
		 " /work/file1     ")))

(def-action-test removing-tests
  (init-repository "file1" "dir/file2")
  (btr-run '("--rm" "file1"))
  (!equal (with-standard-output-to-string (btr-run '("--ls" "-r")))
	  (lines " Path      "
		 ""
		 " dir/file2 " ))
  (btr-run '("--rm" "dir/file2"))
  (!equal (with-standard-output-to-string (btr-run '("--ls" "-r")))
	  (lines "")))

(defun !path= (path1 path2)
  (!t (path= path1 path2)))

(def-action-test removing-wrong-files
  (init-repository "true.file" "dir/true.file")
  (make-files-from-string "false.file" "dir/other.false.file" "false.dir/some.file")
  (flet ((check-repository ()
	   (!equal (with-standard-output-to-string (btr-run '("--ls" "-r")))
		   (lines " Path          "
			  ""
			  " dir/true.file "
			  " true.file     "))))
    (!condition (btr-run '("--rm" "false.file"))
		file-is-not-in-repository-error
		(file-is-not-in-repository-error-path (path-from-string "false.file") :test !path=)
		(file-is-not-in-repository-error-repository-path (path-from-string "") :test !path=))
    (!condition (btr-run '("--rm" "dir/other.false.file"))
		file-is-not-in-repository-error)
    (!condition (btr-run '("--rm" "false.dir/some.file"))
		file-is-not-in-repository-error
		(file-is-not-in-repository-error-path (path-from-string "false.dir/some.file") :test !path=)
		(file-is-not-in-repository-error-repository-path (path-from-string "") :test !path=))
    (check-repository)))

(def-action-test removing-from-subdirectory
  (init-repository "file" "dir/file")
  (vfs-cd (path-from-string "dir/"))
  (btr-run '("--rm" "../file"))
  (!equal (with-standard-output-to-string (btr-run '("--ls" ".." "-r")))
	  (lines " Path "
		 ""
		 " file ")))

(def-action-test removing-from-outside
  (init-repository "file")
  (vfs-cd (path-from-string "/home/"))
  (btr-run '("--rm" "-R" "/work" "/work/file"))
  (!equal (with-standard-output-to-string (btr-run '("--ls" "-R" "/work" "-r")))
	  (lines "")))

(def-action-test removing-directory
  (init-repository "dir/file")
  (btr-run '("--rm" "dir/file"))
  (btr-run '("--rm" "dir"))
  (!equal (with-standard-output-to-string (btr-run '("--ls")))
	  (lines "")))

(def-action-test removing-non-empty-directory
  (init-repository "dir/file")
  (!condition (btr-run '("--rm" "dir"))
	      directory-is-not-empty-error
	      (directory-is-not-empty-error-path (path-from-string "dir/") :test !path=)
	      (directory-is-not-empty-error-repository-path (path-from-string "") :test !path=)))

(def-action-test removing-directory-recursive
  (init-repository "dir/file" "dir/subdir/file")
  (btr-run '("--rm" "-r" "dir"))
  (!equal (with-standard-output-to-string (btr-run '("--ls")))
	  (lines "")))

(def-action-test updating-from-subdirectory
  (init-repository "file" "dir/file")
  (vfs-cd (path-from-string "dir/"))
  (btr-run '("--update")))

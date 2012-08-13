(in-package #:burning-btr-test)

(in-case filesystem-test)

(defmacro def-fs-test (name &body body)
  `(deftest ,name
     (let ((*default-filesystem* (make-virtual-filesystem)))
       ,@body)))

(def-fs-test creating-repository
  (write-repository (make-repository) (path-from-string "/home/"))
  (!t (path-exists-p (path-from-string "/home/.btr/")))
  (let ((path (path-from-string "/home/.btr/repository.conf")))
    (!t (path-exists-p path))
    (!equal (read-file path) (lines "<repository version=\"0.1\"/>"))))

;;writing repository to file path error 

(def-fs-test finding-repository
  (let ((repository-path (path-from-string "/home/")))
    (write-repository (make-repository) repository-path)
    (make-file (path-from-string "/home/dir/dir2/file.ext") :recursive t)
    (labels ((check-repository-path (path-string)
	       (!t (path= (repository-path (path-from-string path-string)) repository-path)))
	     (check-repository-pathes (&rest path-names)
	       (mapc #'check-repository-path path-names)))
      (check-repository-pathes "/home/" 
			       "/home/file.ext"
			       "/home/dir/"
			       "/home/dir/dir2/"
			       "/home/dir/dir2/file.ext")
      (vfs-cd (path-from-string "/home/"))
      (check-repository-path "/home/file.ext")
      (!null (repository-path (path-from-string "/work/"))))))

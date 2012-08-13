(in-package #:burning-btr)

(defun configuration-path (path)
  (path+ path (path-from-string ".btr/repository.conf")))

(defgeneric file-names-to-paths (entity repository-path))

(defmethod file-names-to-paths ((entity unit) repository-path)
  (setf (unit-files entity) (mapcar #'(lambda (path) (path+ (as-absolute-path repository-path) 
							    (path-from-string path)))
				    (unit-files entity))))

(defmethod file-names-to-paths ((entity group) repository-path)
  (mapc #'(lambda (entity) (file-names-to-paths entity repository-path)) (entities entity)))

(defgeneric paths-to-file-names (entity repository-path))

(defmethod paths-to-file-names ((entity unit) repository-path)
  (setf (unit-files entity) 
	(mapcar #'(lambda (path) (path-to-string (path- path (as-absolute-path repository-path))))
		(unit-files entity))))

(defmethod paths-to-file-names ((entity group) repository-path)
  (mapc #'(lambda (entity) (paths-to-file-names entity repository-path)) (entities entity)))

(defun write-repository (repository path)
  (flet ((save-configuration (path)
	   (unless (path-exists-p path)  (make-file path :recursive t))
	   (with-open-file (stream path :direction :output :if-exists :supersede)
	     (print-repository repository stream))))
    (mapc #'(lambda (entity) (paths-to-file-names entity path)) (entities repository))
    (save-configuration (configuration-path path))
    (mapc #'(lambda (entity) (file-names-to-paths entity path)) (entities repository))))

(defun open-repository (path)
  (with-open-file (stream (path+ path (path-from-string ".btr/repository.conf")))
    (let ((repository (read-repository stream)))
      (mapc #'(lambda (entity) (file-names-to-paths entity path)) (entities repository))
      repository)))

(defun repository-path (path)
  (cond
    ((file-path-p path) (repository-path (parent-path path)))
    ((path-exists-p (configuration-path path)) path)
    ((parent-path path) (repository-path (parent-path path)))
    ((relative-path-p path) 
     (let ((absolute-path (repository-path (as-absolute-path path))))
       (if absolute-path
	   (as-relative-path absolute-path path)
	   nil)))
    (t nil)))

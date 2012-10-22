(in-package #:burning-bsdf-generator)

;;
;; make makefile-target struct and return it
;; generate method will write them to file
;;

(defmethod generator-filename ((generator (eql 'makefile)))
  "Makefile")

(defmethod generate-target ((generator (eql 'makefile)) target stream)
  (format stream "~{~a~^ ~} : ~{~a~^ ~}~%" (target-output target) (target-input target))
  (awhen (target-name target)
    (format stream ".PHONY : ~a~%" it))
  (when (target-name target) (format stream "~a : ~{~a~^ ~}" (target-name target) (target-output target))))
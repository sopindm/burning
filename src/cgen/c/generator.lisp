(in-package #:burning-cgen)

(in-language :c)

(defgenerator c-code-generator ())

(defmethod generator-add-source ((generator c-code-generator) source)
  (format t "~a~%" source))

(in-package #:burning-cgen-test)

(in-case reader-test)

(defun ?position= (stream line column chars)
  (?= (burning-cgen::counting-stream-line stream) line)
  (?= (burning-cgen::counting-stream-chars stream) chars)
  (?= (burning-cgen::counting-stream-column stream) column))

(deftest reading-from-file
  (with-input-from-string (stream (lines "(car" "cdr car)" "" "(nth car cdr)"))
    (let ((stream (burning-cgen::make-counting-stream stream)))
      (?position= stream 1 1 1)
      (?equal (read stream) '(car cdr car))
      (?position= stream 2 9 14)
      (?equal (read stream) '(nth car cdr))
      (?position= stream 4 14 29))))
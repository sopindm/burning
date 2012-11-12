(in-package #:bsdf-test)

(in-case reader-test)

(defun ?position= (stream line column chars)
  (?= (bsdf-streams::counting-stream-line stream) line)
  (?= (bsdf-streams::counting-stream-chars stream) chars)
  (?= (bsdf-streams::counting-stream-column stream) column))

(deftest reading-from-file
  (with-input-from-string (stream (lines "(car" "cdr car)" "" "(nth car cdr)"))
    (let ((stream (bsdf-streams::make-counting-stream stream)))
      (?position= stream 1 1 1)
      (?equal (read-preserving-whitespace stream) '(car cdr car))
      (?position= stream 2 9 14)
      (?equal (read-preserving-whitespace stream) '(nth car cdr))
      (?position= stream 4 14 29))))
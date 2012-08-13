(in-package :burning-lexical-test)

(in-case input-test)

(deftest simple-eof-test
  (with-input-from-string (in "")
    (!t (eof-p (make-iterator in))))
  (with-input-from-string (in "some string")
    (!eq (eof-p (make-iterator in)) nil))
  (with-input-from-string (in "other string")
    (with-input-iterator (iter in)
      (!eq (eof-p iter) nil))))

(deftest get-test
  (with-input-from-string (in "bla")
    (with-input-iterator (i in)
      (!eq (get-next i) #\b)
      (!eq (get-next i) #\l)
      (!eq (get-next i) #\a)
      (!t (eof-p i)))))

(deftest read-over-buffer
  (with-input-from-string (in "bb")
    (let ((iterator (make-iterator in 1)))
      (!eq (get-next iterator) #\b)
      (!eq (get-next iterator) #\b)
      (!t (eof-p iterator)))))

(deftest reset-test
  (with-input-from-string (in "bbb")
    (let ((iterator (make-iterator in 1)))
      (!eq (get-next iterator) #\b)
      (!eq (get-next iterator) #\b)
      (!eq (get-next iterator) #\b)
      (!t (eof-p iterator))
      (reset iterator)
      (!eq (get-next iterator) #\b)
      (!eq (get-next iterator) #\b)
      (!eq (get-next iterator) #\b)
      (!t (eof-p iterator)))))

(deftest commit-test
  (with-input-from-string (in "bbb")
    (let ((iterator (make-iterator in 1)))
      (get-next iterator)
      (get-next iterator)
      (get-next iterator)
      (!equal (commit iterator) "bbb")
      (!equal (commit iterator) "")
      (reset iterator)
      (!t (eof-p iterator)))))

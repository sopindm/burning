(in-package :burning-lexical-test)

(in-case state-machine-test)

(deflexic my-lexic if then word integer spaces r-word)

(deftest state-machine-creating
  (let ((machine (create-state-machine sample-lexic)))
    (!oequal (slot-value machine 'values)
	     (list nil nil nil sample-lexeme))
    (!equalp (slot-value machine 'transitions)
	     #(((1 . 0) (0 . 1))
	       ((1 . 2) (0 . 1))
	       ((1 . 3) (0 . 1))
	       ((1 . 0) (0 . 1))))))

(deftest state-machine-value
  (let ((machine (create-state-machine my-lexic)))
    (!eq (lexeme-name (machine-value machine "if")) 'if)
    (!eq (lexeme-name (machine-value machine "then")) 'then)
    (!eq (lexeme-name (machine-value machine "abbasgheqtaba")) 'word)
    (!eq (lexeme-name (machine-value machine "213548906")) 'integer)
    (!eq (lexeme-name (machine-value machine "+11412341230")) 'integer)
    (!eq (lexeme-name (machine-value machine "-110111235123451451450")) 'integer)
    (!eq (lexeme-name (machine-value machine "анализатор")) 'r-word)
    (!eq (lexeme-name (machine-value machine "")) 'spaces)
    (!eq (lexeme-name (machine-value machine "    ")) 'spaces)))

(deftest machine-with-minimal
  (let ((machine (create-state-machine min-lexic)))
    (!eq (lexeme-name (machine-value machine "\"bla-bla\"")) 'c-string)
    (!null (machine-value machine "\"\"bla-bla\""))))

(deflexeme skipped-space (:star #\Space) :skipped t)
(deflexeme value (:positive (:letter)))

(deflexic skip-lexic skipped-space value)

(deftest skip-test
  (let ((machine (create-state-machine skip-lexic)))
    (with-input-iterator (iterator (make-string-input-stream "   blablabla"))
      (let ((value (get-token iterator machine)))
	(!equal (first value) "blablabla")
	(!eq (lexeme-name (rest value)) 'value)))))
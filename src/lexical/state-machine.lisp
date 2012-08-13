(in-package :burning-lexical)

(defstruct (state-machine (:conc-name machine-) (:constructor %make-state-machine))
  translation
  values 
  transitions)

(defun make-state-machine ()
  (%make-state-machine :values (make-array 0 :adjustable t :fill-pointer 0)
		       :transitions (make-array 0 :adjustable t :fill-pointer 0)))

(defun fill-translation (translation first last value)
  (do ((i first (1+ i)))
      ((> i last) t)
    (setf (elt translation i) value)))

(defun (setf translation) (value machine)
  (setf (machine-translation machine)
	(make-array (1+ (char-code (cdaar (last value)))) :initial-element -1))
  (dolist (range value)
    (fill-translation (slot-value machine 'translation)
		      (char-code (caar range))
		      (char-code (cdar range))
		      (second range))))

(defun add-state (machine)
  (vector-push-extend nil (machine-values machine))
  (vector-push-extend nil (machine-transitions machine))
  (- (length (machine-values machine)) 1))

(defun value (machine state)
  (elt (slot-value machine 'values) state))

(defun (setf value) (value machine state)
  (setf (elt (slot-value machine 'values) state)
	value))

(defun next (machine state code)
  (if (null code)
      nil
      (cdr (assoc (elt (slot-value machine 'translation) (char-code code))
		  (elt (slot-value machine 'transitions) 
		       state)))))

(defun (setf next) (value machine state code)
  (push (cons code value)
	(elt (slot-value machine 'transitions) state)))

(defun fill-nexts (state lexic table)
  (let ((next (elt (lexic-nexts lexic) state)))
    (unless (null next)
      (setf (gethash next table)
	    (make-set (copy-list (elt (lexic-follows lexic) state))
		      (gethash next table))))))

(defun add-state-function (state machine visited-states &aux (new-states nil))
  (list 
   (lambda (next next-state)
     (let ((next-state-id (gethash next-state visited-states)))
       (unless next-state-id
	 (setq next-state-id (add-state machine))
	 (push next-state new-states)
	 (setf (gethash next-state visited-states) next-state-id))
       (setf (next machine 
		   (gethash state visited-states)
		   next) 
	     next-state-id)))
   (lambda () new-states)))

(defun state-value (state-set lexic)
  (dolist (state state-set)
    (let ((value (elt (lexic-values lexic) state)))
      (if value
	  (return value)))))

(defun set-state-transitions (state-set lexic machine visited-states)
  (let ((next-states (make-hash-table))
	(add-state-funcs (add-state-function state-set machine visited-states)))
    (dolist (state state-set)
      (fill-nexts state lexic next-states))
    (maphash (first add-state-funcs) next-states)
    (funcall (second add-state-funcs))))

(defun process-state (state lexic machine visited-states &aux next-states)
  (let ((value (state-value state lexic)))
    (setf (value machine (gethash state visited-states))
	  value)
    (unless (and value (lexeme-minimal-p value))
      (setq next-states (set-state-transitions state lexic machine visited-states))
      (dolist (next-state next-states)
	(process-state next-state lexic machine visited-states)))))

(defun create-state-machine (lexic)
  (let ((machine (make-state-machine))
	(visited-states (make-hash-table :test 'equal))
	(first-state (first-pos (lexic-expression lexic))))
    (setf (gethash first-state visited-states) (add-state machine))
    (process-state first-state lexic machine visited-states)
    (setf (translation machine) (lexic-translation lexic))
    machine))

(defun machine-value (machine string &optional (state 0) (pos 0))
  (cond 
    ((null state) nil)
     ((= (length string) pos) (value machine state))
     (t (machine-value machine string (next machine state (char string pos))
		       (1+ pos)))))

(deflexeme eps (:empty))

(defun eof-token ()
  `("" . ,eps))

(defun return-token (iterator safed-iterator machine type)
  (cond ((equal safed-iterator (iterator-back iterator)) (error "Wrong token ~a" (commit iterator)))
	((lexeme-skipped-p type) 
	 (setf (iterator-forward iterator) safed-iterator)
	 (commit iterator)
	 (if (eof-p iterator) 
	     (eof-token)
	     (get-token iterator machine)))
	(t (setf (iterator-forward iterator) safed-iterator)
	   (cons (commit iterator) type))))

(defun get-token (iterator machine)
  (declare (optimize (speed 3) (safety 0)))
  (do ((type nil)
       (state 0)
       (safed-iterator (copy-simple-iterator (iterator-back iterator))))
      ((null state) (return-token iterator safed-iterator machine type))
    (when (value machine state)
      (setq type (value machine state))
      (setq safed-iterator (copy-simple-iterator (iterator-forward iterator))))
    (let ((next-value (get-next iterator)))
      (if (char= next-value #\Null)
	  (setq state nil)
	  (setq state (next machine state next-value))))))

(defun get-lexeme (iterator machine)
  (if (eof-p iterator)
      (eof-token)
      (get-token iterator machine)))
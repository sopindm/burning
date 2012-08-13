(in-package :burning-lexical)

;;
;; BUFFERS LIST
;;

(defstruct (buffer (:constructor %make-buffer))
  value
  size
  next)

(defun make-buffer (size stream)
  (let* ((value (make-array (1+ size) :element-type 'character :initial-element #\Null))
	 (buffer-size (read-sequence value stream :end size)))
    (%make-buffer :value value :size buffer-size :next nil)))

(defun read-buffer (buffer stream size)
  (setf (buffer-next buffer) (make-buffer size stream)))

;;
;;  SIMPLE ITERATOR
;;

(defstruct (simple-iterator (:constructor %make-simple-iterator) (:conc-name si-))
  buffers
  position)

(defun make-simple-iterator (buffers position)
  (%make-simple-iterator :buffers buffers :position position))

(defun eol-p (iterator)
  (>= (si-position iterator)
      (1- (buffer-size (si-buffers iterator)))))

(defun move-to-next-buffer (iterator)
  (setf (si-position iterator) -1)
  (setf (si-buffers iterator) (buffer-next (si-buffers iterator))))

(defun move-to-forward (forward back)
  (if (eq (si-buffers forward) (si-buffers back))
      (let ((value (subseq (buffer-value (si-buffers forward))
			   (1+ (si-position back)) (1+ (si-position forward)))))
	(setf (si-position back) (si-position forward))
	value)
      (let ((value (subseq (buffer-value (si-buffers back))
			   (1+ (si-position back)) (buffer-size (si-buffers back)))))
	(move-to-next-buffer back)
	(concatenate 'string value (move-to-forward forward back)))))

;;
;; ITERATOR
;;

;;iterator
;; * forward : simple iterator
;; * buffer size
;; * stream

(defstruct (iterator (:constructor %make-iterator))
  forward
  back
  buffer-size
  stream)

(defun make-iterator (stream &optional (buffer-size 4096))
  (let* ((buffers (make-buffer buffer-size stream))
	 (forward (make-simple-iterator buffers -1))
	 (back (make-simple-iterator buffers -1)))
    (%make-iterator :forward forward :back back :buffer-size buffer-size :stream stream)))

(defun try-read-buffer (iterator)
  (let ((forward (iterator-forward iterator)))
    (if (>= (si-position forward) (1- (iterator-buffer-size iterator)))
	(progn
	  (when (null (buffer-next (si-buffers forward)))
	    (read-buffer (si-buffers forward) (iterator-stream iterator) (iterator-buffer-size iterator)))
	  (move-to-next-buffer forward)
	  (not (eof-p iterator)))
	nil)))

(defun eof-p (iterator)
  (let ((forward (iterator-forward iterator)))
    (if (eol-p forward)
        (not (try-read-buffer iterator))
	nil)))

(defmacro with-input-iterator ((iterator stream) &body body)
  `(let ((,iterator (make-iterator ,stream)))
     ,@body))

(defun try-next-buffer (iterator)
  (if (eof-p iterator)
      #\Null
      (get-next iterator)))

(defun get-next (iterator)
  (let* ((forward (iterator-forward iterator))
	 (value (aref (buffer-value (si-buffers forward))
		      (incf (si-position forward)))))
    (if (char/= value #\Null)
	value
	(try-next-buffer iterator))))

(defun reset (iterator)
  (setf (iterator-forward iterator)
	(copy-simple-iterator (iterator-back iterator))))

(defun commit (iterator)
  (move-to-forward (iterator-forward iterator)
		   (iterator-back iterator)))
       
		 
	 
       
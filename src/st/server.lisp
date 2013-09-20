(in-package #:burning-st)

(defvar *server* ())
(defvar *server-udp* ())
(defvar *games* ())
(defvar *connections* (make-shared-variable ()))

(defun %accept-connection ()
  (socket-accept *server* :element-type '(unsigned-byte 8)))

(defvar *users* (make-shared-variable ()))

(defun stop-server ()
  #+never(let ((connections *connections*))
	(with-shared-variable connections
	  (mapc #'socket-close connections)))
  (socket-close *server*))

(defvar *start-time*)
(defvar *start-internal-time*)

(defvar *game-queues*)

(defun get-time ()
  (floor (+ (* *start-time* 1000) 
			(* (/ (- (get-internal-real-time) *start-internal-time*) internal-time-units-per-second) 1000))))

(defun start-server ()
  (setf *server* (socket-listen "192.168.171.214" 8003 :element-type '(unsigned-byte 8) :reuse-address t))
  (setf *connections* (make-shared-variable nil))
  (setf *users* (make-shared-variable ()))
  (setf *game-queues* '(() () ()))
  (setf *games* ())
  (spawn-thread (lambda (x)
				  (handler-case 
					  (while t
						(let ((connection (%accept-connection)))
						  (with-shared-variable x
							(push connection x))))
					(connection-aborted-error ()
					  (format t "Closing connection stream"))))
				*connections*)
  (setf *start-time* (* (get-universal-time) 1000))
  (setf *start-internal-time* (get-internal-real-time))
  (let ((timeout 0.1))
	(while t
	  (let* ((connections *connections*)
			 (connections (with-shared-variable connections connections)))
		(multiple-value-bind (ready remains) (wait-for-input connections :timeout timeout :ready-only t)
		  (setf timeout remains)
		  (format t "Ready ~a~%" ready)
		  (if ready
			  (handler-case
				  (send-response (protobuf-read-message (socket-stream (first ready)) 'message) (socket-stream (first ready)))
				(end-of-file ()
				  (let ((connections *connections*))
					(with-shared-variable connections
					  (setf connections (remove (first ready) connections))))))
			  (progn (update-games)
					 (setf timeout 0.1))))))))

(defun update-games ()
  (format t "Updating games..."))

(defun send-response (message stream)
  (format t "Responding on ~a~%" (message-type message))
  (%send-response (message-type message) message stream))

(defmacro defresponse (message-type (message stream) &body body)
  `(defmethod %send-response ((message-type (eql ',message-type)) ,message ,stream)
	 ,@body))

(defun restart-server ()
  (when *server*
	(stop-server))
  (start-server)
  nil)

(defun send-message (msg stream)
  (protobuf-write-message stream msg)
  (force-output stream))

(defresponse time-sync (msg stream)
  (let ((response (make-instance 'message :type 'time-sync
								 :time (get-time))))
	(send-message response stream)))

(defun send-response-message (response request stream)
  (setf (response-request response) request)
  (send-message (make-instance 'message
							   :type 'response
							   :response response)
				stream))

(defun send-request-response (request stream)
  (ecase (request-type request)
	(login (if (login-user (login-name (request-login request)) stream)
			   (send-response-message (make-instance 'response :status t) request stream)))
	(get-inventory (send-inventory (get-user :stream stream) request stream))
	(get-profile (send-profile (get-user :stream stream) request stream))
	(get-shop (send-shop (get-user :stream stream) request stream))
	(ready (add-user-to-game-queue (get-user :stream stream) (ready-type (request-ready request))))
	(game-request (send-game-request-response (request-game-request request) stream))
	(buy-item (buy-item (get-user :stream stream) (item-item-type (buy-item-item (request-buy-item request))) stream))
	(move-item (move-item (get-user :stream stream) (request-move-item request)))))

(defun send-game-request-response (request stream)
  (ecase (game-request-type request)
	(leave-game (leave-game (get-user :stream stream)))))

(defstruct user
  id
  name
  stream
  (inventory (make-inventory))
  (money 10000)
  (exp 0)
  (level 1)
  (game nil))

(defun make-inventory ()
  (let ((inv (make-instance 'inventory
							:items ()
							:active ()
							:passive ())))
	(flet ((make-empty-item (pos)
			 (make-instance 'inventory-item
							:item (make-instance 'item
												 :item-type 'none)
							:inventory-position pos)))
	  (dotimes (i 6)
		(push (make-empty-item i) (inventory-items inv)))
	  (dotimes (i 3)
		(push (make-empty-item i) (inventory-active inv)))
	  (dotimes (i 1)
		(push (make-empty-item i) (inventory-passive inv))))
	inv))

(defun get-user (&key (name nil name-p) (id nil id-p) (stream nil stream-p))
  (with-shared-variable (users *users*)
	(cond (name-p (find name users :test #'equal :key #'user-name))
		  (id-p (find id users :test #'eql :key #'user-id))
		  (stream-p (find stream users :key #'user-stream)))))

(defun login-user (name stream)
  (aif (get-user :name name)
	   (setf (user-stream it) stream)
	   (with-shared-variable (users *users*)
		 (push (make-user :id (length users) :name name :stream stream) users))))

(defun send-inventory (user request stream)
  (send-response-message (make-instance 'response
									   :status t
									   :inventory (user-inventory user))
						request stream))

(defun send-profile (user request stream)
  (send-response-message (make-instance 'response
										:status t
										:profile (make-instance 'player
																:id (user-id user)
																:money (user-money user)
																:exp (user-exp user)
																:level (user-level user)))
						 request stream))

(defun buy-item (user id stream)
  (let ((item (get-item id)))
	(when (and (>= (user-money user) (shop-item-price item)))
	  (let ((slot (find-if (lambda (x) (eq (item-item-type (inventory-item-item x)) 'none)) (inventory-items (user-inventory user)))))
		(when slot
		  (decf (user-money user) (shop-item-price item))
		  (setf (item-item-type (inventory-item-item slot)) id))))))

(defun move-item (user request)
  (let ((inventory (user-inventory user)))
	(let ((inv (elt (inventory-items inventory) (move-item-inventory-slot request)))
		  (active (if (slot-boundp request 'active-slot) (elt (inventory-active inventory) (move-item-active-slot request))))
		  (passive (if (slot-boundp request 'passive-slot) (elt (inventory-passive inventory) (move-item-passive-slot request)))))
	  (let ((item1 inv)
			(item2 (if active active passive)))
		(let ((item1-item (inventory-item-item item1))
			  (item2-item (inventory-item-item item2)))
		  (setf (inventory-item-item item1) item2-item)
		  (setf (inventory-item-item item2) item1-item))))))

(defun get-item (type)
  (flet ((item-price (type)
		   (ecase type
			 (weight 10)
			 (trap 14)
			 (changer 20)
			 (speed-amulet 100)
			 (endurance-amulet 200)
			 (protection-amulet 150)
			 (greed-amulet 250)
			 (pockets-amulet 500)))
		 (item-level (type)
		   1))
	(make-instance 'shop-item
				   :item (make-instance 'item :item-type type)
				   :price (item-price type)
				   :min-level (item-level type))))

(defun get-shop-items ()
  (mapcar #'get-item '(weight trap changer speed-amulet endurance-amulet protection-amulet greed-amulet pockets-amulet)))

(defun send-shop (user request stream)
  (send-response-message (make-instance 'response
										:status t
										:shop-info (make-instance 'shop-info
																  :item-descriptions (get-shop-items)))
						 request stream))

(defun add-user-to-game-queue (user game-type)
  (ecase game-type 
	(one-player (push user (first *game-queues*)))
	(two-player (push user (second *game-queues*)))
	(three-player (push user (third *game-queues*))))
  (setf (first *game-queues*) (check-game-queue (first *game-queues*) 1))
  (setf (second *game-queues*) (check-game-queue (second *game-queues*) 2))
  (setf (third *game-queues*) (check-game-queue (third *game-queues*) 3))
  (format t "Game queues: ~a" *game-queues*))

(defun check-game-queue (queue size)
  (if (>= (length queue) size)
	  (let ((game-players ()))
		(dotimes (i size)
		  (push (nth i queue) game-players))
		(start-game game-players)
		(nthcdr size queue))
	  queue))
		  
(defun start-game (users)
  (let ((game (make-game users)))
	(mapc (lambda (x) (setf (user-game x) game)) users)
	(mapc (lambda (x) (send-game-state game (user-id x) (user-stream x))) users)
	(push game *games*)))

(defun leave-game (user)
  ())

(defresponse request (msg stream)
  (mapc (lambda (x) (send-request-response x stream)) (message-requests msg)))

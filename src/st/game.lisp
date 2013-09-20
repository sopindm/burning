(in-package #:burning-st)

(defstruct (game (:constructor %make-game))
  players
  player-states)

(defstruct (player-state (:constructor make-player-state (x y)))
  x
  y)

(defun make-game (players)
  (let ((game (%make-game :players players)))
	(setf (game-player-states game) (make-hash-table))
	(mapc (lambda (x) (setf (gethash x (game-player-states game)) (make-player-state 0 0))) players)
	game))

(defun broadcast-join (game id)
  ())

(defun send-game-state (game id stream)
  (flet ((make-player (player)
		   (make-instance 'player 
						  :id (user-id player)
						  :name (user-name player)
						  :x (player-state-x (gethash player (game-player-states game)))
						  :y (player-state-y (gethash player (game-player-states game))))))
	(send-message (make-instance 'message
								 :type 'game-state
								 :game-state (make-instance 'game-state
															:title "Sample level"
															:your-id id
															:min-players 1
															:exit (make-instance 'exit 
																				 :x 10
																				 :y 10
																				 :width 10
																				 :height 10)
															:players (mapcar #'make-player (game-players game))))
				  stream)))
				
  
  

#| Player Object Functions |#
;-------------------------
(defclass player () ;define a class player
  ((my_token 
  	:reader my_token 
  	:initarg :my_token))) ;initial value is the parameter given during initialization 
   ;defines that variable "win_status" is a read & write 
;inital value is 0 votes

(defvar *players* '()) ;defines global-variable players as non-persistent list

;store instances of player objects in global dynamic variables
(defvar player_x (make-instance 'player :my_token "X"));
(defvar player_o (make-instance 'player :my_token "O")); 

;sets Player 1 as "O"
(defun setO()
	(push player_x *players*)
	(push player_o *players*))

;sets Player 1 as "X"
(defun setX()
	(push player_o *players*)
	(push player_x *players*))

;print particular token in players 
(defun print-token(index)
	(format t "~a" (my_token (nth index *players*))))

;returns string representation of token
(defun get-token(index)
	(my_token (nth index *players*)))

;-------------------------
#| END Player Object Functions |#


#| Turn Based Functions |#
;-------------------------
;defines turns, starts with Player 1 ie 0
(defvar player-turn 0)

(defun change-turn ()
	(setf player-turn (mod (+ player-turn 1) 2)))

;check whose turn it is
(defun whose-turn()
	(if (= (+ player-turn 0) 0) 
		(format t "Player 1's Turn") 
		(format t "Player 2's Turn")))

;return a turn value based on whose turn it is
(defun turn-value()
	(if (= (+ player-turn 0) 0) 0 1))

(defun whose-turn()
	(if (= (+ player-turn 0) 0) 
		(format t "Player 1's Turn") 
		(format t "Player 2's Turn")))
;-------------------------
#| END Turn Based Functions |#



#| Scoring Game Functions |#
;-------------------------
;defines game-status variable
(defvar is-game-over "false")

(defun change-status()
	(setf is-game-over "true")


;creates an array to keep track of score based
;on the 8 possible win poitions in tic tac toe
;first to get to +15 or -15 wins
(defun score-board()
	(setf scores (make-array 8 
				:element-type 'integer
				:initial-element 0)))


;based on whose turn it is this function
;adds +5 or -5 to scores array
;for sanity's sake Player 1 is always assigned +5 and vice versa
(defun add-score(index value)
	(setf (aref scores index) (+ (aref scores index) value)))

;return a score-value based on whose turn it is
(defun score-value()
	(if (= (+ player-turn 0) 0) 5 -5))

;;create copy of new_scores --> temp_scores
;;assign variable +5 or -5 to assign to new_scores list of indexes
;;go through scores and +5 or -5 if the index is a match
;; for current index use aref to change it then pop temp_scores
;;continue until temp_scores is empty
;(format t "~a" (aref scores index))
;TO BE WRITTEN!

;checks scores for winner
(defun game-check()
	(loop for index from 0 to 7 do
		(if (= (aref scores index) 15) 
			(one-wins)
			(format t ""))
		(if (= (aref scores index) -15) 
			(two-wins)
			(format t ""))))

(defun one-wins()
	(format t "Player One Wins!")
	(change-status))

(defun two-wins()
	(format t "Player Two Wins!")
	(change-status))

;-------------------------
#| END Scoring Game Functions |#


#| Tic Tac Toe Board Formatting |#
;-------------------------

(defun set-board()
	(setf my_board (make-array '(9)))
	(loop for index from 0 to 8 do
    (setf (aref my_board index) (+ index 1))))
	
(defun print-board()
	(loop for index from 0 to 8 do
		(format t "~a" (aref my_board index))))

(defun reset-board()
	(loop for index from 0 to 8 do
    (setf (aref my_board index) (+ index 1))))

(defun empty-board()
	(loop for index from 0 to 8 do
    (setf (aref my_board index) " ")))

;use this for tic-tac-toe display
(defun print-2D()
	(loop for index from 0 to 2 do
		(loop for inner_index from 0 to 2 do
			(format t "   ~a   |" (aref my_board (+ (* index 3) inner_index))))
		(terpri)
		(format t " ______________________ ~%")
		(terpri)))

(defun print-square(index)
	(format t "~a" (aref my_board (- index 1))))

;Method used to add tokens to board
(defun change-square(index token)
	(setf (aref my_board (- index 1)) token))
;-------------------------
#| END Tic Tac Toe Board Formatting |#


#| Game Reset Functions |#
;-------------------------
;Clears out players for the next game
(defun reset-players()
	(pop *players*)
	(pop *players*))
	
;used to reset game
(defun reset-turn()
	(setf player-turn 0))
;-------------------------
#| END Game Reset Functions |#


#| Game Play Functions |#
;-------------------------
;Looped menu method
;-------------------------
#| END Game Play Functions |#
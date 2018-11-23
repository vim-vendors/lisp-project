#| Player Object Functions |#
(defclass player () ;define a class player
  ((my_token 
  	:reader my_token 
  	:initarg :my_token) ;initial value is the parameter given during initialization 
   ;defines that variable "win_status" is a read & write 
   (win_status 
   	:accessor win_status 
   	:initform "false"))) ;inital value is 0 votes

;store instances of player objects in global dynamic variables
(defvar player_x (make-instance 'player :my_token "X"));
(defvar player_o (make-instance 'player :my_token "O")); 

(defvar *players* '()) ;defines global-variable players as non-persistent list

;sets Player 1 as "O"
(defun setO()
	(push player_x *players*)
	(push player_o *players*))

;sets Player 1 as "X"
(defun setX()
	(push player_o *players*)
	(push player_x *players*))

;Clears out players for the next game
(defun reset-players()
	(pop *players*)
	(pop *players*))

;print particular token in players 
(defun access-token(index)
	(format t "~a" (my_token (nth index *players*))))

;creates an array to keep track of score based
;on the 8 possible win poitions in tic tac toe
;first to get to +15 or -15 wins
(defun score-board()
	(setf scores (make-array 8 
				:element-type 'integer
				:initial-element 0)))

;based on whose turn it is this function
;adds +5 or -5 to scores variable
;for sanity's sake Player 1 is always assigned +5 and vice versa
(defun add-score(index token))



#| Tic Tac Toe Board Formatting |#

;useful for defining array from 0 to size, not used here -->future connect four
; (defun printlong(size)
; 		(loop for index from 0 to (- size 1) do
; 		(format t "~a " index)))

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

(defun print-2D()
	(loop for index from 0 to 2 do
		(loop for inner_index from 0 to 2 do
			(format t "   ~a   |" (aref my_board (+ (* index 3) inner_index))))
		(terpri)
		(format t " ______________________ ~%")
		(terpri)))

(defun print-square(index)
	(format t "~a" (aref my_board (- index 1))))

(defun change-square(index token)
	(setf (aref my_board (- index 1)) token))


;;test for winning conditions
;;set winner
;;minmax

(set-board)
(print-2D)
; (print-board)

;accesses an object in players
 (format t "~a" (WIN_STATUS (first *players*)))



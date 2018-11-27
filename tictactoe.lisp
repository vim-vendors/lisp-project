#| Player Object Functions |#
;-------------------------
(defclass player () ;define a class player
  ((my_token 
  	:reader my_token 
  	:initarg :my_token))) ;initial value is the parameter given during initialization 
   ;defines that variable "my_token" is a read only after initialization

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

;return a turn value based on whose turn it is
(defun turn-value()
	(if (= (+ player-turn 0) 0) 0 1))

;used primarily for testing
(defun whose-turn()
	(if (= (+ player-turn 0) 0) 
		(format t "Player 1's Turn~%") 
		(format t "Player 2's Turn~%")))
;-------------------------
#| END Turn Based Functions |#


#| Boolean State Functions |#
;-------------------------
;two player game boolean check
(defvar two-player "true")

;choose one player vs minmax AI game boolean check
(defun change-two-player() 
	(setf two-player "false"))

;reset two player game boolean check
(defun reset-two-player() 
	(setf two-player "true"))


;defines game-status variable
(defvar is-game-over "false")

;change boolean to indicate the game is over
(defun change-status()
	(setf is-game-over "true"))

;build boolean array to define valid board choices
(defun build-valid()
	(setf valid-choices (make-array '(9)))
	(loop for index from 0 to 8 do
    	(setf (aref valid-choices index) "true")))


;update boolean array to define valid board choices
(defun update-valid(index)
    	(setf (aref valid-choices index) "false"))

;print "board" array as 1d array - for testing purposes
(defun print-valid()
	(loop for index from 0 to 8 do
		(format t "~a " (aref valid-choices index))))

;returns 1 (true) or 0 (false) to validate choice before proceeding
(defun check-validity(index)
	(if (string= (aref valid-choices index) "true") 
			(+ 1 0)
			(+ 0 0 )))
;-------------------------
#| END Boolean State Functions |#



#| Scoring/Choice Game Functions |#
;-------------------------

;creates an array to keep track of score based
;on the 8 possible win poitions in tic tac toe
;first to get to +15 or -15 wins
(defun create-scores()
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


;function to tabulate scores based on a given list
(defun scoring (list)
	;;create copy of list of scores to update
	(let ((temp)))
	(setq temp (copy-tree list))
	;;go through scores and +5 or -5 if the index is a match
	(loop for index in temp
		do (add-score index (score-value))))

;switch case that assigns various scores based on user choice
;each choice represents a square, each list passed to scoring
;represents the possible paths to victory for either player
(defun user-score(choice)
	(case choice
		(1 (scoring (write (list 0 3 7))))
		(2 (scoring (write (list 0 4))))
		(3 (scoring (write (list 0 5 6)))) 
		(4 (scoring (write (list 1 3))))
		(5 (scoring (write (list 1 4 6 7))))
		(6 (scoring (write (list 1 5))))
		(7 (scoring (write (list 2 3 6))))
		(8 (scoring (write (list 2 4))))
		(9 (scoring (write (list 2 5 7))))))

;checks scores for winner
(defun game-check()
	(loop for index from 0 to 7 do
		(if (= (aref scores index) 15) 
			(one-wins)
			(format t ""))
		(if (= (aref scores index) -15) 
			(two-wins)
			(format t ""))))

;declares winner and goes to post-game cleanup function
(defun one-wins()
	(format t "Player One Wins!")
	(print-2D)
	(post-game)) 

(defun two-wins()
	(format t "Player Two Wins!")
	(print-2D)
	(post-game)) 

;-------------------------
#| END Scoring Game Functions |#


#| Tic Tac Toe Board Formatting |#
;-------------------------

;create an array of size 9 to represent indexes 1-9 in a tic tac toe board
;for display purposes only, actual score is tracked in scores array
(defun set-board()
	(setf my_board (make-array '(9)))
	(loop for index from 0 to 8 do
    (setf (aref my_board index) (+ index 1))))
	
;print "board" array as 1d array - for testing purposes
(defun print-board()
	(loop for index from 0 to 8 do
		(format t "~a" (aref my_board index))))

;standard board reset with indexes, used for testing
(defun reset-board()
	(loop for index from 0 to 8 do
    (setf (aref my_board index) (+ index 1))))

;use this for tic-tac-toe display
(defun print-2D()
	(terpri)
	(format t " ________BOARD____________ ~%")
	(terpri)
	(loop for index from 0 to 2 do
		(loop for inner_index from 0 to 2 do
			(format t "   ~a   |" (aref my_board (+ (* index 3) inner_index))))
		(terpri)
		(format t " ______________________ ~%")
		(terpri)))

;test code to print a particular square
(defun print-square(index)
	(format t "~a" (aref my_board (- index 1))))

;Method used to add Player tokens to board, parameters 
;are the index of the board array and the identity of the player provided by turn-value
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
	
;used to reset turns
(defun reset-turn()
	(setf player-turn 0))

;used to reset the board to zero
(defun empty-board()
	(loop for index from 0 to 8 do
    (setf (aref my_board index) " ")))

;reset game status for a new game
(defun reset-game-status ()
	(setf is-game-over "false"))

;resets scores array to zero
(defun reset-scores()
	(loop for index from 0 to 7 do
	(setf (aref scores index) 0)))

;reset boolean array for new game
(defun reset-valid()
	(loop for index from 0 to 8 do
    	(setf (aref valid-choices index) "true")))

;-------------------------
#| END Game Reset Functions |#


#| Game Play Functions |#
;-------------------------
;generic prompt function
(defun get-user-input (prompt)
  (format *query-io* "~a: " prompt)
  (force-output *query-io*)
  (read-line *query-io*))

;get and sanitize user input
(defun get-square-choice()
	(setf temp (get-user-input "Please enter a square number between 1 and 9"))
	;convert to integer if possible
	(if (parse-integer temp :junk-allowed t) 
		(setf temp (abs (parse-integer temp))) 
		(user-error))
	;if integer is within bounds and square not already taken
	(if (and (bounds-check temp) (= (check-validity (- temp 1))1)) 
		(implement-choice temp) 
		(user-error)))

;if user choice is valid implement
(defun implement-choice(index)
		;update board (index 1-9)
		(change-square index (get-token (turn-value)))
		;update scores (index 1-9)
		(user-score index)
		;update board vailidity (index 0-8) 
		(update-valid (- index 1)))

;square choice error correction
(defun user-error()
	(format t "That input is invalid, please try again.~%")
	(get-square-choice))

(defun bounds-check(number)
	(and (>= number 1) (< number 10)))

;Player 1 chooses his token, default assigned to Player 2
(defun get-token-choice()
	(setf temp (get-user-input "Please enter an 'X' or an 'O' to choose your symbol"))
	;parse user input
	(setf temp (coerce (subseq (string-upcase temp) 0 1) 'character))
	;based on user choice assigns player one to a token (X or O)
	(cond ((char= temp #\X) (player-is-X)) 
		  ((char= temp #\O) (player-is-O)) 
		  ((not (or (char= temp #\X) (char= temp #\O))) (token-error))))

;get multi-player choice from user - starts or ends game
(defun get-menu-choice()
	(format t "A) Play Game~%")
	(format t "B) Exit Game~%")
	(setf temp (get-user-input "Please choose option A or B: "))
	;parse user input
	(setf temp (coerce (subseq (string-upcase temp) 0 1) 'character))
	;based on user choice A or B chooses to start or end game
	(cond ((char= temp #\A) (start-game)) 
		  ((char= temp #\B) (game-end)) 
		  ((not (or (char= temp #\A) (char= temp #\B))) (menu-error))))

;menu choice error correction
(defun menu-error()
	(format t "Please choose 'A' or 'B'.~%")
	(get-menu-choice))

;get multi-player choice from user
(defun choose-multi()
	(setf temp (get-user-input "Is this a one-player game? Y/N: "))
	;parse user input
	(setf temp (coerce (subseq (string-upcase temp) 0 1) 'character))
	;based on user choice assigns player one to a token (X or O)
	(cond ((char= temp #\Y) (change-two-player)) 
		  ((char= temp #\N) (reset-two-player)) 
		  ((not (or (char= temp #\Y) (char= temp #\N))) (token-error-two))))

;chooses and announces player order
(defun player-is-X()
	(format t "Player One is X. Player Two is O.~%")
	(setX))

(defun player-is-O()
	(format t "Player One is O. Player Two is X.~%")
	(setO)) 

;multi-player error catching
(defun token-error()
	(format t "Please choose 'X' or 'O'.~%")
	(get-token-choice))

;multi-player error catching
(defun token-error-two()
	(format t "Please choose 'Y' or 'N'.~%")
	(choose-multi))

;called from winner function
(defun post-game()
	;(change-status)
	(game-reset)
	(game-menu))

;master game reset
(defun game-reset()
	(reset-players)
	(reset-turn)
	(empty-board)
	;(reset-game-status)
	(reset-scores)
	(reset-valid)
	;(reset-two-player)
	)

;Looped menu method
(defun start-game()
	;initialize board
	(set-board)
	;initialize scores
	(create-scores)
	;initialize validity 
	(build-valid)
	;get user tokens and initialize players
	(get-token-choice)
	;decide if one or two players
	;(choose-multi)
	;go to play-game
	(play-game))

;pre-game check
(defun play-game()
	;check game status
	;if game over announce winner, reset game and go to game-menu
	(game-check)
	;else go to game-loop
	(game-loop))

;main body of game
(defun game-loop()
	;redraw current board
	(print-2D)
	;announce whose turn it is
	(whose-turn)
	;choose player square
	;check and update values: scores, validity, board status
	(get-square-choice)
	;redraw board
	;(print-2D)
	;change turn
	(change-turn)
	;if AI chosen go to ai-loop 
	;else go to play-game
	(play-game))

;(defun ai-loop()
	;check game-status
	;if game over announce winner, reset game and go to game-menu
	;else run min-max
	;update values based on min-max - scores, validity, redraw display board, game status
	;change turn
	;go to play-game
	;)

(defun game-menu()
	;starts or ends loop
	(terpri)
	(get-menu-choice))

;end game and close REPL
(defun game-end()
	(format t "Thank you for playing! Goodbye!~%")
	(quit))
;-------------------------
#| END Game Play Functions |#


(game-menu)




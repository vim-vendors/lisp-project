#| Tic Tac Toe Board Formatting |#

(defclass player () ;define a class player
  ((my_token 
  	:reader my_token 
  	:initarg :my_token) ;initial value is the parameter given during initialization 
   ;defines that variable "win_status" is a read & write 
   (win_status 
   	:accessor win_status 
   	:initform "false"))) ;inital value is 0 votes

;store instances of player objects in global dynamic variables
(defvar player_one (make-instance 'player :my_token "X"));
(defvar player_two (make-instance 'player :my_token "O")); 


;useful for defining array from 0 to size
(defun printlong(size)
		(loop for index from 0 to (- size 1) do
		(format t "~a " index)))

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

;;choose player token
;;change player
;;test for winning conditions
;;set winner
;;minmax

(set-board)
(print-2D)
; (print-board)





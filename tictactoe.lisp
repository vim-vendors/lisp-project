#| Tic Tac Toe Board Formatting |#

;useful for defining array from 0 to size
#| (defun printlong(size)
		(loop for index from 0 to (- size 1) do
		(format t "~a " x))) |#

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





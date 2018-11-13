#| Tic Tac Toe Board Formatting |#

;useful for defining array from 0 to size
#| (defun printlong(size)
		(loop for x from 0 to (- size 1) do
		(format t "~a " x))) |#

(defun set-board()
	(setf my_board (make-array '(9)))
	(loop for x from 0 to 8 do
    (setf (aref my_board x) (+ x 1))))
	
(defun print-board()
	(loop for x from 0 to 8 do
		(format t "~a" (aref my_board x))))



(defun print-2D()
	(loop for x from 0 to 2 do
		(loop for y from 0 to 2 do
			(format t "   ~a   |" (aref my_board (+ (* x 3) y))))
		(terpri)
		(format t " ______________________ ~%")
		(terpri)))

(set-board)
(print-2D)
; (print-board)





;This is how you write a comment in Lisp
; makes a cd entry for database
(defun make-cd (title artist rating ripped)
  (list :title title :artist artist :rating rating :ripped ripped))
;save global database variable
(defvar *db* nil)
;add record to the database with push macro
(defun add-record (cd) (push cd *db*))
(add-record (make-cd "Surfer Rosa" "The Pixies" 10 t))


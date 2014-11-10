(defvar *board-file* nil)
;the board file that contains board data
(defvar *board* nil)
;the list that contains board information. Each element of the list is a row of board, which is also a list
(defvar *original-board* nil)
;the list that contains board information. Each element of the list is a row of board, which is also a list.
;This list is fixed when the board is given
(defvar *move-recorder* nil)
;the list that records moves the player makes
(defvar *board-side* nil)
;number N for an N*N board
(defvar *board-member* nil)
;the list that contains both the original given numbers and the input numbers
(defvar *original-board-member* nil)
;the list that contains both the original given numbers and the input numbers
;This list is fixed when the board is given
(defvar *board-max* nil)
;the number N*N for an N*N board
(defvar *board-max-digit* nil)
;the number of digits of N*N for an N*N board
(defvar *num-digit* nil)
;the number of digits of each number in the board
(defvar *r* nil)
;record the row number when testing continuity of board
(defvar *c* nil)
;;record the column number when testing continuity of board
(defvar *row* nil)
;record the row number where continuity ends
(defvar *col* nil)
;record the column number where continuity ends
(defvar *correctness-number* 1)
;record the number of correct moves made by player


(defun sqr (row col *board*)
"acquire board element if row, column and board are given" 
   (nth (1- col)
       (nth (- *board-side* row) *board*)))
(defun set-sqr (row col *board* val)
"set board element if row, column,value and board are given"
   (setf (nth (1- col)
           (nth (- *board-side* row) *board*))
              val))

(defun print-side(*board*)
"print the board side"
     (terpri)(princ "+")
     (do ((i 1 (+ i 1)))
         ((> i *board-side*))
         (princ "----+"))
     (terpri))

(defun num-digit (number)
"judge the digit of a number"
   (if (equal '- number)
   1
   (floor (1+ (log number 10)))))



(defun print-ele(row)
"print each element(number) of in a board"
     (let ((*num-digit* (num-digit (car row))))
     (cond ((= 1 *num-digit*) (princ " ")(princ (car row))(princ " ")(princ " "))
           ((= 2 *num-digit*) (princ " ")(princ (car row))(princ " "))
           ((= 3 *num-digit*) (princ " ")(princ (car row)))
           (t (princ (car row))))))

(defun p-row(row)
"print a row for a board"
                  (cond ((null row) (princ "|"))
                        (t (princ "|")
                           (print-ele row)
                           (p-row (cdr row)))))

(defun p-board (*board*)
"print an entire board"
                   (cond ((null *board*) (print-side *board*))
                         (t (print-side *board*)
                            (p-row (car *board*))
                            (p-board (cdr *board*)))))


(defun file-to-list (*board-file*)
"convert a original board file into a list,each element of the list contains the row, which is also a list"
  (with-open-file (in *board-file*)
    (loop for line = (read-line in nil nil)
       while line
       collect (read-from-string (concatenate 'string "(" line
")")))))

(defun remove-list-parentheses (*board*)
"remove every parentheses a list"
	(cond ((null *board*) nil)
	      ((listp (car *board*)) (append (remove-list-parentheses (car *board*)) (remove-list-parentheses (cdr *board*))))
	      (t (cons (car *board*) (remove-list-parentheses (cdr *board*))))))

(defun show-board (*board-file*)
"print a board if board file is given"
   (setf *board* (file-to-list *board-file*))
   (setf *original-board* (file-to-list *board-file*))
   (setf *board-member* (remove-list-parentheses (file-to-list *board-file*)))
   (setf *original-board-member* (remove-list-parentheses (file-to-list *board-file*)))
   (setf *board-side* (length *board*))
   (setf *board-max* (* *board-side* *board-side*))
   (setf *board-max-digit* (num-digit *board-max*))
   (p-board *board*))

(defun read-board-file()
"read board file name and send it to show-board function"
   (format t "Please enter the board file you want to run:~%~%")
   (let* ((*board-file* (read-line)))
       (show-board *board-file*))
   (input-directly))

(defun record-move (row col val)
"record all moves the player makes"
   (setf *move-recorder* (append *move-recorder* (list (list row col val)))))

(defun move-input (row col val)
"allow player to input moves"
     (set-sqr row col *board* val)
     (setf *board-member* (remove-list-parentheses *board*))
     (record-move row col val)
     (p-board *board*)
     (format t "Move: (~A ~A): ~A ~%" row col val))

(defun print-moves ()
"print all moves the player makes"
  (format t "~%The moves you made are as follows:~%")
  (dolist (m *move-recorder*)
    (format t "(~d, ~d): ~d~%" (elt m 0) (elt m 1) (elt m 2))))

(defun find-one()
"find the number 1 after all the blank grids are filled by player"
    (setq pos (+ 1 (position 1 *board-member*)))
    (setq r (+ 1 (- *board-side* (ceiling (/ pos *board-side*)))))
    (setq c (mod pos *board-side*))
    (if (= c 0)
        (setq c *board-side*))
    (setf *r* r)
    (setf *c* c))

(defun check-correctness(m n)
"check the correctness of input moves after all the blank grids are filled by player"
    (cond ((and (< m *board-side*) (equal (+ 1 *correctness-number*) (sqr (+ m 1) n *board*)))
           (setf m (+ 1 m)) (setf *correctness-number* (+ 1 *correctness-number*))(setf *row* m *col* n) (check-correctness m n))
          ((and (> m 1) (equal (+ 1 *correctness-number*) (sqr (- m 1) n *board*)))
           (setf m (- m 1)) (setf *correctness-number* (+ 1 *correctness-number*)) (setf *row* m *col* n)(check-correctness m n))
          ((and (< n *board-side*) (equal (+ 1 *correctness-number*) (sqr m (+ n 1) *board*)))
           (setf n (+ 1 n)) (setf *correctness-number* (+ 1 *correctness-number*)) (setf *row* m *col* n)(check-correctness m n))
          ((and (> n 1) (equal (+ 1 *correctness-number*) (sqr m (- n 1) *board*)))
           (setf n (- n 1)) (setf *correctness-number* (+ 1 *correctness-number*)) (setf *row* m *col* n)(check-correctness m n))))

(defun reminding()
"to show whether a result is correct"
   (cond ((equal *correctness-number* *board-max*)
          (format t "Congratulations! The moves you put are correct!~%"))
         (t (format t "Sorry.The moves you put are incorrect!~%"))))

(defun new-game()
"to begin a new game after player's completing the former one"
   (format t "Please press 'n' to load another new game.~%~%")
   (format t "Please press 'r' to return welcome interface.~%~%~%")
   (let ((input (read)))
        (cond ((equal input 'n) (read-board-file))
              ((equal input 'r) (welcome)))))
(defun clear-board()
"clear all the data"
   (setf *board* nil)
   (setf *original-board* nil)
   (setf *board-member* nil)
   (setf *original-board-member* nil)
   (setf *board-side* nil)
   (setf *board-max* nil)
   (setf *board-max-digit* nil)
   (setf *correctness-number* 1)
   (setf *move-recorder* nil))


(defun input-directly ()
"judge player moves' correctness and allow to input if they're correct.Also judge whether the moves are complete and correct."
   (format t "please enter 3 three integers separated by blanks:~%Press r to return.~%")
   (format t "~%Please press 'a' to sovle this this game automatically~%~%~%")
   (let ((row (read)))
        (cond((equal 'r row) (welcome))
             ((equal 'a row) (fill-board-auto) (welcome))
             (t (let ((col (read)) (val (read)))
                 (if (and (typep row 'integer) (typep col 'integer) (typep val 'integer))
                     (cond ((or (< row 1) (> row *board-side*)) 
                                (format t "Invalid row number. Please input valid row number.~%")(input-directly))
                           ((or (< col 1) (> col *board-side*)) 
                                (format t "Invalid column number. Please input valid column number.~%")(input-directly))
                           ((or (< val 1) (> val *board-max*)) 
                                (format t "Number exceeds limits. Please input valid number.~%")(input-directly))
                           ((member val *original-board-member*) 
                                (format t "Number already exits!Please input valid number~%")(input-directly))
                           ((not (equal '- (sqr row col *original-board*))) 
                                (format t "Given number cannot be change. Try other grids~%")(input-directly))
                           (t (move-input row col val)
                              (cond ((member '- *board-member*) (input-directly) (record-move row col val))
                                    (t (find-one)(check-correctness *r* *c*)(reminding)(print-moves)(new-game))))) 
                     (input-directly)))))))


(defun welcome()
"allow player to return to the main menu interface"
  (clear-board)
  (format t "~%Please press 'i' for instructions.~%")
  (format t "~%Please press 'n' to load a board file and start a new game.~%")
  (format t "~%Please press 'q' to quit this Numbrix~%~%~%")
  (let ((input (read)))
       (cond ((equal input 'i) (show-instructions))
             ((equal input 'n) (read-board-file))
             ((equal input 'q) (format t "Thanks for playing! Goodbye!~%~%~%"))
             (t (numbrix)))))

(defun show-instructions()
"Instructions for the game"
   (format t "~%INSTRUCTIONS~%")
   (format t "~%NUMBRIX RULES~%")
   (format t "1.You will be given an N*N board;~%2.The goal is to input numbers in blank grids and create a continuous path of numbers from 1 to N*N;~%3.Numbers can only be connected vertically and horizontally, not diagonally;~%4.There are no duplicate numbers;~&5.Changing given numbers are not allowed;~%6.Entering non-integer is not allowed;~%7.Exceeding number limits is not allowed.~%")
   (format t "~%MANIPULATIONS~%")
   (format t "1.In welcome interface, press 'n' to enter a new game;~%2.Input a file name you want to load,for example,type file 'testboard1' to load this new board;~%3.After loading the new board, enter three integers separated by blanks with no other punctuation,for example x y z ~%  x stands for the number you enter is located in row x,~%  y stands for the number you enter is located in column y,~%  z stands for the number you enter in the blank grid is z;~%4.You may press 'r' to return the welcome interface;~%5.After finishing the game, you may press 'n' to load another new game.~%~%~%")
   (format t "Please press 'r' to return the welcome interface.~%~%~%")
   (let ((input (read)))
        (if (equal input 'r) 
            (welcome))))
             
(defun numbrix()
"main function"
  (format t "~%WELCOME TO NUMBRIX!~%")
  (format t "~%Wish you enjoy this game!~%")
  (format t "~%Please press 'i' for instructions.~%")
  (format t "~%Please press 'n' to load a board file and start a new game.~%")
  (format t "~%Please press 'q' to quit this Numbrix~%~%~%")
  (let ((input (read)))
       (cond ((equal input 'i) (show-instructions))
             ((equal input 'n) (read-board-file))
             ((equal input 'q) (format t "Thanks for playing! Goodbye!~%"))
             (t (numbrix)))))

(defun remove-ele (ele lst)
"remove a certain element in a list"
              (cond ((null lst) nil)
                    ((not (equal ele (car lst))) (cons (car lst) (remove-ele ele (cdr lst))))
                    (t (remove-ele ele (cdr lst)))))

(defun given-num()
"obtain the known numbers in the given board"
  (setq *list-without-unknowns* (remove-ele '- *board-member*))
  (setq *list-without-unknowns-sort* (sort *list-without-unknowns* #'<))
  (setq *given-num-list-length* (length *list-without-unknowns-sort*)))

(defun find-num (r c v)
"Depth first search used to genearal "
  (let ((row r) (col c) (val v))
  (if (or (< row 1) (< col 1) (> row *board-side*) (> col *board-side*))
    (return-from find-num nil))
  (setq judge-num (and  (< current-num *given-num-list-length*) (equal val (nth current-num *list-without-unknowns-sort*))));输入值为已知值中的一个
  
  (if (not (equal '- (sqr row col *board*)))
    (if judge-num
      (if (not (equal val (sqr row col *board*)))
        (return-from find-num nil))

      (return-from find-num nil))
    (if judge-num
      (return-from find-num nil)
      (set-sqr row col *board* val)))
  (if (equal val (* *board-side* *board-side*))
    (return-from find-num T))
  (if judge-num (setq current-num (+ current-num 1)))
  (if (find-num (+ row 1) col (+ val 1)) (return-from find-num T))
  (if (find-num (- row 1) col (+ val 1)) (return-from find-num T))
  (if (find-num row (+ col 1) (+ val 1)) (return-from find-num T))
  (if (find-num row (- col 1) (+ val 1)) (return-from find-num T))
  (setq judge-num (and  (< current-num *given-num-list-length*) (equal val (nth current-num *list-without-unknowns-sort*))))
  (if (not judge-num)
      (set-sqr row col *board* '-))
  (if judge-num
      (setq current-num (- current-num 1)))
  (return-from find-num nil)))

#|(defun fill-board-with-search()
  (given-num)
  (setq current-num 0)
  (do ((i 1 (+ i 1)))
    ((> i *board-side*))
    (do ((j 1 (+ j 1)))
      ((> j *board-side*))
      (find-num i j 1)
      (setq current-num 0))))|#

(defun find-one2()
"find the number 1 if exists before doing the depth first search"
(cond ((not (member 1 *board-member*)) (return-from find-one2 nil))
  ((member 1 *board-member*)
    (setq pos (+ 1 (position 1 *board-member*)))
    (setq r (+ 1 (- *board-side* (ceiling (/ pos *board-side*)))))
    (setq c (mod pos *board-side*))
    (if (= c 0)
        (setq c *board-side*))
    (setf *r1* r)
    (setf *c1* c)
    (return-from find-one2 T))))

(defun fill-board-with-search()
  "do depth first search"
  (given-num)
  (setq current-num 0)
  (if (find-one2)
    (find-num *r1* *c1* 1)
    (do ((i 1 (+ i 1)))
      ((> i *board-side*))
      (do ((j 1 (+ j 1)))
        ((> j *board-side*))
        (find-num i j 1)
        (setq current-num 0)))))

  
(defun fill-board-auto()
  "do depth first search and simple conditions"
  (setq time1 (GET-INTERNAL-RUN-TIME))
  (fill-obvious-grid)
  (fill-board-with-search)
  (p-board *board*)
  (setq time2 (GET-INTERNAL-RUN-TIME))
  (setq unit internal-time-units-per-second)
  (setq time (/ (- time2 time1) unit))
  (format t "Calculating time is ~A secs~%~%~%" (float time)))
  

(defun fill-with-diagonal (r c brd)
  "fill the grids with value if two of these neighbors are located diagonally"
  (let ((row r) (col c) (*board* brd))
    (if (equal '- (sqr row col *board*))
        (cond ((and (< 1 col) (< row *board-side*) (equal 2 (abs (- (sqr (+ row 1) col *board*) (sqr row (- col 1) *board*)))) 
                    (not (equal (/ (+ (sqr (+ row 1) col *board*) (sqr row (- col 1) *board*)) 2) (sqr (+ row 1) (- col 1) *board*))) 
                    (not (member (/ (+ (sqr (+ row 1) col *board*) (sqr row (- col 1) *board*)) 2) *board-member*)))
               (set-sqr row col *board* (/ (+ (sqr (+ row 1) col *board*) (sqr row (- col 1) *board*)) 2))
               (setf *board-member* (remove-list-parentheses *board*)))
              ((and (< col *board-side*) (< row *board-side*) (equal 2 (abs (- (sqr (+ row 1) col *board*) (sqr row (+ col 1) *board*)))) 
                (not (equal (/ (+ (sqr (+ row 1) col *board*) (sqr row (+ col 1) *board*)) 2) (sqr (+ row 1) (+ col 1) *board*)))
                (not (member (/ (+ (sqr (+ row 1) col *board*) (sqr row (+ col 1) *board*)) 2) *board-member*)))
               (set-sqr row col *board* (/ (+ (sqr (+ row 1) col *board*) (sqr row (+ col 1) *board*)) 2))
               (setf *board-member* (remove-list-parentheses *board*)))
              ((and (< col *board-side*) (< 1 row) (equal 2 (abs (- (sqr (- row 1) col *board*) (sqr row (+ col 1) *board*)))) 
                (not (equal (/ (+ (sqr (- row 1) col *board*) (sqr row (+ col 1) *board*)) 2) (sqr (- row 1) (+ col 1) *board*)))
                (not (member (/ (+ (sqr (- row 1) col *board*) (sqr row (+ col 1) *board*)) 2) *board-member*)))
               (set-sqr row col *board* (/ (+ (sqr (- row 1) col *board*) (sqr row (+ col 1) *board*)) 2))
               (setf *board-member* (remove-list-parentheses *board*)))
              ((and (< 1 col) (< 1 row) (equal 2 (abs (- (sqr (- row 1) col *board*) (sqr row (- col 1) *board*)))) 
                (not (equal (/ (+ (sqr (- row 1) col *board*) (sqr row (- col 1) *board*)) 2) (sqr (- row 1) (- col 1) *board*)))
                (not (member (/ (+ (sqr (- row 1) col *board*) (sqr row (- col 1) *board*)) 2) *board-member*)))
               (set-sqr row col *board* (/ (+ (sqr (- row 1) col *board*) (sqr row (- col 1) *board*)) 2))
               (setf *board-member* (remove-list-parentheses *board*)))))))

(defun fill-with-line (r c brd)
  "fill the grids if two grids can be filled with numbers between them"
   (let ((row r) (col c) (*board* brd))
    (if (not (equal '- (sqr row col *board*)))
      (cond 
        ((< 2 row)
          (do ((m 2 (+ m 1)))
            ((> m (- row 1)))
            (if (or (equal (- (sqr row col *board*) m) (sqr (- row m) col *board*)) (equal (+ (sqr row col *board*) m) (sqr (- row m) col *board*)))
             (do ((i 1 (+ i 1)))
               ((equal i m))
                (if (< (sqr row col *board*) (sqr (- row m) col *board*))
                  (set-sqr (- row i) col *board* (+ (sqr row col *board*) i))
                  (set-sqr (- row i) col *board* (- (sqr row col *board*) i)))))))
        ((< row (- *board-side* 2))
          (do ((m 2 (+ m 1)))
            ((> m (- *board-side* row)))
            (if (or (equal (- (sqr row col *board*) m) (sqr (+ row m) col *board*)) (equal (+ (sqr row col *board*) m) (sqr (+ row m) col *board*)))
              (do ((i 1 (+ i 1)))
                ((equal i m))
                (if (< (sqr row col *board*) (sqr (+ row m) col *board*))
                  (set-sqr (+ row i) col *board* (+ (sqr row col *board*) i))
                  (set-sqr (+ row i) col *board* (- (sqr row col *board*) i)))))))))))
(defun fill-with-line2 (r c brd)
  "fill the grids if two grids can be filled with numbers between them"
   (let ((row r) (col c) (*board* brd))
    (if (not (equal '- (sqr row col *board*)))
      (cond 
        ((< 2 col)
          (do ((m 2 (+ m 1)))
            ((> m (- col 1)))
            (if (or (equal (- (sqr row col *board*) m) (sqr row (- col m) *board*)) (equal (+ (sqr row col *board*) m) (sqr row (- col m) *board*)))
             (do ((i 1 (+ i 1)))
               ((equal i m))
                (if (< (sqr row col *board*) (sqr row (- col m) *board*))
                  (set-sqr row (- col i) *board* (+ (sqr row col *board*) i))
                  (set-sqr row (- col i) *board* (- (sqr row col *board*) i)))))))
        ((< col (- *board-side* 2))
          (do ((m 2 (+ m 1)))
            ((> m (- *board-side* col)))
            (if (or (equal (- (sqr row col *board*) m) (sqr row (+ col m) *board*)) (equal (+ (sqr row col *board*) m) (sqr row (+ col m) *board*)))
              (do ((i 1 (+ i 1)))
                ((equal i m))
                (if (< (sqr row col *board*) (sqr row (+ col m) *board*))
                  (set-sqr row (+ col i) *board* (+ (sqr row col *board*) i))
                  (set-sqr row (+ col i) *board* (- (sqr row col *board*) i)))))))))))

(defun fill-with-one-way (r c brd)
  "fill with values if grids are surrounded by 3 side by other numbers or boarder"
  (let ((row r) (col c) (*board* brd) (surrounded 0))
    (cond ((and (not (equal '- (sqr row col *board*))) 
                (not (equal 1 (sqr row col *board*))) 
                (not (equal (* *board-side* *board-side*) (sqr row col *board*))))
            (if (equal row 1) (setq surrounded (+ surrounded 1)))
            (if (equal col 1) (setq surrounded (+ surrounded 1)))
            (if (equal row *board-side*) (setq surrounded (+ surrounded 1)))
            (if (equal col *board-side*) (setq surrounded (+ surrounded 1))) 
            (if (and (> row 1) (not (equal '- (sqr (- row 1) col *board*)))) (setq surrounded (+ surrounded 1)))
            (if (and (> col 1) (not (equal '- (sqr row (- col 1) *board*)))) (setq surrounded (+ surrounded 1))) 
            (if (and (> *board-side* row) (not (equal '- (sqr (+ row 1) col *board*)))) (setq surrounded (+ surrounded 1)))
            (if (and (> *board-side* col) (not (equal '- (sqr row (+ col 1) *board*)))) (setq surrounded (+ surrounded 1)))
            (cond ((equal surrounded 3)
              (cond 
                   ((equal '- (sqr (+ row 1) col *board*))
                    (cond ((and (not (member (- (sqr row col *board*) 1) *board-member*)) 
                                (or (equal (+ (sqr row col *board*) 1) (sqr (- row 1) col *board*))
                                    (equal (+ (sqr row col *board*) 1) (sqr row (+ col 1) *board*))
                                    (equal (+ (sqr row col *board*) 1) (sqr row (- col 1) *board*))))
                            (set-sqr (+ row 1) col *board* (- (sqr row col *board*) 1)) 
                            (setf *board-member* (remove-list-parentheses *board*)))
                          ((and (not (member (+ (sqr row col *board*) 1) *board-member*))
                                (or (equal (- (sqr row col *board*) 1) (sqr (- row 1) col *board*))
                                    (equal (- (sqr row col *board*) 1) (sqr row (+ col 1) *board*))
                                    (equal (- (sqr row col *board*) 1) (sqr row (- col 1) *board*))))
                            (set-sqr (+ row 1) col *board* (+ (sqr row col *board*) 1))
                            (setf *board-member* (remove-list-parentheses *board*)))))
                   ((equal '- (sqr (- row 1) col *board*))
                    (cond ((and (not (member (- (sqr row col *board*) 1) *board-member*)) 
                                (or (equal (+ (sqr row col *board*) 1) (sqr (+ row 1) col *board*))
                                    (equal (+ (sqr row col *board*) 1) (sqr row (+ col 1) *board*))
                                    (equal (+ (sqr row col *board*) 1) (sqr row (- col 1) *board*))))
                            (set-sqr (- row 1) col *board* (- (sqr row col *board*) 1))
                            (setf *board-member* (remove-list-parentheses *board*)))
                          ((and (not (member (+ (sqr row col *board*) 1) *board-member*))
                                (or (equal (- (sqr row col *board*) 1) (sqr (+ row 1) col *board*))
                                    (equal (- (sqr row col *board*) 1) (sqr row (+ col 1) *board*))
                                    (equal (- (sqr row col *board*) 1) (sqr row (- col 1) *board*))))
                            (set-sqr (- row 1) col *board* (+ (sqr row col *board*) 1))
                            (setf *board-member* (remove-list-parentheses *board*))))))))))))


(defun fill-with-one-way2 (r c brd)
    "fill with values if grids are surrounded by 3 side by other numbers or boarder"
  (let ((row r) (col c) (*board* brd) (surrounded 0))
    (cond ((and (not (equal '- (sqr row col *board*))) 
                (not (equal 1 (sqr row col *board*))) 
                (not (equal (* *board-side* *board-side*) (sqr row col *board*))))
            (if (equal row 1) (setq surrounded (+ surrounded 1)))
            (if (equal col 1) (setq surrounded (+ surrounded 1)))
            (if (equal row *board-side*) (setq surrounded (+ surrounded 1)))
            (if (equal col *board-side*) (setq surrounded (+ surrounded 1))) 
            (if (and (> row 1) (not (equal '- (sqr (- row 1) col *board*)))) (setq surrounded (+ surrounded 1)))
            (if (and (> col 1) (not (equal '- (sqr row (- col 1) *board*)))) (setq surrounded (+ surrounded 1))) 
            (if (and (> *board-side* row) (not (equal '- (sqr (+ row 1) col *board*)))) (setq surrounded (+ surrounded 1)))
            (if (and (> *board-side* col) (not (equal '- (sqr row (+ col 1) *board*)))) (setq surrounded (+ surrounded 1)))
            (cond ((equal surrounded 3) 
              (cond 
                   ((equal '- (sqr row (+ col 1) *board*))
                    (cond ((and (not (member (- (sqr row col *board*) 1) *board-member*)) 
                                (or (equal (+ (sqr row col *board*) 1) (sqr (- row 1) col *board*))
                                    (equal (+ (sqr row col *board*) 1) (sqr (+ row 1) col *board*))
                                    (equal (+ (sqr row col *board*) 1) (sqr row (- col 1) *board*))))
                            (set-sqr row (+ col 1) *board* (- (sqr row col *board*) 1))
                            (setf *board-member* (remove-list-parentheses *board*)))
                          ((and (not (member (+ (sqr row col *board*) 1) *board-member*))
                                (or (equal (- (sqr row col *board*) 1) (sqr (- row 1) col *board*))
                                    (equal (- (sqr row col *board*) 1) (sqr (+ row 1) col *board*))
                                    (equal (- (sqr row col *board*) 1) (sqr row (- col 1) *board*))))
                            (set-sqr row (+ col 1) *board* (+ (sqr row col *board*) 1))
                            (setf *board-member* (remove-list-parentheses *board*)))))

                   ((equal '- (sqr row (- col 1) *board*))
                    (cond ((and (not (member (- (sqr row col *board*) 1) *board-member*)) 
                                (or (equal (+ (sqr row col *board*) 1) (sqr (+ row 1) col *board*))
                                    (equal (+ (sqr row col *board*) 1) (sqr row (+ col 1) *board*))
                                    (equal (+ (sqr row col *board*) 1) (sqr row (- col 1) *board*))))
                            (set-sqr row (- col 1) *board* (- (sqr row col *board*) 1))
                            (setf *board-member* (remove-list-parentheses *board*)))
                          ((and (not (member (+ (sqr row col *board*) 1) *board-member*))
                                (or (equal (- (sqr row col *board*) 1) (sqr (+ row 1) col *board*))
                                    (equal (- (sqr row col *board*) 1) (sqr row (+ col 1) *board*))
                                    (equal (- (sqr row col *board*) 1) (sqr row (- col 1) *board*))))
                            (set-sqr row (- col 1) *board* (+ (sqr row col *board*) 1))
                            (setf *board-member* (remove-list-parentheses *board*))))))))))))

(defun fill-obvious-grid()
  "fill grids with simple conditions"
  (do ((i 1 (+ i 1)))
    ((> i *board-side*))
    (do ((j 1 (+ j 1)))
      ((> j *board-side*))
      (fill-with-line i j *board*)
      (fill-with-line2 i j *board*)))
      (setf *board-member* (remove-list-parentheses *board*)))

(defun board-judge()
  "judge wether board is no more filled with simple conditions"
  (fill-obvious-grid)
  (setf *board-judge* *board*)
  (fill-obvious-grid)
  (if (not (equal *board* *board-judge*))
    (board-judge)))
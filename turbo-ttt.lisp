;;;
;;; TURBO TIC-TAC-TOE GAME
;;;
;;; This program is inspired in a case study from the book "Common LISP: A Gentle Introduction to Symbolic Computation - David S. Touretzky".
;;; As a special characteristic, its design addopts a recursive-functional approach.
;;;
;;; Features:
;;;
;;;   - Supports two human players
;;;   - Is possible to define the size board from 3x3 to 9x9
;;;   - A random draw defines which player start
;;;   - Each player's turn is also determined by random draw (using luck to avoid too many ties)
;;;


;;;
;;; PLAYING A MATCH
;;;
                          
;; Sets a control value for each player (this values will do help identify the winning player and prints the board)
(defun turbo-tictactoe () (clean-scr)
                (format t "~&Welcome! Starting a new match...")
                    (setf *player* 1
                          *computer* 10)                         
                    (new-match)
                    (makunbound '*player*)
                    (makunbound '*computer*)
                    (format t "~&~%~%--- End of program ---~%~%"))
             
;; Controls a new match
(defun new-match () (let ((board (set-board (ask-board-size))))                                                                  ; Initializes a empty board
                         (print-board board)                                                                                     ; Prints the empty board
                         (permit-moves-until board)                                                                              ; Permits moves until a win or a tie
                         (if (y-or-n-p "~&~%~%Do you want to play again? ") (new-match) (format t "~&Ok... Goodbye!"))))         ; Play again?
                                                          
;; Asks for a board size
(defun ask-board-size () (labels ((rec-ask-board () (let ((n (read)))
                                                         (cond ((<= 3 n 9) n)
                                                               (t (format t "~&Invalid size! Please type a number from 3 to 9: ") (rec-ask-board))))))     ; A recursive test
                                 
                                 (format t "Please, choose a board size (3x3 to 9x9). Type 3 for a 3x3 board and so on: ")
                                 (rec-ask-board)))

;; Permits alternate moves between players until a win or a tie
(defun permit-moves-until (board) (labels ((choose-player () (let ((player (if (evenp (random 9)) *player* *computer*)))
                                                                  (format t "~&~%Choosing who plays... It's ~A turn!" (if (= player 1) "PLAYER-1's" "PLAYER-2's"))
                                                                  player))                                                                                              ; Uses a random draw to choose a player

                                           (read-dis (disclaimer) (format t "~&~A" disclaimer) (read))

                                           (ask-coord () (let ((x (read-dis "Type the vertical coordinate of the place to mark: "))
                                                               (y (read-dis "Type the horizontal coordinate of the place to mark: ")))
                                                              (cond ((and (<= 1 x (length board)) (<= 1 y (length board)) (= (value-coord board (list x y)) 0)) (list x y))
                                                                    (t (format t "~&This place isn't available! Try again.") (ask-coord))))))                           ; Asks recursively for a point of coordinate
                                          
                                          ; Asks recursively for a point of coordinate                                           
                                          (print-board (persist-move (choose-player) (ask-coord) board))
                                          (cond ((board-full-p board) (format t "~&Ops... We have a tie!"))
                                                ((test-for-a-winner board) (format t "~&~%***********************************************************************~%~%WOW! The ~A is the winner! (winning combination: ~A)~%~%***********************************************************************~%~%" (if (= (car (test-for-a-winner board)) *player*) "PLAYER-1" "PLAYER-2") (cdr (test-for-a-winner board))))
                                                (t (permit-moves-until board)))))


;;;
;;; HANDLING THE BOARD
;;;

;; Generates a new tic-tac-toe's board with dimension "n x n"
;; For instance, a new board with "n = 3" would look like "((0 0 0) (0 0 0) (0 0 0))"
;; This implementation uses the numbers 0, 1 and 10 to represent respectively the blank space, the "O" and the "X" on the game
(defun set-board (n) (labels ((set-row (m) (cond ((zerop m) nil)
                                                 (t (cons 0 (set-row (- m 1))))))
                              (set-column (n m) (cond ((zerop n) nil)
                                                    (t (cons (set-row m) (set-column (- n 1) m))))))
                             (set-column n n)))  

;; Persists a move on a board for a player
;; The "coord" argument must be a coordinate point on the format "(row column)"
;; This function does the assignment to a generalized variable (the car position of a cons cell) and returns the board
(defun persist-move (player coord board) (setf (nth (- (cadr coord) 1) (car (nthcdr (- (car coord) 1) board))) player)
                                         board)
  
;; Returns a value (0 or a player's controle value) for a determined coordinate point on a board
;; The "coord" argument must be a coordinate point on the format "(row column)"
(defun value-coord (board coord) (nth (- (cadr coord) 1) (car (nthcdr (- (car coord) 1) board))))

;; Tests if a board is full
;; A recursive version -> (defun board-full-p (board) (cond ((null board) nil) ((member 0 (car board)) t) (t (board-full-p (cdr board)))))
(defun board-full-p (board) (not (find-if #'(lambda (x) (member 0 x)) board))) 


;;;
;;; CHECKING FOR A WINNER
;;;

;; Generates the set of all possible winning combinations for any board with "n x n" dimension
;; A combination is a set of coordinates on the format "(row column)" like as "((1 1) (1 2) (1 3))"
(defun winning-combinations (n) (labels ((winning-row-x (x y) (cond ((> y n) nil) (t (cons (list x y) (winning-row-x x (+ y 1))))))                         ; The winning combination for the row x
                                         (winning-rows (x) (cond ((> x n) nil) (t (cons (winning-row-x x 1) (winning-rows (+ x 1))))))                      ; All horizontal winning combinations
                                         (winning-column-y (x y) (cond ((> x n) nil) (t (cons (list x y) (winning-column-y (+ x 1) y)))))                   ; The winning combination for the column y
                                         (winning-columns (y) (cond ((> y n) nil) (t (cons (winning-column-y 1 y) (winning-columns (+ y 1))))))             ; All vertical winning combinations
                                         (winning-left-diag (x) (cond ((> x n) nil) (t (cons (list x x) (winning-left-diag (+ x 1))))))                     ; The left diagonal winning combination
                                         (winning-right-diag (x y) (cond ((> x n) nil) (t (cons (list x y) (winning-right-diag (+ x 1) (- y 1)))))))        ; The right diagonal winning combination

                                        ;; The set of all possible winning combinations
                                        (append (winning-rows 1) (winning-columns 1) (list (winning-left-diag 1)) (list (winning-right-diag 1 n)))))


;; Sums the values (0 or a player's control value) for a given set of coordinate points on a board
;; The "comb" argument must be a set coordinates like as "((1 1) (1 2) (1 3))"
(defun sum-combination (board comb) (cond ((null comb) 0)
                                          (t (+ (value-coord board (car comb)) (sum-combination board (cdr comb))))))   

;; Tests if there is a winner
;; For a board with "n x n" dimension, this test consists of checking if the sum of the values for a possible winning combination is equal a n times the control value of one of players   
;; Returns the winning player and the respective winning combination like as "(1 ((1 1) (1 2) (1 3)))"
(defun test-for-a-winner (board) (labels ((rec-test-winner (board set-comb) (cond ((null set-comb) nil)                                                                                              ; There is not a winner
                                                                                  ((equal (sum-combination board (car set-comb)) (* (length board) *player*)) (list *player* (car set-comb)))        ; Player 1 is the winner
                                                                                  ((equal (sum-combination board (car set-comb)) (* (length board) *computer*)) (list *computer* (car set-comb)))    ; Player 2 is the winner
                                                                                  (t (rec-test-winner board (cdr set-comb)))))) 
                                         (rec-test-winner board (winning-combinations (length board)))))


;;;
;;; PRINTING THE BOARD
;;;

;; Clean the screen
(defun clean-scr () (screen:clear-window (screen:make-window)))

;; Converts the players's control value for its respective printed format ("O" or "X")
(defun convert-value (value) (cond ((equal value *player*) "O") 
                                   ((equal value *computer*) "X")
                                   (t " ")))

;; Prints a new line with blank margin on the left for a better aesthetic aspect 
(defun print-left-margin () (format t "~&        "))

;; Prints a horizontal bar to separe rows
(defun print-bar (length-row) (cond ((equal length-row 1)(format t "---")) 
                                    (t (format t "----")(print-bar (- length-row 1)))))

;; Prints a row for a tic-tac-toe board
(defun print-row (x) (cond ((equal (length x) 1) (format t " ~A " (convert-value (car x)))) 
                           (t (format t " ~A |" (convert-value (car x)))(print-row (cdr x)))))

;; Prints a complete board
(defun print-board (board) (labels ((rec-print-board (board) (cond ((equal (length board) 1) (print-left-margin)(print-row (car board)))
                                                                   (t (print-left-margin)(print-row (car board))
                                                                      (print-left-margin)(print-bar (length (car board)))
                                                                      (rec-print-board (cdr board))))))
                                   (format t "~%~%")
                                   (rec-print-board board)
                                   (format t "~%~%~%")))

 
;;;
;;; 
;;;

;; Starts the game
(turbo-tictactoe)



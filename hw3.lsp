;
; CS161 Hw3: Sokoban
; 
; *********************
;    READ THIS FIRST
; ********************* 
;
; All functions that you need to modify are marked with 'EXERCISE' in their header comments.
; Do not modify a-star.lsp.
; This file also contains many helper functions. You may call any of them in your functions.
;
; *Warning*: The provided A* code only supports the maximum cost of 4999 for any node.
; That is f(n)=g(n)+h(n) < 5000. So, be careful when you write your heuristic functions.
; Do not make them return anything too large.
;
; For Allegro Common Lisp users: The free version of Allegro puts a limit on memory.
; So, it may crash on some hard sokoban problems and there is no easy fix (unless you buy 
; Allegro). 
; Of course, other versions of Lisp may also crash if the problem is too hard, but the amount
; of memory available will be relatively more relaxed.
; Improving the quality of the heuristic will mitigate this problem, as it will allow A* to
; solve hard problems with fewer node expansions.
; 
; In either case, this limitation should not significantly affect your grade.
; 
; Remember that most functions are not graded on efficiency (only correctness).
; Efficiency can only influence your heuristic performance in the competition (which will
; affect your score).
;  
;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; General utility functions
; They are not necessary for this homework.
; Use/modify them for your own convenience.
;

;
; For reloading modified code.
; I found this easier than typing (load "filename") every time. 
;
(defun reload()
  (load "hw3.lsp")
  )

;
; For loading a-star.lsp.
;
(defun load-a-star()
  (load "a-star.lsp"))

;
; Reloads hw3.lsp and a-star.lsp
;
(defun reload-all()
  (reload)
  (load-a-star)
  )

;
; A shortcut function.
; goal-test and next-states stay the same throughout the assignment.
; So, you can just call (sokoban <init-state> #'<heuristic-name>).
; 
;
(defun sokoban (s h)
  (a* s #'goal-test #'next-states h)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; end general utility functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; We now begin actual Sokoban code
;

; Define some global variables
(setq blank 0)
(setq wall 1)
(setq box 2)
(setq keeper 3)
(setq star 4)
(setq boxstar 5)
(setq keeperstar 6)

; Some helper functions for checking the content of a square
(defun isBlank (v)
  (= v blank)
  )

(defun isWall (v)
  (= v wall)
  )

(defun isBox (v)
  (= v box)
  )

(defun isKeeper (v)
  (= v keeper)
  )

(defun isStar (v)
  (= v star)
  )

(defun isBoxStar (v)
  (= v boxstar)
  )

(defun isKeeperStar (v)
  (= v keeperstar)
  )

;
; Helper function of getKeeperPosition
;
(defun getKeeperColumn (r col)
  (cond ((null r) nil)
	(t (if (or (isKeeper (car r)) (isKeeperStar (car r)))
	       col
	     (getKeeperColumn (cdr r) (+ col 1))
	     );end if
	   );end t
	);end cond
  )

;
; getKeeperPosition (s firstRow)
; Returns a list indicating the position of the keeper (row col).
; 
; Assumes that the keeper is in row >= firstRow. (initially called with firstRow=0)
; The top row is the zeroth row.
; The first (left) column is the zeroth column.
; The top, left corner has coordinates (0,0), usimg (row,col) coordinates
;
(defun getKeeperPosition (s firstRow)
  (cond ((null s) nil)
	(t (let ((x (getKeeperColumn (car s) 0)))
	     (if x
		 ;keeper is in this row
		 (list firstRow x)
		 ;otherwise move on
		 (getKeeperPosition (cdr s) (+ firstRow 1))
		 );end if
	       );end let
	 );end t
	);end cond
  );end defun

;
; cleanUpList (l)
; returns l with any NIL element removed.
; For example, if l is '(1 2 NIL 3 NIL), returns '(1 2 3).
;
(defun cleanUpList (L)
  (cond ((null L) nil)
	(t (let ((cur (car L))
		 (res (cleanUpList (cdr L)))
		 )
	     (if cur 
		 (cons cur res)
		  res
		 )
	     );end let
	   );end t
	);end cond
  );end 

; ------------------------------ STARTING GOAL-TEST -------------------------------

; HELPER FUNCTION FOR state-contains: row-contains
; returns true if row (row) contains type of content (content)
; ARGUMENTS: row (list), content (integer representing content type)
; RETURN VALUE: t if row contains content, false otherwise
(defun row-contains (row content)
  (cond ((null row) ; if the row is empty
	 NIL) ; return NIL - the row cannot contain content
	(t ; otherwise (the row is not empty)
	 (cond ((equal (first row) content) ; if the first element in the row = content
		t) ; return t - we found content
	       (t ; otherwise - the first element in the row didn't match content
		(row-contains (rest row) content)))))) ; check the rest of the row

; HELPER FUNCTION FOR goal-test: state-contains
; returns t if state (s) contains type of content (content)
; ARGUMENTS: s (state), content (integer representing content type)
; RETURN VALUE: t if s contains content, false otherwise
(defun state-contains (s content)
  (cond ((null s) ; if the state is empty
	 NIL) ; return NIL - the state cannot contain content
	(t ; otherwise (the state is not empty)
	 (cond ((row-contains (first s) content) ; if the first row contains content
		t) ; return true - we found content
	       (t ; otherwise - we didn't find content in the first row
		; check all of the other rows for content
		(state-contains (rest s) content))))))

; EXERCISE: Modify this function to return true (t)
; if and only if s is a goal state of the game.
; (neither any boxes nor the keeper is on a non-goal square)
;
; Currently, it always returns NIL. If A* is called with
; this function as the goal testing function, A* will never
; terminate until the whole search space is exhausted.
;
; We are at the goal state if and only if the keeper and all the boxes
; are all ontop of goals. Therefore, if we are at the goal state, we
; should never see the symbols @ (keeper) or $ (box). Seeing these symbols
; implies that either the keeper or a box is not on a goal because we
; would've instead seen + (keeper + goal) or * (box + goal).
(defun goal-test (s)
  ; return true if s doesn't contain box or keeper
  (not (or (state-contains s box) (state-contains s keeper))))

; ------------------------------ ENDING GOAL-TEST -------------------------------

; ------------------------------ STARTING NEXT-STATES -------------------------------

; HELPER FUNCTION FOR get-square: get-square-in-row
; returns the integer content in row (row) at column (col)
; returns the value of a wall if the row is empty
; ARGUMENTS: row (list), col (integer)
; RETURN VALUE: the content in (row) at column (col)
(defun get-square-in-row (row col)
  (cond ((null row) ; if the row is empty
	 wall) ; return wall if the row is empty
	((equal col 0) ; if col == 0
	 (first row)) ; then return the first element in row (which is in column 0)
	(t ; col != 0
	 ; decrement col and search the rest of row
	 (get-square-in-row (rest row) (- col 1)))))
	 
; HELPER FUNCTION FOR next-states: get-square
; returns the integer content of state s at square (r,c)
; if the square is outside of the state, returns the value of a wall
; ARGUMENTS: s (state), pos (r,c)
; RETURN VALUE: integer (the content of state s at square (r,c))
(defun get-square (s pos)
  (let ((r (first pos)) ; get the row of the position
	(c (second pos))) ; get the column of the position
    (cond ((null s) ; if the state is NIL
	   wall) ; return wall since any square is outside of an empty state
	  ((equal r 0) ; if row == 0
	   ; then return the element in the first row that is in column c
	   (get-square-in-row (first s) c))
	  (t ; otherwise (row != 0)
	   ; check the other rows of the state and decrement r
	   (get-square (rest s) (list (- r 1) c))))))

; HELPER FUNCTION FOR set-square: set-square-in-row 
; returns the row (row) with the item in column (col) set to v 
; returns the row unchanged if col is invalid
; ARGUMENTS: row (list), col (integer), v (integer)
; RETURN VALUE: list (row with the item in column col = v)
(defun set-square-in-row (row col v)
  (cond ((null row) ; if the row is empty
	 NIL) ; return an empty row
	((equal col 0) ; if col == 0
	 ; replace the first element of row with v
	 (cons v (rest row)))
	(t ; otherwise (col != 0)
	 ; append the first element of row to the rest of the row with the replacement
	 (cons (first row) (set-square-in-row (rest row) (- col 1) v)))))
	 
; HELPER FUNCTION FOR next-states: set-square
; takes in a state s, and returns a new state s' that is
; obtained by setting the square (r,c) to value v
; Note: does not modify input state
; ARGUMENTS: s (state), pos (r,c), v (integer)
; RETURN VALUE: state (updated state with square (r,c) = v)
(defun set-square (s pos v)
  (let ((r (first pos)) ; get the row of the position
	(c (second pos))) ; get the column of the position
    (cond ((null s) ; if the state is NIL
	   NIL) ; return empty state
	  ((equal r 0) ; if row == 0
	   ; replace the item in column c in the first row with v
	   (let ((updated-first-row (set-square-in-row (first s) c v)))
	     ; append the updated first row to the rest of the state
	     (cons updated-first-row (rest s))))
	  (t ; otherwise (row != 0)
	   ; check the other rows of s for the (r,c) to be replaced
	   (cons (first s) (set-square (rest s) (list (- r 1) c) v))))))

; HELPER FUNCTION FOR try-move: move-keeper-to-blank-or-goal
; takes in state s, keeper coordinates, new space coordinates, and new space content
; and returns the state s with the keeper where the new space is
; Note: the new space will either be a BLANK or a GOAL
; Note: new-pos-type is what the new space will look like after the keeper goes there (keeper or keeperstar)
; ARGUMENTS: s (state), keeper-pos (r,c), new-pos (r,c), new-pos-type (blank or star)
; RETURN VALUE: state (with keeper in new-pos)
(defun move-keeper-to-blank-or-goal (s keeper-pos new-pos new-pos-type)
  ; first move the keeper to new-pos (putting either keeper or keeperstar in new-pos
  (let ((moved-keeper (set-square s new-pos new-pos-type)))
    ; then move the keeper off of its old spot
    (cond ((isKeeper (get-square s keeper-pos)) ; if the keeper was NOT on a goal
	   ; then replace its old spot with a BLANK
	   (set-square moved-keeper keeper-pos blank))
	  (t ; otherwise - the keeper was ontop of a goal
	   ; then replace its old spot with a GOAL
	   (set-square moved-keeper keeper-pos star)))))

; HELPER FUNCTION FOR try-move: move-keeper-to-box
; takes in state s, keeper coordinates, box coordinates, and a direction, 
; and returns the state after trying to move the keeper to where the box was
; ARGUMENTS: s (state), keeper-pos (r,c), box-pos (r,c), dir ('up, 'down, 'left, or 'right)
; RETURN VALUE: state (after trying to move to where box is)
(defun move-keeper-to-box (s keeper-pos box-pos dir)
  (let* ((box-pos-type (get-square s box-pos)) ; get the content at box-pos (either box or boxstar)
	 (new-box-pos (get-new-pos box-pos dir)) ; get the pos the box would occupy after being pushed
	 (new-box-pos-type (get-square s new-box-pos))) ; then get the content at that spot
    (cond ((or (isBlank new-box-pos-type) (isStar new-box-pos-type))
	   ; if the spot the box would occupy is BLANK or is a GOAL
	   ; then we can push the box into that open spot (either blank or goal)
	   (let* ((box-pos-new-type (if (isBox box-pos-type) keeper keeperstar))  
		  (moved-keeper (move-keeper-to-blank-or-goal s keeper-pos box-pos box-pos-new-type)))
	     (cond ((isBlank new-box-pos-type) ; if the box is going into a BLANK spot
		    ; put the box in the blank spot
		    (set-square moved-keeper new-box-pos box))
		   (t ; if the box is going into a GOAL spot
		    ; put (box + goal) into that spot
		    (set-square moved-keeper new-box-pos boxstar)))))
	  (t ; otherwise (the spot the box would occupy isn't a BLANK or GOAL)
	   NIL)))) ; the box can't be pushed into anything besides a blank or goal

; HELPER FUNCTION FOR next-states: get-new-pos
; takes a position pos and a direction dir
; and returns the adjacent position to pos in direction dir
; Note: we make no checks if the position is valid in this function
; ARGUMENTS: pos (r,c), dir ('up, 'down, 'left, or 'right)
; RETURN VALUE: position (position next to pos in direction dir)
(defun get-new-pos (pos dir)
  (cond ((equal dir 'up) ; moving up
	 (list (- (first pos) 1) (second pos))) ; return (r - 1, c)
	((equal dir 'down) ; moving down
	 (list (+ (first pos) 1) (second pos))) ; return (r + 1, c)
	((equal dir 'left) ; moving left
	 (list (first pos) (- (second pos) 1))) ; return (r, c - 1)
	((equal dir 'right) ; moving right
	 (list (first pos) (+ (second pos) 1))))) ; return (r, c + 1)

; HELPER FUNCTION FOR next-states: try-move
; takes in a state s and a move direction dir, and returns the state
; that is the result of moving the keeper in state s in direction dir
; Note: returns NIL if the move is invalid (ex: wall in that direction)
; Note: dir = {'up, 'down, 'left, 'right}
; ARGUMENTS: s (state), dir ('up, 'down, 'left, or 'right)
; RETURN VALUE: state (result of moving keeper in state s in diretion dir)
(defun try-move (s dir)
  (let* ((keeper-pos (getKeeperPosition s 0)) ; get position of keeper - (r,c)
	 (new-pos (get-new-pos keeper-pos dir)) ; get the position we're trying to move to
	 (new-pos-content (get-square s new-pos))) ; get the content at the new position
    (cond ((isBlank new-pos-content) ; if the new position is BLANK
	   (move-keeper-to-blank-or-goal s keeper-pos new-pos keeper)) ; move the keeper to the blank spot
	  ; if the new position is a BOX or BOX on top of a GOAL
	  ((or (isBox new-pos-content) (isBoxStar new-pos-content))
	   ; try to move the keeper to where the box is
	   (move-keeper-to-box s keeper-pos new-pos dir))
	  ((isStar new-pos-content) ; if the new position is a GOAL
	   (move-keeper-to-blank-or-goal s keeper-pos new-pos keeperstar)) ; move the keeper to the goal
	  (t ; otherwise
	   NIL)))) ; move is invalid - can only move to blank, box, or goal

; EXERCISE: Modify this function to return the list of 
; sucessor states of s.
;
; This is the top-level next-states (successor) function.
; Some skeleton code is provided below.
; You may delete them totally, depending on your approach.
; 
; If you want to use it, you will need to set 'result' to be 
; the set of states after moving the keeper in each of the 4 directions.
; A pseudo-code for this is:
; 
; ...
; (result (list (try-move s UP) (try-move s DOWN) (try-move s LEFT) (try-move s RIGHT)))
; ...
; 
; You will need to define the function try-move and decide how to represent UP,DOWN,LEFT,RIGHT.
; Any NIL result returned from try-move can be removed by cleanUpList.
; 
;
(defun next-states (s)
  ; try moving in all 4 directions
  (let ((result (list (try-move s 'up) (try-move s 'right) (try-move s 'down) (try-move s 'left))))
    (cleanUpList result))) ; then remove any NILs from the result

; ------------------------------ ENDING NEXT-STATES -------------------------------

; ------------------------------ BEGINNING HEURISTICS -------------------------------

; EXERCISE: Modify this function to compute the trivial 
; admissible heuristic.
;
(defun h0 (s)
  0) ; always returns the constant 0

; HELPER FUNCTION for h1: num-boxes-in-row
; returns the number of boxes (not on top of a goal) in a row 
; ARGUMENTS: row (list)
; RETURN VALUE: integer (# of boxes not on top of a goal in row)
(defun num-boxes-in-row (row)
  (cond ((null row) ; if the row is empty
	 0) ; then it has 0 boxes
	(t ; otherwise (the row is not empty)
	 (cond ((equal (first row) box) ; if the first item in the row is a box
		(+ 1 (num-boxes-in-row (rest row)))) ; return 1 + # boxes in rest of row
	       (t ; the first item in the row isn't a box
		(num-boxes-in-row (rest row))))))) ; return # boxes in the rest of the row

; EXERCISE: Modify this function to compute the 
; number of misplaced boxes in s.
;
; This heuristic is admissible because it will never overestimate the # of steps
; needs from s to the goal state. Since we can move at most 1 box per move, the 
; amount of moves until we are at the goal state is at least the amount of boxes
; that still need to be moved to goal states; therefore, h1 never overestimates
; the cost and is an admissible heuristic.
(defun h1 (s)
  (cond ((null s) ; if the state is empty
	 0) ; then it has 0 misplaced boxes
	(t ; otherwise (the state is not empty)
	 ; get the number of misplaced boxes in the first row
	 (let ((num-boxes-in-first-row (num-boxes-in-row (first s))))
	   ; then add that to the # of misplaced boxes in the other rows
	   (+ num-boxes-in-first-row (h1 (rest s)))))))



; HELPER FUNCTION FOR h804993030: manhattan-distance
; returns the Manhattan Distance between two coordinates
; ARGUMENTS: pos1 (r,c), pos2 (r,c)
; RETURN VALUE: integer (Manhattan Distance)
(defun manhattan-distance (pos1 pos2)
  (let* ((row-diff (- (first pos1) (first pos2)))
	 (col-diff (- (second pos1) (second pos2))))
    ; return abs(row-diff) + abs(col-diff)
    (+ (abs row-diff) (abs col-diff))))

; HELPER FUNCTION FOR get-type-coords: get-type-coords-in-row
; returns the coordinates of all of the positions of content "type" in row
; ARGUMENTS: row (list), type (content type), row-num (# row), col-num (col # of first element in row)
; RETURN VALUE: list (coordinates of all positions with content "type")
(defun get-type-coords-in-row (row type row-num col-num)
  (cond ((null row) ; if the row is empty
	 NIL) ; return an empty list - no content
	(t ; otherwise (row isn't empty)
	 (cond ((equal (first row) type) ; if the first item in the row is of type "type"
		; add the coords of this position to the coords of the other "types" in the row
		(cons (list row-num col-num) (get-type-coords-in-row (rest row) type row-num (+ col-num 1))))
	       (t ; otherwise (the first item in the row is not a "type")
		(get-type-coords-in-row (rest row) type row-num (+ col-num 1)))))))

; HELPER FUNCTION FOR h804993030: get-type-coords
; returns the coordinates of all of positions with content "type" in state s
; ARGUMENTS: s (state), type (content type), top-row (row # of top row - starts as 0)
; RETURN VALUE: list (coordinates of all positions with content "type")
(defun get-type-coords (s type top-row)
  (cond ((null s) ; if the state is NIL
	 NIL) ; return an empty list - no content
	(t ; otherwise (state is not NIL)
	 ; get "type" coords in row 1
	 (let ((type-coords-in-first-row (get-type-coords-in-row (first s) type top-row 0)))
	   ; append the coords in the first row to the coords in the rest of the state
	   (append type-coords-in-first-row (get-type-coords (rest s) type (+ top-row 1)))))))
	 
; HELPER FUNCTION FOR h804993030: min-box-to-goal-dist
; returns the minimum manhattan distance between a box and any goal
; ARGUMENTS: box-pos (r,c), goal-coords (list of (r,c))
; RETURN VALUE: the min manhattan distance between box-pos and any item of goal-coords
(defun min-box-to-goal-dist (box-pos goal-coords)
  (cond ((equal (length goal-coords) 1)	; if there's only 1 goal
	 (manhattan-distance box-pos (first goal-coords))) ; return the man-dist between the goal and box
	(t ; otherwise (multiple coordinates)
	 (let ((first-dist (manhattan-distance box-pos (first goal-coords))))
	   ; return min(first-dist, all the other distances)
	   (min first-dist (min-box-to-goal-dist box-pos (rest goal-coords)))))))

	 
; EXERCISE: Change the name of this function to h<UID> where
; <UID> is your actual student ID number. Then, modify this 
; function to compute an admissible heuristic value of s. 
; 
; This function will be entered in the competition.
; Objective: make A* solve problems as fast as possible.
; The Lisp 'time' function can be used to measure the 
; running time of a function call.
;

; current idea: sum of the manhattan distances from each box to its closest goal
(defun h804993030 (s)
  

; ------------------------------ ENDING HEURISTICS -------------------------------

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
 | Some predefined problems.
 | Each problem can be visualized by calling (printstate <problem>). For example, (printstate p1).
 | Problems are ordered roughly by their difficulties.
 | For most problems, we also provide a number which indicates the depth of the optimal solution.
 | These numbers are located at the comments of the problems. For example, the first problem below has optimal solution depth 6.
 | As for the solution depth, any admissible heuristic must make A* return an optimal solution. So, the depths of the optimal solutions provided could be used for checking whether your heuristic is admissible.
 |
 | Warning: some problems toward the end are quite hard and could be impossible to solve without a good heuristic!
 | 
 |#
;(6)
(setq p1 '((1 1 1 1 1 1)
	   (1 0 3 0 0 1)
	   (1 0 2 0 0 1)
	   (1 1 0 1 1 1)
	   (1 0 0 0 0 1)
	   (1 0 4 0 4 1)
	   (1 1 1 1 1 1)))

;(15)
(setq p2 '((1 1 1 1 1 1 1)
	   (1 0 0 0 0 0 1) 
	   (1 0 0 0 0 0 1) 
	   (1 0 0 2 1 4 1) 
	   (1 3 4 0 1 0 1)
	   (1 1 1 1 1 1 1)))

;(13)
(setq p3 '((1 1 1 1 1 1 1 1 1)
	   (1 0 0 0 1 0 0 0 1)
	   (1 0 0 0 2 0 3 4 1)
	   (1 0 0 0 1 0 0 0 1)
	   (1 0 4 0 1 0 0 0 1)
	   (1 1 1 1 1 1 1 1 1)))

;(17)
(setq p4 '((1 1 1 1 1 1 1)
	   (0 0 0 0 0 1 4)
	   (0 0 0 0 0 0 0)
	   (0 0 1 1 1 0 0)
	   (0 0 1 0 0 0 0)
	   (0 2 1 0 0 4 0)
	   (0 3 1 0 0 0 0)))

;(12)
(setq p5 '((1 1 1 1 1 1)
	   (1 1 0 0 1 1)
	   (1 0 0 0 0 1)
	   (1 4 2 2 4 1)
	   (1 0 0 0 4 1)
	   (1 1 3 1 1 1)
	   (1 1 1 1 1 1)))

;(13)
(setq p6 '((1 1 1 1 1 1 1 1)
	   (1 0 0 0 0 0 4 1)
	   (1 4 0 0 2 2 3 1)
	   (1 0 0 1 0 0 4 1)
	   (1 1 1 1 1 1 1 1)))

;(47)
(setq p7 '((1 1 1 1 1 1 1 1 1 1)
	   (0 0 1 1 1 1 4 0 0 3)
	   (0 0 0 0 0 1 0 0 0 0)
	   (0 0 0 0 0 1 0 0 1 0)
	   (0 0 1 0 0 1 0 0 1 0)
	   (0 2 1 0 0 0 0 0 1 0)
	   (0 0 1 0 0 0 0 0 1 4)))

;(22)
(setq p8 '((1 1 1 1 1 1)
	   (1 4 0 0 4 1)
	   (1 0 2 2 0 1)
	   (1 2 0 1 0 1)
	   (1 3 4 0 4 1)
	   (1 1 1 1 1 1)))

;(34)
(setq p9 '((1 1 1 1 1 1 1 1 1) 
	   (1 1 1 0 0 1 1 1 1) 
	   (1 0 0 0 0 0 2 0 1) 
	   (1 0 1 0 0 1 2 0 1) 
	   (1 0 4 4 4 1 3 0 1) 
	   (1 1 1 1 1 1 1 1 1)))

;(59)
(setq p10 '((1 1 1 1 1 0 0)
	    (1 4 0 0 1 1 0)
	    (1 3 2 0 0 1 1)
	    (1 1 0 2 0 0 1)
	    (0 1 1 0 2 0 1)
	    (0 0 1 1 0 0 1)
	    (0 0 0 1 1 4 1)
	    (0 0 0 0 1 4 1)
	    (0 0 0 0 1 4 1)
	    (0 0 0 0 1 1 1)))

;(?)
(setq p11 '((0 0 1 0 0 0 0)
	    (0 2 1 4 0 4 0)
	    (0 2 0 4 0 0 0)	   
	    (3 2 1 1 1 4 0)
	    (0 0 1 4 0 0 0)))

;(?)
(setq p12 '((1 1 1 1 1 0 0 0)
	    (1 0 0 4 1 0 0 0)
	    (1 2 1 0 1 1 1 1)
	    (1 4 0 0 0 0 0 1)
	    (1 0 0 5 0 5 0 1)
	    (1 0 5 0 1 0 1 1)
	    (1 1 1 0 3 0 1 0)
	    (0 0 1 1 1 1 1 0)))

;(?)
(setq p13 '((1 1 1 1 1 1 1 1 1 1)
	    (1 3 0 0 1 0 0 4 4 1)
	    (1 0 2 0 2 0 0 4 4 1)
	    (1 0 2 2 2 1 1 4 4 1)
	    (1 0 0 0 0 1 1 4 4 1)
	    (1 1 1 1 1 1 0 0 0 0)))

;(?)
(setq p14 '((0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
	    (0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
	    (1 1 1 1 1 0 0 0 0 0 0 1 1 1 1 1)
	    (0 0 0 0 0 1 0 0 0 0 1 0 0 0 0 0)
	    (0 0 0 0 0 0 1 0 0 1 0 0 0 0 0 0)
	    (0 0 0 0 0 0 0 0 3 0 0 0 0 0 0 0)
	    (0 0 0 0 0 0 1 0 0 1 0 0 0 0 0 0)
	    (0 0 0 0 0 1 0 0 0 0 1 0 0 0 0 0)
	    (1 1 1 1 1 0 0 0 0 0 0 1 1 1 1 1)
	    (0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
	    (0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
	    (0 0 0 0 1 0 0 0 0 0 4 1 0 0 0 0)
	    (0 0 0 0 1 0 2 0 0 0 4 1 0 0 0 0)	    
	    (0 0 0 0 1 0 2 0 0 0 4 1 0 0 0 0)
	    ))

;(?)
(setq p15 '((0 0 1 1 1 1 1 1 1 0)
	    (1 1 1 0 0 1 1 1 1 0)
	    (1 0 0 2 0 0 0 1 1 0)
	    (1 3 2 0 2 0 0 0 1 0)
	    (1 1 0 2 0 2 0 0 1 0)
	    (0 1 1 0 2 0 2 0 1 0)
	    (0 0 1 1 0 2 4 0 1 0)
	    (0 0 0 1 1 1 1 0 1 0)
	    (0 0 0 0 1 4 1 0 0 1)
	    (0 0 0 0 1 4 4 4 0 1)
	    (0 0 0 0 1 0 1 4 0 1)
	    (0 0 0 0 1 4 4 4 0 1)
	    (0 0 0 0 1 1 1 1 1 1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
 | Utility functions for printing states and moves.
 | You do not need to understand any of the functions below this point.
 |#

;
; Helper function of prettyMoves
; from s1 --> s2
;
(defun detectDiff (s1 s2)
  (let* ((k1 (getKeeperPosition s1 0))
	 (k2 (getKeeperPosition s2 0))
	 (deltaX (- (car k2) (car k1)))
	 (deltaY (- (cadr k2) (cadr k1)))
	 )
    (cond ((= deltaX 0) (if (> deltaY 0) 'DOWN 'UP))
	  (t (if (> deltaX 0) 'RIGHT 'LEFT))
	  );end cond
    );end let
  );end defun

;
; Translates a list of states into a list of moves.
; Usage: (prettyMoves (a* <problem> #'goal-test #'next-states #'heuristic))
;
(defun prettyMoves (m)
  (cond ((null m) nil)
	((= 1 (length m)) (list 'END))
	(t (cons (detectDiff (car m) (cadr m)) (prettyMoves (cdr m))))
	);end cond
  );

;
; Print the content of the square to stdout.
;
(defun printSquare (s)
  (cond ((= s blank) (format t " "))
	((= s wall) (format t "#"))
	((= s box) (format t "$"))
	((= s keeper) (format t "@"))
	((= s star) (format t "."))
	((= s boxstar) (format t "*"))
	((= s keeperstar) (format t "+"))
	(t (format t "|"))
	);end cond
  )

;
; Print a row
;
(defun printRow (r)
  (dolist (cur r)
    (printSquare cur)    
    )
  );

;
; Print a state
;
(defun printState (s)
  (progn    
    (dolist (cur s)
      (printRow cur)
      (format t "~%")
      )
    );end progn
  )

;
; Print a list of states with delay.
;
(defun printStates (sl delay)
  (dolist (cur sl)
    (printState cur)
    (sleep delay)
    );end dolist
  );end defun

; Homework 2: Uninformed Search Problems

; 1. BFS: performs a left-to-right breadth-first search of a tree
; ARGUMENTS: TREE (represented as a list of its child nodes)
; RETURN VALUE: list (the terminal nodes in the order that they would be visited)
(defun BFS (TREE)
  (cond ((null TREE) ; if TREE is NIL
	 NIL) ; then return an empty list
	((atom TREE) ; if the TREE is just a single leaf node
	 (list TREE)) ; then return a list of just that node
	(t ; if the TREE is an internal node (list of its child nodes)
	 (let ((LEFTMOST-CHILD (first TREE))) ; get the leftmost child
	   (cond ((atom LEFTMOST-CHILD) ; if the leftmost child is a leaf node
		  (cons LEFTMOST-CHILD (BFS (rest TREE)))) ; "visit" that node
		 (t ; if the leftmost child is an internal node (list)
		  ; unpack the list and append it to the end of the tree
		  ; so we visit all nodes in the current level before going to the next level
		  (BFS (append (rest TREE) LEFTMOST-CHILD))))))))
		 
; 2. DFS: performs a right-to-left depth-first search of a tree
; ARGUMENTS: TREE (represented as a list of its child nodes)
; RETURN VALUE: list (the terminal nodes in the order that they would be visited)                                                                              
(defun DFS (TREE)
  (cond ((null TREE) ; if TREE is NIL
	 NIL) ; then return an empty list
	((atom TREE) ; if the TREE is just a single leaf node
	 (list TREE)) ; then return a list of just that node
	(t ; if the TREE is an internal node (list of its child nodes)
	 (let ((LEFTMOST-CHILD (first TREE))) ; get the leftmost child
	   (cond ((atom LEFTMOST-CHILD) ; if the leftmost child is a leaf node
		  ; since we're doing right-to-left DFS,
		  ; we'll visit LEFTMOST-CHILD after all of the rest of the tree
		  (append (DFS (rest TREE)) (list LEFTMOST-CHILD)))
		 (t ; if the leftmost child is an internal node (list)
		  (append (DFS (rest TREE)) (DFS LEFTMOST-CHILD))))))))

; HELPER FUNCTION FOR DFID: LIMITED-DFS
; performs a limited depth-first search of a tree, starting at
; depth = CURR-DEPTH, and with maximum depth = MAX-DEPTH
; ARGUMENTS: TREE (represented as a list of its child nodes),
;			 CURR-DEPTH (which depth of the tree we are starting at)
;	         MAX-DEPTH (the maximum depth that we will traverse down to)
; RETURN VALUE: list (the terminal nodes in the order that they would be visited)
(defun LIMITED-DFS (TREE CURR-DEPTH MAX-DEPTH)
  (cond ((or (null TREE) (>= CURR-DEPTH MAX-DEPTH)) ; if TREE is NIL or CURR-DEPTH >= MAX-DEPTH
	 NIL) ; then return an empty list
	(t ; if the TREE is an internal node (list of its child nodes)
	 (let ((LEFTMOST-CHILD (first TREE))) ; get the leftmost child
	   (cond ((atom LEFTMOST-CHILD) ; if the leftmost child is a leaf node
		  ; then "visit" that node - then search its parent's other children
		  (cons LEFTMOST-CHILD (LIMITED-DFS (rest TREE) CURR-DEPTH MAX-DEPTH)))
		 (t ; if the leftmost child is an internal node (list)
		  ; keep searching down its subtree
		  ; making sure to increment the CURR-DEPTH variable
		  (append (LIMITED-DFS LEFTMOST-CHILD (+ CURR-DEPTH 1) MAX-DEPTH)
			  (LIMITED-DFS (rest TREE) CURR-DEPTH MAX-DEPTH))))))))

; HELPER FUNCTION FOR DFID: DFID-HELPER
; this function calls LIMITED-DFS with MAX-DEPTH = 0 to MAX-DEPTH = REAL MAX DEPTH
; and then appends the answers as we iterate
; ARGUMENTS: TREE (represented as a list of its child nodes),
;		     CURR-MAX-DEPTH (the current MAX-DEPTH we're passing to LIMITED-DFS)
;			 GLOBAL-MAX-DEPTH (the max MAX-DEPTH that we'll pass to LIMITED-DFS)
; RETURN VALUE: list (the terminal nodes in the order that they would be visited
;					  by a left-to-right dfid search)
(defun DFID-HELPER (TREE CURR-MAX-DEPTH GLOBAL-MAX-DEPTH)
  (cond ((> CURR-MAX-DEPTH GLOBAL-MAX-DEPTH) ; if CURR-MAX-DEPTH > GLOBAL-MAX-DEPTH
	 NIL) ; return an empty list (stop making recursive calls)
	(t ; CURR-MAX-DEPTH <= GLOBAL-MAX-DEPTH
	 ; do a limited DFS with max depth = CURR-MAX-DEPTH
	 ; then append that result to a limited DFS with max depth = CURR-MAX-DEPTH + 1
	 ; and so on ... until CURR-MAX-DEPTH exceeds GLOBAL-MAX-DEPTH
	 (append (LIMITED-DFS TREE 0 CURR-MAX-DEPTH)
		 (DFID-HELPER TREE (+ CURR-MAX-DEPTH 1) GLOBAL-MAX-DEPTH)))))

; 3. DFID: performs a left-to-right depth-first iterative-deepening search of a tree
; ARGUMENTS: TREE (represented as a list of its child nodes),
;			 MAX-DEPTH (maximum depth that we can traverse to)
; RETURN VALUE: list (the terminal nodes in the order that they would be visited
;					  by a left-to-right dfid search)
(defun DFID (TREE MAX-DEPTH)
  (DFID-HELPER TREE 0 MAX-DEPTH))

; These functions implement a depth-first solver for the missionary-cannibal
; problem. In this problem, three missionaries and three cannibals are trying to
; go from the east side of a river to the west side. They have a single boat
; that can carry two people at a time from one side of the river to the
; other. There must be at least one person in the boat to cross the river. There
; can never be more cannibals on one side of the river than missionaries. If
; there are, the cannibals eat the missionaries.

; In this implementation, a state is represented by a single list
; (missionaries cannibals side). side represents which side the boat is
; currently on, and is T if it is on the east side and NIL if on the west
; side. missionaries and cannibals represent the number of missionaries and
; cannibals on the same side as the boat. Thus, the initial state for this
; problem is (3 3 T) (three missionaries, three cannibals, and the boat are all
; on the east side of the river) and the goal state is (3 3 NIL).

; The main entry point for this solver is the function MC-DFS, which is called
; with the initial state to search from and the path to this state. It returns
; the complete path from the initial state to the goal state: this path is a
; list of intermediate problem states. The first element of the path is the
; initial state and the last element is the goal state. Each intermediate state
; is the state that results from applying the appropriate operator to the
; preceding state. If there is no solution, MC-DFS returns NIL.

; To call MC-DFS to solve the original problem, one would call (MC-DFS '(3 3 T)
; NIL) -- however, it would be possible to call MC-DFS with a different initial
; state or with an initial path.

; Examples of calls to some of the helper functions can be found after the code.



; FINAL-STATE takes a single argument s, the current state, and returns T if it
; is the goal state (3 3 NIL) and NIL otherwise.
(defun final-state (s)
  (equal s '(3 3 NIL)))

; NEXT-STATE returns the state that results from applying an operator to the
; current state. It takes three arguments: the current state (s), a number of
; missionaries to move (m), and a number of cannibals to move (c). It returns a
; list containing the state that results from moving that number of missionaries
; and cannibals from the current side of the river to the other side of the
; river. If applying this operator results in an invalid state (because there
; are more cannibals than missionaries on either side of the river, or because
; it would move more missionaries or cannibals than are on this side of the
; river) it returns NIL.
;
; NOTE that next-state returns a list containing the successor state (which is
; itself a list); the return should look something like ((1 1 T)).
(defun next-state (s m c)
  ; binding values to variables BEFORE moving the ship
  (let* ((m-curr-side (first s)) ; # missionaries on the same side as boat
	 (c-curr-side (second s)) ; # cannibals on the same side as boat
	 (m-other-side (- 3 m-curr-side)) ; # missionaries on the other side
	 (c-other-side (- 3 c-curr-side)) ; # cannibals on the other side
	 (num-in-boat (+ m c))) ; # of missionaries and cannibals going in the boat
    (cond ((or (< num-in-boat 1) (> num-in-boat 2)) ; if we're moving < 1 or > 2 people
	   NIL) ; return NIL - boat must have either 1 or 2 passengers
	  ; if we're moving more m or c than we have on the current side
	  ((or (> m m-curr-side) (> c c-curr-side))
	   NIL) ; then return NIL - not possible
	  (t ; otherwise (we are moving a valid amount of m and c - but still could have c > m)
	   ; now move the boat with m missionaries and c cannibals
	   (let ((new-m-curr-side (+ m-other-side m)) ; new # missionaries on boat side
		 (new-c-curr-side (+ c-other-side c)) ; new # cannibals on boat side
		 (new-m-other-side (- m-curr-side m)) ; new # missionaries on other side
		 (new-c-other-side (- c-curr-side c))) ; new # cannibals on other side
	           ; if there cannibals outnumber the missionaries on the current (boat) side 
	     (cond ((and (> new-m-curr-side 0) (> new-c-curr-side new-m-curr-side))
		    NIL) ; return NIL - missionaries get eaten
		   ; if there cannibals outnumber the missionaries on the other side
		   ((and (> new-m-other-side 0) (> new-c-other-side new-m-other-side))
                    NIL) ; return NIL - missionaries get eaten
		   (t ; otherwise, return updated state
		    (list (list new-m-curr-side new-c-curr-side (not (third s)))))))))))
	   

; SUCC-FN returns all of the possible legal successor states to the current
; state. It takes a single argument (s), which encodes the current state, and
; returns a list of each state that can be reached by applying legal operators
; to the current state.
(defun succ-fn (s)
  (append (next-state s 1 1)
	  (next-state s 2 0)
	  (next-state s 0 2)
	  (next-state s 1 0)
	  (next-state s 0 1)))

; ON-PATH checks whether the current state is on the stack of states visited by
; this depth-first search. It takes two arguments: the current state (s) and the
; stack of states visited by MC-DFS (states). It returns T if s is a member of
; states and NIL otherwise.
(defun on-path (s states)
  (cond ((null states) ; if states is empty
	 NIL) ; then s can't be a member of states
	((equal s (first states)) ; if s is equal to the first item in states
	 t) ; return t - we found s in states
	(t ; otherwise
	 (on-path s (rest states))))) ; look for s in the rest of states

; MULT-DFS is a helper function for MC-DFS. It takes two arguments: a stack of
; states from the initial state to the current state (path), and the legal
; successor states from the current state (states).
; MULT-DFS does a depth-first search on each element of states in
; turn. If any of those searches reaches the final state, MULT-DFS returns the
; complete path from the initial state to the goal state. Otherwise, it returns
; NIL. 
; Note that the path should be ordered as: (S_n ... S_2 S_1 S_0)
(defun mult-dfs (states path)
  (cond ((null states) ; if there are no legal successor states
	 NIL) ; return NIL - no path possible
	(t ; otherwise - there are legal successor states
	 (let* ((first-succ-state (first states)) ; get the first legal successor state
		; do a DFS starting at first-succ-state
		(first-complete-path (mc-dfs first-succ-state path)))
	   (cond (first-complete-path ; if we found a path from first-succ-state -> final state
		  first-complete-path) ; then return that path
		 (t ; otherwise (the DFS couldn't find a path - returned NIL)
		  (mult-dfs (rest states) path))))))) ; try all of the other successor states

; MC-DFS does a depth first search from a given state to the goal state. It
; takes two arguments: a state (S) and the path from the initial state to S
; (PATH). If S is the initial state in our search, PATH should be NIL. MC-DFS
; performs a depth-first search starting at the given state. It returns the path
; from the initial state to the goal state, if any, or NIL otherwise. MC-DFS is
; responsible for checking if S is already the goal state, as well as for
; ensuring that the depth-first search does not revisit a node already on the
; search path.
(defun mc-dfs (s path)
  (cond ((final-state s) ; if s is already the goal state
	  ; then add s to the path and return it - we've finished searching
	  (cons s path))
	 ((on-path s path) ; if s is already in the search path
	  NIL) ; return NIL - don't revisit nodes already in search path
	 (t ; otherwise
	  ; succ-states = all possible legal successor states to s
	  (let ((succ-states (succ-fn s)))
	  	; call mult-dfs, passing it each of the successor states,
	  	; as well as the path with s prepended to it
	    (mult-dfs succ-states (cons s path))))))
	   



; Function execution examples

; Applying this operator would result in an invalid state, with more cannibals
; than missionaries on the east side of the river.
; (next-state '(3 3 t) 1 0) -> NIL

; Applying this operator would result in one cannibal and zero missionaries on
; the west side of the river, which is a legal operator. (NOTE that next-state
; returns a LIST of successor states, even when there is only one successor)
; (next-state '(3 3 t) 0 1) -> ((0 1 NIL))

; succ-fn returns all of the legal states that can result from applying
; operators to the current state.
; (succ-fn '(3 3 t)) -> ((0 1 NIL) (1 1 NIL) (0 2 NIL))
; (succ-fn '(1 1 t)) -> ((3 2 NIL) (3 3 NIL))

; TEST CASES
;(assert (equal (BFS '((A (B)) C (D))) '(C A D B)))
;(assert (equal (DFS '((A (B)) C (D))) '(D C B A)))
;(assert (equal (DFID '((A (B)) C (D)) 3) '(C A C D A B C D)))
;(assert (equal (final-state '(3 3 NIL)) t))
;(assert (equal (final-state '(3 3 t)) NIL))
;(assert (equal (next-state '(3 3 t) 1 0) NIL))
;(assert (equal (next-state '(3 3 t) 0 1) '((0 1 NIL))))
;(assert (equal (succ-fn '(3 3 t)) '((1 1 NIL) (0 2 NIL) (0 1 NIL))))
;(assert (equal (succ-fn '(1 1 t)) '((3 3 NIL) (3 2 NIL))))

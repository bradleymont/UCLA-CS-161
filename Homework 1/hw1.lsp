; Homework 1: Introduction to LISP
; For Homework 1, I implemented several functions having to do with ordered trees, lists, and binary trees. 
; I used a recursive approach in order to break up the problems into similar sub-problems.
; Note: an ordered tree is represented as a number or a list of 3 elements (L m R), where m is a number,
; and L and R are ordered trees, and L < m < R
; Note: a binary tree is represented as either an atom (leaf node) or a list (L R), where
; where L is the left child and R is the right child

; 1. TREE-CONTAINS: returns true if ordered tree TREE contains number N
; ARGUMENTS: N (number), TREE (ordered tree)
; RETURN VALUE: boolean (t if N is in TREE)
(defun TREE-CONTAINS (N TREE)
  (cond ((numberp TREE) ; if the tree is a number
	 (equal TREE N)) ; return whether or not the tree equals N
	((equal (second TREE) N) ; if m equals N
	 t) ; return true
	(t ; otherwise
	 ; return true if N is in L or R 
	 (or (TREE-CONTAINS N (first TREE)) (TREE-CONTAINS N (third TREE))))))

; 2. TREE-MIN: returns the minimum number in ordered tree TREE
; ARGUMENTS: TREE (ordered tree)
; RETURN VALUE: number (minimum number in TREE)
(defun TREE-MIN (TREE)
  (cond ((numberp TREE) ; if the tree is a number
	 TREE) ; then the minimum must be that number
	(t ; otherwise (the tree is a list (L m R))
	 (TREE-MIN (first TREE))))) ; the min must be in the L tree, so make recursive call

; 3. TREE-ORDER: returns a pre-ordered list of the numbers in the ordered tree TREE
; ARGUMENTS: TREE (ordered tree)
; RETURN VALUE: list (pre-ordered list of numbers in TREE)
(defun TREE-ORDER (TREE)
  (cond ((numberp TREE) ; if the tree is a number
	 (list TREE)) ; return a list containing just the number
	(t ; otherwise (the tree is a list (L m R))
	 (let ((LEFT-ORDER (TREE-ORDER (first TREE))) ; get pre-order of left subtree
	       (RIGHT-ORDER (TREE-ORDER (third TREE)))) ; get pre-order of left subtree
	   ; return m -> LEFT-ORDER -> RIGHT-ORDER (this is pre-ordered)
	   (cons (second TREE) (append LEFT-ORDER RIGHT-ORDER))))))

; 4. SUB-LIST: returns a sub-list of list L with offset START and length LEN
; ARGUMENTS: L (list), START (non-negative integer), LEN (non-negative integer)
; RETURN VALUE: list (sublist of L with offset START and length LEN)
(defun SUB-LIST (L START LEN)
  (cond ((= LEN 0) ; if the length is 0
	 NIL) ; return an empty list
	((> START 0) ; if the START > 0
	 ; call recursively with rest of L, and decrement START
	 (SUB-LIST (rest L) (- START 1) LEN))
	(t ; START = 0 and LEN > 0
	 ; append the head of the list to the result of the
	 ; recursive call with the tail of the list, with LEN decremented
	 (cons (first L) (SUB-LIST (rest L) START (- LEN 1))))))
  
; 5. SPLIT-LIST: split a list L into two lists of equal length 
; (or the first list is 1 element longer if length is odd)
; ARGUMENTS: L (list)
; RETURN VALUE: list of the form '(L1 L2), where L1 and L2 are the two lists that L was split into
(defun SPLIT-LIST (L)
  (let ((L1-LENGTH (cond ; L1-LENGTH = length of L1
		    ((oddp (length L)) ; if length of L is odd
		     (/ (+ (length L) 1) 2)) ; add 1 to the length, then divide by 2
		    (t ; if length of L is even
		     (/ (length L) 2)))) ; divide the length by 2
	(L2-LENGTH (cond ; L2-LENGTH = length of L2
		    ((oddp (length L)) ; if length of L is odd
		     (/ (- (length L) 1) 2)) ; subtract 1 from the length, then divide by 2
		    (t ; if length of L is even
		     (/ (length L) 2))))) ; divide the length by 2
    ; now that we have the lengths of L1 and L2,
    ; and the start index of L2 is equal to the length of L1,
    ; use SUB-LIST to split into 2 lists
    (list (SUB-LIST L 0 L1-LENGTH) (SUB-LIST L L1-LENGTH L2-LENGTH))))

; 6. BTREE-HEIGHT: return the height (longest path from root node to farthest leaf node)
; of a binary tree TREE
; ARGUMENTS: TREE (binary tree)
; RETURN VALUE: number (height of the binary tree)
(defun BTREE-HEIGHT (TREE)
  (cond ((atom TREE) ; if the TREE is just a single leaf node
	 0) ; then the height is 0
	(t ; internal node
	 (let ((LEFT-HEIGHT (BTREE-HEIGHT (first TREE))) ; height of the left subtree
	       (RIGHT-HEIGHT (BTREE-HEIGHT (second TREE)))) ; height of the right subtree
	   (let ((MAX-SUBTREE-HEIGHT (cond ; find the max(LEFT-HEIGHT, RIGHT-HEIGHT)
				     ((> LEFT-HEIGHT RIGHT-HEIGHT) ; if left > right
				      LEFT-HEIGHT) ; MAX-SUBTREE-HEIGHT = LEFT-HEIGHT
				     (t ; if left <= right
				      RIGHT-HEIGHT)))) ; MAX-SUBTREE-HEIGHT = RIGHT-HEIGHT
	     ; the height of the binary tree is equal to
	     ; 1 + the max between the height of the left and right subtrees,
	     ; which is binded to MAX-SUBTREE-HEIGHT
	     (+ 1 MAX-SUBTREE-HEIGHT))))))
	   
; 7. LIST2BTREE: take a non-empty list of atoms LEAVES and return it's binary tree representation
; specifically, the leaves are the elements of LEAVES, and the left and right branches are balanced
; for any internal node: (# of leaves in left branch) - (# of leaves in right branch) = 0 or 1
; ARGUMENTS: LEAVES (list of atoms)
; RETURN VALUE: binary tree (whose leaves are LEAVES and is balanced)
(defun LIST2BTREE (LEAVES)
  (cond ((= (length LEAVES) 1) ; if the list has only 1 atom
	 (first LEAVES)) ; the tree is just that one atom (leaf)
	(t ; the list has multiple leaves
	 (let* ((SPLIT-LEAVES (SPLIT-LIST LEAVES)) ; split the list into 2
		(LEFT-LEAVES (first SPLIT-LEAVES)) ; get the first half of leaves
		(RIGHT-LEAVES (second SPLIT-LEAVES))) ; get the second half of leaves
	   (let ((LEFT-CHILD (LIST2BTREE LEFT-LEAVES)) ; get the left child
		 (RIGHT-CHILD (LIST2BTREE RIGHT-LEAVES))) ; get the right child
	     ; return (LEFT-CHILD RIGHT-CHILD)
	     (list LEFT-CHILD RIGHT-CHILD))))))

; 8. BTREE2LIST: take a binary tree TREE and return a list of the atoms in it
; (BTREE2LIST (LIST2BTREE X)) = X for all lists of atoms X
; ARGUMENTS: TREE (binary tree)
; RETURN VALUE: list (containing the atoms in TREE)
(defun BTREE2LIST (TREE)
  (cond ((atom TREE) ; if the TREE is just a single leaf node
	 (list TREE)) ; return a list of just that atom
	(t ; the TREE is an internal node
	 (let ((LEFT-CHILD (first TREE)) ; get the left child
	       (RIGHT-CHILD (second TREE))) ; get the right child
	   (let ((LEFT-LIST (BTREE2LIST LEFT-CHILD)) ; get the atoms in the left subtree
		 (RIGHT-LIST (BTREE2LIST RIGHT-CHILD))) ; get the atoms in the right subtree
	     (append LEFT-LIST RIGHT-LIST)))))) ; append both lists of atoms

; 9. IS-SAME: returns true if two LISP expressions E1 and E2 are identical
; Note: all atoms must be numbers
; ARGUMENTS: E1 (LISP expression), E2 (LISP expression)
; RETURN VALUE: boolean (true if E1 and E2 are identical)
(defun IS-SAME (E1 E2)
  (cond ((and (null E1) (null E2)) ; if E1 and E2 are both NIL
	 t) ; then consider them the same
	((and (numberp E1) (numberp E2)) ; if E1 and E2 are both numbers
	 (= E1 E2)) ; return whether or not they're the same number
	((and (listp E1) (listp E2)) ; if E1 and E2 are both lists
	 ; return true if the heads of E1 and E2 are the same
	 ; then recursively check the tails as well
	 (and (IS-SAME (first E1) (first E2)) (IS-SAME (rest E1) (rest E2))))
	(t ; otherwise (if E1 and E2 aren't the same type)
	 NIL))) ; they can't be equal

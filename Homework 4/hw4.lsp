;;;;;;;;;;;;;;
; Homework 4 ;
;;;;;;;;;;;;;;

; returns true if assignment doesn't violate any constraints of the cnf
; Note: an assignment violates the constraints of the cnf if it causes any of
; the clauses to evaluate to false
; Example: if assignment = x, then it causes the cnf to evaluate to false
; if (-x) is one of the clauses
(defun validAssignment (assignment cnf)
  (cond ((null cnf) ; if the cnf is empty
	 t) ; return true - assignment doesn't violate anything
	(t ; otherwise (cnf is not empty)
	 ; if the first clause is (-assignment)
	 (if (equal (list (- assignment)) (first cnf))
	     NIL ; then it will evaluate to false - assignment not valid
	   (validAssignment assignment (rest cnf)))))) ; check the rest of the CNF

; applies assignment to a single clause and returns resulting clause
; same logic as applyAssignment applies in this function
(defun applyAssignmentToClause (assignment clause)
  (if (member assignment clause) ; if the assignment is in clause
      NIL ; remove that clause - it will always evaluate to true
    ; else, return the clause with -assignment removed
    (remove (- assignment) clause)))
    
; apply assignment to cnf and return the resulting cnf
; Note: if we are applying assignment x:
; Remove all clauses with x since they are satisfied.
; Remove -x from all clauses since it will not contribute toward
; the clause evaluating to true.
(defun applyAssignment (assignment cnf)
  (cond ((null cnf) ; if the cnf is empty
	 NIL) ; return an empty list
	(t ; otherwise (cnf is not empty)
	 ; apply the assignment to the first row
	 (let ((updatedFirstRow (applyAssignmentToClause assignment (first cnf))))
	   (if updatedFirstRow ; if the updated first row isn't empty
		   ; append it to the rest of the updated cnf
	       (cons updatedFirstRow (applyAssignment assignment (rest cnf)))
	     ; if it is empty, just return the rest of the updated cnf
	     (applyAssignment assignment (rest cnf)))))))

; takes in a cnf and an assignment to apply, and attempts to apply that assignment
; returns next cnf if the assignment is valid
; Note: if the assignment completely satifies the cnf, returns NIL 
; Note: if the assignment is invalid, return 'invalid
(defun getNextCnf (literal cnf)
  (if (validAssignment literal cnf) ; if the literal does not violate any constraints
      (applyAssignment literal cnf) ; apply the assignment and return the new cnf
    'invalid)) ; otherwise return NIL

; takes an assignment that ALREADY satisfies a cnf
; and adds the positive literals of all the remaining variables
; to ensure that the assignment has the correct number of variables
; Example: (makeCompleteAssignment '(-1 2 -3) 5) -> '(-1 2 -3 4 5)' 
; (not necessarily in that order)
(defun makeCompleteAssignment (vars n)
  (if (equal (length vars) n)
      vars
    (makeCompleteAssignment (cons (+ (length vars) 1) vars) n)))

; takes in a partial assignment and a cnf of n integers, and returns the model
; returns NIL if not satisfiable
; Note: assume that 1 ... (length vars) variables have already been assigned,
; so we'll try to assign variable (length vars) + 1 next
(defun satSolver (vars n cnf)
  (cond ((equal cnf 'invalid) ; if the cnf is invalid
	 NIL) ; return false
	((null cnf) ; if the cnf is empty
	 ; we consider it always satisfied, so randomly assign any extra variables
	 (makeCompleteAssignment vars n))
	(t ; otherwise (the cnf is not empty)
	 ; posLiteral and negLiteral are the next variables that we try to assign
	 (let* ((posLiteral (+ (length vars) 1))
		(negLiteral (- posLiteral))
		(posNextCnf (getNextCnf posLiteral cnf)) ; get next cnf for pos
		(negNextCnf (getNextCnf negLiteral cnf))) ; get next cnf for neg
	   (cond ((null posNextCnf) ; if the posLiteral finished the model
		  ; we found our answer (will return at the beginning of next call)
		  (satSolver (cons posLiteral vars) n posNextCnf))
		 ; same logic, but for negLiteral
		 ((null negNextCnf)
		  (satSolver (cons negLiteral vars) n negNextCnf))
		 (t ; otherwise (none of the assignments IMMEDIATELY solve the cnf)
		  ; recursively try both, returning whichever one solves it
		  ; or returning null if the cnf is unsatisfiable
		  (or (satSolver (cons posLiteral vars) n posNextCnf)
		      (satSolver (cons negLiteral vars) n negNextCnf))))))))
	 
; EXERCISE: Modify this function to decide satisifiability of delta.
; If delta is satisfiable, sat? returns a list of n integers that represents a model of delta,  
; otherwise it returns NIL. (See spec for details.)
; param n: number of variables in delta
; param delta: a CNF represented as a list of lists
(defun sat? (n delta)
  (satSolver NIL n delta)) ; call helper function with no variables assigned yet

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Functions that help you parse CNF from files in folder cnfs/
; You need not modify any functions in this section
; Usage (solve-cnf <path-to-file>)
; e.g., (solve-cnf "./cnfs/f1/sat_f1.cnf")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun split-line (line)
  (if (equal line :eof)
      :eof
      (with-input-from-string (s line) (loop for x = (read s nil) while x collect x))))

(defun read-cnf (filename)
  (with-open-file (in filename)
    (loop for line = (split-line (read-line in nil :eof)) until (equal line :eof)
      if (equal 'p (first line)) collect (third line)      ; var count
      if (integerp (first line)) collect (butlast line)))) ; clause

(defun parse-cnf (filename)
  (let ((cnf (read-cnf filename))) (list (car cnf) (cdr cnf))))

; Following is a helper function that combines parse-cnf and sat?
(defun solve-cnf (filename)
  (let ((cnf (parse-cnf filename))) (sat? (first cnf) (second cnf))))

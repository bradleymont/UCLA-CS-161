;;;;;;;;;;;;;;
; Homework 4 ;
;;;;;;;;;;;;;;

; tests whether an assignment of variables satisfies a clause
; Note: since a clause is a disjunction, we just need 1 literal in
; the clause to be true, then the entire clause will be satisfied
(defun satisfiesClause (assignment clause)
  (cond ((null clause) ; if the clause is empty
	 NIL) ; consider it not satisied
	(t ; otherwise (clause is not empty)
	 ; if the 1st literal in the clause is in our assignment
	 (if (member (first clause) assignment)
	     t ; return true - the entire clause is satisfied
	   (satisfiesClause assignment (rest clause)))))) ; else, check the rest of the clause

; CONSIDER removing the n bc the check is pointless after the first iteration
; tests whether an assignment of values is a model for a CNF with n variables
(defun isModel (assignment cnf)
  (cond	((null cnf) ; if the cnf is empty
	 t) ; consider it satisfied by default
	(t ; otherwise (the assignment is complete)
	 ; if the assignment satisfies the first clause in the cnf
	 (if (satisfiesClause assignment (first cnf))
	     (isModel assignment (rest cnf)) ; make sure it satisfies the rest
	   NIL)))) ; otherwise return false - it must satisfy every clause

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
  
; takes in a partial assignment and a cnf of n integers, and returns the model
; returns NIL is not satisfiable
;
; Note: assume that the first element of assignment is the NEWEST and
; has not yet been checked for satisfiability, so we must check it first
;
; Note: assume that 1 ... (length assignment) variables have already been assigned,
; so we'll try to assign variable (length assignment) + 1 next
					;







(defun makeCompleteAssignment (vars n)
  (if (equal (length vars) n)
      vars
    (makeCompleteAssignment (cons (+ (length vars) 1) vars) n)))



; vars = our current partial assignment
; new try: assume that any assignment we have is consistent
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
		(posNextCnf (getNextCnf posLiteral cnf))
		(negNextCnf (getNextCnf negLiteral cnf)))
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
  (satSolver NIL n delta))




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


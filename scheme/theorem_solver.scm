;Graden Rea
;Aritifical Intelligence homework #4: Thoerem Prover
;5/18/11
;
;
;Public Interface
;This program can solve arbitrary propositional logic statements, and detect contradictions
;
;To add a statement to the machine use the following syntax:
;	(add-statement <statement>)
;
;You can not remove a statement after it has been inserted, but you can clear all statements with the following command:
;	(clearStatements)
;
;Finally, in order to check whether a variable is true or false, use the following command:
;	(prove variable)
;
;
;-----basic definitions--------------
; you can change these to modify the literals that are used for each operation
; the value on the left is what the operation is called in the code.  DO NOT change this value
; the value on the right is the name used by the user
;
; For instance, to change implies to use an array, change
;	(define IMPLIES 'implies)
;	to
;	(define IMPLIES '=>)
(define NOT     'not)
(define IMPLIES 'implies)
;(define IMPLIES '<=>)
(define IFF     'iff)
(define AND     '^)
(define OR      'v)
(define TRUE    'true)
(define FALSE   'false)

;misc labels that are used internally by the program
(define UNKNOWN '?)
(define CONTRADICTION 'cont)
;If you want to add (A => B) in the default syntax, you would do:
;	(add-statement '(implies A B))
;If you wanted to add that A is true, then use;
;	(add-statement 'A)
;Now if you wanted to know if B was true or false, you type:
;	(prove 'B)
;
;biconditional has the same syntax as implies
;	(add-statement '(iff A B))
;
;Nots are also easy to use:
;	(add-statement '(not A))
;
;All Statements use prefix syntax, and ^ and v can take as many arguments as yo want
;(A v B v C v D) would be written
;	(add-statement '(v A B C D))
;
;variable can be nested to an depth
;	(add-statement '(implies (v (not A) (^ B (v C))) (^ A B C (v D (not E)))))
;
;You can also add true or false if you want:
;	(add-statement '(implies true A))
;
;however, the following command will result in a contradiction
;	(add-statement 'false)

;global variable that stores the current statements
(define state TRUE)
;global variable that store the known variables and their values
(define known '())

;this functions reads a statement, converts it to CNF form, adds it to the global state
(define (add-statement stmt)
  (set! state (list AND (toCNF stmt) state)))

;prints out the values of a given literal
(define (prove A)
  (let ((result (test A)))
    (cond ((equal? result CONTRADICTION) (display "The given input statements form a contradiction\n"))
	  ((equal? result TRUE)  (begin (display A) (display " is true\n" )))
	  ((equal? result FALSE) (begin (display A) (display " is false\n")))
	  (else (begin (display A) (display " is unknown\n"))))))

;removes all statements that have been added using the add-statement function
(define (clearStatements)
  (begin 
    (set! state TRUE)
    (set! known '())))

;this function simpliy converts an ^ to an v, or vice-sersa.  Useful when bringing a not inside
(define (negateAndOr op)
  (cond ((equal? op AND) OR)
	((equal? op OR) AND)
	(else (begin 
		(display "negateAndOr: error, ")
		(display op)
		(display " is not an AND or and OR\n")))))

;This function removes biconditionals and implies statements step 1 and 2 of converting a function to CNF form
(define (removeImp stmt)
  (cond ((or (null? stmt) (atom? stmt)) stmt)
	((equal? (car stmt) IMPLIES)
	 (let ((A (removeImp (list-ref stmt 1)))
	       (B (removeImp (list-ref stmt 2))))
	   (list OR (list NOT A) B)))
	((equal? (car stmt) IFF)
	 (let ((A (removeImp (list-ref stmt 1)))
	       (B (removeImp (list-ref stmt 2))))
	   (list AND (list OR (list NOT A) B) (list OR (list NOT B) A))))
	(else (cons (car stmt) (map removeImp (cdr stmt))))))

;this function moves nots inside of expressions.  Step 3 of converting an expression to CNF form
(define (moveNotsIn stmt)
  (cond ((or (null? stmt) (atom? stmt)) stmt)
	((equal? NOT (car stmt))
	 (let ((innerStmt (cadr stmt)))
	   (cond ((atom? innerStmt) (cond ((equal? TRUE  innerStmt) FALSE)
					  ((equal? FALSE innerStmt) TRUE)
					  (else stmt)))
		 ((equal? NOT (car innerStmt)) (moveNotsIn (cadr innerStmt)))
		 (else (cons (negateAndOr (car innerStmt)) (map (lambda (stmt) (moveNotsIn (list NOT stmt))) (cdr innerStmt)))))))
	(else          (cons              (car stmt)       (map (lambda (stmt) (moveNotsIn      stmt))  (cdr stmt))))))

;converts a function to psuedo-CNF form.  Does not deal with distributivity stuff.  Since every statement is anded togethor, global state will be in CNF form.
;If its not, the inference engine can deal with it
(define (toCNF stmt)
  (moveNotsIn (removeImp stmt)))

;Attempts to deduce if variables must be true or false given the ands in the top level of the statement
;returns a list of the form ((literal-name true/false) (..))
(define (interpret stmt)
  (letrec ((helper
	     (lambda (stmt) 
	       (cond ((null? stmt) '())
		     ((atom? stmt) (list stmt TRUE))
		     (else (let ((current (car stmt))
				 (rest (cdr stmt)))
			     (cond ((or (equal? current TRUE) (equal? current FALSE)) (helper rest))
				   ((atom? current) (cons (list current TRUE) (helper rest)))
				   ((and (equal? (car current) NOT) (atom? (cadr current)))
				    (cons (list (cadr current) FALSE) (helper rest)))
				   (else (helper rest)))))))))
    (cond ((equal? (car stmt) AND) (helper (cdr stmt)))
	  ((and (equal? (car stmt) NOT) (atom? (cadr stmt))) (list (list (cadr stmt) FALSE)))
	  (else '()))))

;this function replaces a list of literals with a value (true or false)
(define (mapKnownToState known stmt)
  (if (null? known) stmt
    (mapKnownToState (cdr known) (substitute (caar known) (cadar known) stmt))))
	
;replaces all occurances of a single literal with another value.  Used for replacing a singe variable with true or false
(define (substitute old new stmt)
  (letrec ((sub (lambda (alist)
		  (cond ((null? alist) alist)
			((atom? (car alist))
			 (if (equal? (car alist) old)
			   (cons new (sub (cdr alist)))
			   (cons (car alist) (sub (cdr alist)))))
			(else (cons (sub (car alist)) (sub (cdr alist))))))))
    (if (atom? stmt)
      (if (equal? stmt old) 
	new
	stmt)
      (sub stmt))))

;brings nested statements up a level if they are of the same type as the top level
;eg (^ A (^ B C)) would be transformed into (^ A B C)
(define (combineLikeOperands stmt)
  ;this function moves horozontal across a list and call combineLikeOperands on each element in the list
  (letrec ((checkList (lambda (op stmts)
			(if (null? stmts) '()
			  (let ((nextStmt (car stmts))
				(restStmts (cdr stmts)))
			    (cond ((atom? nextStmt) (cons nextStmt (checkList op restStmts)))
				  ((equal? (car nextStmt) op) (append (cdr (combineLikeOperands nextStmt)) (checkList op restStmts)))
				  (else (cons (combineLikeOperands nextStmt) (checkList op restStmts)))))))))

    (if (or (null? stmt) (atom? stmt)) stmt
      (let ((op (car stmt)))
	(if (equal? op NOT) (list NOT (combineLikeOperands (cadr stmt)))
	  (cons op (checkList op (cdr stmt))))))))

;this function deals with true and falses in ^ and v statements.  It applies the following formulas:
;	!T -> F
;	!F -> T
;
;	(A ^ T)  -> A
;	(A ^ !A) -> F
;	(A ^ F)  -> F
;
;	(A v F)  -> A
;	(A v !A) -> T
;	(A v T)  -> T
;
(define (simplify stmt)
  (if (or (null? stmt) (atom? stmt)) stmt
    (let ((operand (car stmt))
	  (operees (removeDups (cdr stmt))))
      (cond ((equal? operand NOT) (let ((innerStmt (car operees)))
				    (cond ((equal? innerStmt TRUE)  FALSE)
					  ((equal? innerStmt FALSE) TRUE)
					  ((atom?  innerStmt) stmt)
					  (else (simplify (moveNotsIn stmt))))))
	    ((equal? operand AND) (cond ((member FALSE operees) FALSE)
					((existsNegation stmt) FALSE)
					(else (let ((cleaned (delete TRUE operees)))
						(cond ((null? cleaned) TRUE)
						      ((null? (cdr cleaned)) (simplify (car cleaned)))
						      (else (cons operand (map simplify cleaned))))))))
	    ((equal? operand OR)  (cond ((member TRUE  operees) TRUE)
					((existsNegation stmt) TRUE)
					(else (let ((cleaned (delete FALSE operees)))
						(cond ((null? cleaned) FALSE)
						      ((null? (cdr cleaned)) (simplify (car cleaned)))
						      (else (cons operand (map simplify cleaned))))))))
	    (else (begin (display "simplify: ERROR, ")
			 (display operand)
			 (display "is not an valid operand") 'ERROR))))))

;removes elements that appear twice in a list
(define (removeDups stmt)
  (if (null? stmt) '()
    (let ((fst  (car stmt))
	  (rest (cdr stmt)))
      (cond ((member fst (cdr stmt)) (removeDups rest))
	    (else (cons fst (removeDups rest)))))))

;looks through the function for something like (A ^ !A)
;returns true if there exists X and (not X) in the statement
;This alows a ^ to automatically return false and an v to automtically return true
(define (existsNegation stmt)
  (if (or (null? stmt) (atom? stmt)) #f
    (let ((fst  (car stmt))
	  (rest (cdr stmt)))
      (cond ((and (atom? fst) (member (list NOT fst) (cdr stmt))) #t)
	    ((and (not (atom? fst))
		  (equal? NOT (car fst))
		  (atom? (cadr fst))
		  (member (cadr fst) (cdr stmt)))
	     #t)
	    (else (existsNegation rest))))))

;this function creates one step of resolution
(define (iterate stmt) (combineLikeOperands (simplify stmt)))

;this function evaluates the given statements and attempts to infer as much as possible and save it for later
(define (infer stmt)
  ;store a local version of infered variables so that we dont overwrite the global one
  (define known '())
    (letrec 
      ;this function handles the twisting and reshaping of a statement
      ((manipulate (lambda (stmt)
		     (let ((nextStmt (iterate stmt)))
		       (if (or (atom? nextStmt) (equal? stmt nextStmt)) nextStmt
			 (manipulate nextStmt)))))
       ;this function discovers the variables on the top level and deduces wether they must be true or false
       (evaluate (lambda (stmt) 
		   (let ((inference (interpret stmt)))
		     (begin (set! known (append inference known))
			    (mapKnownToState inference stmt)))))

       (helper (lambda (stmt)
		 (cond ((eq? stmt TRUE)  TRUE)
		       ((eq? stmt FALSE) FALSE)
		       ((atom? stmt)     (begin (set! known (cons (list stmt TRUE) known))
						TRUE))
		       (else (let ((nextCycle (manipulate (evaluate stmt))))
			       (if (equal? stmt nextCycle)
				 stmt
				 (helper nextCycle))))))))

      ;a let is required here so that we return the newest version of known
      (let ((endStmt (helper stmt)))
	(list known endStmt))))

;this function searches the list of known variables for a contradiction ie if it has been determined that the variable must be both true and false
;it also sets the state to FALSE so that every subsequent function will know that there is a contradiction and reduce the work that they do
(define (contradiction vars)
  (cond ((equal? state FALSE) (begin (set! known '()) #t))
	((null? vars) #f)
	(else 
	  (let* ((var      (caar  vars))
		 (value    (cadar vars))
		 (!value   (if (equal? value TRUE) FALSE TRUE))
		 (opposite (list var !value))
		 (rest     (cdr vars)))
	    (if (member opposite rest)
	      (begin (set! state FALSE)
		     (set! known '())
		     #t)
	      (contradiction rest))))))

;returns tue if the argument is an atom ie not a list
(define (atom? a)
  (not (or (pair? a) (null? a))))

;test determines whether a given variable is true, false, or unknown.
;it returns the constant, TRUE, FALSE, or UNKNOWN
(define (test A)
  (begin
  (let ((return (mapKnownToState known (infer state))))
    (if (equal? (cadr return) FALSE) (set! state FALSE))
    (set! known (append (car return) known)))
  (cond ((contradiction known) CONTRADICTION)
	((member (list A TRUE) known) TRUE)
	((member (list A FALSE) known) FALSE)
	;attempt a proof by contradiction
	;start by assuming it is false
	(else (let ((proofByCont 
		      (infer (mapKnownToState (list (list A FALSE)) state))))

		(if (equal? (cadr proofByCont) FALSE) 
		  (begin (set! known (cons (list A TRUE) known)) TRUE)

		  ;since we didnt get a contradicition, test for when the Aiable is true
		  (let ((proofByCont2 
			  (infer (mapKnownToState (list (list A TRUE)) state))))

		    (if (equal? (cadr proofByCont2) FALSE) 
		      (begin (set! known (cons (list A FALSE) known)) FALSE)
		      UNKNOWN))))))))

;here are some sample test cases.
;they are written in the long gnereic form so that they can still be used if the operations are redefines (such as changing implies to =>
;For example, the add-stetment commands in testcase1 could be rewritten as:
;(add-statement '(implies A B))
;(add-statement '(implies B C))
;(add-statement '(not A))
(define (testCase1)
  (clearStatements)
  (add-statement (list IMPLIES 'A 'B))
  (add-statement (list IMPLIES 'B 'C))
  (add-statement (list NOT 'A))
  (and
    (equal? (test 'A) FALSE) 
    (equal? (test 'B) UNKNOWN) 
    (equal? (test 'C) UNKNOWN)
    (equal? (test 'D) UNKNOWN)))

(define (testCase2)
  (clearStatements)
  (add-statement (list IMPLIES 'A 'B))
  (add-statement (list IMPLIES 'B 'C))
  (add-statement 'A)
  (and
    (equal? (test 'A) TRUE) 
    (equal? (test 'B) TRUE)
    (equal? (test 'C) TRUE)
    (equal? (test 'D) UNKNOWN)))

(define (testCase3)
  (clearStatements)
  (add-statement (list IMPLIES 'A 'B))
  (add-statement (list IMPLIES 'B 'C))
  (add-statement 'A)
  (add-statement (list NOT 'C))
  (and
    (equal? (test 'A) CONTRADICTION) 
    (equal? (test 'B) CONTRADICTION) 
    (equal? (test 'C) CONTRADICTION)
    (equal? (test 'D) CONTRADICTION)))

(define (testCase4)
  (clearStatements)
  (add-statement (list IMPLIES 'A 'B))
  (add-statement (list IMPLIES 'B 'C))
  (add-statement 'B)
  (and
    (equal? (test 'A) UNKNOWN)
    (equal? (test 'B) TRUE) 
    (equal? (test 'C) TRUE)
    (equal? (test 'D) UNKNOWN)))

(define (testCase5)
  (clearStatements)
  (add-statement (list IFF      'A 'B))
  (add-statement (list IMPLIES  'A 'C))
  (add-statement 'B)
  (and
    (equal? (test 'A) TRUE)
    (equal? (test 'B) TRUE) 
    (equal? (test 'C) TRUE)
    (equal? (test 'D) UNKNOWN)))

(define (testCase6)
  (clearStatements)
  (add-statement (list AND 'A (list AND 'B (list AND 'C (list AND 'D FALSE) 'E) 'F) 'G))
  (and
    (equal? (test 'A) CONTRADICTION)
    (equal? (test 'B) CONTRADICTION)
    (equal? (test 'C) CONTRADICTION)
    (equal? (test 'D) CONTRADICTION)))

(define (testCase7)
  (clearStatements)
  (add-statement (list OR 'A (list OR 'B (list OR 'C (list OR 'D TRUE) 'E) 'F)' G))
  (and
    (equal? (test 'A) UNKNOWN)
    (equal? (test 'B) UNKNOWN)
    (equal? (test 'C) UNKNOWN)
    (equal? (test 'D) UNKNOWN)))

(define (testCase8)
  (clearStatements)
  (add-statement (list AND (list OR (list NOT 'A) 'C)
		           (list OR 'A            'B)))
  (add-statement (list OR 'A (list NOT 'B)))
  (and
    (equal? (test 'A) TRUE)
    (equal? (test 'B) UNKNOWN)
    (equal? (test 'C) TRUE)))

(define (testCase9)
  (clearStatements)
  (add-statement (list IFF (list IMPLIES (list NOT 'A) 'B           )
		           (list IMPLIES 'C            (list NOT 'B))))
  (add-statement (list NOT 'A))
  (equal? (test 'C) FALSE))

(define (testCase10)
  (clearStatements)
  (add-statement (list IFF (list IMPLIES (list NOT 'A) 'B           )
		           (list IMPLIES 'C            (list NOT 'B))))
  (add-statement 'C)
  (equal? (test 'A) TRUE))

(define (testCase11)
  (clearStatements)
  (add-statement (list IFF (list IMPLIES 'A 'B) 'D))
  (add-statement (list OR (list NOT 'A) 'C))
  (add-statement (list IMPLIES 'C (list NOT 'D)))
  (add-statement (list NOT 'B))
  (add-statement (list IMPLIES 'D 'B))
  (and
    (equal? (test 'A) TRUE)
    (equal? (test 'B) FALSE)
    (equal? (test 'C) TRUE)
    (equal? (test 'D) FALSE)))

(define (runTests)
  (and
    (testCase1)
    (testCase2)
    (testCase3)
    (testCase4)
    (testCase5)
    (testCase6)
    (testCase7)
    (testCase8)
    (testCase9)
    (testCase10)
    (testCase11)))

;Gradem Rea
;Artificial Intelligence Homework #3
;4/25/11

;This program solves the cracker barrel game.  Boards are a list of occupied 
;locations.  Try the following command:
;   (solve '(2 3 4 5 6)) 
;Valid input: any sorted list with the numbers 1-15 without repeats
;There is no validation for invalid input

;For playing around, you can use the solve function to solve and arbitrary game
;or you can use the solveAll function to print all the solutions to a game
;board

;All searching is done with a Depth First Search Algorithm

;Some care has been taken to optimize my solution:
;	-dead end game boards are remembered so that if they are found again
;		they can be skipped over
;	-when not searching for all solutions, the DFS algorithm will try to 
;		determine which move is the most likely to lead to a winning
;		board.
;	-Most functions are tail recursive.  Even the search algoithm.
;

;the "main" function.  give it a board and it prints a solution
;It uses the heuristic function HeuristicB, B stands for best; it is most
;most consistently the fastest
;game boards are assumed to have a valid.  As a result, behavior is undefined
;if the input:
;	-has a peg twice
;	-has a peg in a hole greater than 15
;	-has a peg in a hole less than 0
;	-has pegs not in sorted order
(define (solve board)
  (let* ((return     (DFS heuristicB board))
	 (nodeChecks (car return))
	 (steps     (cdr return)))
    (begin (print prettyPrint steps)
	   (display "this took ")
	   (display nodeChecks)
	   (display " node checks\n"))))

;prints all solutions to a given board in a less-pretty, space-efficient manner
(define (solveAll board)
  (printTree plainPrint (DFSAll board)))


;--functions related to searching for a solution and solving the problem

;Depth First Search function.  All the heavy lifting is done in the helper function
;This function tries to efficiently return a solution
(define (DFS heuristic board)
  (if (null? board) '(1 (()))
    (let ((moves (applyHeuristic heuristic (generateMoveList board) board)))
      (DFSHelper heuristic '() board moves '() 1))))

;this massive beast is a tail recursive depth first search function.  It also
;memoizises board states that are have no solutions to speed up searches.
;
;it take quite a few parameters:
;heuristic - a function that transforms a board and state into a numeric value
;		higher values are better
;hist - a list that contains a list for each step down the stack of the form 
;	((previous board state (moves left to try for previous board state) (..))
;board- the current board state
;moves - a list of moves that have not been tried on the current board
;unsolveable - list of boards that the program has found that are unsolveable.
;		This allows it to safely skp many dead ends.
;nodeChecks - number of node checks performed so far
;
;moves are performed tried sequentially in the order they are found in
;Performance can be tweaked by sorting the elemenets in the list so that better
;moves are in the front.  The apply Heuristic function sorts moves by the value
;given to a move by the heuristic function
(define (DFSHelper heuristic hist board moves unsolveable nodeChecks)
  (cond ((null? (cdr board)) ;ie if there is only one peg in the board, a solution
	  (unwindHist (cons (cons board (list moves)) hist) nodeChecks))
	((null? moves)
	  (loadPrev heuristic hist board (add board unsolveable) nodeChecks))

	(#t (let* ((move           (car moves))
	 	   (remainingMoves (cdr moves))
		   (nextBoard      (makeMove move board))
		   (nextNodeChecks (+ 1 nodeChecks)))

	      (if (member nextBoard unsolveable)
		  (DFSHelper heuristic hist board remainingMoves unsolveable nextNodeChecks)

		  (let* ((nextMoves (applyHeuristic heuristic (generateMoveList nextBoard) nextBoard))
			 (nextHist  (cons (cons board (list remainingMoves)) hist)))

		    (DFSHelper heuristic nextHist nextBoard nextMoves unsolveable nextNodeChecks)))))))

;used by DFSHelper in order to go back a move
;Parameter are the same as the DFS function except it does not have the moves
(define (loadPrev heuristic hist board unsolveable nodeChecks)
  (if (null? hist) (cons nodeChecks (list board))
    (let* ((newHist  (cdr hist))
	   (newBoard (caar hist))
	   (newMoves (cadar hist)))
      (DFSHelper heuristic newHist newBoard newMoves unsolveable nodeChecks))))

;unwinds DFSHelper's hist stack after a solution has been found and returns a 
;list of boards from the initial state to the end state
(define (unwindHist hist nodeChecks)
  (define (helper hist accum nodeChecks)
    (if (null? hist)
      (cons nodeChecks accum)
      (helper (cdr hist) (cons (caar hist) accum) nodeChecks)))
  (helper hist '() nodeChecks))

;This function returns all possible solutions in a tree
;
;unlike the other DFS function, this one does not count node checks because it
;WILL be visiting most nodes.
;
;This function does not take a heuristic either.  No point in spending time 
;choosing a good move when you will be brute forcing everything
;besides that it is almost identical to the previous DFS function
(define (DFSAll board)
  (if (null? board) '(())
    (let ((moves (generateMoveList board)))
      (DFSAllHelper '() board moves '() #t '()))))

;the solutions parameter is a tree of solutions built with the solcons function
;the firstVisit parameter is used tell wether there are no solutions from 
;a given board or if 
(define (DFSAllHelper hist board moves solutions firstVisit unsolveable)
  (cond ((null? (cdr board)) ;ie this board has only one peg and is a solution
	 (let ((newSolutionsList (solcons board solutions)))
	       (loadPrevAll hist board moves newSolutionsList unsolveable)))

	((null? moves)
	 (if firstVisit
	   (loadPrevAll hist board moves solutions  (add board unsolveable))
	   (loadPrevAll hist board moves solutions  unsolveable)))

	(#t (let* ((move           (car moves))
		   (remainingMoves (cdr moves))
		   (nextBoard      (makeMove move board)))

	      (if (member nextBoard unsolveable)
		  (DFSAllHelper hist board remainingMoves solutions #f unsolveable)

		   (let* ((nextMoves (generateMoveList nextBoard))
			  (nextHist  (cons (cons board (list remainingMoves)) hist)))
                          (DFSAllHelper nextHist nextBoard nextMoves solutions #t unsolveable)))))))

;inserts solutions into a tree (scons for solution cons)
;there must be a btter way to create this tree.  
;since DFSAll is tail recursive, we cannot generate this tree easily by unwinding
;the recursive calls.  Intead, we have to build a tree from the leafs up.
;We create a forest a solutions are found and merge them as we go up the tree Not a 
;pretty or easy task.
;
;we temporaraliy keep the length or number of pegs of each node at the beginning of each list so
;that we d not need to calculate the length of lists every call.
(define (solcons board solutions)
  (define (createNode len b bchildren bsibs)
    (cons* len (list b bchildren) bsibs))

  (define (getLen branch)
    (if (null? branch) 0 (caar branch)))

  ;returns the next branch in the forest without the length element
  (define (nextBranch forest)
    (cdar forest))

  ;the load Previous Node function for the findall DFS function
  (if (null? solutions)  ;ignore all input that is not a solution if the board is empty
    (if (= 1 (length board))
	(list (cons* 1 (list board) '()))
	solutions)
    (let ((boardLen     (length board))
	  (firstLen     (getLen solutions))
	  (firstBranch  (nextBranch solutions))
	  (restBranches (cdr solutions)))
      (cond ((= boardLen 1)
	      (if (= firstLen boardLen) ;merge branches with the same # of pegs
		  (cons (createNode boardLen board '() firstBranch) restBranches)
		  (cons (createNode boardLen board '() '()        ) solutions)))
	    ((= (- boardLen 1) firstLen) ;make siblings if previous board had one less peg
		(if (= (getLen restBranches) boardLen)
		    (cons (createNode boardLen board firstBranch (nextBranch restBranches)) (cdr restBranches))
		    (cons (createNode boardLen board firstBranch '()                      ) restBranches)));)
	    (#t solutions))))) ;if board is not length 1 or 1 more than the
			       ;previous branches length, ignore it

(define (loadPrevAll hist board moves solutions unsolveable)
  (if (null? hist)
    (cond ((null? solutions) (list(list board)))
	  ((= 1 (length board)) (cdar solutions))
          (#t (cdar (solcons board solutions))))
    (let* ((newHist  (cdr hist))
           (newBoard (caar hist))
           (newMoves (cadar hist))
	   (newSolutions (if (= 1 (length board)) solutions (solcons board solutions))))
      (DFSAllHelper newHist newBoard newMoves newSolutions #f unsolveable))))

;memoization related stuff
;put some limits on the elements in the unsolveable list so that we dont 
;use too much memory
(define (add elmt alist)
  (if (and (< (length elmt) 14)
           (> (length elmt) 3))
     (cons elmt alist)
     alist))

;--Heuristics

;this simply sorts a list of moves according to the heuristic.  Afterwards, 
;the searching algorithm will try each move in the given order.
(define (applyHeuristic heuristicFun moves board)
  (sort moves (lambda (m1 m2) (compareMoves heuristicFun m1 m2 board))))

(define (compareMoves heuristicFun m1 m2 board)
  (> (heuristicFun m1 board)
     (heuristicFun m2 board)))

;so far this is the best heuristic, it tries to avoid moves that move a piece
;to the  corner or a move that movrs a piece from the center
;this weighting seems to work best
(define (heuristicB move board)
  (let ((dest  (list-ref move 2))
        (start (car move)))
    (cond ((= dest  1)  -2)
          ((= dest  11) -2)
          ((= dest  15) -2)
          ((= start  5) -1)
          ((= start  8) -1)
          ((= start  9) -1)
          (#t 0))))

;this is a simple heuristic that give weight to game boards with more pieces in
;pegs 5, 8,  and 9.
;
;The idea behind this is that there are 4 possible moves from these pegs, and 2
;from every other peg.  Hence these pegs should be saved until the end, when we
;are running out of possible moves to make.
(define (heuristic1 move board)
  (define (helper board accum)
    (if (null? board)
      accum
      (let* ((peg  (car board))
             (tail (cdr board))
             (weight (cond ((= peg 4)  2)
                           ((= peg 6)  2)
                           ((= peg 13) 2)
                           (#t         1))))
        (helper tail (+ accum weight)))))
  (helper (makeMove move board) 0))

;the idea behind this heuristic is to try to keep pegs in the middle, just
;under half of all move involve jumping over one of these pegs
(define (heuristic2 move board)
  (define (helper board accum)
    (if (null? board)
      accum
      (let* ((peg  (car board))
             (tail (cdr board))
             (weight (cond ((= peg 5)  2)
                           ((= peg 8)  2)
                           ((= peg 9)  2)
                           (#t         1))))
        (helper tail (+ accum weight)))))
  (helper (makeMove move board) 0))

;this is an attempt to combine several heuristics togethor.
;it attempts to keep pegs in the center and in the positions with 4 moves
;it also tries to keep pegs out of corners
(define (heuristic3 move board)
  (define (helper board accum)
    (if (null? board)
      accum
      (let* ((peg  (car board))
             (tail (cdr board))
             (weight (cond ((= peg 5)  1)
                           ((= peg 8)  1)
                           ((= peg 9)  1)
                           ((= peg 4)  1)
                           ((= peg 6)  1)
                           ((= peg 13) 1)
                           ((= peg 1)  -1)
                           ((= peg 11) -1)
                           ((= peg 15) -1)
                           (#t         0))))
        (helper tail (+ accum weight)))))
  (helper (makeMove move board) 0))

;idea here is to have pieces close togethor, so we increase the heuristic for
;each adjacent peg
(define (heuristic4 move board)
  (define (helper board accum)
    (if (null? board)
        accum
        (let* ((peg   (car board))
               (tail   (cdr board))
               (weight (cond ((= peg 1)  (+ (present? 2 board) (present? 3 board)))
                             ((= peg 2)  (+ (present? 3 board) (present? 4 board) (present? 5 board)))
                             ((= peg 3)  (+ (present? 5 board) (present? 6 board)))
                             ((= peg 4)  (+ (present? 5 board) (present? 7 board) (present? 8 board)))
                             ((= peg 5)  (+ (present? 6 board) (present? 8 board) (present? 9 board)))
                             ((= peg 6)  (+ (present? 9 board) (present? 10 board)))
                             ((= peg 7)  (+ (present? 8 board) (present? 11 board) (present? 12 board)))
                             ((= peg 8)  (+ (present? 9 board) (present? 12 board) (present? 13 board)))
                             ((= peg 9)  (+ (present? 10 board) (present? 13 board)(present? 14 board)))
                             ((= peg 10) (+ (present? 14 board) (present? 15 board)))
                             ((= peg 11) (present? 12 board))
                             ((= peg 12) (present? 13 board))
                             ((= peg 13) (present? 14 board))
                             ((= peg 14) (present? 15 board))
                             ((= peg 15) 0))))
          (helper tail (+ weight accum)))))
        (helper (makeMove move board) 0))

;this heuristic is similiar to heuristic4, except we only count adjacent pegs if
;we can jump over them
(define (heuristic5 move board)
  (define (helper board accum)
    (if (null? board)
        accum
        (let* ((peg   (car board))
               (tail   (cdr board))
               (weight (cond ((= peg 1)  (+ (present? 2 board) (present? 3 board)))
                             ((= peg 2)  (+ (present? 4 board) (present? 5 board)))
                             ((= peg 3)  (+ (present? 5 board) (present? 6 board)))
                             ((= peg 4)  (+ (present? 5 board) (present? 7 board) (present? 8 board)))
                             ((= peg 5)  (+ (present? 8 board) (present? 9 board)))
                             ((= peg 6)  (+ (present? 9 board) (present? 10 board)))
                             ((= peg 7)  (present? 8 board))
                             ((= peg 8)  (present? 9 board))
                             ((= peg 9)  0)
                             ((= peg 10) 0)
                             ((= peg 11) (present? 12 board))
                             ((= peg 12) (present? 13 board))
                             ((= peg 13) (present? 14 board))
                             ((= peg 14) 0)
                             ((= peg 15) 0))))
          (helper tail (+ weight accum)))))
        (helper (makeMove move board) 0))

;a simple rewrite of member that returns 1 fo true and 0 for false
(define (present? peg board)
  (if (member peg board) 1 0))


;--functions related to moving a gamepiece

;performs a given move
(define (makeMove move board)
  (let ((fst  (list-ref move 0))
        (snd  (list-ref move 1))
        (thrd (list-ref move 2)))
    (insert thrd (delete snd (delete fst board)))))

;adds a peg to the board in sorted order
;behavior is undefined if the peg is already there
(define (insert peg board)
  (cond ((null? board) (list peg))
        ((< peg (car board)) (cons peg board))
        (#t (cons (car board) (insert peg (cdr board))))))


;--functions related to the generation of lists of possible moves

;moves are a list containing 3 numbers:
;first:   the peg jumping
;second:  the peg being jumped over
;third:   the final destination of the jumping peg

;this returns all possible moves that can be made for a given board state
(define (generateMoveList board) 
  (filter (lambda (x) (canMove? x board)) (blindMoveList board)))

;this function looks at the pegs one at a time and returns a list of moves that
;could be made.  It blindly looks up moves from a table and does not check
;wether a given move is in fact possible with the given board state
(define (blindMoveList board)
  (define (helper board accum)
    (if (null? board)accum 
      (helper (cdr board) (moves (car board) accum))))
  (helper board '()))

;lookup table.  Give it a peg number and it will return and it will add the 
;moves the piece could theoretically do to a list of moves
(define (moves x list)
  (cond ((= x 1)  (cons* '(1  2  4)  '(1  3  6)  list))
        ((= x 2)  (cons* '(2  4  7)  '(2  5  9)  list))
        ((= x 3)  (cons* '(3  5  8)  '(3  6  10) list))
        ((= x 4)  (cons* '(4  2  1)  '(4  5  6)  '(4  7  11) '(4  8  13) list))
        ((= x 5)  (cons* '(5  8  12) '(5  9  14) list))
        ((= x 6)  (cons* '(6  3  1)  '(6  5  4)  '(6  9  13) '(6  10 15) list))
        ((= x 7)  (cons* '(7  4  2)  '(7  8  9)  list))
        ((= x 8)  (cons* '(8  5  3)  '(8  9  10) list))
        ((= x 9)  (cons* '(9  5  2)  '(9  8  7)  list))
        ((= x 10) (cons* '(10 6  3)  '(10 9  8)  list))
        ((= x 11) (cons* '(11 7  4)  '(11 12 13) list))
        ((= x 12) (cons* '(12 8  5)  '(12 13 14) list))
        ((= x 13) (cons* '(13 8  4)  '(13 9  6)  '(13 12 11) '(13 14 15) list))
        ((= x 14) (cons* '(14 9  5)  '(14 13 12) list))
        ((= x 15) (cons* '(15 10 6)  '(15 14 13) list))))


;checks whether a given move is possible to make on the board returns #t or #f
;ie it checks that a given move is possible
(define (canMove? dest board)
  (let ((jump (list-ref dest 1))
        (fin  (list-ref dest 2)))
    (if (and (member jump board) 
             (not (member fin board)))
        #t
	#f)))

;---printing related functions

;this function will print out the gameboards in forward order
;printFun specifies how to print the board (options are prettPrint or plainPrint)
(define (print printFun boards)
  (if (null?  boards)
      (display "")
      (begin (printFun (car boards))
             (print printFun (cdr boards)))))

;prints a tree returned from the DFSAll function
(define (printTree printFun tree)
  (define (helper printFun tree num)
    (let* ((return (getNextPath tree))
	   (boards  (list-ref return 0))
	   (newTree (list-ref return 1)))
      (if (null? boards)
	(if (> num 1)
	    (begin (display "found ")
	           (display num)
	           (display " solutions\n"))
	    (display ""))
          (begin (print printFun boards)
	         (display "\n")
	         (helper printFun newTree (+ 1 num))))))
  (helper printFun tree 0))

;the only not tail recursive function.  Im getting sick of these trees
(define (getNextPath tree)
  (if (null? tree) '(()())
    (let* ((return       (getNextPath (children tree)))
	   (retVal       (car return))
	   (childrenLeft (cadr return))
	   (thisValue    (caar tree))
	   (prunedTree   (if (null? childrenLeft)
		             (siblings tree) ;if no children, return the siblings and not the current node
		             (cons (cons thisValue (list childrenLeft)) (siblings tree)))))
      (list (cons thisValue retVal) prunedTree))))

;Tree stuff
(define (siblings tree)
    (cdr tree))

(define (children tree)
  (if (or (null? tree) (null? (cdar tree))) '()
    (cadar tree)))

;a recursive print function that generates pretty triangular representations
;of the game board
(define (prettyPrint board)
  (define (helper x board)
    (let* ((head (if (null? board) '() (car board)))
           (tail (if (null? head)  '() (cdr board))))

      (begin (cond ((= 1  x) (display "        "))
                   ((= 2  x) (display "      "))
                   ((= 4  x) (display "    "))
                   ((= 7  x) (display "  "))
                   ((< x 10) (display "   "))
                   ((= x 10) (display "   "))
                   ((and (> x 10) 
                         (not (= x 11))) (display "  ")))

             (if (equal? head x) (display x)
                                 (if (< x 10) (display "*")
                                              (display "* ")))

             (cond ((= 1 x) (display "\n"))
                            ((= 3 x)  (display "\n"))
                            ((= 6 x)  (display "\n"))
                            ((= 10 x) (display "\n"))
                            ((= 15 x) (display "\n"))
                            (#t       '()))

             (if (= x 15) (display "\n")
                          (if (equal? x head)
                              (helper (+ x 1) tail)
                              (helper (+ x 1) board))))))
  (helper 1 board))

;a basic print function that uses less space to output boards
(define (plainPrint board)
  (begin (display board)
         (display "\n")))

;--test funcions

;randomly creates a gameboard with a given number of pegs
;board is not guaranteed to have a solution
(define (randomBoard pegs)
  (define (helper board pegs)
    (if (= (length board) pegs)
        board
        (let ((try (+ 1 (random 15))))
          (if (not (member try board))
              (helper (insert try board) pegs)
              (helper board pegs)))))
  (helper '() pegs))

;side-by-side comparison of all heuristics when used on a given game board
(define (compareHeuristics board)
  (begin 
    (display "1,")
    (display (testOne heuristic1 board))
    (display "\n")

    (display "2,")
    (display (testOne heuristic2 board))
    (display "\n")

    (display "3,")
    (display (testOne heuristic3 board))
    (display "\n")

    (display "4,")
    (display (testOne heuristic4 board))
    (display "\n")

    (display "5,")
    (display (testOne heuristic5 board))
    (display "\n")

    (display "B,")
    (display (testOne heuristicB board))
    (display "\n"))

;placeholder for more heuristics
;    (display "x,")
;    (display (testOne heuristicX board))
;    (display "\n")

;no heuristic
    (display "0,")
    (display (testOne (lambda (x y) 1) board))
    (display "\n"))

;solves a board and returns the number of node checks required
(define (testOne heuristic board)
  (car (DFS heuristic board)))
        

(define impossible  '(1 2 3   5 6 7 8 9 10 11 12 13 14  ))
(define impossible2 '(  2 3 4 5 6   8 9 10 11 12 13 14 15))

;why is there no fold equivalant implemented in this language?
(define (fold fun init alist)
  (if (null? alist) init
  (fold fun (fun (car alist) init) (cdr alist))))

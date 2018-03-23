#| 8puzzle.lsp
	Authors: Luke Videckis & Katherine MacMillan
	Due: March 11, 2018
	Class: CSC 447 - Introduction to Artificial Intelligence
	Professor: Dr. John Weiss

	Description:
		This program takes a tile puzzle start state and attempts to solve
		the puzzle with two exhaustive search methods and four A* search
		methods using four different heuristic evaluation functions. 

		For the A* search, two of the evaluation functions are considered
		admissible heuristics, and two are considered inadmissible heuristics.

		The program will keep track of the number of nodes generated, the
		number of duplicate nodes generated, and the number of nodes expanded
		for each search implementation, and these values will be printed out
		for the user when the final puzzle state trace is printed.

		If the search expands more than 100,000,000 nodes, the search will
		abort and return NIL, in an effort to not continue searching for
		extreme amounts of time when encountering very difficult puzzles.

		The program can accept puzzle states by reading in a puzzle file, by
		taking a list of numbers to represent a puzzle state or by prompting
		the user for a 3x3 puzzle state.

		The program can accept puzzles of varying dimensions when input as a
		list or from a file, but the puzzles must be square nxn in dimension.
		Any additional puzzle tile entries beyond the square dimension will be
		removed from the puzzle state list, and the program will continue with
		the newly squared puzzle.
	
	Usages: 
		clisp 8puzzle.lsp <filename>

		clisp
		> (8puzzle)

		clisp
		> (8puzzle <filename>)

		clisp
		> (8puzzle <puzzle state>)

	Examples:
		clisp 8puzzle.lsp medium.puz

		clisp
		> (8puzzle)

		clisp
		> (8puzzle "medium.puz")

		clisp
		> (8puzzle '(1 2 3 0 4 5 6 7 8))
|#


; structure for nodes entries
(defstruct node state parent (children (list '())) f_prime h_prime g)

; default goal state we're trying to reach
(defparameter *goal_state* '(1 2 3 8 0 4 7 6 5))

; puzzle dimension for various calculations
(defparameter *dimension* 0)

; counters
(defparameter *nodes_generated* 0)
(defparameter *duplicate_nodes* 0)
(defparameter *nodes_expanded* 0)

; track states we've seen alread
(defparameter *existing_states* '())

; import other files with needed functions
(load "exhaustiveSearch.lsp")
(load "aStar.lsp")
(load "solvable.lsp")
(load "io.lsp")
(load "heuristicFunctions.lsp")

#|8puzzle
	This is the main function of the program, and calls the multiple
	search functions to solve a given puzzle.

	First the input parameter is evaluated. If it is blank the user is
	prompted for an initial puzzle configuration, if it is a list the list
	is used directly as the initial puzzle configuration, otherwise the
	parameter is used as a file location, and will try to open and read in
	a puzzle configuration from the file.

	The initial configuration is used to create a starting node which is
	then sent into the solve function, which will solve the puzzle using
	multiple methods.

	input:
		input - an optional parameter which can be a filename to be
			read in, a list of digits to represent a starting puzzle
			configuration, or it may be left empty
|#
(defun 8puzzle (&optional input)
	; if input is empty prompt user for start_node
	(cond 
		((not input)
			; get start state from the user
			(setf start_state (readPuzzleFromUser))
		)
		((listp input)
			; if input is a list, start_state is the input
			(setf start_state input)
		)
		(t
			; else read in puzzle file
			(setf start_state (readPuzzleFromFile input))
			; start_state will be nil if file failed to open
			(if (not start_state)
				(exit)
			)
		)
	)

	(setf start_state (checkSize start_state))
	; ensure initial puzzle is solvable
	(when (not (solvable start_state))
		(format t "Initial puzzle is not solvable~%")
		(exit)
	)
	
	; create the starting node
    (setf h (countOutOfOrder start_state))
	(setf start_node (make-node :state start_state :g 0 :h_prime h :f_prime h))

	; run solve algorithms
    (solve start_node)

)

#|checkSize
	This function takes in a start puzzle state and calculates the dimension
	of the puzzle. This is done by taking the square root of the puzzle list
	length and rounding down. If this dimension is larger than 3, then the goal
	state is reset to an appropriate goal state for the puzzle size.

	Next the start state list is checked to see if it's length exceeds the
	dimension squared. If it does, it means the puzzle is not a square. In this
	case any list entries beyond the square of the dimension are cut from the
	list. The resulting list will be the puzzle start state.

	input:
		start_state - a list of the initial puzzle configuration to start from
	output:
		a list of the puzzle configurations clipped to a square
|#
(defun checkSize (start_state)
	; get flood or start state square root as dimension
    (setf *dimension* (floor (sqrt (length start_state))))

  	; check if we're not useing a default puzzle size
	(when (> *dimension* 3)
		(resetGoalState)
	)
  	; if start state is not square, remove excess elements to make it square
    (if (/= (* *dimension* *dimension*) (length start_state))
    	; get slice from 0 to (dim*dim)-1 of start_state
    	(setf start_state (slice start_state 0 (- (* *dimension* *dimension*) 1)))
    )
    start_state
)

#|resetGoalState
	This function resets the *goal_state* based on the *dimension* value of the
	start puzzle, making it a spiral, starting in the upper left corner and 
	placing consecutive values in a clockwise diretion with the blank space being
	in the center.
|#
(defun resetGoalState ()

	; resize the goal state
	(setf *goal_state* (make-list (* *dimension* *dimension*)))

	; initialize the first row of the goal state
	(loop for digit from 1 to *dimension* do 
		(setf (nth (- digit 1) *goal_state*) digit)
	)

	; set initial 2D coordinates
	; we'll convert to 1D coordinates by i*dim+j
	(setf i 0)
	(setf j (- *dimension* 1))

	; direction we move, format: (dirI, dirJ)
	; (1,0) is down
	; (-1,0) is up
	; (0,1) is right
	; (0,-1) is left
	; we add dirI to i, and dirJ to j to move (i,j) to the next position
	(setf dirI 1)
	(setf dirJ 0)

	; sideLength is the number of times we walk along the direction
	(setf sideLength (- *dimension* 1))
	; we set the position (i,j) to digit at each step
	(setf digit *dimension*)

	(loop while (> sideLength 0) do

		; walk down or up, setting digits
		(loop for counter from 1 to sideLength do
			(setf (nth (+ (* *dimension* i) j) *goal_state*) digit)
			(setf digit (+ digit 1))
			(setf i (+ i dirI))
			(setf j (+ j dirJ))
		)
		; update direction
		(cond
			((equal dirI 1) (setf dirI 0) (setf dirJ -1))
			((equal dirI -1) (setf dirI 0) (setf dirJ 1))
		)
		; walk left or right, setting digits
		(loop for counter from 1 to sideLength do
			(setf (nth (+ (* *dimension* i) j) *goal_state*) digit)
			(setf digit (+ digit 1))
			(setf i (+ i dirI))
			(setf j (+ j dirJ))
		)
		; update direction
		(cond
			((equal dirJ 1) (setf dirI 1) (setf dirJ 0))
			((equal dirJ -1) (setf dirI -1) (setf dirJ 0))
		)
		; after each pair of 2 walks, we now walk 1 less time in both directions
		(setf sideLength (- sideLength 1))
	)
	; set the empty space to 0
	(setf (nth (+ (* *dimension* i) j) *goal_state*) 0)
)

#|generateSuccessors
	This function takes in a node and examines its state. For each valid
	movement direction of the state's blank space (0), a child node will
	be generated.

	input:
		node - the node having children generated for it
	output:
		a list of child nodes for the input node
|#
(defun generateSuccessors (node)
	(let (successors '())
		; find 0 position, row and col
		(setf pos (position 0 (node-state node) :test #'equal))
		(setf row (floor pos *dimension*))
		(setf col (mod pos *dimension*))
		
		; list to accumulate successors
		(setf successors '())

		; can move north?
		(when (> row 0)
			; push moved north child to list of successors
			(setf child (getChild pos node (- pos *dimension*)))
			(if child
				(setf successors (push child successors))
			)
		)

		;can move east?
		(when (< col (- *dimension* 1))
			; push moved east child to list of successors
			(setf child (getChild pos node (+ pos 1)))
			(if child
				(setf successors (push child successors))
			)
		)

		;can move south?
		(when (< row (- *dimension* 1))
			; push moved south child to list of successors
			(setf child (getChild pos node (+ pos *dimension*)))
			(if child
				(setf successors (push child successors))
			)
		)

		;can move west?
		(when (> col 0)
			; push moved west child to list of successors
			(setf child (getChild pos node (- pos 1)))
			(if child
				(setf successors (push child successors))
			)
		)
		(copy-list successors)
	)
)

#|getChild
	This function takes in a parent node, the position of the blank
	within the parent node's state, and the new position the blank will
	be moving to.

	The value in the new (offset) position is copied over into the position
	of the blank, and the blank space is set to the offset position.

	If the newly genergated state has been seen before, the duplicate node
	count is generated, otherwise the new state is added to a list which
	tracks the states that have been see before.

	This new state is used, along with the parent node, to create a new node,
	which is returned.

	input:
		pos - the 1-D position of the blank (0) in the puzzle state
		parent - the parent node for the new child
		offset - the new position the blank will be moving to
	output:
		a new node with the passed in parent set as parent, and with
		the positions at position and offset swapped in the new node's
		state from the parent state
|#
(defun getChild (pos parent offset)
	(setf puz_list (copy-list (node-state parent)))
	; set position of 0 to element in offset
	(setf (nth pos puz_list) (nth offset puz_list))
	; set offset position to 0
	(setf (nth offset puz_list) 0)

	; increment number of generated nodes
	(setf *nodes_generated* (+ *nodes_generated* 1))

	; check if this state has been seen before and return child node
	(if (not (position puz_list *existing_states* :test #'equal))
		; no - return node with current parent
		(progn
			(setf *existing_states* (push puz_list *existing_states*))
			(return-from getChild (make-node :state puz_list :parent (copy-node parent) :g (+ (node-g parent) 1)))
		)
	)
	NIL
)


#|isGoalState
	This function takes in a node and copares its state to the goal state.
	If they match true is returned, otherwise NIL is returned.

	input:
		node - the node to check for a goal state
	output:
		T - the passed in node's state matches the goal state
		NIL - the passed in node's state does not match the goal state
|#
(defun isGoalState (node)
	(if (equal (node-state node) *goal_state*)
		T
		NIL
	)
)

#|solve
	This function takes in a starting node which is then sent into the A*
	search 4 times with 4 different evaluation functions. Then the starting
	node is sent into the iterated depth DFS search, then the BFS search.

	Each time, upon returning from a given search function, either a goal
	node or NIL will be returned. If NIL is returned, an output message
	stating no goal state could be found is printed, otherwise the goal
	node is sent to a function which will print a trace of all of the 
	steps taken to solve the initial puzzle configuration.

	input:
		start_node
|#
(defun solve (start_node)

    ; run A* search on start node using euclidean distance heuristic function
 	(setf goal_node (aStar (copy-node start_node) #'euclideanSqr))
    (if goal_node
		(printTrace goal_node "A* graph search - Squared Euclidean Distance Heuristic")
		(format t "Goal state not found, expanded nodes exceeded limit: ~d~%" *nodes_expanded*)
	)

	; run A* search on start node using out of order heuristic function
 	(setf goal_node (aStar (copy-node start_node) #'countOutOfOrder))
    (if goal_node
		(printTrace goal_node "A* graph search - Count Out Of Order Heuristic")
		(format t "Goal state not found, expanded nodes exceeded limit: ~d~%" *nodes_expanded*)
	)

    ; run A* search on start node using manhattan distance heuristic function
 	(setf goal_node (aStar (copy-node start_node) #'manhattan))
    (if goal_node
		(printTrace goal_node "A* graph search - Manhattan Distance Heuristic")
		(format t "Goal state not found, expanded nodes exceeded limit: ~d~%" *nodes_expanded*)
	)


    ; run A* search on start node using vector distance heuristic function
 	(setf goal_node (aStar (copy-node start_node) #'vectorDistSqr))
    (if goal_node
		(printTrace goal_node "A* graph search - Squared 1-D Vector Distance Heuristic")
		(format t "Goal state not found, expanded nodes exceeded limit: ~d~%" *nodes_expanded*)
	)
	; run iterated depth depth first serch on start node
	(setf goal_node (IDDFS (copy-node start_node)))
	(if goal_node
		(printTrace goal_node "DFID graph search")
		(format t "Goal state not found, expanded nodes exceeded limit: ~d~%" *nodes_expanded*)
	)

	; run bredth first search on start node
	(setf goal_node (BFS (copy-node start_node)))
	(if goal_node
		(printTrace goal_node "BFS graph search")
		(format t "Goal state not found, expanded nodes exceeded limit: ~d~%" *nodes_expanded*)
	)
)

#|main
	This function is the entry point if the program is executed from
	the command line and not the repl. It checks to make sure at least
	one commandline argument has been entered. If so, the 8puzzle
	function is called and and solving beings, otherwise a useage
	statement is printed, and the program exits.

	input:
		args - the commandline arguments.
	output:
		none
|#
(defun main (args)
	(if args
		(8puzzle args)
		(format t "No puzzle file entered~%Usage: 8Puzzle.lsp filename~%")
	)
)

#|
	Testing functions used
|#
; (8Puzzle)
;(8Puzzle '(1 3 4 8 6 2 7 0 5))
;(main "medium.puz")
; (main "hard.puz")
; (main "tough.puz")
(main "worst.puz")

; (8Puzzle "easy.puz")
; (8Puzzle "unsolvable.puz")
; (8Puzzle "duplicate.puz")

; (8Puzzle "4x4-universeTest.puz")

; (8Puzzle "4x4.puz")
; (8Puzzle "4x4-unsolvable.puz")
; (8Puzzle "4x4-duplicate.puz")

; (8Puzzle "3x4.puz")
; (8Puzzle "3x4-unsolvable.puz")

; (8Puzzle "4x3.puz")
; (8Puzzle "4x3-unsolvable.puz")

; (8Puzzle "4x5.puz")
; (8Puzzle "4x5-unsolvable.puz")

; (8Puzzle "5x4.puz")
; (8Puzzle "5x4-unsolvable.puz")

; (main *args*)

#| io.lsp
	This file contains the functions used to read in puzzles and to print
	puzzle solving results out to the user, as well as a list slicing
	helper function.
|#


#|readPuzzleFromUser
	This function prompts a user for a 9 tile puzzle configuration,
	and then reads in 9 numbers from the user.
	
	output:
		a list of 9 numbers representing a puzzle configuration
|#
(defun readPuzzleFromUser ()
	; prompt user
	(format t "Enter the board as 9 digits (0-8), ex:~%1 2 3~%4 5 6~%7 8 0~%~%")
	; create blank puzzle list
	(setf puzzle '())

	; read in 9 numbers from user and add to puzzle
	(loop for i from 1 to 9 do
		(setf digit (read))
		(setf puzzle (append puzzle (list digit)))
	)
	; clear nil from puzzle and return puzzle
    (setf puzzle (remove NIL puzzle))
)

#|readPuzzleFromUser
	This function takes in a file name an attempts to open it. If the file
	does not open, an error message is displayed and NIL will be returned.

	If the file opens, the contents are read into a list until which is
	returned to the calling function

	input:
		filename - the path to the file to be opened
	output:
		a list of numbers representing a puzzle configuration
		NIL - the file failed to open
|#
(defun readPuzzleFromFile (filename)
    ; try to open file  returning NIL on error
    (setf fin (open filename :if-does-not-exist nil))
    (when (null fin) (return-from readPuzzle (format nil "Error: cannot open file ~a~%" filename)))

    ; initial empty puzzle for read in
	(setf puzzle '())

    ; read file line by line, returning NIL at EOF
    (do ((data (read fin nil) (read fin nil)))
        
        ; exit when file is read
        ((null data) (close fin))
        (setf puzzle (append puzzle (list data)))
    )

    ; remove NIL from puzzle and return
    (setf puzzle (remove NIL puzzle))
)


#|printTrace
	This function takes in an end node, as well as a search method string,
	and displays solution trace data to the user.

	First the trace is obtained from the goal node by creating a state trace
	list. This is done by storing the state of the end node, and then walking
	through all of its ancestors, adding their states to the state trace list
	as well until state trace contains the state progression from start state
	to goal state.

	Next the function will display the search method used and how many moves
	it took to go from the start state to goal state. Then the number of
	generated, distinct and expanded nodes are displayed.

	Finally, the states in the state trace list are sent in at most groups of
	four to a function which will print the puzzles in a horizontal row.

	input:
		end_node - the final node containing the goal state
		search_method - a string containing the search method used to find
			the end node
|#
(defun printTrace (end_node search_method)

	(setf curr_node end_node)
	; initialize state trace list
	(setf state_trace '())

	(loop while (node-parent curr_node) do
		(setf state_trace (push (copy-list (node-state curr_node)) state_trace))
		(setf curr_node (node-parent curr_node))
	)
	; push final parent state onto list
	(setf state_trace (push (copy-list (node-state curr_node)) state_trace))

	(format t "~D~%----------------~%Solution found in ~D moves~%" 
		search_method 
		(- (length state_trace) 1)
	)
	(format t "~D nodes generated (~D distinct nodes), ~D nodes expanded~%" 
		*nodes_generated*
		(- *nodes_generated* *duplicate_nodes*)
		*nodes_expanded*
	)
	(loop while (> (length state_trace) 0) do
		(cond
			; check if state trace list has 4 or more states
			((> (length state_trace) 3)
				; send 4 puzzle states to puzzle print function
				(printPuzzles (slice state_trace 0 3))
				; remove first 4 elements from state trace list
				(setf state_trace (slice state_trace 4 (- (length state_trace) 1)))	
			)
			; state trace list has fewer than 4 elements
			( T
				; send remaining puzzle states to print function
				(printPuzzles (slice state_trace 0 (- (length state_trace) 1)))
				; clear state trace list
				(setf state_trace (slice state_trace -1 -1))
			)
		)
	)
)

#|printPuzzle
	This function takes in a list of at most 4 puzzle states and prints them
	in a pretty fashion.

	input:
		print_list - a list of at most 4 puzzle states to be printed
|#
(defun printPuzzles (print_list)
	; print a row for each dimension of the puzzle
	(loop for row from 0 to (- *dimension* 1) do
		; find starting position for the current row
		(setf pos (* *dimension* row))

		; print the ith row for each puzzle in the print list
		(loop for puz from 0 to (- (length print_list) 1) do
			; grab the nth puzzle in the print list
			(setf state (nth puz print_list))
			; print each tile in the row of the puzzle
			(loop for tile from 0 to (- *dimension* 1) do
				; print blank instead of 0
				(if (= 0 (nth (+ pos tile) state))
					(if (> *dimension* 3)
						; print extra space if dimension > 3
						(format t "   ")
						(format t "  ")
					)
					; print extra space if digit < 10 and dimension > 3
					(if (and (> *dimension* 3) (< (nth (+ pos tile) state) 10))
						(format t " ~d " (nth (+ pos tile) state))
						(format t "~d " (nth (+ pos tile) state))
					)
				)
			)

			; print space between puzzles, exept after last one
			(if (< puz (- (length print_list) 1))
				(if (= row 1)
					; print arrow between second rows
					(format t "  -->  ")
					; print space between rows of different puzzles
					(format t "       ")
				)
			)
		)
		; new line between rows
		(format t "~%")	
	)
	; extra new line to separate groups of printed puzzles
	(format t "~%")			
)

; grab a slice of a list
#|printPuzzle
	This function takes in a list, a start and an end position and returns
	a slice of the list, including both the start and end positions.

	If the start or end values are out of bounds, a NIL is returned instead.

	input:
		original_list - a list to be sliced
		start - the index of first element from the list to be included
		end - the index of the last element from the list to be included
	output:
		a list containing a subset of the original list elements
		NIL - the start or end index was out of bounds
|#
(defun slice (original_list start end)
	(setf new_list '())

	; make sure stare and end are in bounds of list, and start <= end (else return NIL)
	(when (and (<= start end) (> start -1) (< end (length original_list))
		(< start (length original_list)) (> end -1))
		; push elements into new list including start and end element
		(loop for i from start to end do
			(setf new_list (push (nth i original_list) new_list))
		)
		; push adds to the front of the list, so we need to reverse to maintain order
		(setf new_list (reverse new_list))

		(return-from slice new_list)
	)
	NIL
)

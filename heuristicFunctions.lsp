#| heuristicFunctions.lsp
	This file contains the various heuristic functions used to 
	evaluate game states in the A* search method.
|#

#| manhattan - Admissable Heuristic
	This function returns the sum of manhattan distances from each
	digit's location in the current state to the position for that
	digit in the goal state.

	input:
		state - the puzzle configuration state being evaluated
	output:
		evaluation score
|#
(defun manhattan (state)
	; initiate counter
	(setf counter 0)

	; loop for each digit including empty tile
	(loop for digit from 0 to (- (length state) 1) do
		; find the row/col of the digit in the goal state
		(setf index (position digit *goal_state*))
		(setf goal_row (floor (/ index *dimension*)))
		(setf goal_col (mod index *dimension*))

		; find the row/col of the digit in the current state
		(setf index (position digit *goal_state*))
		(setf state_row (floor (/ index *dimension*)))
		(setf state_col (mod index *dimension*))

		; add the manhattan distance between the 2 locations to the counter
		(setf counter (+ counter (abs (- state_row goal_row))))
		(setf counter (+ counter (abs (- state_col goal_col))))
	)
	; return counter
	counter
)

#| euclideanSqr - Inadmissable Heuristic
	This function returns the sum of squared euclidean distances from 
	from each digit's location in the current state to the position
	for that digit in the goal state.

	input:
		state - the puzzle configuration state being evaluated
	output:
		evaluation score
|#
(defun euclideanSqr (state)
	; initiate counter
	(setf counter 0)

	; loop for each digit including empty tile
	(loop for digit from 0 to (- (length state) 1) do
		; find the row/col of the digit in the goal state
		(setf index (position digit *goal_state*))
		(setf goal_row (floor (/ index *dimension*)))
		(setf goal_col (mod index *dimension*))

		; find the row/col of the digit in the current state
		(setf index (position digit state))
		(setf state_row (floor (/ index *dimension*)))
		(setf state_col (mod index *dimension*))
		; add the square of the euclidean distance between
		; the 2 locations to the count
		(setf counter (+ counter (* (- state_row goal_row) (- state_row goal_row))))
		(setf counter (+ counter (* (- state_col goal_col) (- state_col goal_col))))
	)
	; return counter
	counter
)

#| countOutOfOrder - Admissable Heuristic
	This function returns the number of out of place tiles within the
	puzzle state.

	input:
		state - the puzzle configuration state being evaluated
	output:
		evaluation score
|#
(defun countOutOfOrder (state)
	; initiate counter
	(setf counter 0)

	; count how many positions are off in the node's state
	(loop for i from 0 to (- (length state) 1) do
		(if (/= (nth i state ) (nth i *goal_state*))
			(setf counter (+ counter 1))
		)
	)
	; return counter
	counter
)

#| vectorDistSqr - Inadmissable Heuristic
	This function sums the squared 1-D vector distance each digit is in
	the current state from its position in the goal state.

	input:
		state - the puzzle configuration state being evaluated
	output:
		evaluation score
|#
(defun vectorDistSqr (state)
	; initiate counter
	(setf counter 0)

	; loop for each digit including empty tile
	(loop for digit from 0 to (- (length state) 1) do
		; find the 1-D position of the digit in the goal state
		(setf goal_pos (position digit *goal_state*))

		; find the 1-D position of the digit in the current state
		(setf pos (position digit state))

		; get vector position distance from goal to current position
		(setf distance (- goal_pos pos))

		; add squared distance to counter
		(setf counter (+ counter (* distance distance)))
	)
	; return counter
	counter
)

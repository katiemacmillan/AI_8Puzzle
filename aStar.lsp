#| aStar.lsp
	This file contains the functions used in executing the A* search method.
|#

(defparameter *open_list* (list '()))
(defparameter *closed_list* (list '()))

#|aStar
	This function takes in a start node and an evaluation function and
	performs the A* search of the game tree derived from the start node.

	The next node to be expanded is based on the evaluation function passed
	into the aStar function. This is used to evaluate the H' value of each
	generated child.

	When a child is found with a state matching the goal state, that child node
	is returned to the calling function. If no such node is found, then NIL is
	returned to the calling function.

	input:
		start_node - a node containing an initial puzzle start state
		evaluation - a heuristic static evaluation function to evaluate the 
			"fitness" of generated successor nodes
	output:
		a node with a state matching the goal state
		NIL - No successor node was found with a state matching the goal state
|#
(defun aStar (start_node evaluation)
	; reset global tracking variables
	(setf *open_list* '())
	(setf *closed_list* '())
	(setf *nodes_expanded* 0)
	(setf *nodes_generated* 0)
	(setf *duplicate_nodes* 0)
	(setf *existing_states* '())
	(setf *existing_states* (remove nil *existing_states*))
	(setf *existing_nodes* '())
	(setf *existing_nodes* (remove nil *existing_nodes*))

	; evaluate first node
	(setf (node-h_prime start_node) (funcall evaluation (node-state start_node)))
	; add first node to open list
	(setf *open_list* (push start_node *open_list*))

	; repeat until open list is empty or true is returned
	(loop while (car *open_list*) do
		; get best node from open list
		(setf curr_node (getBest))

		; add current node to closed list
		(setf *closed_list* (push curr_node *closed_list*))
		; check if current node is goal
		(if (isGoalState curr_node)
			(return-from aStar curr_node)
		)

		; generate children for current node
		(setf (node-children curr_node) (generateSuccessors curr_node))
		(setf *nodes_expanded* (+ *nodes_expanded* 1))
		(if (> *nodes_expanded* 100000000)
			(return-from aStar NIL)
		)

		(loop for child in (node-children curr_node) do
			; evaluate child and set F'
			(setf (node-h_prime child) (funcall evaluation (node-state child)))
			(setf (node-f_prime child) (+ (node-g child) (node-h_prime child)))
			(cond 
				; if child is on open, update current node
				((onList child *open_list*))
				; if child is on closed, update current node and do... something...
				((setf on_list_child (onList child *closed_list*))
					; remove node from closed list and add back to open list
					(setf *closed_list* (remove on_list_child *closed_list*))
					(setf *open_list* (push on_list_child *open_list*))
				)
				; if child is not on open or closed, evaluate and add to open
				(t
					(setf *open_list* (push child *open_list*))
				)
			)
		)
	)
	NIL
)

#|onList
	This function checks if a node's state exists in a given list. Each
	element in the passed in state list has its state compared to the state
	of the child node sent in.

	When a list element is found with a state matching the state of the child
	node, the F' value of that element and the child node are compared. If the
	child has a smaller F' value than the element, the element in the list is
	updated to have the child's G and F' values as the child, and the element's
	parent is set to the child's parent as well, since the route to the
	element's state is better via the child's parent than via the element's
	original parent.

	Regardless of whether or not the matching element node was altered, it
	is returned to the calling function to indicate that the state exits
	on the open list.

	If the child's state is not found on the state list, NIL is returned.

	input:
		child - the child whose state is being looked for in the list
		state_list - a list of nodes, either the open or closed list
	output:
		a node existing on the passed in state list with the same state
			as child
		NIL - the state in the child node does not exist in any nodes in the
			passed in state list
|#
(defun onList (child state_list)
	; check if child state exists on the list
	(loop for node in state_list do
		(when (equal (node-state node) (node-state child))
			(when (< (node-f_prime node) (node-f_prime child))
				; update node's F' and G values
				(setf (node-g node) (node-g child))
				(setf (node-f_prime node) (node-f_prime child))
				; update node's parent
				(setf (node-parent node) (node-parent child))
				; clear children
				(setf (node-children node) '())
			)
			(return-from onList node)
		)
	)
	NIL
)

#|getBest
	This function retrieves the best node from the open list by first sorting
	the open list if there is more than one value in the list. Once sorted
	the first element in the list has the lowest F' value and is therefore
	considered to be the "best". This element is removed from the open list
	and returned to the calling function.

	output:
		the node in the open list with the lowest F' value
|#
(defun getBest ()
	; check if there is more than one element in the list
	(if (not (equal '(NIL) (cdr *open_list*)))
		; sort open list in ascending order
		(sort *open_list* #'sortTest)
	)
	; grab the first (best) node
	(setf node (car *open_list*))
	; remove first node from list
	(setf *open_list* (cdr *open_list*))
	; return first node
	node
)

#|sortTest
	This function is used in the lisp sort function to compare two elements of
	a list. This is done by comparing the F' value of the a and b nodes in 
	order to sort them in ascending order.

	input:
		a - the first node for comparison
		b - the second node for comparison
	output:
		T or NIL if a and b need to be swapped
|#
(defun sortTest (a b)
	(cond 
	  	((null a) (not (null b)))
	    ((null b) nil)
	    (t (> (node-f_prime b) (node-f_prime a)))
	)
)
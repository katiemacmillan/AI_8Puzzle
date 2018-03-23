#| exhaustiveSearch.lsp
	This file contains the functions used in executing the exhaustive search
	methods of Breadth First Search, Depth First Search and Iterated Depth
	Depth First Serach.
|#

#|IDDFS
	This function iterates increasingly on depths, and calls
	a depth-bounded DFS. Once a goal state is found, we return this
	goal node for output. If a goal is not found with < 100 depth, then
	we return NIL signifying no goal is found.
	
	input:
		start_node - a node representing the position to start from
	output:
		a new node whose state is equal to the goal state, if found.
		NIL otherwise.
|#
(defun IDDFS (start_node)
	(setf *nodes_expanded* 0)
	(setf *nodes_generated* 0)
	(setf *duplicate_nodes* 0)
	(setf depth 0)
	; loop for increasing depths
	(loop while (< *nodes_expanded* 100000000) do
		; clear visited states between DFS's
		(setf *existing_states* '())
		(setf *existing_states* (remove nil *existing_states*))


		(setf goal_node (DFS start_node depth))
		(if goal_node
			; return node with goal state, found goal
			(return-from IDDFS goal_node)
		)
		(setf depth (+ depth 1))
	)
	; return NIL signifying goal was not found
	NIL
)

#|BFS
	This function runs a bounded Depth-First-Search. This is just a
	regular DFS, but if a certain depth is reached, we stop searching.
	
	input:
		start_node - a node representing the position to start from.
		depth - an integer representing how far to search before returning.
	output:
		a new node whose state is equal to the goal state, if found.
		NIL otherwise.
|#
(defun DFS (start_node depth)
	(if (isGoalState start_node)
		; return node, found goal
		(return-from DFS start_node)
	)
	
	(when (> depth 0)
		; generate list of successors
		(setf (node-children start_node) (generateSuccessors start_node))
	
		; update number of nodes expanded
		(setf *nodes_expanded* (+ *nodes_expanded* (length (node-children start_node))))
		(if (> *nodes_expanded* 100000000)
			(return-from DFS NIL)
		)
		; for each successor do:
		(loop for nextNode in (node-children start_node) do

				(setf goal_node (DFS nextNode (- depth 1)))
				(if goal_node
					; return true, found goal
					(return-from DFS goal_node)
				)
		)
	)
	; return NIL, goal state not found
	NIL
)


#|BFS
	This function runs Breadth-First-Search from the start node.

	input:
		start_node - a node representing the position to start from
	output:
		a new node whose state is equal to the goal state, if found.
		NIL otherwise.
|#
(defun BFS (start_node)

	; reset parameters and visited list
	(setf *nodes_expanded* 0)
	(setf *nodes_generated* 0)
	(setf *duplicate_nodes* 0)
	(setf existing_states '())
	(setf existing_states (remove nil existing_states))
	(setf *existing_states* '())
	(setf *existing_states* (remove nil *existing_states*))
	(setf *existing_nodes* '())
	(setf *existing_nodes* (remove nil *existing_nodes*))

	; initialize queue
	(setf queue '())
	(setf queue (remove nil queue))

	; push start node into queue+visited
	(setf existing_states (push (node-state start_node) existing_states))
	(setf queue (nconc queue (list start_node)))


	; insert node onto queue
	;(setf queue (append queue '(start_node)))

	; while queue is not empty
	(loop while (> (length queue) 0) do

		;get node at front of the queue
		(setf curr_node (car queue))

		; pop top state off front of queue
		(setf queue (cdr queue))

		; check if current board equals the goal board, and return if equal
		(when (isGoalState curr_node)
			(return-from BFS curr_node)
		)

		; generate list of successors
		(setf (node-children curr_node) (generateSuccessors curr_node))

		; increment number of expanded nodes
		(setf *nodes_expanded* (+ 1 *nodes_expanded*))
		(if (> *nodes_expanded* 100000000)
			(return-from BFS NIL)
		)

		
		; add all successors to open list which havent been visited
		(loop for nextNode in (node-children curr_node) do
			; push next node onto queue+visited
			(setf existing_states (push (node-state nextNode) existing_states))
			(setf queue (nconc queue (list nextNode)))
		)
	)
	; return false, didn't found goal state
	NIL
)
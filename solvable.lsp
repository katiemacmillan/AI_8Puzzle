#|
        ***** SOLVABLE.LSP *****

The SOLVABLE function returns T if a given 8-puzzle position is solvable, NIL otherwise.

Usage:    (solvable L)
          where L is a 9-element list such as (1 2 3 8 0 4 7 6 5)

Reference:  "Mathematical Games and Pastimes", p.79-85,
             A.P.Domoryad, Macmillan, 1964.

Author: John M. Weiss, PhD
Class:  CSC447/547 Artificial Intelligence
Date:   Spring 2018

Modifications:
    Date: 3/7/18
    Changes: Added an nXnSolvable function to check for solvability of puzzle states
    larger than 3x3. Added an if statement to solvable which will call this function
    instead and return its value if the puzzle dimension is larger than 3x3.
|#

; global *flag* for solvability
(defvar *flag*)

#|solvable
    This function was written by Dr. John Weiss to determine the solvability
    of a 3x3 8-Puzzle. It has been modified by the addition of two if
    statements at the begining of the function. First the input puzzle list is
    checked for duplicates, returning NIL immediately if the list has duplicate
    values. Then, if the puzzle list is larger than 9 (ergo, has a dimension
    larger than 3) then the substitute nXnSolvable function is called instead
    on the puzzle list.

    input:
        L - the nxn puzzle being checked for solvability
    output:
        t - the puzzle is solvable
        NIL - the puzzle is not solvable
|#
(defun solvable (L)
    (if (not (checkDuplicates L))
        (return-from solvable NIL)
    )
    (if (> *dimension* 3)
        (return-from solvable (nXnSolvable L))
    )
    "(solvable L) - returns T if list L contains a solvable 8-puzzle position"
    (setf *flag* nil)
    (mapcar #'(lambda (elem) (disorder elem L)) L)
    (eq *flag* (evenp (position 0 L)))
)

#| disorder
    This is a map function which is applied to a list of values, which helps
    determine the computability of a given puzzle list.

    input:
        elem - an element in a list
        L - a list of puzzle values
|#
(defun disorder (elem L)
    "(disorder elem L) - helper function for solvable routine"
    (cond
        ((eq (car L) elem))
        ((> (car L) elem)
            (setf *flag* (not *flag*))
            (disorder elem (cdr L))
        )
        (t (disorder elem (cdr L)))
    )
)


#|nXnSolvable
    This function uses the parity of the current puzzle to determine if it
    is in the same state universe as the puzzle goal state.

    The general algorithm for this function was retrieved from
    https://stackoverflow.com/questions/34570344/check-if-15-puzzle-is-solvable

    input:
        puzzle - the nxn puzzle being checked for solvability
    output:
        t - the puzzle is solvable
        NIL - the puzzle is not solvable
|#
(defun nXnSolvable (puzzle)
    (setf parity 0)
    ; current row
    (setf row 0)
    ; row with the blank on it
    (setf blank_row 0)

    (loop for i from 0 to (- (length puzzle) 1) do
        ; move to next row
        (if (= (mod i *dimension*) 0)
            (setf row (+ row 1))
        )
        (cond
            ; save row with the blank on it
            ((= (nth i puzzle) 0)
                (setf blank_row row)
            )
            ; count parity
            (t
                (loop for j from (+ i 1) to (- (length puzzle) 1) do
                    (if (and (> (nth i puzzle) (nth j puzzle)) (/= (nth j puzzle) 0))
                        (setf parity (+ parity 1))
                    )
                )
            )
        )   
    )


    ; if dimension is even and 
    (cond
        ((and (= (mod *dimension* 2) 0) (= (mod blank_row 2) 0))
            (if (= (mod parity 2) 0)
                (return-from nXnSolvable t)
                (return-from nXnSolvable NIL)
            )
        )
        ((= (mod *dimension* 2) 0)
            (if (/= (mod parity 2) 0)
                (return-from nXnSolvable t)
                (return-from nXnSolvable NIL)
            )
        )
        (t
            (if (= (mod parity 2) 0)
                (return-from nXnSolvable t)
                (return-from nXnSolvable NIL)
            )
        )
    )
)

#|checkDuplicates
    This function goes through an initial puzzle state and checks to see if it
    contains duplicate values.

    input:
        puzzle - the puzzle to be checked for duplicates
    output:
        t - the puzzle has no duplicate values
        NIL - the puzzle has dupicate values is not solvable
|#
(defun checkDuplicates (puzzle)
    ; list of bools to see if the value has been seen
    (setf seen_list (make-list (length puzzle)))
    (loop for i from 0 to (- (length puzzle) 1) do
        (setf value (nth i puzzle))
        (if (> value (- (length puzzle) 1))
            (return-from checkDuplicates NIL)
        )
        (if (not (nth value seen_list))
            ; set value to seen
            (setf (nth value seen_list) t)
            ; value has been seen return NIL
            (return-from checkDuplicates NIL)
        )
    )
    T
)
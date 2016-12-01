#|
	**** fwgc.lsp
	

A farmer with his wolf, goat, and cabbage arrive at the bank of a river. 
A boat at the river’s edge is only large enough for the farmer and one
of his possessions. The farmer cannot leave the wolf alone with the goat,
or the goat alone with the cabbage.

This is a classic Artificial Intelligence (AI) problem. The high-level
symbolic approach to problem-solving in AI is known as the state space
approach, and involves graph search. In this approach, successor states
are generated from the start state. One or more of these successors are 
selected for exploration in the next iteration, new successors are 
generated, and eventually a path is  determined from the start state to 
the goal state in the state space graph. A variety of search strategies 
have  been  developed  to  explore  the  state  space  in  different  ways. 
Exhaustive  search strategies eventually explore all possible successor
states en route to findinga solution path. 

running:  in clisp - (load' fwgc)
		     (fwgc)

	  cmd      - clisp fwgc.lsp

Author: Charles Bonn
Class: csc461 Programming Languages
Date 11/30/16

|#

;----------------------------------------------------------------------
;global constants
(defconstant farmer 1)
(defconstant wolf 2)
(defconstant goat 3)
(defconstant cabbage 4)
(defconstant id '( F W G C ))

;----------------------------------------------------------------------
(defun main()
	"(main) the main function that runes the program when run
         from command prompt"
	(fwgc)
)

;----------------------------------------------------------------------
(defun fwgc ()
	"(fwgc) runs the fwgc program"

	(setf start-state (make-state '("*start state*") 'l 'l 'l 'l))
	(setf goal-state  (make-state '("problem solved") 'r 'r 'r 'r ))
	;set start and goal states

	(print-problem (reverse(dfs start-state nil goal-state)))	
)

;----------------------------------------------------------------------
(defun print-problem (state)
	"(print problem) prints output of the solution formatting
	the data to fit the chart"

	(setf moves -1) ;set moves to -1 for inital move

	(format t "Left Bank       Right Bank       Action~%")
        (format t "---------       ----------       ------~%")

	;go tough list and print states
	(dolist (n state)
	    (format t "~15A ~15A ~A~%" (trans (cdr n) 'l) (trans (cdr n) 'r)  (car n))
	    (incf moves)
	)
	;if a solved problem print soliton
	(cond
	    ((equal state nil) (format t "*** No Solution ***~%"))
	    (t (format t  "~31A *** problem solved! ***~%" " ")
	       (format t  "~31A *** ~A moves ***~%" " " moves)
	    )
	)
)

;----------------------------------------------------------------------
(defun trans (state side)
	(setf spot 0)
	(setf lst nil)
	(dolist (n state)
	    (cond 
		((equalp n side) (setf lst (cons (nth spot id ) lst)))
	    )
	    (incf spot)
	)

	(cond
	    ((equalp lst nil) (cons '- lst))
	    (t (reverse lst))
	)
)

;----------------------------------------------------------------------
(defun make-state (f w g c s)
	"(make-state) makes a state off of the given values"
        (list f w g c s)
)

;----------------------------------------------------------------------
(defun valid-state (state)
	"(valid-state )
	checks to see if the current state is a valid state. 
        -goat and wolf same side with out farmer
	-goat and cabbage same side with out farmer "

	(cond ((and (equal (state-of state goat) (state-of state wolf))
		(not (equal (state-of state farmer) (state-of state wolf)))) nil)
		;wolf eats goat 
	      ((and (equal (state-of state goat) (state-of state cabbage))
		(not (equal (state-of state farmer) (state-of state goat)))) nil)
		;goat eats cabbage
	      (t state))
)

;----------------------------------------------------------------------
(defun state-of (state obj)
	"(state-of state obj) returns the state of the current
	obj"

	(nth obj state)
)

;----------------------------------------------------------------------
(defun message ( choice state )
	"(message choice) - writes the message for the current
	move"

	(setf msg "farmer " )
	(setf returns nil)

	; if farmer is comming or going
        (cond
	    ((equalp (state-of state farmer) 'r) (setf returns t))
	    (t (setf returns nil))
        )	 

	; message for comming or going
        (cond
	    ((equalp returns t) 
			(setf msg (concatenate 'string msg "returns ")))
	    (t (setf msg  (concatenate 'string msg "takes " )))
	)

        ;animal choice
	(cond
	    ((eq choice farmer)  (setf msg (concatenate 'string msg "alone ")))
	    ((eq choice wolf)    (setf msg (concatenate 'string msg "wolf ")))
	    ((eq choice goat)    (setf msg (concatenate 'string msg "goat ")))
	    ((eq choice cabbage) (setf msg (concatenate 'string msg "cabbage ")))
	)

	; going across
	(cond
	    ((equalp returns t) msg) 
	    (t (setf msg (concatenate 'string msg "across")))
	)
)

;----------------------------------------------------------------------
(defun boat (state choice )
	"(boat state choice) - makes a new state based off of what
	the farmer is currently taking on the boat"

	(valid-state
	    (make-state
		(message choice state)
		;'("test")
		(switch (state-of state farmer))
		(cond
		    ((eq choice wolf) (switch (state-of state wolf)))
		    (t (state-of state wolf))
		)
		(cond
		    ((eq choice goat) (switch (state-of state goat)))
		    (t (state-of state goat))
		)
		(cond
		    ((eq choice cabbage) (switch (state-of state cabbage)))
		    (t (state-of state cabbage))
		)
	    )
	)
)

;----------------------------------------------------------------------
(defun switch (side)
	"(switch side) switches the side of the river that FWGC is 
	currently on"
	(cond
	    ((equalp side 'l) 'r)
	    ((equalp side 'r) 'l)
	)
)

;----------------------------------------------------------------------
(defun dfs (state state-list goal-state)
	"(dfs state-list)
		does a recursive DFS to find a solution to the 
		FWGC problem. Returns a solution in reverse order
		path. "
 
	(cond
	    ((equalp (valid-state state) nil) nil)
	    ;invalid state, return nil
	    ((equalp (cdr state) (cdr goal-state)) (cons state state-list)  )
	    ; found winning state
	    ((not (member  (cdr state) state-list :test #'equalp :key #'cdr))
		    (or (dfs (boat state farmer) (cons state state-list) goal-state)
		        (dfs (boat state wolf) (cons state state-list) goal-state)
		        (dfs (boat state goat) (cons state state-list) goal-state)
		        (dfs (boat state cabbage) (cons state state-list) goal-state)
		    )
	    )
	    (t nil)
	    ;if none of the moves provide a list return null
	)

)

;-------------------------------------------------
;run fgwc upon loading file
(main)

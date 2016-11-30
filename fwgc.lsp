
(defconstant farmer 1)
(defconstant wolf 2)
(defconstant goat 3)
(defconstant cabbage 4)
(defconstant id '( F W G C ))

;----------------------------------------------------------------------
(defun main()
	"(main) the main function that runes the program"
	(fwgc)

)

;----------------------------------------------------------------------
(defun fwgc ()
	"(fwgc) runs the fwgc program)"

	(setf start-state (make-state '("*start state*") 'w 'w 'w 'w))
	(setf goal-state  (make-state '("problem solved") 'e 'e 'e 'e ))

	(print-problem (reverse(dfs start-state nil goal-state)))	
)

;----------------------------------------------------------------------
(defun print-problem (state)
	"(print problem) prints output of the solution"

	(format t "Left Bank       Right Bank       Action~%")
        (format t "---------       ----------       ------~%")

	;go tough list and print states
	(dolist (n state)
	    (format t "~15A ~15A ~A~%" (trans (cdr n) 'w) (trans (cdr n) 'e)  (car n))
	)
	;if a solved problem print soliton
	(cond
	    ((equalp state nil) (format t "*** No Solution ***~%"))
	    (t (format t  "~31A *** problem solved! ***~%" " "))
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
(defun farmer-state (state)
	"(farmer state) returns the state of the farmer"
         (nth 1 state)
)

;----------------------------------------------------------------------
(defun wolf-state (state)
	"(wolf state) returns the state of the wolf"
	(nth 2 state)
)

;----------------------------------------------------------------------
(defun goat-state (state)
	"(goat state) returns the state of the goat"
	(nth 3 state)
)

;----------------------------------------------------------------------
(defun cabbage-state (state)
	"(cabbage state) returns the state of the cabbage"
	(nth 4 state)
)

;----------------------------------------------------------------------
(defun pass-list (new curr-list)
	"(pass-list new curr-list)
		pushes the new state onto the list to pass recursivly 
		without affecting the list used on this level"
	(cons new curr-list)
)


;----------------------------------------------------------------------
(defun f-f (state)
	"(f-f state) - farmer takes him self across the river"
	(valid-state
	    (make-state
		'("farmer crosses alone")
		(switch (farmer-state state))
		(wolf-state state)
		(goat-state state)
		(cabbage-state state)
	    )
	)
	
)

;----------------------------------------------------------------------
(defun f-w (state)
	"(f-w state) - farmer takes the wolf across the river"
	(valid-state
	    (make-state
		'("farmer takes wolf across")
		(switch (farmer-state state))
		(switch (wolf-state state))
		(goat-state state)
		(cabbage-state state)
	    )
	)
	
)

;----------------------------------------------------------------------
(defun f-g (state)
	"(f-g state) - farmer takes thes goat across the river"
	(valid-state
	    (make-state
		'("farmer takes goat across")
		(switch (farmer-state state))
		(wolf-state state)
		(switch (goat-state state))
		(cabbage-state state)
	    )
	)
	
)

;----------------------------------------------------------------------
(defun f-c (state)
	"(f-c state) - farmer takes the cabbage across the river"
	(valid-state
	    (make-state
		'("farmer takes cabbage across")
		(switch (farmer-state state))
		(wolf-state state)
		(goat-state state)
		(switch (cabbage-state state))
	    )
	)
)

;----------------------------------------------------------------------
(defun switch (side)
	"(switch side) switches the side of the river that FWGC is 
	currently on"
	(cond
	    ((equal side 'w) 'e)
	    ((equal side 'e) 'w)
	)
)

;----------------------------------------------------------------------
(defun dfs (state state-list goal-state)
	"(dfs state-list)
		does a recursive DFS to find a solution to the 
		FWGC problem. Returns a solution in reverse order
		path. "
	;(format t "state: ~A~%" state)
	;(format t "state-list ~A~%" state-list)
	;(format t "goal-state ~A~%" goal-state)
 
	(cond
	    ((equalp (valid-state state) nil) nil)
	    ;invalid state, return nil
	    ((equalp (cdr state) (cdr goal-state)) (cons state state-list)  )
	    ; found winning state
	    ((not (member state state-list :test #'equal))
		(or (dfs (f-f state) (cons state state-list) goal-state)
		    (dfs (f-w state) (cons state state-list) goal-state)
		    (dfs (f-g state) (cons state state-list) goal-state)
		    (dfs (f-c state) (cons state state-list) goal-state)
		)
	    )	    
	    (t nil)
	    ;if none of the moves provide a list return null
	)

)


;-------------------------------------------------
;run fgwc upon loading file
(main)

;; path for status check
(defparameter *paths* nil)

;;; fail control
;; record the failure path
(defstruct failure acton)
;; failsystem
(defconstant failsym `@)

;-----------------------------
;;;; knowledge from input 
;; domain knowledge
(defparameter *T* '()) ; tasks from the beginning input
(defparameter *m* '()) ; methods from the domain knowledge
(defparameter *a* '()) ; actions from the domain knowledge
;; problem 
(defparameter *initial-state* '()) ; the beginning state of one problem
;-----------------------------
;;;; global variables for the shop2-operator
;; initialized plan
(defparameter *Plan* '())

;; list of actions 
(defparameter *A* '())

;; t for task without constraint to precede t in T
(defparameter *T0* '()) ; task constraint
; (defparameter *unifier* '()) ;a association of variables that shows current state
(defparameter *current-state* ()) ; the updated state from current position
;------------------------------------------------------------ 
(defun shop2-operator(*initial-state*, *T*, domain)
  (let ((*T0* constraint(*T*)) (current-task (car *T0*)) (substitution nil))
    (loop 
      (cond ((null *T*)(return *Plan*))
            ((taskp current-task) ; the same as current-task
             ; do, if primitive
             (do ((actions *actions* (cdr actions))
                  (*Actions* nil (cond ((and (not (eq (find (car actions) domain-actions nil)) 
                                             (not (eq (find (car actions) (*current-state*))) nil))) (setq *Actions* (cons unifier((car actions) current-task) *Actions*)))
                                       (t *Actions*)))
                )
                  ((null actions) (cond ((eq *Actions* nil) ((return fail-handling) (print "there is no desired actions")))
                                        (t (do ((act *Actions* (cdr act))
                                                (state *current-state* modify(act))
                                                (*Plan* *Plan* (cons (car act) *Plan*))
                                                (*T* *T* (setq *T* (remove current-task *T*)))
                                                (substitution substitution (substitute (cdr act)))) ; (variable initial-value modify-value)
                                               ((null act) *Plan*)))))))
            ; compound tasks
            (t ())
            )
        
        (setq *T0* (cdr *T0*))
        (setq current-task (car *T0*))
      )      
    ))
; constraint T to T0
(defun constraint (tasks)
  (return lis))
; unfiier 
; if task exist in actions then union and return true
; TODO
; theta ausgibt sowie (a, theta)
(defun unifier (action task)
  ()
  )

; substitution 
(defun substitute (theta)
  )

; modify action
(defun modify (action)
  )
;-------------------------------------------------------------
;;; illustration from thesis
#|
The first case is if t is primitive, i.e., 
if t can be accomplished directly using an action(i.e., an instance of a planning operator).  
In this case, SHOP2 finds an action a that matches t and whose preconditions are satisfied in s, and applies a to s
(if no such action exists, then this branch of the search space fails).

The second case is where t is compound, \
i.e., a method needs to be applied tottodecompose it into subtasks. 
In this case, SHOP2 nondeterministically chooses a methodinstancemthat will decompose t into subtasks 
(if no such method instance exists, then thisbranch of the search space fails).

If there is a solution plan that involves m, then the actions in P will be the leaf nodes of a decomposition tree D P 
such as the tree shown in Figure 2. The precondition formula pre(m) must be true in the state that 
immediately precedes the first actionainDPthat isa descendant of m. 
In order to ensure that pre(m) is true in the correct state, SHOP2 needs to generate the 
leftmost branch of D all the way down to the bottom, and evaluate pre(m)in the state just before a. 
The last three lines of the loop ensure that this will happen, 
by telling SHOP2 that if the current methodmhas any subtasks, SHOP2 should generate one of those subtasks 
before generating any other subtasks in the task network
|#
;-------------------------------------------------------------




  



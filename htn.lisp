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
(defparameter *M* '()) ; methods from the domain knowledge
(defparameter *A* '()) ; actions from the domain knowledge
;; problem 
(defparameter initial-state '()) ; the beginning state of one problem
;-----------------------------
;;;; global variables for the shop2-operator
;; initialized plan
(defparameter *Plan* '())

;; list of actions 
(defparameter *a* '())

;; t for task without constraint to precede t in T
(defparameter T0 '()) ; task constraint
; (defparameter *unifier* '()) ;a association of variables that shows current state
(defparameter *current-state* ()) ; the updated state from current position
;------------------------------------------------------------ 

(defun shop2-operator(initial-state Tsk domain)
  (let ((T0 constraint(Tsk)) (substitution nil) (Plan) (current-state) (Task *T*))
      (setq current-task (car T0))
      (loop 
        (cond ((null Task)(return Plan))
              ((hddl-action-name-p current-task) ; 
               ; do, if primitive
               (do ((actions *A* (cdr actions)) ; actions from domain knowledge *A*
                    (Actions-lst nil (cond ((satisfyp((car actions) current-task)) 
                                          (setq Actions-lst (push unifier((car actions) current-task) Actions-lst)))
                                         (t Actions-lst)))
                  )
                    ((null actions) Actions-lst) ; return actions-lst for debug ; 
                      (cond ((eq Actions-lst nil) ((return fail-handling) (print "there is no desired actions")))
                                       ; (t (let ((act (car Action-lst)))   
                                         (t (do ((act Actions-lst (cdr act))
                                                  (state current-state modify(act)) ; TODO 
                                                  (Plan Plan (push (car act) Plan))
                                                  (Task Task (setq Task (remove current-task Task))) ;TODO
                                                  (substitution substitution (substitute (cdr act)))) ; (variable initial-value modify-value)
                                                 ((null act) Plan)))
                                         )))
              ; compound tasks
              (t 
                 (do ((mthds *m* (cdr methds)) ;; methods from domain as local variables 
            ; M ← {(m, θ) : m is an instance of a method in D, θ unifies {head(m), t},
            ; pre(m) is true in s, and m and θ are as general as possible}
                      (Methods-lst nil (cond ((satisfyp ((car mthds) current-task))
                                            (setq Methods-lst (push unifier((car mthds) current-task) Methods-lst)))
                                            (t Methods-lst)))) ; if not fulfill the conditions, return Methods-list to restore current history.
                      )
                     ((null mthds) Methods-lst) ; return *Methods* to debug 
                     ; another return value
                       ; if M = empty then return failure
                     (cond ((eq Methods-lst nil)((return fail-handling) (print "there is no desired methods")))
                  ; nondeterministically choose a pair (m, θ) ∈ M
              (t (do ((ms Methods-lst (cdr ms))
                          ; modify T by removing t, adding sub(m), constraining each task
                                  (Tsk Tsk (remove current-task Tsk))
                                   ; in sub(m) to precede the tasks that t preceded
                                    (subm (hddl-method-subtasks (car ms)))
                                   ;, and applying θ 
                                    (substitution substitution (substitute (cdr ms)))) ; variable initial-value modify-value
                                   ((null ms)  ) ; TODO return handle
                                   ; if sub(m) is not empty then T0 ← {t ∈ sub(m) : no task in T is constrained to precede t}
                                   (cond ((subm) (push subm T0))
                                         (t (return nil))))
                          )
                                       )
              )
          )
          (setq T0 (cdr T0))
          (setq current-task (car T0))
        )      
    )
; constraint T to T0
(defun constraint (tasks)
  (return tasks))

;; satisfy check 
; parameters (action), (*current-state*)
; check the precondition of one action suits current-state
; A ← {(a, θ) : a is a ground instance of an operator in D, θ is a substitution that unifies {head(a), t von problem}, and (s von problem) satisfies a’s preconditions}
(defun satisfyp (act tsk)
  (cond (()())
    (t (return nil)))
  )
  
;; content check
(defun contentp (act tsk)
  (let (()))
  )
  
; unfiier 
; if task exist in actions then union and return true
; TODO
; two parameters check
; (defun unifier (act tsk)
;   )

; ; substitution 
; (defun substitute (theta)
;   )

; ; modify action
; (defun modify (action)
  ; )
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




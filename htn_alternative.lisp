;-------------------------------------------
#|shop2.todo
## TODO:
1. modify-constraints für actions erweitern
2. generelles debugging
3. read-hddl optimiteren
|#
					;-------------------------------------------



;; main stream of shop2
(defun main-operator ()
  (read-input)
  (shop2-plan)
  )

(defun read-input (&optional (domain-path "domain.hddl") (problem-path "problem.hddl"))
  ;(unless domain-path (setq domain-path "domain.hddl" "problem.hddl"))
  (princ "Enter domain file")
  (setq domain-path (string(read)))
  (princ "Enter problem file")
  (setq problem-path (string(read)))
  ;; global variables from domain knowledge and problem.hddl
  (fetch-initial-state domain-path problem-path))
 

(defun fetch-initial-state (domain-path problem-path)
  (defparameter *domain* (hddl:read-hddl-domain domain-path))
  (defparameter *problem* (hddl:read-hddl-problem problem-path))
  (defparameter *T0* '())
  (defparameter *Plan* '())
  (defparameter *actions*  (hddl:hddl-domain-actions *domain*))
  (defparameter *methods*  (hddl:hddl-domain-methods *domain*))
  (defparameter *current-task* nil)
  (defparameter *current-status* (hddl:hddl-problem-init-status *problem*))
  (defparameter *Tasks* (hddl:hddl-problem-tasks *problem*)))

;--------------------------------------------
;; main method for first layer of shop2
(defun shop2-plan(&optional plan tasks state substitution T0)
  (setq *T0* (constraint));;T0 ← {t ∈ T : no other task in T is constrained to precede t}
  	(loop while (not (null *T0*)) do
		(if (null *Tasks*) (return *Plan*))
        (setq *current-task* (car *T0*)
			  *T0* (cdr *T0*))
		(resolve-task)
	)))
  
;; second layer of shop2 
;; check primitive and restore the plan 
(defun resolve-task ()
  (if (primitivep *current-task* *actions*)
    ; primitive (true)
    (update-primitive-task) ;todo handle return value from function
    ; non-primitive (nil)  
    (update-nonprimitive-task ) ;todo handle return value from function
    )
)


(defun primitivep (task)
 (let ((taskname (hddl:hddl-task-name task))
      (actionname))
  (loop for a in *actions* do
    (setq actionname (hddl:hddl-action-name a))
    (if (equal actionname taskname)
	(return t)))));;ausreichend, wenn nicht t ausgegeben wird, wird automatisch nil ausgegeben!

 
 ;; third layer of shop2
 ;; unify action and update state from primitive task
 (defun update-primitive-task ()
   ;;A ← {(a, θ) : a is a ground instance of an operator in D, θ is a substitution that unifies {head(a), t}, and s satisfies a’s preconditions}
   (let* ((unifying_actions (action-unifier *actions* *current-task*))
          (Actions-lst (action-satisfyp *unifying_actions* *current-state*)))
                      (cond ((null Actions-lst) (return-from update-primitive-task nil))
		       (t (update-action-values (car Action-lst))))))
                                                
 
 (defun update-action-values (action)
 ;;nondeterministically choose a pair (a, θ) ∈ A -> choose first action
   (let* ((theta (cdr action)))
		   ;;apply θ action
          (setq action (act-substitute theta action)
		  ;;modify s by deleting del(a) and adding add(a)
               *current-state* (modify-status *current-state* action)) 
			;;append a to P
          (push action *Plan*))
		  ;; modify T by removing t and applying theta
		(setq *Tasks* (task-substitute theta *Tasks*)
          (*Tasks* (modify-constraint)) 
          (*T0* (constraint))
     (return-from update-action-values (values *Plan* *Tasks* *current-state* theta *T0*))) 
   )
 
 ;; unify methods and update state from nonprimitive task
 (defun update-nonprimitive-task ()
   ; M ← {(m, θ) : m is an instance of a method in D, θ unifies {head(m), t},
   ; and m and θ are as general as possible} 
   (let* (Methods-lst (method-unifier)) ; {(m . theta)...}  
     (cond ((null Methods-lst) (return-from update-nonprimitive-task nil)) ; if M = empty then return nil to resolve task
	   (t (update-nonprimitive-values (car Methods-lst)))))) ; nondeterministically choose a pair (m, θ) ∈ M

 
 (defun update-nonprimitive-values (method)
   (setq *Tasks* (cdr *Tasks*) ; modify T by removing t, (removin in constraint-lists happens later through constraining with subtasks!
	 subm (hddl:hddl-method-subtasks (car method))
	 theta (cadr method); in sub(m) to precede the tasks that t precede
	 subm (task-substitute theta subm)
	 *Tasks* (modify-constraints subm)) ;;constrain tasks with subtasks where appropriate
     (append subm *Tasks*)  ;;adding sub(m) -> use append because push adds subtasks as list!
(if (not (null subm)) (setq *T0* (constraint subm))
         (setq *T0* (constraint)))
     (return-from update-nonprimitive-values (values *Plan* *Tasks* *current-state* *substitution* *T0*)))





;--------------------------------------
;;builds T0: checks for all tasks if constraint-slot is empty and adds it to T0 if that's the case
;;Input: if no tasklist is provided, the global tasklist is used as default value
;; Output: *T0*
(defun constraint(&optional (tasks *Tasks*))
(setq *T0* nil)
      (loop for task in tasks do
		(if (null (hddl:hddl-task-constraints task)) 
			(push task *T0*)))
  (reverse *T0*))

 ;----------------------------------------------
;; Modifies the constraint-lists of all tasks in *Tasks* by either removing every occurrence of *current-task* or if given a list of subtasks substituting every occurrence of *current-task* by that list
;; Input (optional): List of subtasks
;; no output, modifies *Tasks* directly
(defun modify-constraints (&optional (subtasks nil))
  (declare (optimize debug))
(loop for task in *Tasks* do
  (let ((constraints (hddl:hddl-task-constraints task))
	(newconstraints ()))
    (unless (null constraints)
      (loop for c in constraints do
	(if (equalp c *current-task*)
	    (append subtasks newconstraints)
	    (push c newconstraints)))
      (setf (hddl:hddl-task-constraints task) newconstraints)))))


;--------------------------------------------------------------

;; für alle eingegebenen Aktionen prüfe, ob die preconditions einer Aktion im aktuellen Status erfüllt sind, falls ja, füge sie in Ergebnisliste ein
;; preconditions sind dann erfüllt, wenn sie im aktuellen Status enthalten sind
  ;; gibt es auch negative preconditions?
  ;;Achtung: actions haben jetzt die Form (a. theta)
  (defun action-satisfyp(actions current-state)
    (let ((satisfying_actions))  
      (loop for a in actions do
    (let ((preconditions (hddl:hddl-action-preconditions (car a)))
        (satisfies T))  ;;set satyisfies to true at first and let it be disproven for every action
    (loop for p in preconditions do
      (if (not(find p current-state))
    (setq satisfies nil)))
    (if satisfies
        (push a satisfying_actions)))
  (reverse satisfying_actions))))   

;-------------------------------------------------

;;unnötig? bei uns keine preconditions 
(defun method-satisfyp (m)
  (let ((method-satisfied nil))
    (loop
      (cond ((null m) (return method-satisfied))
          ((not (null (find ((hddl-method-preconditions (car m)) *current-status*)))) (setq method-satisfied (push (car m) method-satisfied))))
      (setq m (cdr m))
    )))
					
;--------------------------------------------------------
;; unify-m 
; prueft, ob die Laenge der Parameters gleich
; prueft, ob Typen der Parameters gleich
; return a unified list such as {(m . theta)...}      
(defun method-unifier ()
  (let* ((m (method-satisfyp *methods*))
         (task-name (hddl:hddl-task-name *current-task*))
	 (taskparams (hddl:hddl-task-parameters *current-task*))
	 ;;TODO:Achtung, wir müssen erst aus objects auslesen!
	 (objects (hddl:hddl-problem-objects *problem*))
	 (taskobjects)
	 (taskparam-types)
	 (method-lst))
    
    (loop for o in objects do
      (loop for taskparam in taskparams do
	(if (equal (car o) taskparam)
	    (progn
	      (push o taskobjects)
	      (push (cadr o) taskparam-types)))))

    (loop for method in m do
      (when (and
	      (eq task-name (car (hddl:hddl-method-task method)));;task corresponds to method
	      (eq (length taskparams) (length (hddl:hddl-method-parameters method)))
	      (equalp (sort taskparam-types #'string<) (sort cadr (apply #'mapcar #'list (hddl:hddl-method-parameters method #'string<))))

	     (setq Methods-lst (push (cons method (car (apply #'mapcar #'list (hddl:hddl-method-parameters (car m))))) Methods-lst)))))))  ;;TODO:Achtung, wird noch nicht richtig gepusht- nu v? statt alles!
;------------------------------------------------------

;;unifier sollte am besten 
;;1. alle actionen sammeln, die den gleichen Namen haben wie die task
;;2. prüfen, ob die gleiche Anzahl Parameter vorliegt (Länge der Liste)
;;3. prüfen, ob die Parameter den selben Typ haben (Reihenfolge in Parameterliste egal)
;;4. die Parameter der task als theta ausgeben
;; Ergebnis wäre dann eine Liste mit ((action . theta)(action . theta)...)
;; hier würde ich also auch gar nicht jede action einzeln übergeben, sondern gleich alle auf einmal

(defun action-unifier(actions task)
  (let ((same_name)
  (unified_actions)
  (taskname (hddl:hddl-task-name task));;get name and params of task for easier comparing
  (taskparams (hddl:hddl-task-parameters task)))
    ;; first for all actions collect only those that have the same name & amount of parameters as the task
    (loop for a in actions do
      (let ((a-params (hddl:hddl-action-parameters a)))
  (if (eq (hddl:hddl-action-name a) taskname)
      (if (eq (length taskparams) (length a-params)) ;; then compare types
    (push a same_name)))))
    ;;then compare if parameters have the same types
    (loop for a in same_name do
    ;;collect all parameter-types of an action
       (let* ((a-params (hddl:hddl-action-parameters a))
        
	      (copy-list a-params)
	      (theta))
         ;;loop through the task-parameter-types; whenever the same type is found in the action parameter-type-list, remove it there; if after the loop the a-paramtypes list is empty, the action can be unified with the task, set the parameters of the task as theta
	 (loop for taskparam in taskparams do
	   (loop for actionparam in a-params do
	     (if (eq (cdr taskparam)(cdr actionparam)) ;;if the types are the same
		 (push (cons (car actionparam)taskparam) theta) ;;push the variable to be substituted and the substitution to theta: (v1? . (truck-0 . VEHICLE))
		 (remove actionparam copy-list))) ;; remove it from the copied actionparam list
	   (if (null copy-list)         ;; only if the list is empty both sets of parameters match exactly
	       (push (cons a theta) unified_actions))))) ;;push the action and related theta to the result-list
    (reverse unified_actions)))

;------------------------------------------------------
;;substitution: muss alle Variablen in action entsprechend Theta substituieren
;; reminder: elements of theta: (?v . (truck . vehicle)) -> (variable.(entity.type))

(defun act-substitute(theta action)
  (let* ((a-params (hddl:hddl-action-parameters action))
	(a-pos-effects (hddl:hddl-action-pos-effects action))
	(a-neg-effects (hddl:hddl-action-neg-effects action)))
    ;; for all substitutions
    (loop for sub in theta do
      ;;parameter-substitution
      (loop for param in a-params do
	;;check if first part of parameter matches first part of theta
	(if (eq (car sub)(car param))
	    ;;if so, set parameter to middle part of theta (actual entity)
	    (setq param (car (cdr sub)))))
      
	;;if pos-effects ist not empty, do the same there
	(unless (null a-pos-effects)
	  (loop for effect in a-pos-effects do
	    ;;for every part of the effect (looks like this: (AT ?V ?L1)) check if it
	    ;; can be found in theta, if yes substitute
	    (loop for x in (cdr effect) do
	      (if (eq (car sub) x)
		  (setq x (car (cdr sub)))))))
	   ;;if neg-effects ist not empty, do the same there
	(unless (null a-neg-effects)
	  (loop for effect in a-neg-effects do
	    ;;for every part of the effect (looks like this: (AT ?V ?L1)) check if it
	    ;; can be found in theta, if yes substitute
	    (loop for x in (cdr effect) do
	      (if (eq (car sub) x)
		  (setq x (car (cdr sub))))))))))
	   
		  
;------------------------------------------------		 

(defun task-substitute (theta Tasks)
;TODO
  )


;-------------------------------------------------------------------
;;hier würde ich denke ich (defun modify (status action) schreiben, damit wir den aktuellen
;; Status auch mitübergeben, den wir dann verändern
;; dafür würde ich 
;; 1. durch die current-status-Liste iterieren und für alle negativen Effekte der Aktion herauslöschen
;; 2. alle positiven Effekte der Aktion hinzufügen, und das als neuen Status ausgeben lassen:
  (defun modify-status (status action)
    (let ((addeffect (hddl:hddl-action-pos-effects action))
    (deleffect (hddl:hddl-action-neg-effects action))
    (new-status))
      (loop for e in status do
  (if (not(find e deleffect))
      (push e new-status)))
      (cons new-status addeffect)
      new-status))
       

  
;; failure handling
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




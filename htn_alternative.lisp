
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

(defun shop2-operator(domain-file problem-file)

  (let* ((domain (read-hddl-domain domain-file)) ;; read domain-file -> domain structure
	 (problem (read-hddl-problem problem-file)) ;;read problem-file -> problem-structure
	 (Tasks (hddl-problem-task problem)) ;;our tasks to be solved come from the problem-file
	 (current-state (hddl-problem-init problem))
	 (Plan);;P = the empty plan
         (methods (hddl-domain-methods domain)) 
	 (actions (hddl-domain-actions domain))
	 (T0 constraint(Tasks));;T0 ← {t ∈ T : no other task in T is constrained to precede t}
	 (current-task)) 

    (loop 
        (if ((null Tasks)(return Plan))) ;;if T = empty then return P
        (setq current-task (car T0)) ;;nondeterministically choose any t € T0

           ;;if t is a primitive task then
      (cond ((hddl-action-name-p current-task-name)
	     ;;A ← {(a, θ) : a is a ground instance of an operator in D, θ is a substitution that unifies {head(a), t}, and s satisfies a’s preconditions}
             (let* ((unifying_actions (action-unifier actions current-task))
		    (Actions-lst (action-satisfyp unifying_actions current-state)))
	       
               ;;if A = empty then return failure
	       (cond ((null Actions-lst) ((error "There is no action that can be executed at this point! Please check your hddl-files and make sure that your tasks can be solved!")))
		    
		     ;;nondeterministically choose a pair (a, θ) ∈ A
		     (t ((let ((act (car Action-lst)))
			   (setq current-state modify-status(current-state act)) ;;modify s by deleting del(a) and adding add(a)
			   (push act Plan) ;;append a to P  ;TODO: mit oder ohne theta?
			    ;; modify T by removing t and applying θ
			   (setq Tasks (remove-for-all-tasks current-task Tasks)) ;TODO function zusätzlich müssten wir dann auch schauen, dass wir die task aus allen task constraint-Listen löschen!
			   (substitute ((cdr act) Tasks)))))) ;TODO: write function substitute!
	       
                     ;; T0 ← {t ∈ T : no task in T is constrained to precede t}
               (setq T0 constraint(Tasks))))
	    
              ; compound tasks
            (t
	     ; M ← {(m, θ) : m is an instance of a method in D, θ unifies {head(m), t},
            ; pre(m) is true in s, and m and θ are as general as possible}  ;;TODO: how to make it as general as possible? Wir haben keine Preconditions!
             (let* ((unifying_methods (method-unifier methods current-task)) ;;TODO: write method-unifier or check if action-unifier can encompass both!
		    
                       ; if M = empty then return failure
                     (cond ((null Methods-lst)((error "There is no method that can be executed at this point! Please check your hddl-files and make sure that your tasks can be solved!")))
			 ; nondeterministically choose a pair (m, θ) ∈ M
			   (t (let* ((method (car Methods-lst))
				     (subtasks (hddl-method-subtasks method)))
                          ; modify T by removing t, adding sub(m), constraining each task in sub(m) to precede the tasks that t preceded
                                (setq Tasks (remove-for-all-tasks current-task Task))
				 (setq Tasks (add-constraints Tasks subtasks)) ;; TODO: write add-constraints
				(push subtasks Tasks) ;;Frage an Prof. Wolter: ist das hier zu einschränkend? Da wir immer den ersten Wert herausnehmen, iel Beeinflussungsmöglichkeit!
                                   ;, and applying θ 
                                    (substitute (method current-task Tasks)))) ; TODO: write substitute!
                                   ; if sub(m) is not empty then T0 ← {t ∈ sub(m) : no task in T is constrained to precede t}
                                   (cond ((subtasks) (setq T0 constraint(subtasks)))
                                         (t  (setq T0 constraint(Tasks)))))
                          ))))))
                                       
; constraint T to T0 
;; Alisa: muss also für alle t in T prüfen, dass nicht eine andere task vorher ausgeführt werden muss
;; ich würde ganz am Anfang des Codes für jede task in Tsk eine leere Liste erstellen, die die constraints enthält,
;; dann müssten hier nur prüfen, ob die constraints leer sind oder nicht
;; dann müssen wir bei den Methoden dran denken, dass die constraints bei den Subtasks eingefügt werden müssen

  (defun constraint (tasks)
    (let ((t0))
      (loop for task in tasks do
		(if (null (hddl-task-constraints task)) 
			(push task t0)))
      (reverse t0)))


;; für alle eingegebenen Aktionen prüfe, ob die preconditions einer Aktion im aktuellen Status erfüllt sind, falls ja, füge sie in Ergebnisliste ein
;; preconditions sind dann erfüllt, wenn sie im aktuellen Status enthalten sind
  ;; gibt es auch negative preconditions?
  ;;Achtung: actions haben jetzt die Form (a. theta)
  (defun action-satisfyp(actions current-state)
    (let ((satisfying_actions))  
      (loop for a in actions do
    (let ((preconditions (hddl-action-preconditions (car a)))
        (satisfies T))  ;;set satyisfies to true at first and let it be disproven for every action
    (loop for p in preconditions do
      (if (not(find p current-state))
    (setq satisfies nil)))
    (if satisfies
        (push a satisfying_actions)))
  (reverse satisfying_actions))))   

  
;; removes tasks that have been finished by adding actions to the plan from the Tasks list, and from every task-constraint-list in Tasks
(defun remove-for-all-tasks (curren-task Tasks)
  ;;TODO!
  )
 
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
  (taskname (hddl-task-name task));;get name and params of task for easier comparing
  (taskparams (hddl-task-parameters task))
  (taskparam-types (loop for p in taskparams collect
               (cdr p))))
    ;; first for all actions collect only those that have the same name & amount of parameters as the task
    (loop for a in actions do
      (let ((a-params (hddl-action-parameters a)))
  (if (eq (hddl-action-name action) taskname)
      (if (eq (length taskparams) (length a-params)) ;; then compare types
    (push a same_name)))))
    ;;then compare if parameters have the same types
    (loop for a in same_name do
    ;;collect all parameter-types of an action
       (let* ((a-params (hddl-action-parameters a))
        (a-paramtypes (loop for p in a-params collect
					      (cdr p)))
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
	 
         #|(loop for type in taskparam-types do
           (if (find type a-paramtypes)
         (remove type a-paramtypes)))
         (if (null a-paramtypes) ;; testen das task-parametertypes auch null
       (push (cons a taskparams) unified_actions))))
   (reverse unified_actions)))|#



; substitution: muss alle Variablen in Tasks entsprechend Theta substituieren
; (defun substitute (theta Tasks)
;   )

;; Alisa: hier würde ich denke ich (defun modify (status action) schreiben, damit wir den aktuellen
;; Status auch mitübergeben, den wir dann verändern
;; dafür würde ich 
;; 1. durch die current-status-Liste iterieren und für alle negativen Effekte der Aktion herauslöschen
;; 2. alle positiven Effekte der Aktion hinzufügen, und das als neuen Status ausgeben lassen:
  (defun modify-status (status action)
    (let ((addeffect (hddl-action-poseffect action))
    (deleffect (hddl-action-negeffect action))
    (new-status))
      (loop for e in status do
  (if (not(find e deleffect))
      (push e new-status)))
      (cons new-status addeffect)
      new-status))
       
; ; modify action
; (defun modify (action)
  ; )
  
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




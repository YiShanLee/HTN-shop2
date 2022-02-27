;-------------------------------------------
#|shop2.todo
## TODO:
0. set T0 as global variable, such that while-loop always uses the T0 we are currently 
working with
1. while loop inside shop2-plan
2. debug method unifier and satisfy & primitive task & non primitive task functions
3. print status for current-standpoint 
4. run read input
|#
;-------------------------------------------
;; main stream of shop2
(defun main-operator ()
  (read-input)
  (shop2-plan)
  )
(defun read-input (&optional (domain-path "domain.hddl") (problem-path "problem.hddl"))
(unless domain-path (setq domain-path "domain.hddl" "problem.hddl"))
  (princ "Enter domain file")
  (setq domain-path (string(read)))
  (princ "Enter problem file")
  (setq problem-path (string(read)))
  ;; global variables from domain knowledge and problem.hddl
  (defparameter *domain* (hddl:read-hddl-domain domain-path))
  (defparameter *problem* (hddl:read-hddl-problem problem-path)))

;--------------------------------------------
;; main method for first layer of shop2
(defun shop2-plan(&optional plan tasks state substitution T0)
  (let* (
   (tasks (Hddl:hddl-problem-tasks *problem*))
   (current-state (hddl:hddl-problem-init-status *problem*))
   (Plan plan);;P = the empty plan
   (methods (hddl:hddl-domain-methods *domain*)) 
   (actions (hddl:hddl-domain-actions *domain*))
   (T0 (constraint(tasks)));;T0 ← {t ∈ T : no other task in T is constrained to precede t}
   (substitution substitution) ;; warum substitution?
   ) 
    
	(loop while (not (null T0)) do
		(if (null tasks) (return Plan))
        (setq current-task (car T0)
			  T0 (cdr T0))
		(resolve-task *domain* *problem* current-state Plan methods actions T0 current-task substitution tasks)
	;; Frage: müssen wir domain und problem als globale Variablen überhaupt übergeben
	;; müssen wir T0 übergeben, wenn wir hier loopen?
	)))
  
;; second layer of shop2 
;; check primitive and restore the plan 
(defun resolve-task (domain problem current-state Plan methods actions T0 current-task substitution Tasks) 
  (if (primitivep current-task actions)
    ; primitive (true)
	;Frage: warum substitution, nötig?
    (update-primitive-task Plan actions current-task substitution Tasks current-state T0) ;todo handle return value from function
    ; non-primitive (nil)  
    (update-nonprimitive-task Plan methods current-task substitution Tasks current-state T0) ;todo handle return value from function
    )
)


(defun primitivep (task actions)
 (let ((taskname (hddl:hddl-task-name task)))
 (loop for a in actions do
  (if (eq (hddl:hddl-action-name a) taskname)
      (return t)))
     (return nil)))	  

 
 ;; third layer of shop2
 ;; unify action and update state from primitive task
 (defun update-primitive-task (Plan actions current-task substitution Tasks current-state T0)
   ;;A ← {(a, θ) : a is a ground instance of an operator in D, θ is a substitution that unifies {head(a), t}, and s satisfies a’s preconditions}
   (let* ((unifying_actions (action-unifier actions current-task))
          (Actions-lst (action-satisfyp unifying_actions current-state)))
                      (cond ((null Actions-lst) (return-from update-primitive-task nil))
                                         
										 ;; TODO: Frage, müsste es hier nicht andersherum sein, wir werten erst aus und fangen dann von vorne an?
			  ;; Bzw. durch while-Schleife müssen wir gar nicht von vorne anfangen, sondern nur die entsprechenden Variablen anpassen (T0, Tasks, Plan)
			
										 (t (shop2-plan (multiple-value-bind (plan tasks state substitution T0) ;return updated list
                                              (update-action-values Actions-lst current-state Plan Tasks substitution))
                                                 ))
                                         ))
   )
 
 (defun update-action-values (actions state Plan Tasks substitution)
 ;;nondeterministically choose a pair (a, θ) ∈ A -> choose first action
   (let* ((act (car Action-lst)) 
           (theta (cdr act)))
		   ;;apply θ action
		   ;TODO: write function substitute!
          (setq act (act-substitute theta action)
		  ;;modify s by deleting del(a) and adding add(a)
                current-state (modify-status current-state act)) 
			;;append a to P
          (push act Plan))
		  ;; modify T by removing t and applying theta
		(setq Tasks (task-substitute theta Tasks)
          (Tasks (remove-task current-task Tasks)) 
          (T0 (constraint Tasks))
     (return-from update-action-values (values Plan Tasks current-state substitution T0))) 
   )
 
 ;; unify methods and update state from nonprimitive task
 (defun update-nonprimitive-task (Plan methods current-task substitution Tasks current-state T0)
   ; M ← {(m, θ) : m is an instance of a method in D, θ unifies {head(m), t},
   ; and m and θ are as general as possible} 
   ;;TODO: welche Funktion hier für methods?
   (let* (Methods-lst (method-unifier methods current-task)) ; {(m . theta)...}  
        (cond ((null Methods-lst) (return-from update-nonprimitive-task nil)) ; if M = empty then return nil to resolve task
              ;; TODO: Frage, müsste es hier nicht andersherum sein, wir werten erst aus und fangen dann von vorne an?
			  ;; Bzw. durch while-Schleife müssen wir gar nicht von vorne anfangen, sondern nur die entsprechenden Variablen anpassen (T0, Tasks, Plan)
			  (t  (shop2-plan (multiple-value-bind (plan tasks state substitution T0) ; else, from begin
                    (update-nonprimitive-values Methods-lst current-state Plan Tasks substitution))) ; nondeterministically choose a pair (m, θ) ∈ M
                                   ) 
                                       ))
   )
 
 (defun update-nonprimitive-values (Methods-lst current-state Plan Tasks substitution)
   (let* ((ms (car Methods-lst))
          (Tasks (cdr Tasks)) ; modify T by removing t, (removin in constraint-lists happens later through constraining with subtasks!
          (subm (hddl:hddl-method-subtasks ms))
		  (theta (cdr ms)))
		  ; in sub(m) to precede the tasks that t preceded
		  (setq subtasks (task-substitute theta subm)
			;; TODO: when constraining, search for current-task and replace it with subtasks! Subtasks themselves should already have constraints from reading-in of subtasks
				Tasks (add-constraints Tasks subm) ) ;;constraining each task 
     (push subm Tasks)))))  ;;adding sub(m), constraining each task
(if (not (null subm)) (setq T0 (constraint subm))
         (setq T0 constraint(Tasks)))
     (return-from update-nonprimitive-values (values Plan Tasks current-state substitution T0))
   )
  )




#|
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

  (let* ((domain (hddl:read-hddl-domain domain-file)) ;; read domain-file -> domain structure
	 (problem (hddl:read-hddl-problem problem-file)) ;;read problem-file -> problem-structure
	 (Tasks (hddl:hddl-problem-tasks problem)) ;;our tasks to be solved come from the problem-file
	 (current-state (hddl:hddl-problem-init-status problem))
	 (Plan);;P = the empty plan
         (methods (hddl:hddl-domain-methods domain)) 
	 (actions (hddl:hddl-domain-actions domain))
	 (T0 (constraint Tasks));;T0 ← {t ∈ T : no other task in T is constrained to precede t}
	 (current-task)) 

    (loop 
        (if (null Tasks)(return Plan)) ;;if T = empty then return P
      (setq current-task (car T0)) ;;nondeterministically choose any t € T0

           ;;if t is a primitive task then
      (cond ((eq (hddl:hddl-action-name-p (hddl:hddl-task-name current-task))
	     ;;A ← {(a, θ) : a is a ground instance of an operator in D, θ is a substitution that unifies {head(a), t}, and s satisfies a’s preconditions}
             (let* ((unifying_actions (action-unifier actions current-task))
		    (Action-lst (action-satisfyp unifying_actions current-state)))
	       
               ;;if A = empty then return failure
	       (cond ((null Action-lst) (error "There is no action that can be executed at this point! Please check your hddl-files and make sure that your tasks can be solved!"))
		    
		     ;;nondeterministically choose a pair (a, θ) ∈ A
		     (t (let* ((act (car Action-lst))
			      (theta (cdr act)))
			  ;;apply θ action
			  ;TODO: write function substitute!
			  (setq act (act-substitute theta action)
				;;modify s by deleting del(a) and adding add(a)
				current-state (modify-status current-state act))
			  ;;append a to P
			   (push act Plan)   
			    ;; modify T by removing t and applying theta
			  (setq	Tasks (task-substitute theta Tasks)
			   Tasks (remove-for-all-tasks current-task Tasks)) ;TODO function zusätzlich müssten wir dann auch schauen, dass wir die task aus allen task constraint-Listen löschen!
			   ))) 
	       
                     ;; T0 ← {t ∈ T : no task in T is constrained to precede t}
               (setq T0 (constraint Tasks))))
	    
              ; compound tasks
            (t
	     ; M ← {(m, θ) : m is an instance of a method in D, θ unifies {head(m), t},
            ; and m and θ are as general as possible}  ;;TODO: how to make it as general as possible? Wir haben keine Preconditions!
             (let ((Methods-lst (method-unifier methods current-task current-state)))		    
                       ; if M = empty then return failure
                     (cond ((null Methods-lst)(error "There is no method that can be executed at this point! Please check your hddl-files and make sure that your tasks can be solved!"))
			 ; nondeterministically choose a pair (m, θ) ∈ M
			   (t (let* ((selected-method (car Methods-lst))
				     (subtasks (hddl:hddl-method-subtasks selected-method))
				     (theta (cdr selected-method)))
                          ; modify T by removing t, adding sub(m), constraining each task in sub(m) to precede the tasks that t preceded
                                (setq Tasks (remove-for-all-tasks current-task Task)
				      Tasks (add-constraints Tasks subtasks)
				      subtasks (task-substitute theta subtasks))
				(push subtasks Tasks)))))  
                                   ; if sub(m) is not empty then T0 ← {t ∈ sub(m) : no task in T is constrained to precede t}
                                   (cond ((subtasks) (setq T0 (constraint subtasks)))
                                         (t  (setq T0 (constraint Tasks)))))
                          )))))
   
|#
;--------------------------------------   
; constraint T to T0 
;; Alisa: muss also für alle t in T prüfen, dass nicht eine andere task vorher ausgeführt werden muss
;; ich würde ganz am Anfang des Codes für jede task in Tsk eine leere Liste erstellen, die die constraints enthält,
;; dann müssten hier nur prüfen, ob die constraints leer sind oder nicht
;; dann müssen wir bei den Methoden dran denken, dass die constraints bei den Subtasks eingefügt werden müssen

(defun constraint(tasks)
  (let ((t0))
      (loop for task in tasks do
		(if (null (hddl:hddl-task-constraints task)) 
			(push task t0)))
      (reverse t0)))

;----------------------------------------------

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
(defun method-satisfyp (m state)
  (let ((method-satisfied nil))
    (loop
      (cond ((null m) (return method-satisfied))
          ((not (null (find ((hddl-method-preconditions (car m)) state)))) (setq method-satisfied (push (car m) method-satisfied))))
      (setq m (cdr m))
    )))
 
;------------------------------------------------------------------ 
;; removes tasks that have been finished by adding actions to the plan from the Tasks list, and from every task-constraint-list in Tasks
(defun remove-task (current-task tasks)
  (setq tasks (remove current-task tasks))
  (loop for (task constraint) in tasks do
	(setq constraint (remove current-task constraint)))
  tasks
  )
;--------------------------------------------------------------
 
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

;--------------------------------------------------------
;; unify-m 
; prueft, ob die Laenge der Parameters gleich
; prueft, ob Typen der Parameters gleich
; return a unified list such as {(m . theta)...}      
(defun method-unifier (methods task state)
  (let ((m method-satisfyp (methods state)))
        (task-name (hddl-task-name task))
        (taskparams (hddl-task-parameters task))
        (taskparam-types (cadr (apply #'mapcar #'list (hddl-task-parameters task))))
        (taskparam (car (apply #'mapcar #'list (hddl-task-parameters task))))
        (Methods-lst nil)
        )
    (loop
      (cond ((and (eq (length taskparams) (length (hddl-method-parameters (car m))))
                 (equalp (taskparam-types) (cadr (apply #'mapcar #'list (hddl-method-parameters (car m))))))
            (setq Methods-lst (push (cons (car m) (car (apply #'mapcar #'list (hddl-method-parameters (car m))))) Methods-lst)))
            ((null m) (return Methods-lst))
            )
      (setq m (cdr m))
    )
  )
)
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




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
;; Alisa: am besten wäre es wahrscheinlich, wenn wir in diesem let das Einlesen von Domain und Code 
;; mit einbinden, also (*domain* (read-hddl-domain "domain-file")) (*problem* (read-hddl-problem "problem-file"))
;; dann sollten wir die Funktion vielleicht so aufrufen lassen:  (defun shop2-operator(domain-file problem-file) und alles
;; weitere wie initial-state und so weiter dann erst rausziehen, oder? Also das man zum Aufrufen wirklich nur (shop2-operator ("domain.hddl" "problem.hddl")) eintippen muss
;; dann wäre es (let ((*domain* (read-hddl-domain "domain-file")) (*problem* (read-hddl-problem "problem-file"))
;; 					 (Tsk (hddl-problem-task *problem*)) (initial-state (hddl-problem-init problem)) und dann alles, was du jetzt schon hast)
 (let ((T0 constraint(Tsk)) (substitution nil) (Plan) (current-state) (Task *T*)) ;;Alisa: Ist Task und Tsk hier dann nicht das gleiche?
      (setq current-task (car T0))
	  ;; Alisa: hier vielleicht auch noch für jede Task eine constraint-list anhängen, 
	  ;; die erstmal leer ist, und später bei Methoden-Aufrufen durch Subtasks gefüllt wird, 
	  ;; wenn es constraints gibt? Dann müssten wir für T0 auch immer nur prüfen, ob die constraint-Listen leer sind
      (loop 
        (cond ((null Task)(return Plan))
              ((hddl-action-name-p current-task) ;; Alisa: hier dann auch current-task-name! 
               ; do, if primitive
               (do ((actions *A* (cdr actions)) ; actions from domain knowledge *A*
					;; Alisa: ich glaube, hier wäre es sinnvoller, erst zu schauen, welche actions unifien 
					;;(also den selben Namen wie die task haben, die selben Parameter brauchen und dann die Parameter als theta zu speichern)
					;; und danach erst, ob die preconditions erfüllt sind, weil wir dann nur noch viel weniger actions durchgehen müssen!
                    (Actions-lst nil (cond ((satisfyp((car actions) current-task)) ;; Alisa: für satisfyp würde ich dann die Liste aller actions übergeben, die beim unifier rauskamen
                                          (setq Actions-lst (push unifier((car actions) current-task) Actions-lst)))
                                         (t Actions-lst)))
                  )
                    ((null actions) Actions-lst) ; return actions-lst for debug ; 
                      (cond ((eq Actions-lst nil) ((return fail-handling) (print "there is no desired action")))
                                       ; (t (let ((act (car Action-lst)))   ;; Alisa: wir würden hier ja wie bei T0 einfach erstmal "nondeterministically" die erste Aktion auswählen
                                         (t (do ((act Actions-lst (cdr act)) ;; Alisa: fehlt hier danach ein setq?
                                                  (state current-state modify(act)) ; TODO ;; Alisa: ich habe unten mal eine Funktion dafür angefangen
                                                  (Plan Plan (push (car act) Plan))
                                                  (Task Task (setq Task (remove current-task Task))) ;TODO ;;Alisa: zusätzlich müssten wir dann auch schauen, dass wir die task aus allen task constraint-Listen löschen!
                                                  (substitution substitution (substitute (cdr act)))) ; (variable initial-value modify-value)
                                                 ((null act) Plan)))
                                         )))
              ; compound tasks
              (t 
                 (do ((mthds *m* (cdr methds)) ;; methods from domain as local variables 
            ; M ← {(m, θ) : m is an instance of a method in D, θ unifies {head(m), t},
            ; pre(m) is true in s, and m and θ are as general as possible}
			
			;; Alisa: auch hier würde ich erst die Zeile mit unifier und danach mit satisfyp ausführen
                      (Methods-lst nil (cond ((satisfyp ((car mthds) current-task))
                                            (setq Methods-lst (push unifier((car mthds) current-task) Methods-lst)))
                                            (t Methods-lst)))) ; if not fulfill the conditions, return Methods-list to restore current history.
                      )
                     ((null mthds) Methods-lst) ; return *Methods* to debug 
                     ; another return value
                       ; if M = empty then return failure
                     (cond ((eq Methods-lst nil)((return fail-handling) (print "there is no desired methods")))
                  ; nondeterministically choose a pair (m, θ) ∈ M
		;; Alisa: ; (t (let ((ms (car Methods-lst))) 
              (t (do ((ms Methods-lst (cdr ms))
                          ; modify T by removing t, adding sub(m), constraining each task
                                  (Tsk Tsk (remove current-task Tsk))
                                   ; in sub(m) to precede the tasks that t preceded
                                    (subm (hddl-method-subtasks (car ms)))
									;; Alisa: hier müssen wir noch die Subtasks zu Tsk hinzufügen und dort 
									;; gegebenenfalls in die constraint-Listen der tasks! (überall wo current-task rausgelöscht wird, neue subtasks einfügen)
                                   ;, and applying θ 
                                    (substitution substitution (substitute (cdr ms)))) ; variable initial-value modify-value
                                   ((null ms)  ) ; TODO return handle
                                   ; if sub(m) is not empty then T0 ← {t ∈ sub(m) : no task in T is constrained to precede t}
                                   (cond ((subm) (push subm T0)) ;; Alisa: hier müssen wir T0 ganz neu erstellen, aus nur den Subtasks, die keine constraints haben
                                         (t (return nil)))) ;; Alisa ansonsten müssen wir T0 wieder wie oben erstellen, also aus nur den tasks in Tsk, die keine constraints haben
                          )
                                       )
              )
          )
          (setq T0 (cdr T0))
          (setq current-task (car T0))
        )      
    )
; constraint T to T0 
;; Alisa: muss also für alle t in T prüfen, dass nicht eine andere task vorher ausgeführt werden muss
;; ich würde ganz am Anfang des Codes für jede task in Tsk eine leere Liste erstellen, die die constraints enthält,
;; dann müssten hier nur prüfen, ob die constraints leer sind oder nicht
;; dann müssen wir bei den Methoden dran denken, dass die constraints bei den Subtasks eingefügt werden müssen

  (defun constraint2 (tasks)
    (let ((t0))
      (loop for task in tasks do
	(if (eq (cdr task) nil)
	    (cons task t0)))
      t0))

(defun constraint (tasks)
  (return tasks))

;; satisfy check 
; parameters (action), (*current-state*)
; check the precondition of one action suits current-state
; A ← {(a, θ) : a is a ground instance of an operator in D, θ is a substitution that unifies {head(a), t von problem}, and (s von problem) satisfies a’s preconditions}

;; Alisa: für alle eingegebenen Aktionen prüfe, ob die preconditions einer Aktion im aktuellen Status erfüllt sind, falls ja, füge sie in Ergebnisliste ein
;; preconditions sind dann erfüllt, wenn sie im aktuellen Status enthalten sind
;; gibt es auch negative preconditions?
  (defun satisfyp2(actions current-status)
    (let ((satisfying_actions))  
      (loop for a in actions do
	(let ((preconditions (hddl-action-preconditions a))
	      (satisfies T))  ;;set satyisfies to true at first and let it be disproven for every action
	  (loop for p in preconditions do
	    (if (not(find p current-status))
		(setq satisfies nil)))
	  (if satisfies
	      (push a satisfying_actions)))
	(reverse satisfying_actions))))		



(defun satisfyp (act tsk)
  (cond (()())
    (t (return nil)))
  )
  
;; content check
(defun contentp (act tsk)
  (let (()))
  )
 
;;Alisa: unifier sollte am besten 
;;1. alle actionen sammeln, die den gleichen Namen haben wie die task
;;2. prüfen, ob die gleiche Anzahl Parameter vorliegt (Länge der Liste)
;;3. prüfen, ob die Parameter den selben Typ haben (Reihenfolge in Parameterliste egal)
;;4. die Parameter der task als theta ausgeben
;; Ergebnis wäre dann eine Liste mit ((action . theta)(action . theta)...)
;; hier würde ich also auch gar nicht jede action einzeln übergeben, sondern gleich alle auf einmal

(defun unify(actions task)
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
								(cdr p))))
		     ;;loop through the task-parameter-types; whenever the same type is found in the action parameter-type-list, remove it there; if after the loop the a-paramtypes list is empty, the action can be unified with the task, set the parameters of the task as theta
		     (loop for type in taskparam-types do
		       (if (find type a-params)
			   (remove type a-paramtypes)))
		     (if (null a-paramtypes)
			 (push (cons a taskparams) unified_actions))))
   (reverse unified_actions)))



; unfiier 
; if task exist in actions then union and return true
; TODO
; two parameters check
; (defun unifier (act tsk)
;   )

; ; substitution 
; (defun substitute (theta)
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




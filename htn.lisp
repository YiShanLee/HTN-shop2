;; main stream of shop2
(defun main-operator ()
  (read-input)
  (shop2-plan)
  )
(defun read-input ()
  ; (princ "Enter domain file")
  ; (setq domain-path (read))
  ; (princ "Enter problem file")
  ; (setq problem-path (read))
  ; (setq domain-file (values (downcase domain-path)))
  ; (setq problem-file (values (downcase problem-path)))
  ; ; (defvar domain-path (merge-pathnames domain-file *default-pathname-defaults*))
  ; ; (defvar problem-path (merge-pathnames problem-file *default-pathname-defaults*))
  ; (format t "d: ~S~%p: ~S" domain-file problem-file)
  (get-domain-knowledge ("domain.hddl" "problem.hddl"))
  )
;; global variables from domain knowledge and problem.hddl
(defun get-domain-knowledge (domain-file problem-file)
  (defparameter *domain* (read-hddl-domain domain-file))
  (defparameter *problem* (read-hddl-problem problem-file))
  )
;--------------------------------------------
;; main method for first layer of shop2
(defun shop2-plan(&optional plan tasks state substitution T0)
  (let* (
   (tasks (hddl-problem-tasks *problem*))
   (current-state (hddl-problem-init-status *problem*))
   (Plan plan);;P = the empty plan
   (methods (hddl-domain-methods *domain*)) 
   (actions (hddl-domain-actions *domain*))
   (T0 (constraint(tasks)));;T0 ← {t ∈ T : no other task in T is constrained to precede t}
   (substitution substitution)
   ) 
    (dotimes (current-num (length T0))
          (unless (length tasks) (return Plan))
          (setq current-task (nth current-num T0))
        (resolve-task domain problem current-state Plan methods actions T0 current-task substitution Tasks)
      )
    )
  )
  
;; second layer of shop2 
;; check primitive and restore the plan 
(defun resolve-task (domain problem current-state Plan methods actions T0 current-task substitution Tasks) 
  (if (primitivep current-task)
    ; primitive (true)
    (update-primitive-task Plan actions current-task substitution Tasks current-state T0) ;todo handle return value from function
    ; non-primitive (nil)
    
    (update-nonprimitive-task Plan methods current-task substitution Tasks current-state T0) ;todo handle return value from function
    )
)
  
(defun primitivep (task)
 (if (eql (type-of task) 'HDDL-ACTION)
   T
   nil
   )
 )
 
 ;; third layer of shop2
 ;; unify action and update state from primitive task
 (defun update-primitive-task (Plan actions current-task substitution Tasks current-state T0)
   (let* ((unifying_actions (action-unifier actions current-task))
          (Actions-lst (action-satisfyp unifying_actions current-state)))
                      (cond ((eq Actions-lst nil) (return-from update-primitive-task nil))
                                         (t (shop2-operator (multiple-value-bind (plan tasks state substitution T0) ;return updated list
                                              (update-action-values Actions-lst current-state Plan Tasks substitution))
                                                 ))
                                         ))
   )
 
 (defun update-action-values (action state Plan Tasks substitution)
   (let* ((act (car Action-lst)) 
          (current-state (modify-status current-state act)) 
          (Plan (cons (car act) Plan))
          (Tasks (remove-task current-task Tasks)) ;TODO function ;;Alisa: zusätzlich müssten wir dann auch schauen, dass wir die task aus allen task constraint-Listen löschen!
          (T0 (constraint(Tasks)))
          (substitution (substitute (act current-task))))
     (return-from update-action-values (values Plan Tasks current-state substitution T0) )) 
   )
 
 ;; unify methods and update state from nonprimitive task
 (defun update-nonprimitive-task (Plan methods current-task substitution Tasks current-state T0)
   (let (Methods-lst (method-unifier methods current-task current-state)) 
        
        (cond ((eq Methods-lst nil) (return-from update-nonprimitive-task nil)) ; if M = empty then return nil to resolve task
              (t  (shop2-operator (multiple-value-bind (plan tasks state substitution T0) ; else, from begin
                    (update-nonprimitive-values Methods-lst current-state Plan Tasks substitution))) ; nondeterministically choose a pair (m, θ) ∈ M
                                   ) 
                                       ))
   )
 
 (defun update-nonprimitive-values (Actions-lst current-state Plan Tasks substitution)
   (let* ((ms (car Methods-lst))
          (Tasks (remove-task current-task Tasks)) ; modify T by removing t, adding sub(m), constraining each task     
          (subm (hddl-method-subtasks (car ms))) ; in sub(m) to precede the tasks that t preceded
          (substitution (substitute (cdr ms))))
     (if (not (null subm)) (setq T0 constraint(subm))
         (setq T0 constraint(Tasks)))
     (return-from update-nonprimitive-values (values Plan Tasks current-state substitution T0))
   )
   
; constraint T to T0 
;; hier muss also für alle t in T prüfen, dass nicht eine andere task vorher ausgeführt werden muss
;; prüft, ob die constraints leer sind oder nicht
;; bei den Methoden, dass die constraints bei den Subtasks eingefügt werden müssen
(defun constraint (tasks)
   (setq T0 nil)
  (dotimes (curr-num (length tasks))
     (if (not (null (hddl-task-constraints (nth curr-num tasks)))) (setq T0 (cons (nth curr-num tasks) T0)) nil))
  (return-from constraint T0)
)
;------------------------------------------------------------------------------------
;;Alisa: unifier sollte am besten 
;;1. alle actionen sammeln, die den gleichen Namen haben wie die task
;;2. prüfen, ob die gleiche Anzahl Parameter vorliegt (Länge der Liste)
;;3. prüfen, ob die Parameter den selben Typ haben (Reihenfolge in Parameterliste egal)
;;4. die Parameter der task als theta ausgeben
;; Ergebnis wäre dann eine Liste mit ((action . theta)(action . theta)...)
;; hier würde ich also auch gar nicht jede action einzeln übergeben, sondern gleich alle auf einmal

(defun action-unifier(actions task)
  (let* ((same_name)
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
           (if (find type a-paramtypes)
         (remove type a-paramtypes)))
         (if (null a-paramtypes) ;; testen das task-parametertypes auch null
       (push (cons a taskparams) unified_actions))))
   (reverse unified_actions)))

;; für alle eingegebenen Aktionen prüfe, ob die preconditions einer Aktion im aktuellen Status erfüllt sind, falls ja, füge sie in Ergebnisliste ein
;; preconditions sind dann erfüllt, wenn sie im aktuellen Status enthalten sind
  ;; gibt es auch negative preconditions?
  ;;Achtung: actions haben jetzt die Form (a. theta)
  (defun action-satisfyp (actions current-state)
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
;-------------------------------------------------------------------------
   ; (defun action-satisfyp (actions current-state)
 ;    (let ((satisfying_actions))  
 ;      (dotimes (curr-num (length actions))
 ;    (let ((preconditions (hddl-action-preconditions (nth curr-num actions)))
 ;        (satisfies T))  ;;set satyisfies to true at first and let it be disproven for every action
 ;    ; (return-from action-satisfyp T)
 ;    (dotimes (curr-p (length preconditions))
 ;      (if (not (find (nth curr-p preconditions) current-state))
 ;    (setq satisfies nil)))
 ;    (if satisfies
 ;        (push a satisfying_actions)))
 ;  (reverse satisfying_actions))
 ;      (return-from action-satisfyp (reverse satisfying_actions))
 ;      ))  

;----------------------------------------------------------------------------
 ; M ← {(m, θ) : m is an instance of a method in D, θ unifies {head(m), t},
            ; pre(m) is true in s, and m and θ are as general as possible}
      ;; Alisa: auch hier würde ich erst die Zeile mit unifier und danach mit satisfyp ausführen
; return a list of methods to filter the precondition of methods fitting the current-state

;;;parameters-binding 
;; output: (?V TRUCK-0 VEHICLE)) (?L2 CITY-LOC-0 LOCATION)) as theta
;; pass to unifier
;; operator as method
(defun parameters-binding (operator task)
  (let ((op-param (hddl-method-parameters operator)) ; ((?V VEHICLE) (?L2 LOCATION))
        (task-params (hddl-task-parameters task)) ;(TRUCK-0 CITY-LOC-0)
        (types (hddl-problem-objects *problem*)) ;((CITY-LOC-2 LOCATION) (CITY-LOC-1 LOCATION) (CITY-LOC-0 LOCATION) (TRUCK-0 VEHICLE)
        (binding-list nil))
    (dotimes (i (length task-params))
          (setq one-set (assoc (nth i task-params) types)) ;(TRUCK-0 VEHICLE)
          (setq reversed-op-param (mapcar #'reverse op-param))
          (setq binding-list (cons (cons (cadr (assoc (cadr one-set) reversed-op-param)) one-set) binding-list))
          )
    (return-from parameters-binding binding-list) ; (?V TRUCK-0 VEHICLE)) (?L2 CITY-LOC-0 LOCATION))
    )
  )

;;; method-unifier
;; prueft die Gleichheit der Namen und der Parameters
;; prueft die parameters, ob die Anzahl der Parameters gleich
;;                      , ob Typen der Parameters gleich     
(defun method-unifier-p (method task)
  (let ((m-name (hddl-method-name method))
        (m-params (hddl-method-parameters method))
        (task-name (hddl-task-name task))
        (task-params (hddl-task-parameters task))
        (Methods-lst nil)
        )
      (cond ((eq task-name m-name) (return-from method-unifier T))
            ((same-parameters-p task-params m-params) (return-from method-unifier T)))
            (t (return nil)))
  )
(defun same-parameters-p (task-params m-params) ; (TRUCK-0 CITY-LOC-0) / ((?V VEHICLE) (?L2 LOCATION))
  (let ((types (hddl-problem-objects *problem*))
        (task-type nil)) ;((CITY-LOC-2 LOCATION) (CITY-LOC-1 LOCATION) (CITY-LOC-0 LOCATION) (TRUCK-0 VEHICLE)
  (dotimes (i task-params)
      (setq one-set (assoc (nth i task-params) types)) ;(TRUCK-0 VEHICLE)
      (setq task-type (cons (cadr one-set) task-type))
    )
  (setq m-type (cadr (apply #'mapcar #'list m-params))
  (if (eql task-type m-type) (retrun-from same-parameters-p T) nil)
  )
  )

;; method-satisfyp 
; ; return a unified list such as {(m . theta)...} 
(defun method-satisfyp (methods curr-state)
  
  )
;--------------------------------------------------
; (defun method-satisfyp1 (m state)
;   (let ((method-satisfied nil))
;     (loop
;       (cond ((null m) (return method-satisfied))
;           ((not (null (find ((hddl-method-preconditions (car m)) state)))) (setq method-satisfied (push (car m) method-satisfied))))
;       (setq m (cdr m))
;     )))
;----------------------------------------------------

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
;------------------------------------------------------------
;;; substitution with unifier and replace the required variables within each update
;; unify variables 
(defun substitute (action task)
  
)
;-------------------------------------------------------------
; remove task from task list
(defun remove-task (current-task tasks)
  (setq tasks (remove current-task tasks))
  (return-from remove-task tasks)
  )
;-------------------------------------------------------------
;; failure handling
; (define-condition failure-handling (err)
;   ((actual-input :initarg :actual-input
;                  :reader actual-input
;                  :initform nil))
;   (:report (lambda (condition stream)
;              (format stream "~a is null!"
;                      (actual-input condition)))))
;-------------------------------------------------------------
;;; illustration from thesis
#|
The first case is if t is primitive, i.e., 
if t can be accomplished directly using an action(i.e., an instance of a planning operator).  
In this case, SHOP2 finds an action a that matches t and whose preconditions are satisfied in s, and applies a to s
(if no such action exists, then this branch of the search space fails).

The second case is where t is compound, \
i.e., a method needs to be applied to t to decompose it into subtasks. 
In this case, SHOP2 nondeterministically chooses a method instance m that will decompose t into subtasks 
(if no such method instance exists, then this branch of the search space fails).

If there is a solution plan that involves m, then the actions in P will be the leaf nodes of a decomposition tree D P 
such as the tree shown in Figure 2. The precondition formula pre(m) must be true in the state that 
immediately precedes the first action a in D P that is a descendant of m. 
In order to ensure that pre(m) is true in the correct state, SHOP2 needs to generate the 
leftmost branch of D all the way down to the bottom, and evaluate pre(m)in the state just before a. 
The last three lines of the loop ensure that this will happen, 
by telling SHOP2 that if the current method m has any subtasks, SHOP2 should generate one of those subtasks 
before generating any other subtasks in the task network
|#
;-------------------------------------------------------------




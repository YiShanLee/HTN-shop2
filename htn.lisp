;-------------------------------------------
#|shop2.todo
## TODO:
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
; (defun read-input ()
(unless domain-path (setq domain-path "domain.hddl" "problem.hddl"))
  (princ "Enter domain file")
  (setq domain-path (string(read)))
  (princ "Enter problem file")
  (setq problem-path (string(read)))
  (fetch-initial-state domain-path problem-path)
  )
;; global variables from domain knowledge and problem.hddl
(defun fetch-initial-state (domain-file problem-file)
  (defparameter *domain* (read-hddl-domain domain-file))
  (defparameter *problem* (read-hddl-problem problem-file))
  (defparameter *depth* 0)
  )
;--------------------------------------------
;; main method for first layer of shop2
(defun shop2-plan(*depth* &optional plan1 tasks1 state1 substitution1 T0)
  (let* (
   (tasks (if (eq depth 0)(hddl-problem-tasks *problem*) tasks1))
   (current-state (if (eq depth 0)(hddl-problem-init-status *problem*) state1))
   (Plan (if (eq depth 0) nil plan1));;P = the empty plan
   (methods (hddl-domain-methods *domain*)) 
   (actions (hddl-domain-actions *domain*))
   ; (T0 (constraint(tasks)));;T0 ← {t ∈ T : no other task in T is constrained to precede t}
   (substitution (if (eq depth 0) nil substitution1))
   (depth (incf depth))
   ) 
   ; (print tasks)
   ; (print current-state)
   (setq T0 (constraint tasks)) ;; **remove the return value of T0, on the other hand, renew T0 direct through tasks
    (do* ((tasks T0 (rest tasks))
          (current-task (first tasks) (resolve-task depth domain problem current-state Plan methods actions T0 current-task substitution Tasks)))
         ((null tasks)(planner-output Plan)))
    )
  )
  
;; second layer of shop2 
;; check primitive and restore the plan 
(defun resolve-task (depth domain problem current-state Plan methods actions T0 current-task substitution Tasks) 
  (if (primitivep current-task)
    ; primitive (true)
    (update-primitive-task depth Plan actions current-task substitution Tasks current-state T0) ;todo handle return value from function
    ; non-primitive (nil)  
    (update-nonprimitive-task depth Plan methods current-task substitution Tasks current-state T0) ;todo handle return value from function
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
 (defun update-primitive-task (depth Plan actions current-task substitution Tasks current-state T0)
   (let ((Actions-lst (action-satisfier actions current-task current-state)))
                      (cond ((eq Actions-lst nil) (return-from update-primitive-task nil))
                                         (t (shop2-plan (multiple-value-bind (depth plan tasks state substitution T0) ;return updated list
                                              (update-action-values depth Actions-lst current-state Plan Tasks substitution))
                                                 ))
                                         ))
   )
 
 (defun update-action-values (depth actions state Plan Tasks substitution)
   (let* ((act (car Action-lst)) 
           (theta (cdr act)))
          (setq act (act-substitute theta action)
          (current-state (modify-status current-state act)) 
          (Plan (cons (car act) Plan))
          (Tasks (remove-task current-task Tasks)) ;TODO function ;;Alisa: zusätzlich müssten wir dann auch schauen, dass wir die task aus allen task constraint-Listen löschen!
          (T0 (constraint(Tasks)))
          (substitution (substitute (act current-task))))
     (return-from update-action-values (values depth Plan Tasks current-state substitution T0) )) 
   )
 
 ;; unify methods and update state from nonprimitive task
 (defun update-nonprimitive-task (depth Plan methods current-task substitution Tasks current-state T0)
   (let (Methods-lst (method-satisfier methods current-task)) ; {(m . theta)...}  
        (cond ((eq Methods-lst nil) (return-from update-nonprimitive-task nil)) ; if M = empty then return nil to resolve task
              (t  (shop2-plan (multiple-value-bind (depth plan tasks state substitution T0) ; else, from begin
                    (update-nonprimitive-values depth Methods-lst current-state Plan Tasks substitution))) ; nondeterministically choose a pair (m, θ) ∈ M
                                   ) 
                                       ))
   )
 
 (defun update-nonprimitive-values (depth Methods-lst current-state Plan Tasks substitution)
   (let* ((ms (first Methods-lst))
          (Tasks (remove-task current-task Tasks)) ; modify T by removing t, adding sub(m), constraining each task     
          (subm (hddl-method-subtasks (car ms))) ; in sub(m) to precede the tasks that t preceded
          (substitution (cadr ms)))
     (if (not (null subm)) (setq T0 constraint(subm))
         (setq T0 constraint(Tasks)))
     (return-from update-nonprimitive-values (values depth Plan Tasks current-state substitution T0))
   )
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

; A ← {(a, θ) : a is a ground instance of an operator in D, 
;        θ is a substitution that unifies {head(a), t von problem}, 
;        and (s von problem) satisfies a’s preconditions}
; (defun action-satisfier (actions current-task current-state)
;   (let ((actions-satisfied nil))
;     (dotimes (curr-action (length actions))
;       ()
;       )
;     )
; )
;----------------------------------------------------------------------

;;;parameters-binding (theta-binding)
;; input: task & operator (as action or method)
;; output: (?V TRUCK-0 VEHICLE)) (?L2 CITY-LOC-0 LOCATION)) as theta
;; pass to method-satisfier
(defun parameters-binding (operator task)
  (let ((op-param (if (eql (type-of operator) 'HDDL-ACTION) (hddl-action-parameters operator) (hddl-method-parameters operator))) ; ((?V VEHICLE) (?L2 LOCATION))
        (task-params (hddl-task-parameters task)) ;(TRUCK-0 CITY-LOC-0)
        (types (hddl-problem-objects *problem*)) ;((CITY-LOC-2 LOCATION) (CITY-LOC-1 LOCATION) (CITY-LOC-0 LOCATION) (TRUCK-0 VEHICLE)
        (binding-list nil))
    (dotimes (i (length task-params))
          (setq one-set (assoc (nth i task-params) types)) ;(TRUCK-0 VEHICLE)
          (setq reversed-op-param (mapcar #'reverse op-param))
          (setq binding-list (cons (cons (cadr (assoc (cadr one-set) reversed-op-param)) one-set) binding-list))
          )
    (if (eql (type-of operator) 'HDDL-ACTION)
        (format t "step-> parameters-binding with action ~S" (list binding-list))
        (format t "step-> parameters-binding with method ~S" (list binding-list))
        )
    (return-from parameters-binding (list binding-list)) ; ((?V TRUCK-0 VEHICLE) (?L2 CITY-LOC-0 LOCATION))
    )
  )

;;; operator-unifier-p
;; input: task & operator (as action or method)
;; output: True/ False
;; check name of tasks from action and current-task the same :: todo
;;       each parameter identical from operator (as action or method) and task 
;; pass to next function as method-satisfier 
(defun operator-unifier-p (operator task)
  (let ((op-task-name (if (eql (type-of operator) 'HDDL-ACTION) (hddl-action-name operator) (first (hddl-method-task operator))))
        (op-params (if (eql (type-of operator) 'HDDL-ACTION) (hddl-action-parameters operator)(hddl-method-parameters operator)))
        (task-name (hddl-task-name task))
        (task-params (hddl-task-parameters task))
        )
      (cond ((and (eql (type-of operator) 'HDDL-ACTION) (identical-parameters-p task-params op-params)) (return-from operator-unifier-p T))
            ((and (eql (type-of operator) 'HDDL-METHOD) (eq task-name op-task-name) (identical-parameters-p task-params op-params)) (return-from operator-unifier-p T))
            (t (return-from operator-unifier-p nil)))
  ))
;;; identical-parameters-p
;; input: task parameters & operator parameters (operator as action or method)
;; output: True / False
;; pass to next function as operator-unifier-p
(defun identical-parameters-p (task-params op-params) ; (TRUCK-0 CITY-LOC-0) / ((?V VEHICLE) (?L2 LOCATION))
  (let ((types (hddl-problem-objects *problem*))
        (task-type nil)) ;((CITY-LOC-2 LOCATION) (CITY-LOC-1 LOCATION) (CITY-LOC-0 LOCATION) (TRUCK-0 VEHICLE)
  (dotimes (i (length task-params))
      (setq one-set (assoc (nth i task-params) types)) ;(TRUCK-0 VEHICLE)
      (setq task-type (cons (cadr one-set) task-type))
    )
  (setq op-type (cadr (apply #'mapcar #'list op-params))) ;(VEHICLE LOCATION)
  (setq op-type (sort op-type #'string<)) ; sort the ordering to compare string
  (setq task-type (sort task-type #'string<))  
  (cond
    ((not (eq (length task-type) (length op-type))) (return-from identical-parameters-p nil)) ; quick check and return 
    ((equal task-type op-type) (return-from identical-parameters-p T))
    (t (return-from identical-parameters-p nil)))
  )
  )

;; method-satisfier
;; pre(m) to be seen as deprecated tuple
;; output: {(method . theta)...}
(defun method-satisfier (methods task)
  (let ((methods-satisfied nil))
    (dotimes (i (length methods))
      (setq method (nth i methods))
      (if (operator-unifier-p method task) (setq methods-satisfied (cons (cons method (parameters-binding method task)) methods-satisfied)))
    ) (return-from method-satisfier (values methods-satisfied))
  )
)

;; Alisa: hier würde ich denke ich (defun modify (status action) schreiben, damit wir den aktuellen
;; Status auch mitübergeben, den wir dann verändern
;; dafür würde ich 
;; 1. durch die current-status-Liste iterieren und für alle negativen Effekte der Aktion herauslöschen
;; 2. alle positiven Effekte der Aktion hinzufügen, und das als neuen Status ausgeben lassen:
  
(defun modify-status (status action)
(let 
  ((addeffect (hddl-action-pos-effects action))
  (deleffect (hddl-action-neg-effects action))
  (new-status nil))
    (loop for e in status do
      (if (not (find e deleffect))
          (push e new-status)))
    (setq new-status (cons new-status addeffect))
  new-status))


;; für alle eingegebenen Aktionen prüfe, ob die preconditions einer Aktion im aktuellen Status erfüllt sind, falls ja, füge sie in Ergebnisliste ein
;; preconditions sind dann erfüllt, wenn sie im aktuellen Status enthalten sind
  ;; gibt es auch negative preconditions?
  ;;Achtung: actions haben jetzt die Form (a. theta)
   (defun action-satisfyp (actions current-state)
    (let ((satisfying_actions))  
      (dotimes (curr-num (length actions))
    (let ((preconditions (hddl-action-preconditions (nth curr-num actions)))
        (satisfies T))  ;;set satyisfies to true at first and let it be disproven for every action
    ; (return-from action-satisfyp T)
    (dotimes (curr-p (length preconditions))
      (if (not (find (nth curr-p preconditions) current-state))
    (setq satisfies nil)))
    (if satisfies
        (push a satisfying_actions)))
    (reverse satisfying_actions))
      (return-from action-satisfyp (reverse satisfying_actions))
      ))  


;------------------------------------------------------------
;;; substitution with unifier and replace the required variables within each update
;; unify variables 

;;substitution: muss alle Variablen in action entsprechend Theta substituieren
;; reminder: elements of theta: (?v . (truck . vehicle)) -> (variable.(entity.type))

(defun act-substitute(theta action)
  (let* ((a-params (hddl-action-parameters action))
  (a-pos-effects (hddl-action-pos-effects action))
  (a-neg-effects (hddl-action-neg-effects action)))
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
     
      
     

;-------------------------------------------------------------
; remove task from task list
(defun remove-task (current-task tasks)
  (setq tasks (remove current-task tasks))
  (return-from remove-task tasks)
  )
;-------------------------------------------------------------
; ; failure handling
; (define-condition failure-handling (err)
;   ((actual-input :initarg :actual-input
;                  :reader actual-input
;                  :initform nil))
;   (:report (lambda (condition stream)
;              (format stream "~a is null!"
;                      (actual-input condition)))))
;-------------------------------------------------------------
(defun planner-output (plan)
  (format t "Shop2-operator finds current possible plan as ~S" plan))
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




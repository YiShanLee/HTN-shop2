;-------------------------------------------
#|shop2.todo
## TODO:
1. while loop inside shop2-plan
2. debug action unifier and satisfy & primitive task & non primitive task functions
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
  (defparameter *curren-state* nil)
  (defparameter *T0* nil)
  (defparameter *Plan* '())
  (defparameter *actions*  (hddl-domain-actions *domain*))
  (defparameter *methods*  (hddl-domain-methods *domain*))
  (defparameter *current-task* nil)
  (defparameter *current-status* nil)
  (defparameter *Tasks* (hddl-problem-tasks *problem*))
  )
;--------------------------------------------
;; main method for first layer of shop2
; (defun shop2-plan(&optional plan1 tasks1 state1 substitution1 T0)
;   (let* (
;    (tasks (if (eq depth 0)(hddl-problem-tasks *problem*) tasks1))
;    (current-state (if (eq depth 0)(hddl-problem-init-status *problem*) state1))
;    (Plan (if (eq depth 0) nil plan1));;P = the empty plan
;    (methods (hddl-domain-methods *domain*)) 
;    (actions (hddl-domain-actions *domain*))
;    ; (T0 (constraint(tasks)));;T0 ← {t ∈ T : no other task in T is constrained to precede t}
;    (substitution (if (eq depth 0) nil substitution1))
;    (depth (incf depth))
;    ) 
;    (setq T0 (constraint tasks)) ;; **remove the return value of T0, on the other hand, renew T0 direct through tasks
;     (do* ((tasks T0 (rest tasks))
;           (current-task (first tasks) (resolve-task depth domain problem current-state Plan methods actions T0 current-task substitution Tasks)))
;          ((null tasks)(planner-output Plan)))
;     )
;   )
(defun shop2-plan (&optional plan tasks state theta))
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
 (let ((Actions-lst (action-satisfier actions current-task current-state)))
                    (cond ((eq Actions-lst nil) (return-from update-primitive-task nil))
                                       (t (shop2-plan (multiple-value-bind (plan tasks state substitution T0) ;return updated list
                                            (update-action-values Actions-lst current-state Plan Tasks substitution))
                                               ))
                                       ))
 )
 
(defun update-action-values (Action-lst)
 (let* ((action (first Action-lst)) 
        (theta (second action))
        (act (action-substitute theta action)) ; (AT TRUCK-0 CITY-LOC-2)
        (Tasks (remove-task current-task Tasks)) 
        )
   (modify-state action) ;; add pos-effect & delete neg-effect
   (setq *Plan* (cons act *Plan*))
   (setq *Tasks* )
   (return-from update-action-values (values Plan Tasks current-state substitution T0))
   ) 
)
 
 ;; unify methods and update state from nonprimitive task
(defun update-nonprimitive-task (Plan methods current-task substitution Tasks current-state T0)
 (let (Methods-lst (method-satisfier methods current-task)) ; {(m . theta)...}  
      (cond ((eq Methods-lst nil) (return-from update-nonprimitive-task nil)) ; if M = empty then return nil to resolve task
            (t  (shop2-plan (multiple-value-bind (plan tasks state substitution T0) ; else, from begin
                  (update-nonprimitive-values Methods-lst current-state Plan Tasks substitution))) ; nondeterministically choose a pair (m, θ) ∈ M
                                 ) 
                                     ))
 )
 
 (defun update-nonprimitive-values (Methods-lst current-state Plan Tasks substitution)
   (let* ((ms (first Methods-lst))
          (Tasks (remove-task current-task Tasks)) ; modify T by removing t, adding sub(m), constraining each task     
          (subm (hddl-method-subtasks (car ms))) ; in sub(m) to precede the tasks that t preceded
          (substitution (cadr ms)))
     (if (null subm) (setq T0 constraint(Tasks))
         (setq T0 constraint(subm))
         )
     (return-from update-nonprimitive-values (values Plan Tasks current-state substitution T0))
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
;--------------------------------------------------------------
;;; state CRUD
;; get-initial-state
;; init a hash table as a dictionary for current-state 
;; input: current-state
;; output: current-state as hash-table
(defun get-initial-state (current-state) #|((ROAD CITY-LOC-0 CITY-LOC-1) (ROAD CITY-LOC-1 CITY-LOC-0) (ROAD CITY-LOC-1 CITY-LOC-2) (ROAD CITY-LOC-2 CITY-LOC-1)(AT TRUCK-0 CITY-LOC-2))|#
  (let ((problem-types (hddl-problem-objects *problem*)) ;((CITY-LOC-2 LOCATION) (CITY-LOC-1 LOCATION) (CITY-LOC-0 LOCATION) (TRUCK-0 VEHICLE))
        (state-type (delete-duplicates (car (apply #'mapcar #'list current-state)))) ; (ROAD AT)
        (current-state-list (make-hash-table))
        (lis nil)) 
    (dotimes (i (length state-type))
     (setf (gethash (nth i state-type) current-state-list) (remove nil (mapcar #'(lambda(c) (if (eq (first c) (nth i state-type)) (rest c))) current-state)))
     (setq lis nil)
     )
    (setq *current-state* current-state-list )
    ) 
  )
;; get-current-state
;; check if global variable not null, return *current-state*
; else initialize *current-state*
;; output: *current-state*
(defun get-current-state ()
  (if (null *current-state*) (get-initial-state (hddl-problem-init-status *problem*))
      *current-state*)
  )
;; add-state
(defun add-state (pos-effects) ; (AT ?V ?L2)
  (dotimes (i (length pos-effects))
    (setq operator (nth i pos-effects))
    (if (null (gethash (first operator) *current-state*))
        (setf (gethash (first operator)) (rest operator))
        (let ((value1 (gethash (first operator) *current-state*)))
              (setf (gethash (first operator) *current-state*) (cons (rest operator) value1)))))
  )
;; delete-state
(defun delete-state (neg-effects) ; (AT ?V ?L2)
  (dotimes (i (length neg-effects))
    (setq operator (nth i neg-effects))
    (if (gethash (first operator) *current-state*)
        (let((value1 (gethash (first operator) *current-state*)))
            (setf (gethash (first operator) *current-state*) (remove (rest operator) value1))
          )
        ))
)
 ;; modify-state
(defun modify-state (action)
   (let ((pos-effects (hddl-action-pos-effects action))
         (neg-effects (hddl-action-neg-effects action)))
     (if neg-effects 
         (delete-state neg-effects))
     (if pos-effects 
         (add-state neg-effects))
     )
   ) 
 
 ;; find-state-p
 ;; input: (AT TRUCK-0 CITY-LOC-2)
 ;; output: T or NIL
(defun find-state-p (effect) ; (AT ?V ?L2)
    (if (mapcar #'(lambda(unit) (equal (rest effect) unit)) (gethash (first effect) *current-state*)) T nil)
  )      
;------------------------------------------------------------------------------
; A ← {(a, θ) : a is a ground instance of an operator in D, 
;        θ is a substitution that unifies {head(a), t von problem}, 
;        and (s von problem) satisfies a’s preconditions}

;;;parameters-binding (theta-binding)
;; input: task & operator (as action or method)
;; output: ((?V TRUCK-0 VEHICLE)) (?L2 CITY-LOC-0 LOCATION)) as theta
;; pass to action-satisfier or method-satisfier
; ----- todo method
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
;; pass to next function as actino-satisfier or method-satisfier 
(defun operator-unifier-p (operator task)
  (let ((op-task-name (if (eql (type-of operator) 'HDDL-ACTION) (hddl-action-name operator) (first (hddl-method-task operator))))
        (op-params (if (eql (type-of operator) 'HDDL-ACTION) (hddl-action-parameters operator) (rest (hddl-method-task operator))))
        (task-name (hddl-task-name task))
        (task-params (hddl-task-parameters task))
        )
      (cond ((and (eql (type-of operator) 'HDDL-ACTION) (a-identical-parameters-p task-params op-params)) (return-from operator-unifier-p T))
            ((and (eql (type-of operator) 'HDDL-METHOD) (eq task-name op-task-name) (m-identical-parameters-p task-params op-params)) (return-from operator-unifier-p T))
            (t (return-from operator-unifier-p nil)))
  ))
;;; a-identical-parameters-p
;; input: task parameters & action parameters (operator as action or method)
;; output: True / False
;; pass to next function as operator-unifier-p
(defun a-identical-parameters-p (task-params op-params) ; (TRUCK-0 CITY-LOC-0) / ((?V VEHICLE) (?L2 LOCATION))
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
    ((not (eq (length task-type) (length op-type))) (return-from a-identical-parameters-p nil)) ; quick check and return 
    ((equal task-type op-type) (return-from a-identical-parameters-p T))
    (t (return-from a-identical-parameters-p nil)))
  )
)

;;; m-identical-parameters-p
;; check the value of current-task-parameters & method-task-parameters identical
;; input: task parameters & method parameters
;; output: True / False
;; pass to next function as operator-unifier-p
(defun m-identical-parameters-p (task-params m-task-params) ; (TRUCK-0 CITY-LOC-0) / (?v ?l2)
  (let ((types (hddl-problem-objects *problem*)) ; ((CITY-LOC-2 LOCATION) (CITY-LOC-1 LOCATION) (CITY-LOC-0 LOCATION) (TRUCK-0 VEHICLE))
        (task-type nil))
    (dotimes (i (length task-params))
      (setq one-set (assoc (nth i task-params) types)) ;(TRUCK-0 VEHICLE)
      (setq task-type (cons (char (symbol-name (cadr one-set)) 0) task-type)) ; ("v")
      )
    (setq task-type (sort task-type #'string<))  ;("L" "V")
    (setq m-task-params (mapcar #'(lambda(c) (char c 1)) (mapcar #'symbol-name m-task-params))) ;("V" "L")
    (setq m-task-params (sort m-task-params #'string<))  ;("L" "V")
  (cond 
    ((not (eq (length task-type) (length m-task-params))) (return-from m-identical-parameters-p nil)) ; quick check and return 
    ((equal task-type m-task-params) (return-from m-identical-parameters-p T))
    (t (return-from m-identical-parameters-p nil)))
    )
  )

;; action-unifier
;; input: actions
;; output: {(a . theta)...}
;; unify the parameters of actions with task 
#|((#S(HDDL-ACTION
     :NAME NOOP
     :PARAMETERS ((?V VEHICLE) (?L2 LOCATION))
     :PRECONDITIONS (AT ?V ?L2)
     :NEG-EFFECTS NIL
     :POS-EFFECTS NIL)
  ((?L2 CITY-LOC-0 LOCATION) (?V TRUCK-0 VEHICLE))))|#
(defun action-unifier (actions current-task)
  (let ((actions-satisfied nil))
    (dotimes (i (length actions))
      (setq action (nth i actions))
      (if (operator-unifier-p action current-task) 
          (setq actions-satisfied (cons (cons action (parameters-binding action current-task)) actions-satisfied))
          ) 
      ) (return-from action-unifier actions-satisfied)
    )
  )

;; action-satisfier
;; input:
;; output: actions list (unified with parameters and action's precondition satisfied with current-state)
;; action's precondition corrspond to current-state
;; leave out the satifying action list filtered by current-status
(defun action-satisfier (actions current-task)
  (let ((actions-satisfied nil)
        (actions (action-unifier actions current-task)))
    (dotimes (i (length actions))
      (let* ((action (nth i actions))
            (action-precondition (hddl-action-preconditions (first action))) ;(AT ?V ?L2)
            (action-params (second action)) ; ((?V TRUCK-0 VEHICLE)) (?L2 CITY-LOC-0 LOCATION))
              ;; binding parameters to precondition
            (precondtion nil) 
            )
        (setq precondition (cons (first action-precondition) (mapcar 'second action-params))) ; (AT TRUCK-0 CITY-LOC-0)
        ;; check the precondition in current-state
        (if (find-state-p precondition) (setq actions-satisfied (cons action actions-satisfied)))
      )
    )
  (return-from action-satisfier actions-satisfied)
  #|((#S(HDDL-ACTION
     :NAME NOOP
     :PARAMETERS ((?V VEHICLE) (?L2 LOCATION))
     :PRECONDITIONS (AT ?V ?L2)
     :NEG-EFFECTS NIL
     :POS-EFFECTS NIL)
  ((?L2 CITY-LOC-0 LOCATION) (?V TRUCK-0 VEHICLE))))|#
  )
)
;; method-satisfier
;; pre(m) to be seen as deprecated tuple
;; output: {(method . theta)...}
;--- todo parameters binding not in this block?
(defun method-satisfier (methods task)
  (let ((methods-satisfied nil))
    (dotimes (i (length methods))
      (setq method (nth i methods))
      (if (operator-unifier-p method task) (setq methods-satisfied (cons (cons method (parameters-binding method task)) methods-satisfied)))
    ) (return-from method-satisfier (values methods-satisfied))
  )
)
;----------------------------------------------------------------------------------------------
;;; substitution with unifier and replace the required variables within each update
;; unify variables 

;;substitution: muss alle Variablen in action entsprechend Theta substituieren
;; reminder: elements of theta: (?v . (truck . vehicle)) -> (variable.(entity.type))

(defun action-substitute(theta action)
  
)      
     

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
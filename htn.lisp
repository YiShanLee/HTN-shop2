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
  (defparameter *T0* nil)
  (defparameter *Plan* '())
  (defparameter *actions*  (hddl-domain-actions *domain*))
  (defparameter *methods*  (hddl-domain-methods *domain*))
  (defparameter *current-task* nil)
  (defparameter *current-status* nil)
  (defparameter *Tasks* (hddl-problem-tasks *problem*))
  (defparameter *theta* nil)
  )
;--------------------------------------------
;; main method for first layer of shop2
(defun shop2-plan (&optional plan tasks state theta)
  (setq *T0* (constraint *Tasks*))
  (loop while (not (null *T0*)) do
    (if (null *Tasks*) (return *Plan*))
    (setq *current-task* (car *T0*)
          *T0* (cdr *T0*))
    (resolve-task)
  )
  )  
;; second layer of shop2 
;; check primitive and restore the plan 
(defun resolve-task () 
  (if (primitivep *current-task*)
    ; primitive (true)
    (update-primitive-task) ;todo handle return value from function
    ; non-primitive (nil)  
    (update-nonprimitive-task) ;todo handle return value from function
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
(defun update-primitive-task ()
 (let ((Actions-lst (action-satisfier *actions* *current-task* *current-status*)))
                    (cond ((eq Actions-lst nil) (return-from update-primitive-task nil))
                          (t (update-action-values (nth (random (length Actions-lst)) Actions-lst)))
                    )
  )
)
 ; nondeterministically choose a pair (a, θ) ∈ A  ; name (theta)
 ;      modify s by deleting del(a) and adding add(a)
 ;      append a to P
 ;      modify T by removing t and applying θ
      
 ;      !wenn tasks aus T enfernt werden darauf achten, dass sie auch aus allen *task-constraints*-Listen gelöscht wird! 
 ;      T0 ← {t ∈ T : no task in T is constrained to precede t}
 
(defun update-action-values (action)
 (let* ((act (action-substitute action)) ; (AT TRUCK-0 CITY-LOC-2)
        )
         (modify-status action) ;; add pos-effect & delete neg-effect for current-status
         (setq *Plan* (append *Plan* act))
         (setq *Tasks* (remove-task *current-task* *Tasks*))
         (setq *T0* (constraint *Tasks*))
         (setq *theta* (second action))
         (return-from update-action-values (values *Plan* *Tasks* *current-status* *theta*))
 ) 
)
;; unify methods and update state from nonprimitive task
(defun update-nonprimitive-task ()
 (let (Methods-lst (method-satisfier *methods* *current-task*)) ; {(m . theta)...}  
      (cond ((eq Methods-lst nil) (return-from update-nonprimitive-task nil)) ; if M = empty then return nil to resolve task
            (t  (update-nonprimitive-values (nth (random (length Methods-lst)) Methods-lst)))) ; nondeterministically choose a pair (m, θ) ∈ M (random choose)
  )
)
 
(defun update-nonprimitive-values (method)
 (let* (subm (hddl-method-subtasks method))
   
   (setq *Tasks* (remove-task *current-task* *Tasks*))
   (if (not (null subm)) 
       (constraint subm)
       (constraint Tasks))
   (setq *theta* (task-substitute))
   (return-from update-nonprimitive-values (values *Plan* *Tasks* *current-status* *theta* *T0*))
 )
)
   
; constraint T to T0 
;; hier muss also für alle t in T prüfen, dass nicht eine andere task vorher ausgeführt werden muss
;; prüft, ob die constraints leer sind oder nicht
;; bei den Methoden, dass die constraints bei den Subtasks eingefügt werden müssen
(defun constraint (tasks)
   (setq *T0* nil)
  (dotimes (curr-num (length tasks))
     (if (not (null (hddl-task-constraints (nth curr-num tasks)))) (setq *T0* (cons (nth curr-num tasks) T0)) nil))
  (return-from constraint *T0*)
)
;--------------------------------------------------------------
;;; status CRUD
;; get-initial-status
;; init a hash table as a dictionary for current-status
;; input: current-status
;; output: current-status as hash-table
(defun get-initial-status () 
 (let ((problem-types (hddl-problem-objects *problem*)) ;((CITY-LOC-2 LOCATION) (CITY-LOC-1 LOCATION) (CITY-LOC-0 LOCATION) (TRUCK-0 VEHICLE))
      (current-status (hddl-problem-init-status *problem*)) #|((ROAD CITY-LOC-0 CITY-LOC-1) (ROAD CITY-LOC-1 CITY-LOC-0) (ROAD CITY-LOC-1 CITY-LOC-2) (ROAD CITY-LOC-2 CITY-LOC-1)(AT TRUCK-0 CITY-LOC-2))|#
      (state-type (delete-duplicates (car (apply #'mapcar #'list (hddl-problem-init-status *problem*))))) ; (ROAD AT)
      (current-status-list (make-hash-table))
      (lis nil)) 
  (dotimes (i (length state-type))
   (setf (gethash (nth i state-type) current-status-list) (remove nil (mapcar #'(lambda(c) (if (eq (first c) (nth i state-type)) (rest c))) current-status)))
   (setq lis nil)
   )
  (setq *current-status* current-status-list )
  ) 
)
;; get-current-status
;; check if global variable not null, return *current-status*
; else initialize *current-status*
;; output: *current-status*
(defun get-current-status ()
  (if (null *current-status*) (get-initial-status)
      *current-status*)
  )
;; add-state
(defun add-state (pos-effects) ; (AT ?V ?L2)
  (dotimes (i (length pos-effects))
    (setq operator (nth i pos-effects))
    (if (null (gethash (first operator) *current-status*))
        (setf (gethash (first operator)) (rest operator))
        (let ((value1 (gethash (first operator) *current-status*)))
              (setf (gethash (first operator) *current-status*) (cons (rest operator) value1)))))
  )
;; delete-state
(defun delete-state (neg-effects) ; (AT ?V ?L2)
  (dotimes (i (length neg-effects))
    (setq operator (nth i neg-effects))
    (if (gethash (first operator) *current-status*)
        (let((value1 (gethash (first operator) *current-status*)))
            (setf (gethash (first operator) *current-status*) (remove (rest operator) value1))
          )
        ))
)
 ;; modify-status
(defun modify-status (action)
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
    (if (mapcar #'(lambda(unit) (equal (rest effect) unit)) (gethash (first effect) *current-status*)) T nil)
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
;; output: actions list (unified with parameters and action's precondition satisfied with current-status)
;; action's precondition corrspond to current-status
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
        ;; check the precondition in current-status
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

(defun action-substitute(action)
  (let ((action-name (hddl-action-name (first action))) ; NOOP
        (action-params (hddl-action-parameters (first action))) ;((?V VEHICLE) (?L2 LOCATION))
        (theta (reverse (second action))) ; ((?V TRUCK-0 VEHICLE) (?L2 CITY-LOC-1 LOCATION))
        (action-head nil)    
        )
    (setq action-head (cons action-name (mapcar 'second theta))) ;(NOOP TRUCK-0 CITY-LOC-1)
  action-head
  )
)      
;; task-substitute
;;      
(defun task-substitute(theta task)

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
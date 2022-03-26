;; main stream of shop2
(defun main-operator ()
  (read-input)
  (shop2-plan)
  (planner-output)
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
  (defparameter *domain* (hddl:read-hddl-domain domain-file))
  (defparameter *problem* (hddl:read-hddl-problem problem-file))
  (defparameter *T0* nil)
  (defparameter *Plan* '())
  (defparameter *actions*  (hddl:hddl-domain-actions *domain*))
  (defparameter *methods*  (hddl:hddl-domain-methods *domain*))
  (defparameter *current-task* nil)
  (defparameter *current-status* nil)
  (defparameter *Tasks* (hddl:hddl-problem-tasks *problem*))
  (defparameter *theta* nil)
  (defparameter *lexicon* (make-lexicon))
  (get-current-status)
  )
;--------------------------------------------
;; main method for first layer of shop2
(defun shop2-plan (&optional plan tasks state theta)
  (setq *T0* (constraint *Tasks*))
  (loop while (not (null *T0*)) do
    (if (null *Tasks*) (return *Plan*))
    (setq *current-task* (car *T0*)
          *T0* (cdr *T0*))
    (format t "~%step-> shop2-plan: *current-task*: ~A~% *T0*: ~A~%" *current-task* *T0*)
    (resolve-task)
  ))  
;; second layer of shop2 
;; check primitive and restore the plan 
(defun resolve-task () 
  (if (primitivep)
    ; primitive (true)
    (update-primitive-task) 
    ; non-primitive (nil)  
    (update-nonprimitive-task) 
    )
)

(defun primitivep ()
 (let ((taskname (hddl:hddl-task-name *current-task*))
      (actionname))
  (loop for a in *actions* do
    (setq actionname (hddl:hddl-action-name a))
    (if (equal actionname taskname)
  (return t) nil))))
 
 ;; third layer of shop2
 ;; unify action and update state from primitive task
(defun update-primitive-task ()
 (let* ((Actions-lst (action-satisfier)))
        (format t "~%step of update-primitive-task -> Actions-lst: ~A" Actions-lst)
                    (cond ((eq Actions-lst nil) (return-from update-primitive-task nil))
                          (t (update-action-values (nth (random (length Actions-lst)) Actions-lst)))
                    )
  )
)

(defun update-action-values (action)
 (let* ((act (action-substitute action)) ; (NOOP TRUCK-0 CITY-LOC-2)
        )
        (format t "~%step-> update-action-values")
         (modify-status action) ;; add pos-effect & delete neg-effect for current-status
         (push act *Plan*)
         (setq *Tasks* (cdr *Tasks*)) 
         (modify-constraints)
         (constraint)
 ) 
)
;; unify methods and update state from nonprimitive task
(defun update-nonprimitive-task ()
 (let* ((Methods-lst (method-satisfier))) ; {(m . theta)...}  
     (format t "~%step-> update-nonprimitive-task -> Methods-lst: ~A" (length Methods-lst))
     (cond ((eq Methods-lst nil) (return-from update-nonprimitive-task nil)) ; if M = empty then return nil to resolve task
            (t  (update-nonprimitive-values (nth (random (length Methods-lst)) Methods-lst)))) ; nondeterministically choose a pair (m, θ) ∈ M (random choose)
 ) 
)
 
; (defun update-nonprimitive-values (method-list)
;   ; (format t "~%step-> update-nonprimitive-values: subtasks for method ~A" subtasks)
;   (format t "~%update-nonprimitive-values-> *Tasks*: ~A" *Tasks*)
;   (modify-constraints method-list) 
; )

(defun update-nonprimitive-values (method)
   (setq *Tasks* (cdr *Tasks*) ; modify T by removing t, (removin in constraint-lists happens later through constraining with subtasks!
          subm (copy-seq (hddl:hddl-method-subtasks (car method)))
          theta (cadr method); in sub(m) to precede the tasks that t precede
          subm (update-tasks subm theta) 
          *Tasks* (modify-constraints subm)) ;;constrain tasks with subtasks where appropriate
     (format t "~%update-nonprimitive-values-> *Tasks*: ~A~% subtasks: ~A~% gewaehlte methode: ~A" *Tasks* subm method)
     (push subm *Tasks*)  ;;adding sub(m) -> use append because push adds subtasks as list!
   (if (not (null subm)) (setq *T0* (constraint subm))
         (setq *T0* (constraint))))

; (defun update-nonprimitive-values (method)
;    (setq *Tasks* (cdr *Tasks*) ; modify T by removing t, (removin in constraint-lists happens later through constraining with subtasks!
;    subm (hddl-method-subtasks (car method))
;    theta (cadr method); in sub(m) to precede the tasks that t precede
;    subm (task-substitute theta subm) 
;    *Tasks* (modify-constraints subm)) ;;constrain tasks with subtasks where appropriate
;      (format t "~%update-nonprimitive-values-> *Tasks*: ~A~% subtasks: ~A~% gewaehlte methode: ~A" *Tasks* subm method)
;      (append subm *Tasks*)  ;;adding sub(m) -> use append because push adds subtasks as list!
;    (if (not (null subm)) (setq *T0* (constraint subm))
;          (setq *T0* (constraint))))
;; Search for current-task in the constraint-slots of all task in global tasklist and replaces it with subtasks
; the subtasks themselves are already constrained by each other in where appropriat when the methods are read in

;; modify-constraints
;; input: subtasks of one method, could be zero could be a list
#|(#S(HDDL-TASK :NAME GET-TO :PARAMETERS (?V ?L2) :CONSTRAINTS NIL)
 #S(HDDL-TASK
    :NAME DRIVE
    :PARAMETERS (?V ?L2 ?L3)
    :CONSTRAINTS (#S(HDDL-TASK
                     :NAME GET-TO
                     :PARAMETERS (?V ?L2)
                     :CONSTRAINTS NIL))))|#
;; output: *Tasks* with updated subtasks
; if subtasks has null constraint push it to *Tasks*, else append its constraint until no constraint within one task. 
; (defun modify-constraints (method-list)
;   (let* ((updated-tasks-list nil) ;;constraint
;         (theta (cadr method-list))
;         (method (first method-list))
;         (subtasks (hddl-method-subtasks method))
;         )
;     ; (format t "~%step-> modify-constraints: method-list -> ~A~% subtasks: ~A" method-list subtasks)
;     (format t "~%step-> modify-constraints: theta -> ~A~% subtasks: ~A" theta subtasks)
;     (if subtasks
;        (update-tasks-and-T0 subtasks theta)
;       )
;     )
; )
;; Modifies the constraint-lists of all tasks in *Tasks* by either removing every occurrence of *current-task* or if given a list of subtasks 
;; substituting every occurrence of *current-task* by that list
;; Input (optional): List of subtasks
;; Output: *Tasks*
;;TODO: Achtung: im Test werden subtasks noch komplett verändert - schauen, ob es an einem Kopierfehler liegt!
(defun modify-constraints (&optional (subtasks nil)(tasks *Tasks*))
  (loop for task in tasks do
    (let ((constraints (hddl:hddl-task-constraints task))
    (newconstraints ()))
      (unless (null constraints)
  (loop for c in constraints do
    (let ((cconstraints (hddl:hddl-task-constraints c)))
    (cond
      ;; if the constraint is the current-task and it has no more constraints,
      ;; append the subtasks to the list of newconstraints (but not the *current-task*
      ((equalp c *current-task*)
       (setq newconstraints (append subtasks newconstraints)))
      ((not(null c constraints))
       (and
        (setq c (modify-constraints subtasks (list c)))
        ;;(setf (hddl:hddl-task-constraints c)(modify-constraints subtasks cconstraints))
        ;;(pprint (hddl:hddl-task-constraints c))
        (setq newconstraints (reverse (push c newconstraints)))))
      ;;otherwise push the *current-task to newconstraints
      (t (setq newconstraints (push c newconstraints))
         (setq newconstraints (reverse newconstraints))))))
  (setf (hddl:hddl-task-constraints task) newconstraints))))
  tasks)

; constraint T to T0 
;; hier muss also für alle t in T prüfen, dass nicht eine andere task vorher ausgeführt werden muss
;; prüft, ob die constraints leer sind oder nicht
;; bei den Methoden, dass die constraints bei den Subtasks eingefügt werden müssen
; (defun constraint (tasks)
;    (setq *T0* nil)
;    (setq tasks (delete-duplicates tasks))
;   (dotimes (curr-num (length tasks))
;      (if (null (hddl-task-constraints (nth curr-num tasks))) (setq *T0* (cons (nth curr-num tasks) *T0*)) nil))
;   (format t "~%step-> constraint: constraint list of tasks-> *T0*: ~A" *T0*)
;   (return-from constraint *T0*)
; )


;;builds T0: checks for all tasks if constraint-slot is empty and adds it to T0 if that's the case; if no tasklist is provided, the global tasklist is used as default value
(defun constraint(&optional (tasks *Tasks*))
(setq *T0* nil)
      (loop for task in tasks do
    (if (null (hddl:hddl-task-constraints task)) 
      (push task *T0*)))
  (reverse *T0*))


;--------------------------------------------------------------
;;; status CRUD
;; get-initial-status
;; init a hash table as a dictionary for current-status
;; input: current-status
;; output: current-status as hash-table
(defun get-initial-status () 
 (let ((problem-types (hddl:hddl-problem-objects *problem*)) ;((CITY-LOC-2 LOCATION) (CITY-LOC-1 LOCATION) (CITY-LOC-0 LOCATION) (TRUCK-0 VEHICLE))
      (current-status (hddl:hddl-problem-init-status *problem*)) #|((ROAD CITY-LOC-0 CITY-LOC-1) (ROAD CITY-LOC-1 CITY-LOC-0) (ROAD CITY-LOC-1 CITY-LOC-2) (ROAD CITY-LOC-2 CITY-LOC-1)(AT TRUCK-0 CITY-LOC-2))|#
      (state-type (delete-duplicates (car (apply #'mapcar #'list (hddl:hddl-problem-init-status *problem*))))) ; (ROAD AT)
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
(defun add-state (pos-effects) 
  (dotimes (i (length pos-effects))
    (setq effect (nth i pos-effects))
    (if (null (gethash (first effect) *current-status*))
        (setf (gethash (first effect)) (rest effect))
        
        (let ((value1 (gethash (first effect) *current-status*)))
              (setf (gethash (first effect) *current-status*) (cons (rest effect) value1)))))
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
(defun modify-status (action) ; (action . theta)
 (let ((pos-effects (hddl:hddl-action-pos-effects (first action)))
       (neg-effects (hddl:hddl-action-neg-effects (first action)))
       (theta (second action)))
   (if neg-effects 
       (delete-state (effect-substitute neg-effects theta))) ; {(and (AT ?V ?L2) (ROAD ?l1 ?l2)) & ((?V TRUCK-0 VEHICLE)) (?L2 CITY-LOC-0 LOCATION))}
   (if pos-effects 
       (add-state (effect-substitute pos-effects theta)))
  )
) 
 
;---------------------------------------------------------------------------


;; action-satisfier
;; input: actions & current-task
;; output: actions list (unified with parameters and action's precondition satisfied with current-status)
;; action's precondition corrspond to current-status
;; leave out the satifying action list filtered by current-status
(defun action-satisfier ()
  (let ((actions-satisfied nil)
        (actions (action-unifier))) ;;{(a.theta)}
    (unless (null actions) 
     (dotimes (i (length actions))
          (let* ((action (nth i actions))
                (action-preconditions (hddl:hddl-action-preconditions (first action))) ;((AT ?V ?L1) (ROAD ?L1 ?L2))
                (action-theta (second action)) ; ((?V TRUCK-0 VEHICLE) (?L2 CITY-LOC-0 LOCATION))  
                (preconditions nil)
     (variabled-action nil))

      (setq preconditions (precondition-substitute action-preconditions action-theta)) ;;ex. ((AT TRUCK-0 ?L1) (ROAD ?L1 CITY-LOC-0))
      
            ;; returns a list of actions that can be appended to the action-list, consisting of the input-action with all possible variable-bindings in theta that
      ;;satisfy the preconditions of the action, of the form ((action theta1).. (action thetaN)
      (setq variabled-action (action-precondition-satisfier action preconditions))   ;;ex. (action theta)((AT TRUCK-0 ?L1) (ROAD ?L1 CITY-LOC-0))
            (unless (null variabled-action)                                 ;;unless the list is emtpy -> there is no possibility for the action-preconditions to be fulfilled
        (setq actions-satisfied (append variabled-action actions-satisfied))))))   ;;add the variabled-actions to the list of actions-satisfied
    
    (format t "~%step-> action-satisfier: current satisfied actions list: ~A" actions-satisfied)
(return-from action-satisfier actions-satisfied)))

;------------------------------------------------
;; 
(defun make-lexicon ()
  (let ((lexicon nil)
        (method-type nil))
    (mapcar #'(lambda(c)
               (mapcar #'(lambda(d)
                          (push d method-type))(hddl:hddl-method-parameters c))) 
            *methods*)
    (setq method-type (delete-duplicates method-type :test #'eq :key 'car))
    (setq lexicon (append method-type lexicon))
    (setq lexicon (append (hddl:hddl-problem-objects *problem*) lexicon))
    
  lexicon))
;------------------------------------------------------------------------------
;;;parameters-binding (theta-binding)
;; input: task & operator (as action or method)
;; output: ((?V TRUCK-0 VEHICLE)) (?L2 CITY-LOC-0 LOCATION)) as theta
;; pass to action-satisfier or method-satisfier
;; change: if theta inside a method, then the formation of theta should follow the form from method-theta. so we can substitute the theta directly. 
(defun parameters-binding (operator)
  (let ((op-param (if (eql (type-of operator) 'READ-HDDL-PACKAGE::HDDL-ACTION) (hddl:hddl-action-parameters operator) (hddl:hddl-method-parameters operator))) ; ((?V VEHICLE) (?L2 LOCATION))
        (task-params (hddl:hddl-task-parameters *current-task*)) ;(TRUCK-0 CITY-LOC-0)
        (types (hddl:hddl-problem-objects *problem*)) ;((CITY-LOC-2 LOCATION) (CITY-LOC-1 LOCATION) (CITY-LOC-0 LOCATION) (TRUCK-0 VEHICLE)
        (binding-list nil))
    
    (dotimes (i (length task-params))
      (setq one-set (assoc (nth i task-params) types)) ;(TRUCK-0 VEHICLE)
      (setq reversed-op-param (mapcar #'reverse op-param))
      (setq binding-list (cons (cons (cadr (assoc (cadr one-set) reversed-op-param)) one-set) binding-list))
        )
    (if (eql (type-of operator) 'READ-HDDL-PACKAGE::HDDL-ACTION)
        (format t "~%step-> parameters-binding with action ~S" binding-list)
        (format t "~%step-> parameters-binding with method ~S" binding-list)
        )
    (return-from parameters-binding (list (reverse binding-list))) ; (((?V TRUCK-0 VEHICLE) (?L2 CITY-LOC-1 LOCATION)))
    )
  )

;;; operator-unifier-p
;; input: task & operator (as action or method)
;; output: True/ False
;; check name of tasks from action and current-task the same :: todo
;;       each parameter identical from operator (as action or method) and task 
;; pass to next function as action-satisfier or method-satisfier 
(defun operator-unifier-p (operator)
  (declare (optimize debug))
  (let ((op-task-name (if (eql (type-of operator) 'READ-HDDL-PACKAGE::HDDL-ACTION) (hddl:hddl-action-name operator) 
                          (first (hddl:hddl-method-task operator)))) 
        (op-params (if (eql (type-of operator) 'READ-HDDL-PACKAGE::HDDL-ACTION) (hddl:hddl-action-parameters operator) 
                       (rest (hddl:hddl-method-task operator))))
        (task-name (hddl:hddl-task-name *current-task*))
        (task-params (hddl:hddl-task-parameters *current-task*))
        )
      (cond ((and (eql (type-of operator) 'READ-HDDL-PACKAGE::HDDL-ACTION) (eq task-name op-task-name) (a-identical-parameters-p task-params op-params)) (return-from operator-unifier-p T))
            ((and (eql (type-of operator) 'READ-HDDL-PACKAGE::HDDL-METHOD) (eq task-name op-task-name) (m-identical-parameters-p task-params op-params operator)) (return-from operator-unifier-p T))
            (t (return-from operator-unifier-p nil)))
  ))

;;; a-identical-parameters-p
;; input: task parameters (operator as action)
;; output: True / False
;; pass to next function as operator-unifier-p
(defun a-identical-parameters-p (task-params op-params) ; (TRUCK-0 CITY-LOC-0) or (TRUCK-0 ?L2) / ((?V VEHICLE) (?L2 LOCATION))
  (let ((task-type nil))
 (setq task-type (mapcar #'(lambda(c)(second (assoc c *lexicon*))) task-params)) ; (VEHICLE LOCATION)
  (setq op-type (mapcar 'second op-params)) ;(VEHICLE LOCATION) 
  (cond
    ((not (eq (length task-type) (length op-type))) (return-from a-identical-parameters-p nil)) ; quick check and return 
    ((equal (stable-sort (copy-seq task-type) #'string<) (stable-sort (copy-seq op-type) #'string<)) (return-from a-identical-parameters-p T))
    (t (return-from a-identical-parameters-p nil)))
  )
)

;;; m-identical-parameters-p
;; check the value of current-task-parameters & method-task-parameters identical
;; input: task parameters & method parameters
;; output: True / False
;; pass to next function as operator-unifier-p
(defun m-identical-parameters-p (task-params m-task-params operator) ; (TRUCK-0 CITY-LOC-0) / (?v ?l2)
  (let ((op-param (hddl:hddl-method-parameters operator)) ;((?V VEHICLE) (?L LOCATION))
        (task-type nil))
       (setq task-type (mapcar #'(lambda(c)(second (assoc c *lexicon*))) task-params)) ; (VEHICLE LOCATION)
       (setq task-type (stable-sort task-type #'string<))  ;(LOCATION VEHICLE)
      (setq m-task-params (mapcar #'(lambda(c)(second (assoc c *lexicon*))) m-task-params)) ;(VEHICLE LOCATION)
      (setq m-task-params (stable-sort m-task-params #'string<))  ; (LOCATION VEHICLE)
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
(defun action-unifier ()
  (let ((actions-satisfied nil))
    (dotimes (i (length *actions*))
      (setq action (nth i *actions*))
      (if (operator-unifier-p action) 
          (setq actions-satisfied (cons (cons action (parameters-binding action)) actions-satisfied))
          ) 
      ) (return-from action-unifier actions-satisfied) ;;{(a.theta)}
    (format t "~%step-> action-unifier: current unified actions list: ~A" actions-satisfied)
    )
  )
;3/26 actions is unbound
;; action-satisfier
;; input: actions 
;; output: actions list (unified with parameters and action's precondition satisfied with current-status)
;; action's precondition corrspond to current-status
;; leave out the satifying action list filtered by current-status
(defun action-satisfier ()
  (let ((actions-satisfied nil)
        (actions (action-unifier))) ;;{(a.theta)}
    (unless (null actions) 
     (dotimes (i (length actions))
          (let* ((action (nth i actions))
                (action-preconditions (hddl:hddl-action-preconditions (first action))) ;((AT ?V ?L1) (ROAD ?L1 ?L2))
                (action-theta (second action)) ; ((?V TRUCK-0 VEHICLE) (?L2 CITY-LOC-0 LOCATION))  
                (preconditions nil)
                (variabled-action nil))

      (setq preconditions (precondition-substitute action-preconditions action-theta)) ;;ex. ((AT TRUCK-0 ?L1) (ROAD ?L1 CITY-LOC-0))
      
            ;; returns a list of actions that can be appended to the action-list, consisting of the input-action with all possible variable-bindings in theta that
      ;;satisfy the preconditions of the action, of the form ((action theta1).. (action thetaN)
      (setq variabled-action (action-precondition-satisfier action preconditions))   ;;ex. (action theta)((AT TRUCK-0 ?L1) (ROAD ?L1 CITY-LOC-0))
            (unless (null variabled-action)                                 ;;unless the list is emtpy -> there is no possibility for the action-preconditions to be fulfilled
        (setq actions-satisfied (append variabled-action actions-satisfied))))))   ;;add the variabled-actions to the list of actions-satisfied
    
    (format t "~%step-> action-satisfier: current satisfied actions list: ~A" actions-satisfied)
(return-from action-satisfier actions-satisfied)))


;; method-satisfier
;; pre(m) to be seen as deprecated tuple
;; output: {(method . theta)...}
;--- todo parameters binding not in this block?
(defun method-satisfier ()
  (let ((methods-satisfied nil))
    (dotimes (i (length *methods*))
      (setq method (nth i *methods*))
      (if (operator-unifier-p method) (setq methods-satisfied (cons (cons method (parameters-binding method)) methods-satisfied)))
    ) (and (return-from method-satisfier (values methods-satisfied))
          (format t "~%step-> method-satisfier: current satisfied methods list: ~A" (length methods-satisfied)))
  )
)


;----------------------------------------------------------------------------------------------
;;; substitution with unifier and replace the required variables within each update
;; unify variables 
(defun action-substitute(action)
  (let ((action-name (hddl:hddl-action-name (first action))) ; NOOP
        (action-params (hddl:hddl-action-parameters (first action))) ;((?V VEHICLE) (?L2 LOCATION))
        (theta (second action)) ; ((?V TRUCK-0 VEHICLE) (?L2 CITY-LOC-1 LOCATION))
        (action-head nil)    
        )
    (setq action-head (cons action-name (mapcar 'second theta))) ;(NOOP TRUCK-0 CITY-LOC-1)
  action-head
  )
)   
;; todo effect of saction1 with parameter substitute
;; input: {(AT ?V ?L2) & ((?V TRUCK-0 VEHICLE)) (?L2 CITY-LOC-0 LOCATION) (?L3 CITY-LOC-1 LOCATION))}
;; output: list of effects {(AT TRUCK-0 CITY-LOC-0)}
(defun effect-substitute(effects theta)
  (let ((effects-lst nil)
        (effect nil))
    (dotimes (i (length effects))
      (setq effect (cons (first (nth i effects)) 
                         (mapcar #'(lambda(c) (second (assoc c theta))) (rest (nth i effects)))))
      (push effect effects-lst)
      )  effects-lst
    )
   
)
  
;; task
;; input: theta->  ((?L3 CITY-LOC-1 LOCATION) (?V TRUCK-0 VEHICLE)) /task-params-> (?V ?L2 ?L3) or (?V ?L2)
;; output: T/ Nil
; (defun task-substitute-p (theta task-type)
;   (let ((theta-type (mapcar #'third theta)))
;     (if 
;       (and (eq (length task-type) (length theta)) 
;            (equal (stable-sort (copy-seq theta-type) #'string<) (stable-sort (copy-seq task-type) #'string<)))
;       T
;       ; (format t "~%theta-type: ~A~% task-type: ~A" (stable-sort (copy-seq theta-type) #'string<) (stable-sort (copy-seq task-type) #'string<))
;       nil)
;     )
; )
;; todo
;; task-substitute
;; situation: 
#|
*Tasks* ->
(#S(HDDL-TASK
    :NAME DRIVE
    :PARAMETERS (?V ?L2 ?L3)
    :CONSTRAINTS (#S(HDDL-TASK
                     :NAME GET-TO
                     :PARAMETERS (?V ?L2)
                     :CONSTRAINTS NIL)))
 #S(HDDL-TASK :NAME GET-TO :PARAMETERS (?V ?L2) :CONSTRAINTS NIL)
 #S(HDDL-TASK :NAME NOOP :PARAMETERS (?V ?L) :CONSTRAINTS NIL)
 #S(HDDL-TASK :NAME GET-TO :PARAMETERS (TRUCK-0 CITY-LOC-1) :CONSTRAINTS NIL)
 #S(HDDL-TASK :NAME GET-TO :PARAMETERS (TRUCK-0 CITY-LOC-0) :CONSTRAINTS NIL))|#
;; input: theta ex.((?L3 CITY-LOC-1 LOCATION) (?V TRUCK-0 VEHICLE)) 
;; output: update *Tasks*
;; parse the parameters to each tasks 
#|
CL-USER> (task-substitute theta subtasks)

step-> task-substitute-> task-list : (#S(HDDL-TASK
                                         :NAME DRIVE
                                         :PARAMETERS (?V ?L1 ?L2)
                                         :CONSTRAINTS NIL))  : one this stage, then we need to give out two possible substitute to check which one the best for the next step

CL-USER> theta
((?V TRUCK-0 VEHICLE) (?L2 CITY-LOC-1 LOCATION))
|#  
;;;; todo typecheck for task-params
  #|
  tp: tp -> all of the task params
((?V ?L2) (?V ?L2 ?L3) (?V ?L) (TRUCK-0 CITY-LOC-1) (TRUCK-0 CITY-LOC-0))

  (sublis the11 tp)
((TRUCK-0 CITY-LOC-1) (TRUCK-0 CITY-LOC-1 ?L3) (TRUCK-0 ?L)
 (TRUCK-0 CITY-LOC-1) (TRUCK-0 CITY-LOC-0))
  |#
  
  #|
  THETA: ((?v truck veh)(?l munich location))
TASK-P:   (?V ?L2 ?L3)
    |#
  
  ;; only one task as input and output 
; (defun task-substitute(subtasks theta)
;  (let ((task-list nil)
;        (method-types nil) 
;        (theta-params (mapcar 'car theta))
;        (theta-dotted (mapcar #'(lambda(c) (cons (caddr c) (cadr c))) theta)) ;((VEHICLE . TRUCK-0) (LOCATION . CITY-LOC-1))
;        (theta-dot-value (mapcar #'(lambda(c) (cons (car c) (cadr c))) theta)) ;((?V . TRUCK-0) (?L2 . CITY-LOC-1))
;        )
;   (setq task-list (update-tasks subtasks task-list))
;   (mapcar #'(lambda(cons-lis) (mapcar #'(lambda(single-cons) (push single-cons method-types)) cons-lis)) omp11) ;((?L1 LOCATION) (?L2 LOCATION) (?V VEHICLE) (?L2 LOCATION) (?L3 LOCATION) (?V VEHICLE))
;   (setq method-types (mapcar #'(lambda(c) (apply 'cons c)) method-types)) ; ((?L1 . LOCATION) (?L2 . LOCATION) (?V . VEHICLE) (?L2 . LOCATION) (?L3 . LOCATION) (?V . VEHICLE))
  
;   (loop for task in task-list do
;      (let* ((task-params (hddl-task-parameters task)) ; (?V ?L2) (?V ?L)/ (?V ?L2 ?L3)
;            (task-params-type nil))
       
;        ; (setf (hddl-task-parameters task) (mapcar #'(lambda(c) (second (assoc c theta-reversed))) (reverse task-params-type)))
;       ;; ex. (?V ?L2) 
;       ;; if there is no multiple overlapped types, return only one to one pair task that corresponds to theta, then return only one kind of lists 
;       (setq task-params-type (sublist method-types task-params)) ; (VEHICLE LOCATION)
      
;       ;; ex1. 
;       ;; mm: (VEHICLE LOCATION) / mm1: (VEHICLE LOCATION LOCATION)
;       ;; task-parameters: (?V ?L) / theta-paramesters: ((?V . TRUCK-0) (?L2 . CITY-LOC-1)) 
;       ;; task-parameters: (?V ?L ?L3) / theta-paramesters: ((?V . TRUCK-0) (?L2 . CITY-LOC-1)) 
;       ;; return nil
;       ;; else 
;       ;; we can return actual parameters AS variables (TRUCK-0 CITY-LOC-1)
;       (cond 
;         ((subsetp task-params theta-params) (setf (hddl-task-parameters task) (sublis theta-dot-value task-params-type))) ;; if theta is subset of task-params, direct setf 
        
;         ((eq task-params-type (delete-duplicates (copy-seq task-params-type))) (setf (hddl-task-parameters task) (sublis theta-dotted task-params-type))) ; (VEHICLE LOCATION)
;           ;; special handling for (?V ?L2 ?L3) with (?V ?L)
;         (()())
;             )
;      )
;    )
;   ; (if task-list (setq *Tasks* (union task-list *Tasks*)))
;   ; task-list
;   (return-from task-substitute task-list)
;    ; (format t "~%step-> task-substitute-> task-list : ~A~%"  task-list)
;  )
; )

(defun task-substitute(subtask theta)
 (let ( 
       (theta-params (mapcar 'car theta)) ;(?v ?l2)
       (theta-dot-value (mapcar #'(lambda(c) (cons (car c) (cadr c))) theta)) ;((?V . TRUCK-0) (?L2 . CITY-LOC-1))
       (task-params (hddl:hddl-task-parameters subtask)) ; (?V ?L2) (?V ?L)/ (?V ?L2 ?L3)
       )
  
    (setf (hddl:hddl-task-parameters subtask) (sublis theta-dot-value task-params)) ;; if theta is subset of task-params, direct setf 
           
   ) 
 )

(defun update-tasks-and-T0 (subtasks theta)
  (task-substitute theta subtasks) ; substitute theta for tasks
  (constraint *Tasks*); remove duplicates of tasks and remove taks with constraint list 
  )
;; update one methods'subtasks to new task list, which is copy-structure of one hddl-task
; (defun update-tasks (subtasks task-list)
;     (mapcar #'(lambda(c)
;                (cond ((hddl-task-constraints c) ((setq task-list (union (list (copy-structure c)) task-list)) )) ; if subtask has constraints, push its task first to *Tasks*
;                      ((hddl-task-constraints c) (update-tasks (hddl-task-constraints c) task-list)) ; Then, loop
;                      (t (setq task-list (union (list (copy-structure c)) task-list))))) ; if there is no constraint push to *Tasks*
;             subtasks)
;     (return-from update-tasks task-list)
; )
;; update one methods'subtasks to new task list, which is copy-structure of one hddl-task


; (task-substitute c) (setf (hddl-task-constraints c) (update-tasks (hddl-task-constraints c))
;; subtasks should not be the same list as tasks but should be a copy sequence from nonprimitive values
(defun update-tasks (tasklist theta)
     (mapcar #'(lambda(c)
                   (cond ((hddl:hddl-task-constraints c) (and (task-substitute c theta) (setf (hddl:hddl-task-constraints c) (update-tasks (hddl:hddl-task-constraints c) theta)))); if subtask has constraints, push its task first to *Tasks*
                         (t (task-substitute c theta)))) ; if there is no constraint push to *Tasks*
                tasklist)
    (return-from update-tasks tasklist)
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
(defun planner-output ()
  (format t "Shop2-operator finds current possible plan as ~S" *Plan*))
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

;------------------------------------------------------------------------------------
;;Alisa: unifier sollte am besten 
;;1. alle actionen sammeln, die den gleichen Namen haben wie die task
;;2. prüfen, ob die gleiche Anzahl Parameter vorliegt (Länge der Liste)
;;3. prüfen, ob die Parameter den selben Typ haben (Reihenfolge in Parameterliste egal)
;;4. die Parameter der task als theta ausgeben
;; Ergebnis wäre dann eine Liste mit ((action . theta)(action . theta)...)
;; hier würde ich also auch gar nicht jede action einzeln übergeben, sondern gleich alle auf einmal

;;unifier sollte am besten 
;;1. alle actionen sammeln, die den gleichen Namen haben wie die task
;;2. prüfen, ob die gleiche Anzahl Parameter vorliegt (Länge der Liste)
;;3. prüfen, ob die Parameter den selben Typ haben (Reihenfolge in Parameterliste egal)
;;4. die Parameter der task als theta ausgeben
;; Ergebnis wäre dann eine Liste mit ((action . theta)(action . theta)...)
;; hier würde ich also auch gar nicht jede action einzeln übergeben, sondern gleich alle auf einmal

;; für alle eingegebenen Aktionen prüfe, ob die preconditions einer Aktion im aktuellen Status erfüllt sind, falls ja, füge sie in Ergebnisliste ein
;; preconditions sind dann erfüllt, wenn sie im aktuellen Status enthalten sind
  ;; gibt es auch negative preconditions?
  ;;Achtung: actions haben jetzt die Form (a. theta) 
;--------------------------------------------------------------------- 

;----------------------------------------------------------------------
;okay 
; ;;;parameters-binding (theta-binding)
; ;; input: task & operator (as action or method)
; ;; output: (?V TRUCK-0 VEHICLE)) (?L2 CITY-LOC-0 LOCATION)) as theta
; ;; pass to method-satisfier
; (defun parameters-binding (operator task)
;   (let ((op-param (if (eql (type-of operator) 'HDDL-ACTION) (hddl-action-parameters operator) (hddl-method-parameters operator))) ; ((?V VEHICLE) (?L2 LOCATION))
;         (task-params (hddl-task-parameters task)) ;(TRUCK-0 CITY-LOC-0)
;         (types (hddl-problem-objects *problem*)) ;((CITY-LOC-2 LOCATION) (CITY-LOC-1 LOCATION) (CITY-LOC-0 LOCATION) (TRUCK-0 VEHICLE)
;         (binding-list nil))
;     (dotimes (i (length task-params))
;           (setq one-set (assoc (nth i task-params) types)) ;(TRUCK-0 VEHICLE)
;           (setq reversed-op-param (mapcar #'reverse op-param))
;           (setq binding-list (cons (cons (cadr (assoc (cadr one-set) reversed-op-param)) one-set) binding-list))
;           )
;     (if (eql (type-of operator) 'HDDL-ACTION)
;         (format t "step-> parameters-binding with action ~S" (list binding-list))
;         (format t "step-> parameters-binding with method ~S" (list binding-list))
;         )
;     (return-from parameters-binding (list binding-list)) ; ((?V TRUCK-0 VEHICLE) (?L2 CITY-LOC-0 LOCATION))
;     )
;   )

; ;;; operator-unifier-p
; ;; input: task & operator (as action or method)
; ;; output: True/ False
; ;; check name of tasks from action and current-task the same :: todo
; ;;       each parameter identical from operator (as action or method) and task 
; ;; pass to next function as method-satisfier 
; (defun operator-unifier-p (operator task)
;   (let ((op-task-name (if (eql (type-of operator) 'HDDL-ACTION) (hddl-action-name operator) (first (hddl-method-task operator))))
;         (op-params (if (eql (type-of operator) 'HDDL-ACTION) (hddl-action-parameters operator) (hddl-method-parameters operator)))
;         (task-name (hddl-task-name task))
;         (task-params (hddl-task-parameters task))
;         )
;       (cond ((and (eql (type-of operator) 'HDDL-ACTION) (identical-parameters-p task-params op-params)) (return-from operator-unifier-p T))
;             ((and (eql (type-of operator) 'HDDL-METHOD) (eq task-name op-task-name) (identical-parameters-p task-params op-params)) (return-from operator-unifier-p T))
;             (t (return-from operator-unifier-p nil)))
;   ))
; ;;; identical-parameters-p
; ;; input: task parameters & operator parameters (operator as action or method)
; ;; output: True / False
; ;; pass to next function as operator-unifier-p
; (defun identical-parameters-p (task-params op-params) ; (TRUCK-0 CITY-LOC-0) / ((?V VEHICLE) (?L2 LOCATION))
;   (let ((types (hddl-problem-objects *problem*))
;         (task-type nil)) ;((CITY-LOC-2 LOCATION) (CITY-LOC-1 LOCATION) (CITY-LOC-0 LOCATION) (TRUCK-0 VEHICLE)
;   (dotimes (i (length task-params))
;       (setq one-set (assoc (nth i task-params) types)) ;(TRUCK-0 VEHICLE)
;       (setq task-type (cons (cadr one-set) task-type))
;     )
;   (setq op-type (cadr (apply #'mapcar #'list op-params))) ;(VEHICLE LOCATION)
;   (setq op-type (sort op-type #'string<)) ; sort the ordering to compare string
;   (setq task-type (sort task-type #'string<))  
;   (cond
;     ((not (eq (length task-type) (length op-type))) (return-from identical-parameters-p nil)) ; quick check and return 
;     ((equal task-type op-type) (return-from identical-parameters-p T))
;     (t (return-from identical-parameters-p nil)))
;   )
;   )

; ;; method-satisfier
; ;; pre(m) to be seen as deprecated tuple
; ;; output: {(method . theta)...}
; (defun method-satisfier (methods task)
;   (let ((methods-satisfied nil))
;     (dotimes (i (length methods))
;       (setq method (nth i methods))
;       (if (operator-unifier-p method task) (setq methods-satisfied (cons (cons method (parameters-binding method task)) methods-satisfied)))
;     ) (return-from method-satisfier (values methods-satisfied))
;   )
; )
;----------------------------------------------------------------------------------------------------------------------------------------------- 


;; input: (AT TRUCK0 LOCATION1)
; L- =((AT TRUCK ?l2) (ROAD ?l2 Munich))
;; Input: a list of preconditions
;; Output: a list of the unsatisfied preconditions of an action
;; yishan: additional condition to filter single value or a list should be added
; (defun find-state-p (preconditions) 
;     (let ((satisfied nil)
;           (unsatisfied nil))
;       (unless (listp (first preconditions)) 
;        (and (if (find T (mapcar #'(lambda(unit) (equal (rest preconditions) unit)) (gethash (first preconditions) *current-status*)))
;                                    (push preconditions satisfied)     ;;if the precondition can be found as-is in the currentstatus push it to satisfied
;                                           ; (format t "precondition: ~A~% value: ~A~%" (rest precondition) (gethash (first precondition) *current-status*))  
;                                    (push preconditions unsatisfied))
;            (return-from find-state-p unsatisfied)  ))
            
;       (dotimes (i (length preconditions))   ;;for all preconditions
;                (let ((precondition (nth i preconditions)))
;                     (if (find T (mapcar #'(lambda(unit) (equal (rest precondition) unit)) (gethash (first precondition) *current-status*)))
;                             (push precondition satisfied)     ;;if the precondition can be found as-is in the currentstatus push it to satisfied
;                                    ; (format t "precondition: ~A~% value: ~A~%" (rest precondition) (gethash (first precondition) *current-status*))  
;                             (push precondition unsatisfied))));; if the precondition can not be found in the currentstatus push to unsatisfied
           
;      (return-from find-state-p unsatisfied))
;     (format t "~%steo: find-state-p: ~A" unsatisfied))
;; action-satisfier
;; input: actions & current-task
;; output: actions list (unified with parameters and action's precondition satisfied with current-status)
;; action's precondition corrspond to current-status
;; leave out the satifying action list filtered by current-status
(defun action-satisfier ()
  (let ((actions-satisfied nil)
        (actions (action-unifier actions))) ;;{(a.theta)}
    (unless (null actions) 
     (dotimes (i (length actions))
          (let* ((action (nth i actions))
                (action-preconditions (hddl:hddl-action-preconditions (first action))) ;((AT ?V ?L1) (ROAD ?L1 ?L2))
                (action-theta (second action)) ; ((?V TRUCK-0 VEHICLE) (?L2 CITY-LOC-0 LOCATION))  
                (preconditions nil)
     (variabled-action nil))

      (setq preconditions (precondition-substitute action-preconditions action-theta)) ;;ex. ((AT TRUCK-0 ?L1) (ROAD ?L1 CITY-LOC-0))
      
            ;; returns a list of actions that can be appended to the action-list, consisting of the input-action with all possible variable-bindings in theta that
      ;;satisfy the preconditions of the action, of the form ((action theta1).. (action thetaN)
      (setq variabled-action (action-precondition-satisfier action preconditions))   ;;ex. (action theta)((AT TRUCK-0 ?L1) (ROAD ?L1 CITY-LOC-0))
            (unless (null variabled-action)                                 ;;unless the list is emtpy -> there is no possibility for the action-preconditions to be fulfilled
        (setq actions-satisfied (append variabled-action actions-satisfied))))))   ;;add the variabled-actions to the list of actions-satisfied
    
    (format t "~%step-> action-satisfier: current satisfied actions list: ~A" actions-satisfied)
(return-from action-satisfier actions-satisfied)))
  #|((#S(HDDL-ACTION
     :NAME NOOP
     :PARAMETERS ((?V VEHICLE) (?L2 LOCATION))
     :PRECONDITIONS (AT ?V ?L2)
     :NEG-EFFECTS NIL
     :POS-EFFECTS NIL)
  ((?L2 CITY-LOC-0 LOCATION) (?V TRUCK-0 VEHICLE))))|#


#|Input: an action and a list of preconditions
   ((#S(READ-HDDL-PACKAGE::HDDL-ACTION
   :NAME DRIVE
   :PARAMETERS ((?V VEHICLE) (?L1 LOCATION) (?L2 LOCATION))
   :PRECONDITIONS ((AT ?V ?L1) (ROAD ?L1 ?L2))
   :NEG-EFFECTS ((AT ?V ?L1))
   :POS-EFFECTS ((AT ?V ?L2))) ((?V TRUCK-0 VEHICLE)(?L2 LOCATION CITY-LOC-1)))((AT TRUCK-0 ?L1) (ROAD ?L1 CITY-LOC-1)))
;;Output: a list containing the action paired with every possible parameterbinding theta that satisfies the preconditions or nil if the preconditions can't be satisfied at all
((#S(READ-HDDL-PACKAGE::HDDL-ACTION
   :NAME DRIVE
   :PARAMETERS ((?V VEHICLE) (?L1 LOCATION) (?L2 LOCATION))
   :PRECONDITIONS ((AT ?V ?L1) (ROAD ?L1 ?L2))
   :NEG-EFFECTS ((AT ?V ?L1))
   :POS-EFFECTS ((AT ?V ?L2))) ((?V TRUCK-0 VEHICLE)(?L2 CITY-LOC-1 LOCATION)(?L1 CITY-LOC-2 LOCATION)))
|#
(defun action-precondition-satisfier (action preconditions)
  (let ((only-action (first action))
  (theta (second action))
  (unsatisfied-preconditions nil)
  (variabled-preconditions nil)
  (actions-satisfied nil)) ;;the list of actions to be returned

    (setq unsatisfied-preconditions (find-unsatisfied-preconditions preconditions)) ;;takes a list of preconditions and returns only unsatisfied ones,ex. ((AT TRUCK-0 ?L1) (ROAD ?L1 CITY-LOC-1))
      (cond
        ;;if there are no unsatisfied preconditions, add the action to the list of satisfied actions and return it -> all preconditions satisfied, no additional steps needed
        ((null unsatisfied-preconditions)
         (setq actions-satisfied (cons action actions-satisfied))
         (return-from action-precondition-satisfier actions-satisfied))
        ;;otherwise if there are unsatisfied preconditions
        ;;check if all preconditions in unsatisfied contain variables  -> returns nil otherwise
        (T (setq variabled-preconditions (all-contain-variables unsatisfied-preconditions)) ;;ex. (((AT TRUCK-0 ?L1) (2)) ((ROAD ?L1 CITY-LOC-1) (1)))
          
  ;;if variabled-preconditions is null, return nil immediately, because there is at least one precondition without variables that cannot be satisfied
          (when (null variabled-preconditions)
        (return-from action-precondition-satisfier nil))

        (let* ((variabled-pre nil)                      ;;from the list of variable-preconditions ex. (((AT TRUCK-0 ?L1) (2)) ((ROAD ?L1 CITY-LOC-1) (1)))
        (pre (first variabled-preconditions))   ;;take the first precondition to work with, ex.((AT TRUCK-0 ?L1) (2)) 
         (variabled-preconditions (mapcar 'car (rest variabled-preconditions)))) ;;take the rest of the preconditions for later, ex. (((ROAD ?L1 CITY-LOC-1) (1)))
          
          (setq variabled-pre (get-variables pre))  ;;get all possible variable-bindings for the first precondition, ex. (((?L1 CITY-LOC-2 LOCATION)))
          (pprint variabled-pre)
          ;;if there are no possible-variable-bindings for the precondition return nil -> cannot be satisfied under any circumstances
          (if (null variabled-pre) (return-from action-precondition-satisfier nil))
          (loop for b in variabled-pre do   ;;for every possible parameter-binding
            (pprint b)
                    
            (let* ((new-theta (append theta b))  ;;new-theta is the old theta with the new variable-binding in the same form, ex. ((?V TRUCK-0 VEHICLE)
             (new-action (cons only-action new-theta))) ;; new-action is the action with the new theta
          (cond
          ;;if there are no other preconditions left push the new action to action-satisfied
          ((null variabled-preconditions)      
          (setq actions-satisfied (push new-action actions-satisfied)))
          
          
          ;;otherwise if there are more preconditions with variables left, ex. (((ROAD ?L1 CITY-LOC-1) (1)))
          (T
           ;;for every possible variable-binding in variabled-pre bind it to every precondition still in variabled-preconditions and then
           ;;recursively call action-precondition-satisfier with the action and variabled-preconditions
           ;; -> list of still unsatisfiede preconditions that now contain new variables and must be analysed again
           ;;(and(let ((preconditions (mapcar 'car variabled-preconditions))) ;; ex. ((ROAD ?L1 CITY-LOC-1))
            ;;(loop for b in variabled-pre do             ;; ex. b= (((?L1 CITY-LOC-2 LOCATION)))
             (let*((actions-satisfied-rec nil)
              (substituted-preconditions nil)) 
                (setq substituted-preconditions (precondition-substitute preconditions b)) ;; ex. ((ROAD CITY-LOC-2 CITY-LOC-1))
             ;;recursively check if unsatisfied preconditions are now satisfied  
              (setq actions-satisfied-rec (action-precondition-satisfier new-action substituted-preconditions)) ;;nil or a list of satisfied actions (action theta)
                                             ;;if nil there was no way to satisfy the preconditions with this binding - do not add this to the actions-satisfied-rec- list
              (unless (null actions-satisfied-rec)
                (push actions-satisfied-rec actions-satisfied)))))))))) ;;if the branch was succesful, push its new actions with theta to the return list, otherwise ignore
          (return-from action-precondition-satisfier actions-satisfied))) ;; might be nil if there was no branch that satisfied the preconditions

  



;; Input: a list of preconditions ((AT TRUCK-0 ?L) (ROAD CITY-LOC-1 CITY-LOC-0)) or ((AT TRUCK-0 CITY-LOC-0) (ROAD CITY-LOC-1 CITY-LOC-0))
;; Output: a list of the unsatisfied preconditions of an action ((AT TRUCK-0 ?L)) or ((AT TRUCK-0 CITY-LOC-0))
(defun find-unsatisfied-preconditions (preconditions) 
    (let ((satisfied nil)
          (unsatisfied nil))
          
     (dotimes (i (length preconditions))   ;;for all preconditions
          (let ((precondition (nth i preconditions)))
               (if (find T (mapcar #'(lambda(unit) (equal (rest precondition) unit)) (gethash (first precondition) *current-status*)))
                       (push precondition satisfied)     ;;if the precondition can be found as-is in the currentstatus push it to satisfied
                              ; (format t "precondition: ~A~% value: ~A~%" (rest precondition) (gethash (first precondition) *current-status*))  
                       (push precondition unsatisfied))));; if the precondition can not be found in the currentstatus push to unsatisfied
      (return-from find-unsatisfied-preconditions (reverse unsatisfied))))

    

;;Input: A list of unsatisfied preconditions ((AT TRUCK-0 ?L) (ROAD ?L CITY-LOC-1))
;;Output: A list of preconditions paired with a list of positions of their variables or nil if one precondition does not contain variables (((AT TRUCK-0 ?L) (2)) ((ROAD ?L CITY-LOC-1)(1)))
(defun all-contain-variables (unsatisfied)
  (let ((contain-variables nil))
      (dotimes (i (length unsatisfied))
        (let* ((precondition (nth i unsatisfied))  ;;for every precondition in unsatisfy test if it contains a variable, ex. (AT TRUCK-0 ?L)
              (position-variables (mapcar #'(lambda(c)(search "?" c)) (mapcar 'symbol-name precondition)))) ;;by testing if any of its components contain a ?, example:(NIL NIL 0)
              (unless (find-if-not 'null position-variables) (return-from all-contain-variables nil)) ;;if one precondition does not contain a variable return nil 

     ;if the precondition contains variables, get the variable-positons-list, pair it with the precondition, and push that pair to contain-variables, ex. (precondition (1 2))
    (push (cons precondition (list(get-position-list position-variables))) contain-variables)
          ))
    (return-from all-contain-variables (reverse contain-variables))))
    


;; Input: a list containing the elements NIL and 0, with a 0 indicating the position of a variable, ex. (nil 0 0)
;; output: a list with the positions of 0 in the input list, ex. (1 2)
(defun get-position-list (lis)
  (let ((positions nil))
    (dotimes (i (length lis))                    ;;for every element in the list
      (if (eq 0 (nth i lis)) (push i positions)) ;;check if the element equals 0 and push its position i to the positions-list if that is the case
      )
    (reverse positions)) ;;reverse to return positions in the proper order
  )  


;; Input: a pair of a precondition containing at least one variable and the position-list of its variable(s), ex. ((AT TRUCK-0 ?L) (2))or ((ROAD CITY-LOC-1 ?L) (2))
;; output: a list of possible variable-bindings or nil if no binding could be found , ex. (((?L CITY-LOC-2 LOCATION)))
;;         or (((?L CITY-LOC-0 LOCATION)) ((?L CITY-LOC-2 LOCATION))) if both possibilities for ?L would satisfy the precondition
(defun get-variables (precondition)
  (let* ((variables nil)    ;;a list of possible variable bindings
        (precond (car precondition)) ; (AT TRUCK-0 ?L)
        (head (car precond)) ; AT
        (parameters (rest precond)) ;(TRUCK-0 ?L)
        (pos (mapcar '1- (second precondition))) ;; the list of positions of variables, each reduced by one because the predicate is handled separately, (2) -> (1)
        (cur-state nil) ;; to be filled from the current-status
        (matches-state nil)) ;; to be filled with parameters from the current state that are the same as the  precondition-parameters except for variables
    (setq cur-state (gethash head *current-status*)) ;; get a list of parameters of the same predicate as the precondition from the current-status ; ((TRUCK-0 LOC-1))  
  
    (loop for cur in cur-state do            ;;for every parameter-list of the same predicate as the precondition from the current-status
     (let((matches T))                       ;;Boolean to check if every parameter matches
      (dotimes (i (length parameters))            
               (let ((param (nth i parameters)))    ;;for every parameter of the precondition
                 (unless (find i pos)               ;;unless the position i is indicated in pos as a variable-position
       ;;when the parameter of the precondition and the parameter of the current-status parameterlist at the same position are not equal, set matches to nil -> parameters don't match
       (when (not (equal param (nth i cur))) (setq matches nil)))))
      (when matches                      ;;if matches is still T every parameter in cur was equal to every parameter in param at the same position except for variables, thus the parameters match
        (push cur matches-state))))
    
    (when (null matches-state)
      (return-from get-variables nil))  ;; if no matching parameters were found, the precondition cannot be satsified regardless of variables -> return nil

    (loop for p in matches-state do     ;;for every matching parameter-list, ex. p = TRUCK-0 LOC-1
  (let ((p-variables))             ;;list of variable-bindings matching p
    (loop for i in pos do              ;;for every variable-position indicated in pos
       ;;push to p-variables the variable from the precondition params and the constant from p at the same position to p-variables, ex. (l? LOC-1)
    (push (type-variable-bindings (list (nth i parameters) (nth i p))) p-variables)) ;;ex. (?L LOC-1)
    ;; push the variables-binding for p to the variables list - this way, if there is more than one possible variable-binding all of them are collected in variables
    (push (reverse p-variables) variables)))
    (return-from get-variables variables)))

  ;; Input: a list of action-preconditions, ex. ((AT ?V ?L1) (ROAD ?L1 ?L2)), and a theta
  ;; Output: the list of action-preconditions with substituted variables according to theta, ex. ((AT TRUCK-0 ?L1) (ROAD ?L1 CITY-LOC-1))
  (defun precondition-substitute (action-preconditions theta)
    (let ((precondlist nil))
      (unless (listp (first action-preconditions))
        (let ((predicate (first action-preconditions))
                    (params (rest action-preconditions))
                    (substitutedp nil))
                    (setq params (variable-substitute params theta)
                    substitutedp (cons predicate params))
                    (push substitutedp precondlist))
        (return-from precondition-substitute (reverse precondlist))
        )
        (loop for p in action-preconditions do
               (let ((predicate (first p))
                    (params (rest p))
                    (substitutedp nil))
                    (setq params (variable-substitute params theta)
                    substitutedp (cons predicate params))
                    (push substitutedp precondlist)))
      (reverse precondlist)))

  ;; Input: list of parameter-variables, ex. (?V ?L), and a theta, ex. '((?V TRUCK-0 VEHICLE)(?L2 CITY-LOC-1 LOCATION))
  ;;Output: a list of substituted parameters where a substitution could be found, ex. (TRUCK-0 ?L)
  (defun variable-substitute (parameters theta)
    (let ((new-parameters nil))
    (loop for e in parameters do
     (let ((new-param 
       (second (assoc e theta))))
         (if new-param
       (push new-param new-parameters)
       (push e new-parameters))))
     (reverse new-parameters)))

  
;;Input: a variable-binding of the form (?L CITY-LOC-0)
;;Output: the same variable-binding annotated with types (?L CITY-LOC-0 LOCATION) to match elements of theta
(defun type-variable-bindings (binding)
  (let ((types (hddl:hddl-problem-objects *problem*)))
  (let ((variable (first binding))
      (typed (assoc (second binding) types)))                             
  (return-from type-variable-bindings (cons variable typed)))))
      
      
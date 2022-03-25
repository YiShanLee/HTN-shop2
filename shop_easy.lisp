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
  (if (primitivep *current-task*)
    ; primitive (true)
    (update-primitive-task) 
    ; non-primitive (nil)  
    (update-nonprimitive-task) 
    )
)


(defun primitivep ()
 (let ((taskname (hddl-task-name *current-task*))
      (actionname))
  (loop for a in *actions* do
    (setq actionname (hddl-action-name a))
    (if (equal actionname taskname)
  (return t) nil))))
 
 ;; third layer of shop2
 ;; unify action and update state from primitive task
(defun update-primitive-task ()
 (let* ((Actions-lst (action-satisfier *actions* *current-task*)))
        (format t "~%step of update-primitive-task -> Actions-lst: ~A" Actions-lst)
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
 (let* ((act (action-substitute action)) ; (NOOP TRUCK-0 CITY-LOC-2)
        )
        (format t "~%step-> update-action-values")
         (modify-status action) ;; add pos-effect & delete neg-effect for current-status
         (push act *Plan*)
         (setq *Tasks* (cdr *Tasks*)) 
         (modify-constraints)
         (constraint)
         ; (setq *theta* (second action)) ;; todo)
 ) 
)
;; unify methods and update state from nonprimitive task
(defun update-nonprimitive-task ()
 (let* ((Methods-lst (method-satisfier *methods* *current-task*))) ; {(m . theta)...}  
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
   subm (copy-structure (hddl-method-subtasks (car method)))
   theta (cadr method); in sub(m) to precede the tasks that t precede
   subm (update-tasks subm) 
   *Tasks* (modify-constraints subm)) ;;constrain tasks with subtasks where appropriate
     (format t "~%update-nonprimitive-values-> *Tasks*: ~A~% subtasks: ~A~% gewaehlte methode: ~A" *Tasks* subm method)
     (append subm *Tasks*)  ;;adding sub(m) -> use append because push adds subtasks as list!
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
(defun modify-constraints (method-list)
  (let* ((updated-tasks-list nil) ;;constraint
        (theta (cadr method-list))
        (method (first method-list))
        (subtasks (hddl-method-subtasks method))
        )
    ; (format t "~%step-> modify-constraints: method-list -> ~A~% subtasks: ~A" method-list subtasks)
    (format t "~%step-> modify-constraints: theta -> ~A~% subtasks: ~A" theta subtasks)
    (if subtasks
       (update-tasks-and-T0 subtasks theta)
      )
    )
)


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
    (if (null (hddl-task-constraints task)) 
      (push task *T0*)))
  (reverse *T0*))


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
 (let ((pos-effects (hddl-action-pos-effects (first action)))
       (neg-effects (hddl-action-neg-effects (first action)))
       (theta (second action)))
   (if neg-effects 
       (delete-state (effect-substitute neg-effects theta))) ; {(and (AT ?V ?L2) (ROAD ?l1 ?l2)) & ((?V TRUCK-0 VEHICLE)) (?L2 CITY-LOC-0 LOCATION))}
   (if pos-effects 
       (add-state (effect-substitute pos-effects theta)))
  )
) 
 
 ;; find-state-p
 ;; input: (AT TRUCK-0 CITY-LOC-2)
 ;; output: T or NIL
; (defun find-state-p (preconditions) 
;     (let ((satisfied nil))
;      (dotimes (i (length preconditions))
;           (let ((precondition (nth i preconditions)))
;                (if (find T (mapcar #'(lambda(unit) (equal (rest precondition) unit)) (gethash (first precondition) *current-status*))) 
;                        (push T satisfied)  
;                               ; (format t "precondition: ~A~% value: ~A~%" (rest precondition) (gethash (first precondition) *current-status*))  
;                       (push nil satisfied))))
;      (if (find-if-not 'null satisfied) T nil)
;      )
;   )  
;; input: (AT TRUCK0 LOCATION1)
; L- =((AT TRUCK ?l2) (ROAD ?l2 Munich))
(defun find-state-p (preconditions) 
    (let ((satisfied nil)
          (unsatisfied nil)
          (contain-variables nil))
     (dotimes (i (length preconditions))
          (let ((precondition (nth i preconditions)))
               (if (find T (mapcar #'(lambda(unit) (equal (rest precondition) unit)) (gethash (first precondition) *current-status*))) 
                       (push precondition satisfied)  
                              ; (format t "precondition: ~A~% value: ~A~%" (rest precondition) (gethash (first precondition) *current-status*))  
                      (push precondition unsatisfied))))
     (if (null unsatisfied) (return-from find-state-p T)) ;; our precondition is correspond to current status
      (dotimes (i (length unsatisfied))
        (let ((precondition (nth i unsatisfied))
              (position-variables (mapcar #'(lambda(c)(search "?" c)) (mapcar 'symbol-name precondition)))) ;;(NIL NIL 0)
              (unless (find-if-not 'null position-variables) (return-from find-state-p nil)) 
            (push (cons precondition (get-position-list position-variables)) contain-variables) ;(precondition (1 2))
          )
        ) 
      ;[9:37 PM] Alisa Veronique Münsterberg
      ; (AT ?V ?L)   (AT TRUCK0 ?L)

      ; [9:41 PM] Alisa Veronique Münsterberg
      ; (((AT ?V ?L) (1 2)) ((AT TRUCK0 ?L) (2)))

      ; [9:42 PM] Alisa Veronique Münsterberg
      ; sort b y length (cdr element))
      ()

     )
  )

;; get the position 0 from a list (nil 0 0)
;; output: (1 2)
(defun get-position-list (lis)
  (let ((positions nil))
    (dotimes (i (length lis))
      (if (eq 0 (nth i lis)) (push i positions))
      )
    (reverse positions))
  )  

;; input: ((AT TUCK0 ?L) (2))
;; output: ((precondition (at )) ())  
(defun get-variables (precondition)
  (let* ((nonvariable nil)
        (variables nil)
        (precond (car precondition)) ; (AT TUCK0 ?L)
        (head (car precond)) ; at
        (parameters (rest precond)) ;(TUCK0 ?L)
        (pos (mapcar '1- (second precondition))) ;; without predicate
        (cur-state nil)
        (matches-state nil))
    (setq cur-state (gethash head *current-status*)) ;; list of the same predicate in side of the current-status ; ((Truck0 location1) (Truck0 location2))  
    ; (dotimes (i (length parameters))
    ;   (let ((param (nth i parameters)))
    ;     ; (mapcar #'(lambda(c)(if (equal param (nth i c)) (push c filtered-params))) cur-state)
        
    ;     )
    ;   )
    (loop for cur in cur-state do
     (let((matches T))
      (dotimes (i (length parameters))
               (let ((param (nth i parameters)))
                 (unless (find i pos) (if (not (equal param (nth i cur))) (setq matches nil))
                             ))
               )
      (if matches 
          (push cur matches-state))))
  )
;---------------------------------------------
;; for programming use to retrieve hashtable easier 
(defun print-hash-entry (key value)
(format t "~S ~S~%" key value))

(defun peek-status ()
 (maphash #'print-hash-entry *current-status*)  
)

;------------------------------------------------
;; make-lexicon of parameters and input values for all possible value
;; input: method-params or action-params *((?V VEHICLE) (?L3 LOCATION) (?L2 LOCATION) (?L1 LOCATION)) /  ; ((CITY-LOC-2 LOCATION) (CITY-LOC-1 LOCATION) (CITY-LOC-0 LOCATION) (TRUCK-0 VEHICLE))
; (defun make-lexicon (operator)
;   (let* ((types (hddl-problem-objects *problem*)) ; ((CITY-LOC-2 LOCATION) (CITY-LOC-1 LOCATION) (CITY-LOC-0 LOCATION) (TRUCK-0 VEHICLE))
;         (op-params (if (eql (type-of operator) 'HDDL-ACTION) (hddl-action-parameters operator) (hddl-method-parameters operator))) ;((?V VEHICLE) (?L3 LOCATION) (?L2 LOCATION) (?L1 LOCATION))
;         (method-params-list (mapcar #'hddl-method-parameters *methods*)  ;((?V VEHICLE) (?L3 LOCATION) (?L2 LOCATION) (?L1 LOCATION) (?L LOCATION)) 
;         (r-types (mapcar 'reverse types))      ; ((LOCATION CITY-LOC-2) (LOCATION CITY-LOC-1) (LOCATION CITY-LOC-0) (VEHICLE TRUCK-0))
;         (r-params (mapcar 'reverse op-params)) ;((VEHICLE ?V) (LOCATION ?L3) (LOCATION ?L2) (LOCATION ?L1))
;         (dotted-params (mapcar #'(lambda(c) (apply 'cons c)) r-params)) ;((VEHICLE . ?V) (LOCATION . ?L3) (LOCATION . ?L2) (LOCATION . ?L1))
;         (classified-head (delete-duplicates (mapcar 'first r-params)))
;         (newlist nil)
;         )
;     (dotimes (i (length r-params)) ; seperate to three group
;         (push (sublis (list (pop dot-params)) r-types) newlist)
;       )
;     ;; version 1
;     ; (setq newlist (delete-duplicates (flatten (apply 'mapcar 'list newlist))))
;     #|
;     ((?L1 CITY-LOC-2) (?L2 CITY-LOC-2) (LOCATION CITY-LOC-2) (?L1 CITY-LOC-1)
;      (?L2 CITY-LOC-1) (LOCATION CITY-LOC-1) (?L1 CITY-LOC-0) (?L2 CITY-LOC-0)
;      (LOCATION CITY-LOC-0) (VEHICLE TRUCK-0) (?V TRUCK-0))|#
;     ;; version 2 : prefered 
;     ; newlist 
;     #|(((?L1 CITY-LOC-2) (?L1 CITY-LOC-1) (?L1 CITY-LOC-0) (VEHICLE TRUCK-0))
;     ((?L2 CITY-LOC-2) (?L2 CITY-LOC-1) (?L2 CITY-LOC-0) (VEHICLE TRUCK-0))
;     ((LOCATION CITY-LOC-2) (LOCATION CITY-LOC-1) (LOCATION CITY-LOC-0)
;     (?V TRUCK-0)))|#
;     ;; version 3
;     ; (apply 'mapcar 'list newlist) 
;     #|
;     (((?L1 CITY-LOC-2) (?L2 CITY-LOC-2) (LOCATION CITY-LOC-2))
;      ((?L1 CITY-LOC-1) (?L2 CITY-LOC-1) (LOCATION CITY-LOC-1))
;      ((?L1 CITY-LOC-0) (?L2 CITY-LOC-0) (LOCATION CITY-LOC-0))
;      ((VEHICLE TRUCK-0) (VEHICLE TRUCK-0) (?V TRUCK-0)))
;     |#    
    
;     (defparameter *lexicon* newlist)
;     newlist)
; ) )
; ;; destruct list of list to one list 
; (defun flatten (li)
;   (cond ((null li) nil)
;      ( (= (length li) 2) `(,li))
;      (t (mapcan #'flatten li)))
;   )

;; shortcut for first action check to quickly bound current known status to run
;; if one action's precondition has already fulfilled the current-status and push this method to first-run tasks
;; need: current-status, actions, substitution of action's precondition 
;; if no positive effects, reluctant
;; if positive effects, search for the task of the same action name from *Tasks*, do it first 
;; get precondition from actions
;; parameters-binding within one action
;; input: *actions* & *current-status*
;; output: 
(defun quick-check ()
  (let ((actions (copy-structure *actions*))
        (pos-actions-list nil)
        )
    (mapcar #'(lambda(c) (if (hddl-action-pos-effects c) (push c pos-actions-list))) actions) ; get-actions that have postive effects
    ;; follow the action's precondition to search for its corresponding current-status
    (loop for pos-action in pos-actions-list
          #|
           #S(HDDL-ACTION
            :NAME DRIVE
            :PARAMETERS ((?V VEHICLE) (?L2 LOCATION) (?L1 LOCATION))
            :PRECONDITIONS (AND (AT ?V ?L1) (ROAD ?L1 ?L2))
            :NEG-EFFECTS ((AT ?V ?L1))
            :POS-EFFECTS ((AT ?V ?L2)))
          |#
          do(
             (let* ((precondition (remove-if-not 'consp (hddl-action-precondition pos-action))) ;((AT ?V ?L1) (ROAD ?L1 ?L2))
                   (precondition-head (mapcar 'first precondition))  ; (AT ROAD) 
                   (pos-effect (hddl-action-pos-effects pos-action)) ; ((AT ?V ?L1))
                   (neg-effect (hddl-action-neg-effects pos-action)) ; ((AT ?V ?L2))
                   (action-params (hddl-action-parameters pos-action)) ; ((?V VEHICLE) (?L2 LOCATION) (?L1 LOCATION)) 
                   (types (hddl-problem-objects *problem*)) ; ((CITY-LOC-2 LOCATION) (CITY-LOC-1 LOCATION) (CITY-LOC-0 LOCATION) (TRUCK-0 VEHICLE))
                   (theta-lists nil)
                   (current-state nil))               
            
               ;; (if (gethash (first aa1) *current-status*) (nconc (rest (assoc (first aa1) aa)) (gethash (first aa1) *current-status*)) nil) **aa: ((AT ?V ?L1) (ROAD ?L1 ?L2)) **aa1 (AT ROAD) 
               ;; precondition's parameter binding 
               ; get current-status to leave out suited predicates 
                (dotimes (i (length precondition-head))
                  (let* ((predicate (nth i precondition-head))
                        (status-lists (gethash predicate *current-status*))
                        )
                    (if status-lists
                        ;; get all of the current-status with the same predicate
                        (setq theta-lists (mapcar #'(lambda(status) (list* (cons predicate status))) status-lists))
                        )
                    )
                  )              
               ; if precondition true, then do substitute
               )
             )
      )
  )
  )
   #|current-status
               ROAD ((CITY-LOC-0 CITY-LOC-1) (CITY-LOC-1 CITY-LOC-0) (CITY-LOC-1 CITY-LOC-2) (CITY-LOC-2 CITY-LOC-1))
               AT ((TRUCK-0 CITY-LOC-2))
               |#
               #|(merge 'list t1 a1 #'eq :key #'cdr)
                ((LOCATION CITY-LOC-2) (LOCATION CITY-LOC-1) (LOCATION CITY-LOC-0)
                 (VEHICLE TRUCK-0) (VEHICLE ?V) (LOCATION ?L2) (LOCATION ?L1))
                &
                (setq newlist (make-list (length aa) :initial-element tt))
                (((LOCATION CITY-LOC-2) (LOCATION CITY-LOC-1) (LOCATION CITY-LOC-0)
                  (VEHICLE TRUCK-0))
                 ((LOCATION CITY-LOC-2) (LOCATION CITY-LOC-1) (LOCATION CITY-LOC-0)
                  (VEHICLE TRUCK-0))
                 ((LOCATION CITY-LOC-2) (LOCATION CITY-LOC-1) (LOCATION CITY-LOC-0)
                  (VEHICLE TRUCK-0)))
                &
                (apply #'mapcar #'list newlist)
                (((LOCATION CITY-LOC-2) (LOCATION CITY-LOC-2) (LOCATION CITY-LOC-2))
                 ((LOCATION CITY-LOC-1) (LOCATION CITY-LOC-1) (LOCATION CITY-LOC-1))
                 ((LOCATION CITY-LOC-0) (LOCATION CITY-LOC-0) (LOCATION CITY-LOC-0))
                 ((VEHICLE TRUCK-0) (VEHICLE TRUCK-0) (VEHICLE TRUCK-0)))
                &
                (delete-duplicates (mapcar 'first aa))
                (VEHICLE LOCATION)
                |#
                #|
                 (find (car (first a1)) t1 :key 'car)
                  (VEHICLE TRUCK-0) 
                |#
;------------------------------------------------------------------------------
; A ← {(a, θ) : a is a ground instance of an operator in D, 
;        θ is a substitution that unifies {head(a), t von problem}, 
;        and (s von problem) satisfies a’s preconditions}

;;;parameters-binding (theta-binding)
;; input: task & operator (as action or method)
;; output: ((?V TRUCK-0 VEHICLE)) (?L2 CITY-LOC-0 LOCATION)) as theta
;; pass to action-satisfier or method-satisfier
; ----- todo method
(defun parameters-binding (operator)
  (let ((op-param (if (eql (type-of operator) 'HDDL-ACTION) (hddl-action-parameters operator) (hddl-method-parameters operator))) ; ((?V VEHICLE) (?L2 LOCATION))
        (task-params (hddl-task-parameters *current-task*)) ;(TRUCK-0 CITY-LOC-0)
        (types (hddl-problem-objects *problem*)) ;((CITY-LOC-2 LOCATION) (CITY-LOC-1 LOCATION) (CITY-LOC-0 LOCATION) (TRUCK-0 VEHICLE)
        (binding-list nil))
    (dotimes (i (length task-params))
          (setq one-set (assoc (nth i task-params) types)) ;(TRUCK-0 VEHICLE)
          (setq reversed-op-param (mapcar #'reverse op-param))
          (setq binding-list (cons (cons (cadr (assoc (cadr one-set) reversed-op-param)) one-set) binding-list))
          )
    (if (eql (type-of operator) 'HDDL-ACTION)
        (format t "~%step-> parameters-binding with action ~S" binding-list)
        (format t "~%step-> parameters-binding with method ~S" binding-list)
        )
    (return-from parameters-binding (list (reverse binding-list))) ; ((?V TRUCK-0 VEHICLE) (?L2 CITY-LOC-0 LOCATION))
    )
  )

;;; operator-unifier-p
;; input: task & operator (as action or method)
;; output: True/ False
;; check name of tasks from action and current-task the same :: todo
;;       each parameter identical from operator (as action or method) and task 
;; pass to next function as action-satisfier or method-satisfier 
(defun operator-unifier-p (operator)
  (let ((op-task-name (if (eql (type-of operator) 'HDDL-ACTION) (hddl-action-name operator) (first (hddl-method-task operator)))) 
        (op-params (if (eql (type-of operator) 'HDDL-ACTION) (hddl-action-parameters operator) (rest (hddl-method-task operator))))
        (task-name (hddl-task-name *current-task*))
        (task-params (hddl-task-parameters *current-task*))
        )
      (cond ((and (eql (type-of operator) 'HDDL-ACTION) (eq task-name op-task-name) (a-identical-parameters-p task-params op-params)) (return-from operator-unifier-p T))
            ((and (eql (type-of operator) 'HDDL-METHOD) (eq task-name op-task-name) (m-identical-parameters-p task-params op-params operator)) (return-from operator-unifier-p T))
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
  (let ((types (hddl-problem-objects *problem*)) ; ((CITY-LOC-2 LOCATION) (CITY-LOC-1 LOCATION) (CITY-LOC-0 LOCATION) (TRUCK-0 VEHICLE))
        (op-param (hddl-method-parameters operator)) ;((?V VEHICLE) (?L LOCATION))
        (task-type nil))
      (setq task-type (mapcar #'(lambda(c) (second (assoc c types))) task-params))
      (setq task-type (sort task-type #'string<))  ;(LOCATION VEHICLE)
      (setq m-task-params (mapcar #'(lambda(c) (second (assoc c op-param))) m-task-params)) ;(VEHICLE LOCATION)
      (setq m-task-params (sort m-task-params #'string<))  ; (LOCATION VEHICLE)
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
(defun action-unifier (actions)
  (let ((actions-satisfied nil))
    (dotimes (i (length actions))
      (setq action (nth i actions))
      (if (operator-unifier-p action) 
          (setq actions-satisfied (cons (cons action (parameters-binding action)) actions-satisfied))
          ) 
      ) (return-from action-unifier actions-satisfied) ;;{(a.theta)}
    (format t "~%step-> action-unifier: current unified actions list: ~A" actions-satisfied)
    )
  )

;; action-satisfier
;; input: actions & current-task
;; output: actions list (unified with parameters and action's precondition satisfied with current-status)
;; action's precondition corrspond to current-status
;; leave out the satifying action list filtered by current-status
(defun action-satisfier (actions)
  (let ((actions-satisfied nil)
        (actions (action-unifier actions *current-task*))) ;;{(a.theta)}
    (if actions
     (dotimes (i (length actions))
          (let* ((action (nth i actions))
                (action-preconditions (hddl-action-preconditions (first action))) ;(AT ?V ?L2)
                (action-params (second action)) ; ((?V TRUCK-0 VEHICLE)) (?L2 CITY-LOC-0 LOCATION))
                  ;; binding parameters to precondition
                (preconditions nil) 
                )
            (setq preconditions (mapcar #'(lambda(c) (cons (first c) (mapcar 'second action-params))) action-preconditions))  ; (AT TRUCK-0 CITY-LOC-0)
           
           ;; function
            (dotime (i (length preconditions))      
             (let* ((precondition (nth i preconditions))
                   (pre-string (mapcar #'(lambda(c)(search "?" c)) (mapcar 'symbol-name precondition)))) ; (T nil nil)
              ;; null is to check content inside list, if there is (nil nil nil) then it return false
              (cond ((find-if-not 'null pre-string) (if (find-state-p preconditions) (setq actions-satisfied (cons action actions-satisfied))))
                     (t ()) ;;todo- variable precondition of (at ?v CITY-LOC-2)
                                 ))
                    
                    )
            ;; check the precondition in current-status ;;todo plural
            (if (find-state-p preconditions) (setq actions-satisfied (cons action actions-satisfied)))
          )
        ))
        (format t "~%step-> action-satisfier: current satisfied actions list: ~A" actions-satisfied)

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
(defun method-satisfier ()
  (let ((methods-satisfied nil))
    (dotimes (i (length *methods*))
      (setq method (nth i *methods*))
      (if (operator-unifier-p method) (setq methods-satisfied (cons (cons method (parameters-binding method)) methods-satisfied)))
    ) (return-from method-satisfier (values methods-satisfied))
      (format t "~%step-> method-satisfier: current satisfied methods list: ~A" (length methods-satisfied))
  )
)


;----------------------------------------------------------------------------------------------
;;; substitution with unifier and replace the required variables within each update
;; unify variables 
;; 
(defun action-substitute(action)
  (let ((action-name (hddl-action-name (first action))) ; NOOP
        (action-params (hddl-action-parameters (first action))) ;((?V VEHICLE) (?L2 LOCATION))
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
       (task-params (hddl-task-parameters subtask)) ; (?V ?L2) (?V ?L)/ (?V ?L2 ?L3)
       )
  
    (setf (hddl-task-parameters subtask) (sublis theta-dot-value task-params)) ;; if theta is subset of task-params, direct setf 
           
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
                   (cond ((hddl-task-constraints c) (and (task-substitute c theta) (setf (hddl-task-constraints c) (update-tasks (hddl-task-constraints c) theta)))); if subtask has constraints, push its task first to *Tasks*
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
; ; remove task from task list
; (defun remove-task (current-task tasks)
;   (setq tasks (remove current-task tasks))
;   (return-from remove-task tasks)
;   )
#|
Contains only finished and tested functions and methods
|#

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
  (defparameter *current-status* nil)
  (defparameter *Tasks* (hddl:hddl-problem-tasks *problem*))
  (defparameter *theta* nil)  \
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
 ;; todo: ueberpruefen
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
(defun find-state-p (precondition) 
    (if (find T (mapcar #'(lambda(unit) (equal (rest precondition) unit)) (gethash (first precondition) *current-status*))) 
         T  
                ; (format t "precondition: ~A~% value: ~A~%" (rest precondition) (gethash (first precondition) *current-status*))  
        nil)
  )     
;---------------------------------------------
(defun primitivep (task)
 (let ((taskname (hddl:hddl-task-name task))
      (actionname))
  (loop for a in *actions* do
    (setq actionname (hddl:hddl-action-name a))
    (if (equal actionname taskname)
  (return t)))));;ausreichend, wenn nicht t ausgegeben wird, wird automatisch nil ausgegeben!
;---------------------------------------------
;; unifier & satisfier & substitute

;;;action
;;; a-identical-parameters-p
;; input: task parameters & action parameters (operator as action or method)
;; output: True / False
;; pass to next function as operator-unifier-p
(defun a-identical-parameters-p (task-params op-params) ; (TRUCK-0 CITY-LOC-0) / ((?V VEHICLE) (?L2 LOCATION))
  (let ((types (hddl:hddl-problem-objects *problem*))
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
      ) (return-from action-unifier actions-satisfied) ;;{(a.theta)}
    (format t "~%step-> action-unifier: current unified actions list: ~A" actions-satisfied)
    )
  )

;; action-satisfier
;; input: actions & current-task
;; output: actions list (unified with parameters and action's precondition satisfied with current-status)
;; action's precondition corrspond to current-status
;; leave out the satifying action list filtered by current-status
(defun action-satisfier (actions current-task)
  (let ((actions-satisfied nil)
        (actions (action-unifier actions current-task))) ;;{(a.theta)}
    (if actions
     (dotimes (i (length actions))
          (let* ((action (nth i actions))
                (action-precondition (hddl-action-preconditions (first action))) ;(AT ?V ?L2)
                (action-params (second action)) ; ((?V TRUCK-0 VEHICLE)) (?L2 CITY-LOC-0 LOCATION))
                  ;; binding parameters to precondition
                (precondition nil) 
                )
            (setq precondition (cons (first action-precondition) (mapcar 'second action-params))) ; (AT TRUCK-0 CITY-LOC-0)
            ;; check the precondition in current-status
            (if (find-state-p precondition) (setq actions-satisfied (cons action actions-satisfied)))
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
;; method


;;zsm
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
;; pass to next function as action-unifier or method-satisfier 
(defun operator-unifier-p (operator task)
  (let ((op-task-name (if (eql (type-of operator) 'HDDL-ACTION) (hddl-action-name operator) (first (hddl-method-task operator)))) 
        (op-params (if (eql (type-of operator) 'HDDL-ACTION) (hddl-action-parameters operator) (rest (hddl-method-task operator))))
        (task-name (hddl-task-name task))
        (task-params (hddl-task-parameters task))
        )
      (cond ((and (eql (type-of operator) 'HDDL-ACTION) (eq task-name op-task-name) (a-identical-parameters-p task-params op-params)) (return-from operator-unifier-p T))
            ((and (eql (type-of operator) 'HDDL-METHOD) (eq task-name op-task-name) (m-identical-parameters-p task-params op-params operator)) (return-from operator-unifier-p T))
            (t (return-from operator-unifier-p nil)))
  ))
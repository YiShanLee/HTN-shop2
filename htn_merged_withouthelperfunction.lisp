#|
The functions in this file utilize helper-functions and structures from the 
:read-hddl-package, called HDDL-structures (ex. HDDL-action-structures) in the 
following commentary.
(nicknames :hddl and :read-hddl).
|#
;;(defparameter *file* (probe-file "read_hddl.lisp"))
;;(eval-when (:compile-toplevel :load-toplevel :execute)
;;  (load *file*))

;; main operator of shop2 reads input files and starts the SHOP2-Planner
(defun main-operator ()
  "Reads input file and starts the Shop2-Planner"
  (read-input)
  (shop2-plan)
  )

;-----------------------------------------------------------
;;Functions for reading of files and defining and initializing of global parameters

;;Prompts the user to enter the appropriate filepaths for the HDDL-domain and -problem
;;and calls fetch-initial-state with the filepaths to read all relevant information from the files
;;Input: optional domain- and problem-filepath 
(defun read-input (&optional (domain-path "domain.hddl") (problem-path "problem.hddl"))
  (unless domain-path (setq domain-path "domain.hddl"
                          problem-path "problem.hddl"))
  (princ "Enter domain filepath")
  (setq domain-path (string(read)))
  (princ "Enter problem filepath")
  (setq problem-path (string(read)))
  (fetch-initial-state domain-path problem-path))
 
;;Defines global variables based on the input domain-knowledge and problem-filepaths and
;;initializes the *current-status*
#|*domain* = the HDDL-domain-structure extracted from the domain-filepath
*problem* = the HDDL-problem-structure exttracted from the domain-filepath
*T0* = the list of tasks that are not constrained by other tasks, i.e. that could potentially 
		be executed at the point of this iteration of *T0*'s construction;
		starts as an empty list and is first constructed at the beginning of the 
		planning process and then regularly reconstructed whenever the task-list is changed
		to account for changes in task-constraints
*Plan* = the plan that will be returned as a solution for the HTN-planning-problem;
		 starts as an empty list and is gradually filled over the planning process 
		 whenever a suitable executable action is found; 
		 if a solution for the problem could be found, the Plan will contain a sequence of
		 executable actions that will fulfill all given tasks
*actions* = a list of all the HDDL-actions in the domain as HDDL-action-structures
*methods* = a list of all the HDDL-methods in the domain as HDDL-method-structures
*Tasks* = a list of all HDDL-tasks to be fulfilled in the planning problem;
			tasks are removed as they are fulfilled or exchanged for subtasks 
			if deconstructed by a suitable method

*current-task* = the HDDL-task as a HDDL-task-structure whose executability is being tested at a 
				 particular time-step; starts as empty and is chosen at the beginning of the 
				 planning process and then rechosen if it could be fulfilled succesfully 
				 or not fulfilled at the time-step
*current-status* = the state of the world at a time-step as a hashtable; 
                   starts empty and is subsequently initialized:
				   gets changed during the planning process according to the effects of executed actions
*lexicon* = a list of pairs of variables from the domain and their types as well as
			objects from the problem and their types, ex. ((?V VEHICLE) (CITY-LOC-0 LOCATION))
*analyzing-Subtasks* = a Boolean that is initialized as nil; it is set to T only after decomposition of a method to subtasks to indicate that in the next step the subtasks should be analyzed as opposed to all general tasks
|#
(defun fetch-initial-state (domain-path problem-path)
  "Gets the initial state of global variables"
  (defparameter *domain* (hddl:read-hddl-domain domain-path))
  (defparameter *problem* (hddl:read-hddl-problem problem-path))
  (defparameter *T0* '())
  (defparameter *Plan* '())
  (defparameter *actions*  (hddl:hddl-domain-actions *domain*))
  (defparameter *methods*  (hddl:hddl-domain-methods *domain*))
  (defparameter *Tasks* (hddl:hddl-problem-tasks *problem*))
  (defparameter *current-task* nil)
  (defparameter *analyzing-Subtasks* nil) 
  (shop2:get-global-variables *domain* *problem* *actions* *methods*)
)
  
 ;--------------------------------------------
;; main method for first layer of shop2
;; As long as there are still executable tasks, try to solve them,
;; if there are no more tasks left, return the *Plan*.
(defun shop2-plan (&optional plan tasks state)
  "The begin of Shop2-Planner"
  (setq *T0* (shop2:constraint *Tasks*))
    (loop while *T0* do
  ;;if analyzing subtasks is T take the first task in *T0* as current-task, guaranteeing that the next task will be a subtask, because the subtasks are appended at the beginning of *Tasks*
      (if *analyzing-Subtasks*
	  (setq *current-task* (car *T0*))
	  (setq *current-task* (nth (random (length *T0*)) *T0*)))
      (format t "~%----------------------------------------------~%shop2-plan->~% *T0*: ~A~% *current-task*: ~A ~%----------------------------------------------~%" *T0* *current-task*)
      (setq *analyzing-Subtasks* nil
	    *T0* (remove *current-task* *T0*))
      (resolve-task))
   (if (null *Tasks*)
     (return-from shop2-plan (shop2:planner-output *Plan*)))) 

;; second layer of shop2 
;; check if the *current-task* is a primitive or compound-task and continue accordingly
(defun resolve-task () 
  "Checks the task as primitive one or not"
  (if (shop2:primitivep *current-task*)
    (update-primitive-task)   
    (update-nonprimitive-task) 
    )
)

;; third layer of shop2 for primitive tasks
 ;; unify action and update state from primitive tasks
(defun update-primitive-task ()
  "Searches for a list of applicable actions through prarameters binding and updates the primitive task"
 (let* ((Actions-lst (shop2:action-satisfier *current-task*)))
        (format t "~%----------------------------------------------~%update-primitive-task->~% Actions-lst: ~A~%----------------------------------------------~%" Actions-lst)
          (cond ((eq Actions-lst nil) (return-from update-primitive-task nil))
                (t (update-action-values (nth (random (length Actions-lst)) Actions-lst)))
          )
  )
)
 
(defun update-action-values (action)
  "Updates the tasks list and plan and T0 with one applicable action binding with its parameters"
 (let* ((act (shop2:action-substitute action)) ; (NOOP TRUCK-0 CITY-LOC-2)
        )
         (shop2:modify-status action) 
         (setq *Plan* (append *Plan* (list act))) 
         (setq *Tasks* (remove *current-task* *Tasks*)) 
         (shop2:modify-constraints *current-task* *Tasks*)
         (shop2:constraint *Tasks*)
         (format t "~%----------------------------------------------~%update-action-values->~% *Plan*: ~A~% *T0*: ~A~%----------------------------------------------~%" *Plan* *T0*)
 ) 
)

;; unify methods and update state from nonprimitive task
(defun update-nonprimitive-task ()
  "Searches for a list of applicable methods through parameters binding and updates the non-primitive task"
 (let* ((Methods-lst (shop2:method-satisfier *current-task*))) ; {(m . theta)...}  
     (format t "~%----------------------------------------------~%update-nonprimitive-task->~% Methods-lst: ~A~%----------------------------------------------~%" (length Methods-lst))
     (cond ((eq Methods-lst nil) (return-from update-nonprimitive-task nil)) ; if M = empty then return nil to resolve task
            (t  (update-nonprimitive-values (nth (random (length Methods-lst)) Methods-lst)))) ; nondeterministically choose a pair (m, θ) ∈ M (random choose)
 ) 
)

(defun update-nonprimitive-values (method)
  "Updates tasks list through removment of current task and through concatenation with its parameters-contained subtasks"
   (setq *Tasks* (remove *current-task* *Tasks*) ; modify T by removing t, (removin in constraint-lists happens later through constraining with subtasks!
          subm (mapcar #'hddl:copy-hddl-task (hddl:hddl-method-subtasks (car method)))         
          theta (cadr method); in sub(m) to precede the tasks that t precede
           subm (shop2:substitute-tasks-list subm theta) 
          *Tasks* (shop2:modify-constraints *current-task* subm) ;;constrain tasks with subtasks where appropriate
          *Tasks* (append subm *Tasks*))  ;;adding sub(m) -> use append because push adds subtasks as list!
(format t "~%----------------------------------------------~%update-nonprimitive-values->~% Chosen method: ~A~% Subtasks: ~A~% New-tasks: ~A~%----------------------------------------------~%" method subm *Tasks*)
;; (if (not (null subm)) (setq *T0* (constraint subm)))
(if (not (null subm)) (setq *analyzing-Subtasks* T))
(setq *T0* (shop2:constraint *Tasks*)))



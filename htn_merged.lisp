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
  (defparameter *current-status* (hddl:hddl-problem-init-status *problem*))
  (defparameter *Tasks* (hddl:hddl-problem-tasks *problem*))
  (defparameter *theta* nil))
  
 ;--------------------------------------------
;; main method for first layer of shop2


;; second layer of shop2 
;; check primitive and restore the plan 


;; third layer of shop2
 ;; unify action and update state from primitive task
 
 
 
 
 ;--------------------------------------
;;builds T0: checks for all tasks if constraint-slot is empty and adds it to T0 if that's the case
;;Input: if no tasklist is provided, the global tasklist is used as default value
;; Output: *T0*(defun constraint(&optional (tasks *Tasks*))
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

 
 
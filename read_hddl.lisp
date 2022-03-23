
(defpackage :read-hddl-package
  (:use :cl)(:nicknames :read-hddl :hddl)
  (:export read-file read-hddl-domain read-hddl-problem
	   hddl-predicate-name  hddl-predicate-parameters
	   make-hddl-method hddl-method-name hddl-method-parameters  hddl-method-task
	   hddl-method-subtasks hddl-method-ordered-subtasks
	   make-hddl-action hddl-action-name hddl-action-name-p hddl-action-parameters
	   hddl-action-preconditions hddl-action-neg-effects hddl-action-pos-effects
	   make-hddl-domain hddl-domain-name hddl-domain-requirements  hddl-domain-types
	   hddl-domain-predicates hddl-domain-tasks hddl-domain-methods hddl-domain-actions
	   make-hddl-task hddl-task-name hddl-task-parameters hddl-task-constraints
	   make-hddl-object hddl-object-name hddl-object-type
	   make-hddl-problem hddl-problem-name hddl-problem-domain  hddl-problem-objects
	   hddl-problem-tasks hddl-problem-ordering hddl-problem-constraints
	   hddl-problem-init-status))
(in-package read-hddl-package)

;;define global variables

;; structures for the elements of a hddl-domain or hddl-problem and for the problem and domain as a whole
(defstruct hddl-predicate name parameters)
(defstruct hddl-method name parameters task subtasks ordered-subtasks);;Achtung: Subtasks können auch ordered-subtasks heißen
(defstruct hddl-action name parameters preconditions neg-effects pos-effects)
(defstruct hddl-domain name requirements types predicates tasks methods actions)
(defstruct hddl-task name parameters constraints)  
(defstruct hddl-object name type)
(defstruct hddl-problem name domain objects tasks ordering constraints init-status)

;; read in a file
;;Input: a filename as a string
;;Output: the content of the file as a stream
(defun read-file (filename)
  (with-open-file (in filename)
    (read in))) 
   

;;read in hddl-domain-files and transform them into usable hddl-structures
;; Input: a hddl-domain-file
;; Output: a hddl-domain-structure
(defun read-hddl-domain (filename)
   (declare (optimize debug))
  (print "Reading domain-file...")
  ;;error handling
  (unless (search ".hddl" filename)  
    (error "This function can only read a HDDL file - make sure the file you want to read ends in .hddl! "))
  (unless (search "domain" filename) 
    (error "This function can only read a domain-HDDL file - make sure the file you want to read is the domain-file! "))

  ;;read in file and separate into htn-components:
  (let* ((read-domain (read-file filename))
	 types 
	 requirements
	 predicates
	 tasks 
	 methods 
	 actions 
	 (look-up '((:types . types) (:requirements . requirements) (:predicates . predicates) (:task . tasks) (:method . methods) (:action . actions))))

    ;;lexical declaration of variables for matching of defined lists and elements in look-up:
    (declare (special types requirements predicates tasks methods actions))
    
    ;; for each element of the domain-list search for its first element in the look-up list and
    ;; push it to the corresponding list (second half of the pair)
    (dolist (element (cddr  read-domain))
      (let ((key (assoc (first element) look-up)))
	(when key
	  (set (cdr key) (cons element (symbol-value (cdr key))))))) ;;add elements of the domain to the appropriate lists
    
    ;; transform elements of the domain-lists into the appropriate structures
    (let ((hddl-requirements)
	  (hddl-types)
	  (hddl-tasks)
	  (hddl-methods)
	  (temp-actions)
	  (hddl-actions)
	  (hddl-predicates)
	  (domain))
      
      (setq hddl-requirements
	    (cdr (car requirements)) ;;remove unneccessary key-word from beginning of list
	    hddl-types
	    (typing (cdr (car types)))
	    
	    hddl-tasks
	     (loop for (x name y . parameters) in tasks collect
		     (make-hddl-task :name name :parameters (typing (car parameters)) :constraints nil))

	     ;;for hddl-methods, first check if methods have ordered-subtasks and annotate accordingly
	     hddl-methods (ordered-subtasks-p methods)
	     ;;then constrain subtasks according to that annotation
	     hddl-methods (constrain-subtasks hddl-methods)
	    
      ;; separate effects into positive and negative effects before building hddl-actions:
	    temp-actions (separate-effects actions)
      ;;use list with separated effects to build hddl-actions
	    hddl-actions
	    (loop for (x name y parameters z preconditions w neg-effects pos-effects) in temp-actions collect
		      (make-hddl-action :name name :parameters (typing parameters) :preconditions (remove-if-not 'consp preconditions) :neg-effects  neg-effects :pos-effects pos-effects))
	    
	    hddl-predicates
	    (loop for (name . parameters) in (cdr (car predicates)) collect
		      (make-hddl-predicate :name name :parameters (typing parameters))))
      

      ;;finally combine everything into domain structure, reversing the task-, method- and actions-lists
      ;;because their lines in the stream each started with the keyword and were thus added to the lists in reverse through cons
	   (setq domain (make-hddl-domain :name (second read-domain) :requirements hddl-requirements
		    :types hddl-types :predicates hddl-predicates :tasks (reverse hddl-tasks)
					  :methods (reverse hddl-methods) :actions (reverse hddl-actions))))))




;---------------------------------------------------------------------------
;;read in hddl-problems and transform them into usable hddl-problem structures
;;Input: a hddl-problem-file
;;Output: a hddl-problem structure
(defun read-hddl-problem (filename)
  (declare (optimize debug))
  (print "Reading problem-file...")
  (unless (search ".hddl" filename) 
    (error "This function can only read a HDDL file - make sure the file you want to read ends in .hddl! "))
  (unless (search "problem" filename) 
    (error "This function can only read a problem-HDDL file - make sure the file you want to read is the problem-file! "))
  
  (let* ((read-problem (read-file filename))
	 (objects)
	 (htn)
	 (status)
	 (look-up '((:objects . objects)(:htn . htn) (:init . status))))
    (declare (special objects htn status))
    (dolist (element (cdr read-problem))
     (let ((key (assoc (first element) look-up)))
       (when key
	 (set (cdr key) (cons element (symbol-value (cdr key)))))))
    (setq htn (cdr (car htn)))

;;second iteration of separating file into components - separate htn into tasks, ordering,constraints; not using look-up-list-method, because the components are not separated into differnet lists already, e.g. a list with first element ":TASKS" and second element a list of tasks, but rather elements in the list beginning with ":HTN")
    (let ((tasks)
	  (ordering)
	  (constraints))
      
      (if (equal (string (car htn)) "TASKS")
	  (setq tasks (remove-if-not 'consp (second htn)) ;;remove "AND" at the beginning of the task-list if there is one
		htn (cddr htn)))
      (if (equal (string (car htn)) "ORDERING")
	  (setq ordering (second htn)
		htn (cddr htn)))
      (if (equal (string (car htn)) "CONSTRAINTS")
	  (setq constraints (second htn)
		htn (cddr htn)))
   
    ;; transform elements of the domain-lists into the appropriate structures
      (let ((hddl-tasks)
	    (problem))
      (setq hddl-tasks
	    (loop for (name . parameters) in tasks collect
		   (make-hddl-task :name name :parameters parameters :constraints nil))
	    problem (make-hddl-problem :name (second read-problem) :domain (cdr (third read-problem)) :objects (typing (cdr (car objects))) :tasks hddl-tasks
				       :ordering ordering :constraints constraints :init-status (cdr (car status))))))))
    




 ;---------------------------------------------------------------------------
;;Helper-functions for read-hddl-domain and read-hddl-problem

;;checks if method-subtasks are of type ordered-subtasks
;;Input: a list of methods
;;Output: a list of hddl- methods annotated with the slot :ordered-subtasks containing the predicate indicating if subtasks are ordered
(defun ordered-subtasks-p (methods)
  (let ((pushmethods)
	(ordered))
    (loop for (x name y parameters z task w subtasks) in methods do
      (if (search "ORDERED" (string w))
	  (setq ordered t)
	  (setq ordered nil))
	  (push (make-hddl-method :name name :parameters (typing parameters) :task task :subtasks subtasks :ordered-subtasks ordered) pushmethods))
    (reverse pushmethods)))

;;constrains subtasks of a method with each other if the method has ordered-subtasks, in such a way
;;that every subtask is constrained by the subtask(s) before it, to ensure that they are fulfilled in the exact given order
;;Input: a list of hddl-methods
;;Output: a list of hddl-methods with constrained subtasks where subtasks are ordered
(defun constrain-subtasks (hddl-methods)
  (let ((new-hddl-methods))
    (loop for method in hddl-methods do
	(let ((subtasks  (remove-if-not 'consp(hddl-method-subtasks method)))
	      (new-subtasks)
	      (ordered nil))
	  (if (not (null (hddl-method-ordered-subtasks method))) ;; set ordered to true if the ordered-subtasks slot of the method is not nil -> the subtasks are ordered
	      (setq ordered t))
	  (setq new-subtasks (loop for (name . parameters) in subtasks collect
								       (make-hddl-task :name name :parameters parameters :constraints nil)))
	  ;;if the subtasks are ordered, add  all subtasks that need to be acted out first to the constraints of all others
	  (if ordered
	      (loop for subs on (reverse new-subtasks) do 
			(setf (hddl-task-constraints (car subs)) 
				(loop for sub in (cdr subs) collect
					sub))))
			
	  (setf (hddl-method-subtasks method) new-subtasks))
	(setq new-hddl-methods (push method new-hddl-methods)))
    (reverse new-hddl-methods)))
    
    

;;separate-effects separates the effects of a list of actions into positive and negative effects
;;Input: a list of actions
;; Output: the same list of actions but now annotated with separate positive and negative effects
(defun separate-effects (actions)
  (loop for action in actions collect
	(let* ((new-action (butlast action));;keep everything but the effects of an action intact
	      (effect (remove-if-not 'consp (car (last action))));; all effects,remove "AND" at the beginning if there is one
	      (pos-effects)
	      (neg-effects)) 
	  
	  ;;separate effects into pos/neg using keyword "not":
	      (loop for e in effect do
		(if (equal (string (car e)) "NOT") 
		    (push (cdr e) neg-effects)
		    (push (list e) pos-effects)))
	  (setq new-action (append new-action neg-effects pos-effects))
	  new-action)) ;; collect the new actions
  )

;;takes list of variables and types and returns a list of pairs (variable . type)
;; works for all typing needs!
(defun typing (lst)
  (let* ((p 0) ;; start with 0 instead of -1 because we want to take the element after "-",too
	(separateby))
    (loop for x in lst do
      (setq p (+ p 1))
	  (if (equal "-" (string x))
	      (push p separateby)))
    (setq separateby (reverse separateby))
    (let ((separate)
	  (j 0)
	  (typedvariables))
      (loop for i in separateby do
	(push (subseq lst j (+ i 1)) separate)
	(setq j (+ 1 i))
	(if (> j (- (length lst) 1))
	    (setq j (- (length lst) 1))))
      (loop for e in separate do
	(let ((type (last e))
	      (variables (reverse (cddr(reverse e)))))
	  (loop for v in variables do
	    (push (cons v type) typedvariables))))
      typedvariables)))
		   


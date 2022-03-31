#|
This package defines functions for reading in domain- and problem-files written in the HTN-planning 
language HDDL.
The files are analysed and returned as structures with the approproate components.
The components of the  returned domain or problem can themselves contain other structures or lists of structures.
|#

;;Reading functions, access functions and constructors to HDDL-structures are exported:
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
	   make-hddl-task hddl-task-name hddl-task-parameters hddl-task-constraints copy-hddl-task 
	   make-hddl-problem hddl-problem-name hddl-problem-domain  hddl-problem-objects
	   hddl-problem-tasks hddl-problem-ordering hddl-problem-constraints
	   hddl-problem-init-status))
(in-package read-hddl-package)

;---------------------------------------------------------------------------------------------------------------
;;HDDL-structures:

;;A HDDL-predicate is defined as a name (String) and list of parameters (variables with types).
(defstruct hddl-predicate name parameters)

;;A HDDL-task is defined as a name (String), a list of parameters (variables with types) and a list of constraints (HDDL-tasks)
;;representing other tasks that have to be fulfilled before this task.
(defstruct hddl-task name parameters constraints)

;;A HDDL-method is defined as a name(String), a list of parameters(variables with types), an abstract task that can be solved by the method (HDDL-task),
;;the subtasks the task is deconstructed to via the method (a list of HDDL-tasks) and a Boolean ordered-subtasks indicating whether the subtasks are ordered or not.
(defstruct hddl-method name parameters task subtasks ordered-subtasks)

;;A HDDL-action is defined as a name(String), a list of parameters(variables with types), a list of preconditions, a list of negative and a list of positive effects.
(defstruct hddl-action name parameters preconditions neg-effects pos-effects)

;;A HDDL-domain is defined as a name (String), a list of requirements (list of keywords), a list of types of variables in the domain (list of lists of the form (subtype type)),
;; a list of HDDL-predicates, a list of HDDL-tasks, a list of HDDL-methods and a list of HDDL-actions.
(defstruct hddl-domain name requirements types predicates tasks methods actions)

;;A HDDL-problem is defined as a name (String), a corresponding domain (String), a list ob typed objects ((object type)), a list of HDDL-tasks, an ordering,
;; a list of constraints and the initial status given as a list of ground predicates.
(defstruct hddl-problem name domain objects tasks ordering constraints init-status)

;---------------------------------------------------------------------------
;;Helper-functions for read-hddl-domain and read-hddl-problem

;;Reads in a file from a filepath.
;;Input: a filepath as a string
;;Output: the contents of the file as a stream
(defun read-file (filename)
  "Reads in a file from a filepath."
  (with-open-file (in filename)
    (read in))) 

;;Takes a list that may begin with "AND" and returns the same list without and or the input list without changes if there was no "AND"		   
;;Input: a list
;;Output: the same list with removed "AND" if there was one
(defun remove-and (list)
  "Takes a list that may begin with AND and returns the same list without and or the input list without changes if there was no AND"
  (let ((newlst nil))
  (if (equal (string (car list)) "AND")
      (setq newlst (cdr list))
      (setq newlst list))))

;;Takes a list of variables and types and returns a list of pairs (variable  type)
;;Input: a list of variables and types of the form (variable - type) or (variable1 .. variablen - type)
;;Output: a list of typed variables of the form (variable type)

(defun typing (lst) ;;example input: (?v - vehicle ?l1 ?l2 - location)
  "Takes a list of variables and types and returns a list of pairs (variable type)"
  (let* ((p 0) ;;denotes the position in a string
	 (separateby))
    
    (loop for x in lst do  ;;for every element in the list
      (setq p (+ p 1))     ;;set p +1
	  (if (equal "-" (string x)) ;; if x is a "-" (denoting the separation between variables and a type)
	      (push p separateby)))  ;; push p to the list to separate by (the item after "-")
    (setq separateby (reverse separateby)) ;;example: (2 7)
    
    (let ((separate)
	  (j 0)                               ;;marks the beginning of each separate list of variables with the same type, example: (?v - vehicle)
	  (typedvariables))
      
      (loop for i in separateby do            ;;for every element in separateby
	(push (subseq lst j (+ i 1)) separate) ;; push the subsequence of the input list starting at j and ending at i+1 to separate, example (subseq lst 0 3) will return (?v - vehicle)
	(setq j (+ 1 i))                       ;; restart with the position after the extracted subsequence, example: 0 + 3 = 3
	(if (> j (- (length lst) 1))           ;; if j is now bigger than the last position in the list set it to the last position in the list
	    (setq j (- (length lst) 1))))     
      
      (loop for e in separate do               ;; for every list of (variables - type) in the separate list, example (?v - vehicle)
	(let ((type (last e))                  ;; let the last element be the type, example: vehicle
	      (variables (cddr(reverse e))))  ;;let the variables be the list of every item except the last two ( - type), in reverse to preserve ordering when pushing,  example (?v)
	  (loop for v in variables do               ;;for every variable push the variable with the type as a list to the list of typed variables, example (?v vehicle)
	     (push (cons v type) typedvariables))))
      typedvariables)))                           ;;return the list of all typed variables

;;Checks if method-subtasks are of the type ordered-subtasks
;;Input: a list of methods
;;Output: a list of HDDL-methods annotated with the slot :ordered-subtasks containing a Boolean indicating whether subtasks are ordered or not
(defun ordered-subtasks-p (methods)
  "Checks if method-subtasks are of the type ordered-subtasks"
  (let ((newmethods)
	(ordered))
    (loop for (x name y parameters z task w subtasks) in methods do
      (if (search "ORDERED" (string w))       ;;if the string before the subtasks contains the word ordered (:ORDERED-SUBTASKS) then set ordered to true, else to nil
	  (setq ordered t)
	  (setq ordered nil))
      
	  (push (make-hddl-method :name name :parameters (typing parameters) :task task :subtasks subtasks :ordered-subtasks ordered) newmethods));;make HDDL-method and push to new methodslist
    (reverse newmethods)))



;;Constrains subtasks of a method with each other if the method has ordered subtasks in such a way
;;that every subtask is constrained by the subtask(s) before it, to ensure that they are fulfilled in the exact given order.
;;Input: a list of hddl-methods
;;Output: a list of hddl-methods with constrained subtasks where subtasks are ordered
(defun constrain-subtasks (hddl-methods)
  "Constrains subtasks of a method with each other if the method has ordered subtasks in such a way that every subtask is constrained by the subtask(s) before it, to ensure that they are fulfilled in the exact given order."
  (let ((new-hddl-methods))
    
    (loop for method in hddl-methods do
	(let ((subtasks  (remove-and (hddl-method-subtasks method))) ;;remove unnecessary "AND" from subtasks-list if present
	      (new-subtasks)
	      (ordered nil))
	  
	  (if (not (null (hddl-method-ordered-subtasks method))) ;; set ordered to true if the ordered-subtasks slot of the method is not nil -> the subtasks are ordered
	      (setq ordered t))
	  
	  (setq new-subtasks (loop for (name . parameters) in subtasks collect         ;;make HDDL-tasks out of subtasks with empty constraint-lists
								       (make-hddl-task :name name :parameters parameters :constraints nil)))
	  
	  ;;if the subtasks of the method are ordered, constrain the subtasks of the method in such a way that the constraint-list of each subtask  contains the
	  ;; subtask(s) that are before it in the ordering and thus have to be fulfilled before the subtask can be fulfilled.
	  (if ordered
	      (loop for subs on (reverse new-subtasks) do 
			(setf (hddl-task-constraints (car subs)) 
				(loop for sub in (cdr subs) collect
					sub))))
			
	  (setf (hddl-method-subtasks method) new-subtasks)) ;;set the newly constrained subtasks as the new subtasks of the method
	(setq new-hddl-methods (push method new-hddl-methods))) ;; push the method to the list of constrained methods
    (reverse new-hddl-methods)))                               ;;reverse to preserve previous method-order
    
    

;;Separates the effects of a list of actions into positive and negative effects for each contained action
;; Input: a list of actions
;; Output: the same list of actions but now annotated with separate positive and negative effects

(defun separate-effects (actions)
  "Separates the effects of a list of actions into positive and negative effects for each contained action"
  (loop for action in actions collect                            ;;for every action
	(let* ((new-action (butlast action))                     ;;keep everything but the effects of an action intact
	      (effect (remove-and (car (last action))));; make a list of all effects,remove "AND" at the beginning if present
	      (pos-effects)
	      (neg-effects)) 
	  
	      (loop for e in effect do                  ;;separate effects into pos/neg using keyword "not"
		(if (equal (string (car e)) "NOT")  
		    (push (cdr e) neg-effects)
		    (push (list e) pos-effects)))
	  (setq new-action (append new-action neg-effects pos-effects)) ;;append the negative and positive effect lists to the actions
	  new-action)) ;; collect the new actions
  )


;;---------------------------------------------------------------------------------- 
;;--------------------------------------------------------------------------------------------------------------------------------
;; Main functions for reading in domain.hddl and problem.hddl

;; Reads in a HDDL-domain-file and transforms it into a usable HDDL-structure.
;; Input: a HDDL-domain-filepath as a String
;; Output: a HDDL-domain-structure

(defun read-hddl-domain (filename)
  "Reads in a HDDL-domain-file and transforms it into a usable HDDL-structure."
   (declare (optimize debug))
  (print "Reading domain-file...")
  
  ;;error handling
  (unless (search ".hddl" filename)  
    (error "This function can only read a HDDL file - make sure the file you want to read ends in .hddl! "))

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
      
      (setq hddl-requirements        ;; requirements as a list of keywords
	    (cdr (car requirements)) ;;remove unneccessary key-word from beginning of list
	    
	    hddl-types                 ;; types as a list of lists (subtype type)
	    (typing (cdr (car types)))

	    hddl-predicates           ;;transform each predicate in the list of predicates into a HDDL-predicate
	    (loop for (name . parameters) in (cdr (car predicates)) collect
		      (make-hddl-predicate :name name :parameters (typing parameters)))
	    
	    hddl-tasks                 ;;transform each task in the list of tasks into a HDDL-task
	     (loop for (x name y . parameters) in tasks collect
		     (make-hddl-task :name name :parameters (typing (car parameters)) :constraints nil))

	    
	     hddl-methods (ordered-subtasks-p methods)  ;;for hddl-methods, first check if methods have ordered-subtasks and annotate accordingly
	     hddl-methods (constrain-subtasks hddl-methods) ;;then constrain subtasks according to that annotation
	     
     
	    temp-actions (separate-effects actions) ;; for actions first separate effects into positive and negative effects
	    hddl-actions                            ;;then use that action-list with separated effects to build hddl-actions
	    (loop for (x name y parameters z preconditions w neg-effects pos-effects) in temp-actions collect
		      (make-hddl-action :name name :parameters (typing parameters) :preconditions (remove-and preconditions) :neg-effects  neg-effects :pos-effects pos-effects)))
	    
      ;;Finally combine everything into a HDDL-domain, reversing the task-, method- and actions-lists
      ;;because the lines for those elements in the stream each start with the keyword and they are thus added to the lists in reverse through cons.
      ;;The domain-name is taken as the second element from the stream.
	   (setq domain (make-hddl-domain :name (second read-domain) :requirements hddl-requirements
		    :types hddl-types :predicates hddl-predicates :tasks (reverse hddl-tasks)
					  :methods (reverse hddl-methods) :actions (reverse hddl-actions))))))




;---------------------------------------------------------------------------
;;Reads in a HDDL-problem-files and transforms it into a usable HDDL-problem structure.
;; Input: a HDDL-problem-filepath as a String
;; Output: a HDDL-problem structure

(defun read-hddl-problem (filename)
  "Reads in a HDDL-problem-files and transforms it into a usable HDDL-problem structure."
  (declare (optimize debug))
  (print "Reading problem-file...")

  ;;error handling
  (unless (search ".hddl" filename) 
    (error "This function can only read a HDDL file - make sure the file you want to read ends in .hddl! "))
  (unless (search "problem" filename) 
    (error "This function can only read a problem-HDDL file - make sure the file you want to read is the problem-file! "))
  
  (let* ((read-problem (read-file filename))
	 (objects)
	 (htn)
	 (status)
	 (look-up '((:objects . objects)(:htn . htn) (:init . status))))
    
     ;;lexical declaration of variables for matching of defined lists and elements in look-up:
    (declare (special objects htn status))
    ;; for each element of the domain-list search for its first element in the look-up list and
    ;; push it to the corresponding list (second half of the pair)
    (dolist (element (cdr read-problem))
     (let ((key (assoc (first element) look-up)))
       (when key
	 (set (cdr key) (cons element (symbol-value (cdr key)))))))
    
    (setq htn (cdr (car htn))) ;;remove the key :htn from the beginning of the htn-list

    ;;second iteration of separating file into components - separate htn into tasks, ordering,constraints.
    ;;not using look-up-list-method, because the components are not separated into differnet lists already, e.g. a list with first element ":TASKS" and second element a list of tasks,
    ;; but rather elements in the list beginning with ":HTN")
    ;; An order of tasks - ordering- constraints is assumed
    (let ((tasks)
	  (ordering)
	  (constraints))

      (if (equal (string (car htn)) "TASKS")
	  (setq tasks (remove-and (second htn)) ;;remove "AND" at the beginning of the task-list if there is one
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
    




 

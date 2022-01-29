#|
TODO:
- Sonderfall ordered-subtasks!
|#

;;define global variables
(defparameter *domain* (make-hash-table))
(defparameter *problem* (make-hash-table))

;; structures for the elements of a hddl-domain or hddl-problem and for the problem and domain as a whole
(defstruct hddl-predicate name parameters)
(defstruct hddl-method name parameters task subtasks);;Achtung: Subtasks können auch ordered-subtasks heißen
(defstruct hddl-action name parameters preconditions neg-effects pos-effects)
(defstruct hddl-domain name requirements types predicates tasks methods actions)
(defstruct hddl-task name parameters) 
(defstruct hddl-object name type)
(defstruct hddl-problem name domain objects tasks ordering constraints init-status)

;; read in a file
(defun read-file (filename)
  (with-open-file (in filename)
    (read in))) 
   

;;read in hddl-domain-files and transform them into usable hddl-structures
(defun read-hddl-domain (filename)
   (declare (optimize debug))
  (print "Reading domain-file...")
  (unless (search ".hddl" filename) ;;search:http://cl-cookbook.sourceforge.net/strings.html#find-sub 
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
		     (make-hddl-task :name name :parameters (typing (car parameters))))
		
	   hddl-methods
	    (loop for (x name y parameters z task w subtasks) in methods collect
		 (make-hddl-method :name name :parameters (typing parameters) :task task :subtasks subtasks))
	    
      ;; separate effects into positive and negative effects before building hddl-actions:
      temp-actions
      (loop for action in actions collect
	(let* ((new-action (butlast action));;keep everything but the effects of an action intact
	      (effect (car (last action)));; mixed effects
	      (pos-effects)
	      (neg-effects))
	  
	  ;; remove the "and" from effects - unnecessary for future use
	      (if (equal (string (car effect)) "AND")
		  (setq effect (cdr effect))) 
	  
	  ;;separate effects into pos/neg using keyword "not":
	      (loop for e in effect do
		(if (equal (string (car e)) "NOT") 
		    (push (cdr e) neg-effects)
		    (push e pos-effects)))
	  (setq new-action (append new-action neg-effects pos-effects))
		new-action)) ;; collect the new actions

      ;;use list with separated effects to build hddl-actions
	  hddl-actions
	  (loop for (x name y parameters z preconditions w neg-effects pos-effects) in temp-actions collect												    (make-hddl-action :name name :parameters (typing parameters) :preconditions preconditions :neg-effects  neg-effects :pos-effects pos-effects))
	    
	    hddl-predicates
	    (loop for (name . parameters) in (cdr (car predicates)) collect
								    (make-hddl-predicate :name name :parameters (typing parameters))))
      (loop for method in hddl-methods do
	(let ((subtasks (hddl-method-subtasks method))
	      (new-subtasks))
	 (if (equal (string (car subtasks)) "AND")
	     (setq subtasks (cdr subtasks)))
	  (setq new-subtasks (loop for (name . parameters) in subtasks collect
				   (make-hddl-task :name name :parameters parameters)))
				   
	  (pprint new-subtasks)
	  (setf (hddl-method-subtasks method) new-subtasks)))

	    ;;finally combine everything into domain structure:
	   (setq domain (make-hddl-domain :name (second read-domain) :requirements hddl-requirements
		    :types hddl-types :predicates hddl-predicates :tasks hddl-tasks
					  :methods hddl-methods :actions hddl-actions))
	   *domain* domain))))
			
	       
		       
;;read in hddl-problems and transform them into usable hddl-problem structures
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
	  (setq tasks (second htn)
		htn (cddr htn)))
      (if (equal (string (car htn)) "ORDERING")
	  (setq ordering (second htn)
		htn (cddr htn)))
      (if (equal (string (car htn)) "CONSTRAINTS")
	  (setq constraints (second htn)
		htn (cddr htn)))
     
      ;;remove unnecessary "AND" from task-list:
      (if (equal (string (car tasks)) "AND")
	  (setq tasks (cdr tasks))) 
   
    ;; transform elements of the domain-lists into the appropriate structures
      (let ((hddl-tasks)
      (problem))
      (setq hddl-tasks
	    (loop for (name . parameters) in tasks collect
		   (make-hddl-task :name name :parameters parameters))
	    problem (make-hddl-problem :name (second read-problem) :domain (cdr (third read-problem)) :objects (typing (cdr (car objects))) :tasks hddl-tasks :ordering ordering :constraints constraints :init-status (cdr (car status))))
    (setq *problem* problem)))))
    

;;takes list of variables and types and returns a list of pairs (variable . type)
;;works only for variables beginning with "?"
#|(defun typing (lst)
(setq *domain* domain))  (let ((currentvariables)
	(type)
	(typedvariables))
  (loop for x in lst do
    (cond
      ((equal (char (string x) 0) #\?) (push x currentvariables)) ;;variables start with "?"
      ((equal (string x) "-") ());; do nothing -> "-" signals that next element is a type
      (t (setq type x)
	  (loop for v in currentvariables do
	    (push (cons v type) typedvariables))
	  (setq currentvariables nil))))
    typedvariables))|#

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
		   

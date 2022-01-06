#|
HDDL-einlesen: DomÃ¤ne + Problem!

read-domain -> *tasks*, *actions*, *methods*, *types*, *predicates*
read-problem -> *goal-tasks*, *status*, *objects*
			
 -> Domäne: - Liste mit tasks (Name, Parameter) -> name, *parameter*, *task-constraints* (-> alle tasks, die vor einer task erledigt sein mÃ¼ssen)              
			 - Liste mit Aktionen (Namen, Parameter, zugehÃ¶rige task,preconditions, Effekte) -> name, *parameter*, task, *pre*, *effect+*, *effect-*  
			 - Liste mit Methoden (Namen, Parameter,zugehÃ¶rige task,preconditions, Subtasks) -> name, *parameter*, task, *pre*, *subtasks*
			 - types als einzelnes?
			 - PrÃ¤dikate als einzelnes?
			 - ordering?
			
 -> Problem: - Liste mit Objekten? -> (name, type)
			  - Status (:init) -> *status* -> (predicate, True/False)
			  - tasks -> was wir machen wollen ("Ziel") -> name, *parameter*
|#

;; aus Projekt-Sitzung, Vorschlag von Prof. Wolter zum Einlesen:
(defun read-file (filename)
  (with-open-file (in filename)
    (read in))) ;;liegt schon als Liste vor! bspw. (first *) first des letzten Aufrufs
   


(defun read-hddl-domain (filename)
  (print "Reading domain-file...")
  (unless (search ".hddl" filename) ;;search:http://cl-cookbook.sourceforge.net/strings.html#find-sub 
    (error "This function can only read a HDDL file - make sure the file you want to read ends in .hddl! "))
  (unless (search "domain" filename) 
    (error "This function can only read a domain-HDDL file - make sure the file you want to read is the domain-file! "))
  (let* ((*domain* (read-file filename))
	 *types*
	 *requirements*
	*predicates* 
	*tasks*
	*methods*
	 *actions*
	 look-up '((:types . *types*)(:requirements . *requirements*) (:predicates . *predicates*) (:task . *tasks*) (:method . *methods*) (:action . *actions*)))
    (dolist (element *domain*)
      (let ((key (assoc (first element) look-up)))
	(when key
	  (push element (cdr key)))))

    (dolist (element *tasks*)
      (destructuring-bind (name . parameters) 'element) 
      (make-hddl-task (:name name :parameters parameters)))

    (dolist (element *methods*)
      (destructuring-bind (x name y parameters z task y subtasks) 'element) 
      (make-hddl-method (:name name :parameters parameters :task task :subtasks subtasks)))

    (dolist (element *actions*)
      (destructuring-bind (x name y parameters z preconditions y effects) 'element) 
      (make-hddl-action (:name name :parameters parameters :preconditions preconditions :effects  effects)))

    (dolist (element *predicates*)
      (destructuring-bind (name . parameters) 'element) 
      (make-hddl-predicate (:name name :parameters parameters)))

    (setq domain (make-hddl-domain (:name (second '*domain*) :requirements *requirements* :types *types* :predicates *predicates* :tasks *tasks* :methods *methods* :actions *actions)))

    domain
    ))
			
		
;; oder als defstruct :action -> Objekt

;; Datenstruktur für Method etc bauen, element zu method intialisieren;
;; Datenstruktur an HDDL orientieren (defstruct...) oder auch Klasse
;; requirements -> z.B. Typing aktiviert erst types
;; look-up könnte auch mit constructor geschrieben werden zB für Methoden-Datenstruktur
;; Typing könnte dann zB andere Konstruktoren laden
;; bei Klassen könnte zB von Methode noch eine Klasse Methoden abgehen, die auch negative
;; preconditions haben kann; dann zwischen Klassen umschalten
;; erstmal mit Struct
		       

(defun read-hddl-problem (filename)
  (print "Reading problem-file...")
  (unless (search ".hddl" filename) 
    (error "This function can only read a HDDL file - make sure the file you want to read ends in .hddl! "))
  (unless (search "problem" filename) 
    (error "This function can only read a problem-HDDL file - make sure the file you want to read is the problem-file! "))
  (let* ((*problem* (read-file filename))
	 *objects*
	 *tasks*
	 *status*
	 look-up '((:objects . *objects*)(:htn . *tasks*) (:status . *status*)))
   (dolist (element *problem*)
     (let ((key (assoc (first element) look-up)))
       (when key
	 (push element (cdr key)))))))

#| so könnten alle Elemente entfernt werden, die keine Methoden sind, also nicht mit :method beginnen: (remove-if-not :method HDDL :key #'first)
oder (find :method HDDL :key #'first) -> dann wird es gefunden
matching durch destructuring-bind:
(destructuring-bind ((a b) . rest) '((1 2) (3 4))   (+ a b)) ----> 3
-> findet dann genau die angegebenen Elemente
im onlist: destructuring-bind mit ?k, also Variablen; so können wir pattern-matchen mit den Variablen und unifizieren; mal im Buch schauen und auch in dessen
online-Code schauen und Teile herausnehmen
|#

;; Strukturen für die einzelnen Elemente der Domäne + die Domäne selbst
(defstruct hddl-predicate name parameters)
(defstruct hddl-method name parameters task subtasks);;Achtung: Subtasks können auch ordered-subtasks heißen
(defstruct hddl-action name parameters preconditions effects)
(defstruct hddl-domain name requirements types predicates tasks methods actions)
(defstruct hddl-task name parameter)
(defstruct hddl-object name type)
(defstruct hddl-problem name domain objects tasks ordering constraints init-status)


	     

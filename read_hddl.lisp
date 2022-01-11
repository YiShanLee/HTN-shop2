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

;; Strukturen für die einzelnen Elemente der Domäne/Problem + die Domäne/Problem selbst
(defstruct hddl-predicate name parameters)
(defstruct hddl-method name parameters task subtasks);;Achtung: Subtasks können auch ordered-subtasks heißen
(defstruct hddl-action name parameters preconditions effects)
(defstruct hddl-domain name requirements types predicates tasks methods actions)
(defstruct hddl-task name parameter)
(defstruct hddl-object name type)
(defstruct hddl-problem name domain objects tasks ordering constraints init-status)

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
	 (look-up '((:types . *types*)(:requirements . *requirements*) (:predicates . *predicates*) (:task . *tasks*) (:method . *methods*) (:action . *actions*))))
    (dolist (element *domain*)
      (let ((key (assoc (first element) look-up)))
	(when key
	  (push element (second key))))) ;; add elements of the domain to the appropriate lists

    ;; transform elements of the domain-lists into the appropriate structures
    (let (*hddl-tasks*
	  *hddl-methods*
	  *hddl-actions*
	  *hddl-predicates*
	  domain)
      
      (dolist (element *tasks*)
	(destructuring-bind (x name . parameters) 'element) 
	(push (make-hddl-task (:name name :parameters parameters)) *hddl-tasks*))

    (dolist (element *methods*)
      (destructuring-bind (x name y parameters z task y subtasks) 'element) 
      (push (make-hddl-method
	     (:name name :parameters parameters :task task :subtasks subtasks))
	    *hddl-methods*))

    (dolist (element *actions*)
      (destructuring-bind (x name y parameters z preconditions y effects) 'element) 
      (push (make-hddl-action
	     (:name name :parameters parameters :preconditions preconditions :effects  effects))
	    *hddl-actions*))

    (dolist (element *predicates*)
      (destructuring-bind (name . parameters) 'element) 
      (push (make-hddl-predicate (:name name :parameters parameters)) *hddl-predicates*))

      (setq domain (make-hddl-domain (:name (second '*domain*) :requirements *requirements*
				      :types *types* :predicates *hddl-predicates* :tasks *hddl-tasks*
				      :methods *hddl-methods* :actions *hddl-actions*)))

    domain
    )))
			
		
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
	 *ordering*
	 *constraints*
	 *status*
	 look-up '((:objects . *objects*)(:htn :tasks . *tasks*)(:ordering . *ordering*)(:constraints . *constraints) (:status . *status*)))
   (dolist (element *problem*)
     (let ((key (assoc (first element) look-up)))
       (when key
	 (push element (cdr key)))))

    ;; transform elements of the domain-lists into the appropriate structures
    (let (*hddl-tasks*)
    ;;objects noch machen: Schwierigkeit: Typen mit - müssen allen davorstehenden Objekten hinzugefügt werden
    (dolist (element *tasks*)
      (destructuring-bind (x name . parameters) 'element) 
      (push (make-hddl-task (:name name :parameters parameters)) *hddl-tasks*))

    (setq problem (make-hddl-problem (:name (second '*problem*) :requirements (third '*problem*)
				      :objects *objects* :tasks *tasks* :ordering *ordering* :constraints *constraints :init-status *status*))))

    problem))

#| so könnten alle Elemente entfernt werden, die keine Methoden sind, also nicht mit :method beginnen: (remove-if-not :method HDDL :key #'first)
oder (find :method HDDL :key #'first) -> dann wird es gefunden
matching durch destructuring-bind:
(destructuring-bind ((a b) . rest) '((1 2) (3 4))   (+ a b)) ----> 3
-> findet dann genau die angegebenen Elemente
im onlist: destructuring-bind mit ?k, also Variablen; so können wir pattern-matchen mit den Variablen und unifizieren; mal im Buch schauen und auch in dessen
online-Code schauen und Teile herausnehmen
|#




	     

#|
HDDL-einlesen: Domäne + Problem!

read-domain -> *tasks*, *actions*, *methods*, *types*, *predicates*
read-problem -> *goal-tasks*, *status*, *objects*
			
 -> Domäne: - Liste mit tasks (Name, Parameter) -> name, *parameter*, *task-constraints* (-> alle tasks, die vor einer task erledigt sein müssen)              
			 - Liste mit Aktionen (Namen, Parameter, zugehörige task,preconditions, Effekte) -> name, *parameter*, task, *pre*, *effect+*, *effect-*  
			 - Liste mit Methoden (Namen, Parameter,zugehörige task,preconditions, Subtasks) -> name, *parameter*, task, *pre*, *subtasks*
			 - types als einzelnes?
			 - Prädikate als einzelnes?
			 - ordering?
			
 -> Problem: - Liste mit Objekten? -> (name, type)
			  - Status (:init) -> *status* -> (predicate, True/False)
			  - tasks -> was wir machen wollen ("Ziel") -> name, *parameter*
|#

;; aus Projekt-Sitzung, Vorschlag von Prof. Wolter zum Einlesen:
(defun read-file (filename)
  (with-open-file (in filename)
    (read in)
    ()
  )

;; vielleicht noch Fehlermeldung hinzuf�gen, wenn nicht "domain" im filetitle?
(defun read-hddl-domain (filename)
  (print "Reading domain-file...")
  (unless (search ".hddl" filename)  ;;search:http://cl-cookbook.sourceforge.net/strings.html#find-sub 
      (error "This function can only read a HDDL file - make sure the file you want to read ends in .hddl! "))
  (read-file filename)
;; wenn Zeile mit :TYPES beginnt -> in types-Liste (n�tig?)
;; wenn Zeile mit :PREDICATES beginnt -> in predicates-Liste
;; wenn Zeile mit :TASK beginnt -> in task-Liste
;; wenn Zeile mit :METHOD beginnt -> in method-Liste
;; wenn Zeile mit :ACTION beginnt -> in action-Liste
  )

(defun read-hddl-problem (filename)
  (print "Reading problem-file...")
  (unless (search ".hddl" filename) 
      (error "This function can only read a HDDL file - make sure the file you want to read ends in .hddl! "))
  (read-file filename)
  ;; wenn Zeile mit :OBJECTS beginnt -> in objects-Liste
  ;; wenn Zeile mit :HTN :TASKS beginnt -> in task-Liste, Achtung: AND ggf. l�schen
  ;; wenn Zeile mit :INIT beginnt -> in status-Liste
  )

	     

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
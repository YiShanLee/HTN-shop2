#|
HDDL-einlesen: DomÃ¤ne + Problem!

read-domain -> *tasks*, *actions*, *methods*, *types*, *predicates*
read-problem -> *goal-tasks*, *status*, *objects*
			
 -> DomÃ¤ne: - Liste mit tasks (Name, Parameter) -> name, *parameter*, *task-constraints* (-> alle tasks, die vor einer task erledigt sein mÃ¼ssen)              
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
    (read filename)
	(let (*elements* (make-list 0))
		(when (eql (read-char in) ":")
		(append (*elements* (read-delimited-list #\( #\: in)))))))
	

#|(with-open-file (instream infile :direction :input :if-does-not-exist nil)
    (when instream 
      (let ((string (make-string (file-length instream))))
        (read-sequence string instream)
        string)))) 
		https://riptutorial.com/common-lisp/example/19473/reading-and-writing-entire-files-to-and-from-strings|#

 #| (set-macro-character #\’
#’(lambda (stream char)
(list ’quote (read stream t nil t))))  -> aus Buch on lisp|#

;; vielleicht noch Fehlermeldung hinzufügen, wenn nicht "domain" im filetitle?
(defun read-hddl-domain (filename)
  (print "Reading domain-file...")
  (unless (search ".hddl" filename) ;;search:http://cl-cookbook.sourceforge.net/strings.html#find-sub 
    (error "This function can only read a HDDL file - make sure the file you want to read ends in .hddl! "))
  (let ((*domain* (read-file filename)) (*types* (make-list 0)) 
	(*predicates* (make-list 0)) (*tasks* (make-list 0))(*methods* (make-list 0)) (*actions* (make-list 0)))
   (dolist (element *domain*)
			(case 
			((search ":types" element :start 1) (append *types* )) ;; wenn Zeile mit :TYPES beginnt -> in types-Liste (nötig?)
			((search ":predicates" element :start 1) (append *predicates* element)) ;; wenn Zeile mit :PREDICATES beginnt -> in predicates-Liste
			((search ":task" element :start 1) (append *tasks* element)) ;; wenn Zeile mit :TASK beginnt -> in task-Liste
			((search ":method" element :start 1) (append *methods* element)) ;; wenn Zeile mit :METHOD beginnt -> in method-Liste
			((search ":action" element :start 1) (append *actions* element)) ;; wenn Zeile mit :ACTION beginnt -> in action-Liste
		;;return list of sorted lists?
		
  ))))

(defun read-hddl-problem (filename)
  (print "Reading problem-file...")
  (unless (search ".hddl" filename) 
      (error "This function can only read a HDDL file - make sure the file you want to read ends in .hddl! "))
  (read-file filename)
  ;; wenn Zeile mit :OBJECTS beginnt -> in objects-Liste
  ;; wenn Zeile mit :HTN :TASKS beginnt -> in task-Liste, Achtung: AND ggf. löschen
  ;; wenn Zeile mit :INIT beginnt -> in status-Liste
  )

	     

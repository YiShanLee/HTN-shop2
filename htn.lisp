;; path for status check
(defparameter *paths* nil)

;;; fail control
;; record the failure path
(defstruct failure acton)
;; failsystem
(defconstant failsym `@)

;-----------------------------
;;;; knowledge from input 
;; domain knowledge
(defparameter *T* '()) ; tasks from the beginning input
(defparameter *M* '()) ; methods from the domain knowledge
(defparameter *A* '()) ; actions from the domain knowledge
;; problem 
(defparameter initial-state '()) ; the beginning state of one problem
;-----------------------------
;;;; global variables for the shop2-operator
;; initialized plan
(defparameter *Plan* '())

;; list of actions 
(defparameter *a* '())

;; t for task without constraint to precede t in T
(defparameter T0 '()) ; task constraint
; (defparameter *unifier* '()) ;a association of variables that shows current state
(defparameter *current-state* ()) ; the updated state from current position
;------------------------------------------------------------ 

(defun shop2-operator(initial-state Tasks domain)
;; Alisa: am besten wäre es wahrscheinlich, wenn wir in diesem let das Einlesen von Domain und Code 
;; mit einbinden, also (*domain* (read-hddl-domain "domain-file")) (*problem* (read-hddl-problem "problem-file"))
;; dann sollten wir die Funktion vielleicht so aufrufen lassen:  (defun shop2-operator(domain-file problem-file) und alles
;; weitere wie initial-state und so weiter dann erst rausziehen, oder? Also das man zum Aufrufen wirklich nur (shop2-operator ("domain.hddl" "problem.hddl")) eintippen muss
;; dann wäre es (let ((*domain* (read-hddl-domain "domain-file")) (*problem* (read-hddl-problem "problem-file"))
;;           (Tsk (hddl-problem-task *problem*)) (initial-state (hddl-problem-init problem)) und dann alles, was du jetzt schon hast)
 (let ((T0 constraint(Tasks)) (substitution nil) (Plan) (current-state)) 
    ;; Alisa: hier vielleicht auch noch für jede Task eine constraint-list anhängen, 
    ;; die erstmal leer ist, und später bei Methoden-Aufrufen durch Subtasks gefüllt wird, 
    ;; wenn es constraints gibt? Dann müssten wir für T0 auch immer nur prüfen, ob die constraint-Listen leer sind
      (loop 
        (if (null Tasks)(return Plan))
        (setq current-task (car T0))
           (cond ((hddl-action-name-p (hddl-task-name current-task) ) ;; Alisa: hier dann auch current-task-name! 
                (let ((Actions-lst) (unify (*A* current-task)))
                      (cond ((eq Actions-lst nil) (error (make-condition 'failure-handling :actual-input 'action)))
                                         (t ((let ((act (car Action-lst))) 
                                                  (setq current-state modify-status(current-state act)) 
                                                  (push (car act) Plan)
                                                  (setq Tasks (remove current-task Tasks)) ;TODO function ;;Alisa: zusätzlich müssten wir dann auch schauen, dass wir die task aus allen task constraint-Listen löschen!
                                                  (substitute (action current-task))) ; (variable initial-value modify-value)
                                                 ))
                                         )
                      (setq T0 constraint(Tasks))) ; 
              ; compound tasks
              (t 
                  (let (Methods-lst (unify-m (*M* current-task current-state))) ;; methods from domain as local variables
                       ; if M = empty then return failure
                     (cond ((eq Methods-lst nil)(error (make-condition 'failure-handling :actual-input 'method)))
                  ; nondeterministically choose a pair (m, θ) ∈ M
                               (t (let ((ms Methods-lst (car ms))
                          ; modify T by removing t, adding sub(m), constraining each task
                                        (remove current-task Tasks))
                                   ; in sub(m) to precede the tasks that t preceded
                                        (subm (hddl-method-subtasks (car ms)))
                  ;; TODO Alisa: hier müssen wir noch die Subtasks zu Tsk hinzufügen und dort 
                  ;; gegebenenfalls in die constraint-Listen der tasks! (überall wo current-task rausgelöscht wird, neue subtasks einfügen)
                                    ;, and applying θ 
                                    (substitution substitution (substitute (cdr ms)))) ; variable initial-value modify-value
                                   ; if sub(m) is not empty then T0 ← {t ∈ sub(m) : no task in T is constrained to precede t}
                                   (cond ((subm) (setq T0 constraint(subm))) 
                                         (t  (setq T0 constraint(Tasks))))) 
                                       ))
              )
          )
        )      
    ))
 )
; constraint T to T0 
;; Alisa: muss also für alle t in T prüfen, dass nicht eine andere task vorher ausgeführt werden muss
;; ich würde ganz am Anfang des Codes für jede task in Tsk eine leere Liste erstellen, die die constraints enthält,
;; dann müssten hier nur prüfen, ob die constraints leer sind oder nicht
;; dann müssen wir bei den Methoden dran denken, dass die constraints bei den Subtasks eingefügt werden müssen

  (defun constraint2 (tasks)
    (let ((t0))
      (loop for task in tasks do
  (if (null (cdr task)) 
      (cons task t0)))
      t0))

;; satisfy check 
; parameters (action), (*current-state*)
; check the precondition of one action suits current-state
; A ← {(a, θ) : a is a ground instance of an operator in D, θ is a substitution that unifies {head(a), t von problem}, and (s von problem) satisfies a’s preconditions}

;; Alisa: für alle eingegebenen Aktionen prüfe, ob die preconditions einer Aktion im aktuellen Status erfüllt sind, falls ja, füge sie in Ergebnisliste ein
;; preconditions sind dann erfüllt, wenn sie im aktuellen Status enthalten sind
;; gibt es auch negative preconditions? nein
  (defun satisfyp2(actions current-status)
    (let ((satisfying_actions))  
      (loop for a in actions do
    (let ((preconditions (hddl-action-preconditions a))
        (satisfies T))  ;;set satyisfies to true at first and let it be disproven for every action
    (loop for p in preconditions do
      (if (not(find p current-status))
    (setq satisfies nil)))
    (if satisfies
        (push a satisfying_actions)))
  (reverse satisfying_actions))))   


;---------------------------------------------------------------------
 
;;Alisa: unifier sollte am besten 
;;1. alle actionen sammeln, die den gleichen Namen haben wie die task
;;2. prüfen, ob die gleiche Anzahl Parameter vorliegt (Länge der Liste)
;;3. prüfen, ob die Parameter den selben Typ haben (Reihenfolge in Parameterliste egal)
;;4. die Parameter der task als theta ausgeben
;; Ergebnis wäre dann eine Liste mit ((action . theta)(action . theta)...)
;; hier würde ich also auch gar nicht jede action einzeln übergeben, sondern gleich alle auf einmal

(defun unify(actions task)
  (let ((same_name)
  (unified_actions)
  (taskname (hddl-task-name task));;get name and params of task for easier comparing
  (taskparams (hddl-task-parameters task))
  (taskparam-types (loop for p in taskparams collect
               (cdr p))))
    ;; first for all actions collect only those that have the same name & amount of parameters as the task
    (loop for a in satisfyp2(actions) do
      (let ((a-params (hddl-action-parameters a)))
  (if (eq (hddl-action-name action) taskname)
      (if (eq (length taskparams) (length a-params)) ;; then compare types
    (push a same_name)))))
    ;;then compare if parameters have the same types
    (loop for a in same_name do
    ;;collect all parameter-types of an action
       (let* ((a-params (hddl-action-parameters a))
        (a-paramtypes (loop for p in a-params collect
                (cdr p))))
         ;;loop through the task-parameter-types; whenever the same type is found in the action parameter-type-list, remove it there; if after the loop the a-paramtypes list is empty, the action can be unified with the task, set the parameters of the task as theta
         (loop for type in taskparam-types do
           (if (find type a-paramtypes)
         (remove type a-paramtypes)))
         (if (null a-paramtypes) ;; testen das task-parametertypes auch null
       (push (cons a taskparams) unified_actions))))
   (reverse unified_actions)))

 ; M ← {(m, θ) : m is an instance of a method in D, θ unifies {head(m), t},
            ; pre(m) is true in s, and m and θ are as general as possible}
      ;; Alisa: auch hier würde ich erst die Zeile mit unifier und danach mit satisfyp ausführen
;; satisfyp-m 
; return a list of methods to filter the precondition of methods fitting the current-state
(defun satisfyp-m (m state)
  (let ((satisfied-m nil))
    (loop
      (cond ((null m) (return satisfied-m))
          ((not (null (find ((hddl-method-subtasks (car m)) state)))) (setq satisfied-m (push (car m) satisfied-m))))
      (setq m (cdr m))
    )))

;; unify-m 
; prueft, ob die Laenge der Parameters gleich
; prueft, ob Typen der Parameters gleich
; return a unified list such as {(m . theta)...}      
(defun unify-m (methods task state)
  (let ((m (satisfyp-m (methods state)))
        (task-name (hddl-task-name task))
        (taskparams (hddl-task-parameters task))
        (taskparam-types (cadr (apply #'mapcar #'list (hddl-task-parameters task))))
        (taskparam (car (apply #'mapcar #'list (hddl-task-parameters task))))
        (Methods-lst nil)
        )
    (loop
      (cond ((and (eq (length taskparams) (length (hddl-method-parameters (car m))))
                 (equalp (taskparam-types) (cadr (apply #'mapcar #'list (hddl-method-parameters (car m))))))
            (setq Methods-lst (push (cons (car m) (car (apply #'mapcar #'list (hddl-method-parameters (car m))))) Methods-lst)))
            ((null m) (return Methods-lst))
            )
      (setq m (cdr m))
    )
  )
)
; ; substitution 
; (defun substitute (theta)
;   )

;; Alisa: hier würde ich denke ich (defun modify (status action) schreiben, damit wir den aktuellen
;; Status auch mitübergeben, den wir dann verändern
;; dafür würde ich 
;; 1. durch die current-status-Liste iterieren und für alle negativen Effekte der Aktion herauslöschen
;; 2. alle positiven Effekte der Aktion hinzufügen, und das als neuen Status ausgeben lassen:
  (defun modify-status (status action)
    (let ((addeffect (hddl-action-poseffect action))
    (deleffect (hddl-action-negeffect action))
    (new-status))
      (loop for e in status do
  (if (not(find e deleffect))
      (push e new-status)))
      (cons new-status addeffect)
      new-status))
       

  
;; failure handling
(define-condition failure-handling (err)
  ((actual-input :initarg :actual-input
                 :reader actual-input
                 :initform nil))
  (:report (lambda (condition stream)
             (format stream "~a is null!"
                     (actual-input condition)))))
;-------------------------------------------------------------
;;; illustration from thesis
#|
The first case is if t is primitive, i.e., 
if t can be accomplished directly using an action(i.e., an instance of a planning operator).  
In this case, SHOP2 finds an action a that matches t and whose preconditions are satisfied in s, and applies a to s
(if no such action exists, then this branch of the search space fails).

The second case is where t is compound, \
i.e., a method needs to be applied to t to decompose it into subtasks. 
In this case, SHOP2 nondeterministically chooses a method instance m that will decompose t into subtasks 
(if no such method instance exists, then this branch of the search space fails).

If there is a solution plan that involves m, then the actions in P will be the leaf nodes of a decomposition tree D P 
such as the tree shown in Figure 2. The precondition formula pre(m) must be true in the state that 
immediately precedes the first action a in D P that is a descendant of m. 
In order to ensure that pre(m) is true in the correct state, SHOP2 needs to generate the 
leftmost branch of D all the way down to the bottom, and evaluate pre(m)in the state just before a. 
The last three lines of the loop ensure that this will happen, 
by telling SHOP2 that if the current method m has any subtasks, SHOP2 should generate one of those subtasks 
before generating any other subtasks in the task network
|#
;-------------------------------------------------------------




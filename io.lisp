(defparameter *input* '())
;; read file to get the cons from the hddl file
; use open to dummy open the file
(defun read-hddl (filename)
  (let ((stream (open filename)))
   (when stream 
     (setq *input* '())
     (push (read-file filename) *input*))
    (close stream)))

;; defdomain construction to read item(argument)
(defstruct problem
  (namep)
  (namedo)
  (objects) 
  (htn) 
  (init) 
  (goal)
  )
;; if type is needed? 
(defstruct domain
  (namedo) 
  (requirements) 
  (types) 
  (predicates) 
  (tasks) 
  (methods) 
  (actions)
  )

;; <TODO>to get one task easier using defstruct  
; ;; tasks
; (defstruct task
;   parameters precondition effect)
; ;; methods 
; (defstruct method
;   parameters task subtasks ordering)

; ;; actions
; (defstruct action
;   parameters preconditions effect)


;;<TODO>method to get individual data of one domain
; {:namedo `(,@(mapcar #'cadadr *input*))
;                                 :requirements `(,@(mapcar #'caddr *input*))
;                                 :types `(,@(mapcar #'cadddr *input*)) 
;                                 :predicates `(,@(mapcar #'cadr `(,@(mapcar #'cdddr *input*)))
;                                 :tasks `(,@(mapcar #'cadr `(,@(mapcar #'cddddr *input*))))
                                ;:methods
                                ;:actions  }
                                
;; <finished> with a hash table to retrieve domains as struct
(defvar *domains* (make-hash-table)) 
(defun defdomain (name &optional requirement type predicate task method action) 
        (setf (gethash name *domains*) 
              (make-domain
                                :namedo name
                                :requirements requirement
                                :types type 
                                :predicates predicate
                                :tasks task
                                :methods method
                                :actions action
                        )  ))


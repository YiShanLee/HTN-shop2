(defparameter *input* '())
;; read file to get the object from the hddl file
; use open to dummy open the file
(defun read-hddl (filename)
  (let ((stream (open filename)))
    ; (when stream 
    ;   (loop for line = (read-line in nil)
    ;         ;while line do (format t "~a~%" line)
    ;         while line do (push (cdr line) *input*)
   (when stream 
     (setq *input* '())
     (push (read-file filename) *input*))
    (close stream)))
;; idea2 
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

; ;; tasks
; (defstruct task
;   parameters precondition effect)
; ;; methods 
; (defstruct method
;   parameters task subtasks ordering)

; ;; actions
; (defstruct action
;   parameters preconditions effect)

; destructuring-bind for the datasets separation (see reference on "practical common lisp- destructuring-bind")

;;list domain's elements to get individual data
;; need another function to retrieve the datasets of each element
; {:namedo `(,@(mapcar #'cadadr *input*))
;                                 :requirements `(,@(mapcar #'caddr *input*))
;                                 :types `(,@(mapcar #'cadddr *input*)) 
;                                 :predicates `(,@(mapcar #'cadr `(,@(mapcar #'cdddr *input*)))
;                                 :tasks `(,@(mapcar #'cadr `(,@(mapcar #'cddddr *input*))))
                                ;:methods
                                ;:actions  }
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
                          ; amend as (let* (key (mapcar ))
                        )  ))


;; idea3 
;; build up keywords as formation for basis structure
; (defconst hddl-keywords 
;   (list (cons (regexp-opt '(":requirements" ":types" ":constants" ":predicates"
; 			    ":action" ":domain" ":parameters" ":effect"
; 			    ":precondition" ":objects" ":init" ":goal"
; 			    ":functions" ":duration" ":condition" ":derived"
;			    ":metric") t))))
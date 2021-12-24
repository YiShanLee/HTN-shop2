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

(defstruct domain
  name requirements types predicate tasks methods actions)

(defstruct problem-struct
  name domain-name objects htn init goal)
; destructuring-bind for the datasets separation (see reference on "practical common lisp- destructuring-bind")
(defun destructuring-bind) (matching pattern)
;;make domain to get individual data
(defun make-domain (name requirements types predicate tasks methods actions)
  (list :name name :requirements requirements :types types :predicate predicate :task tasks :methods methods :actions actions))
;still need to amend as if condition
(defun set-domain ()
  `(make-domain
        :name `(,@(mapcar #'cadadr *input*))
        :requirements `(,@(mapcar #'caddr *input*))
        :types `(,@(mapcar #'cadddr *input*)) 
        ;:predicates ,@(mapcar #)
        ;:tasks
        ;:methods
        ;:actions  
  ; amend as (let* (key (mapcar ))
  ))



;; idea3 
;; build up keywords as formation for basis structure
; (defconst hddl-keywords 
;   (list (cons (regexp-opt '(":requirements" ":types" ":constants" ":predicates"
; 			    ":action" ":domain" ":parameters" ":effect"
; 			    ":precondition" ":objects" ":init" ":goal"
; 			    ":functions" ":duration" ":condition" ":derived"
;			    ":metric") t))))
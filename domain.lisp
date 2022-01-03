;; read problems 

;; dictionary of domain
; (defmacro defdomain (name-and-options items)
  
;   )
;; CRUD of Domain
;-------------------------------------------
;; dummy domain as cons to get the input
(defparameter *domain* (make-hash-table))
(defparameter *methods* (make-hash-table))
(defparameter *actions* (make-hash-table))
(defparameter *tasks* '())
(defparameter *objs* '())
(defstruct obj location vehicle)
(defstruct methods name parameters tasks subtasks)
(defstruct actions name parameters preconditions effects)
(defstruct domain name requirements types predicates task methods actions)

(defun set-task () ; before set-task need one function to get-task in order to get the list of tasks
  (let* ((drive-v-l1-l2 'drive-v-l1-l2)
    	(drive-v-l2-l3 'drive-v-l2-l3)
     	(get-to-v-l 'get-to-v-l)
     	(get-to-v-l1 'get-to-v-l1)
     	(get-to-v-l2 'get-to-v-l2)
     	(get-to-v-l3 'get-to-v-l3)
     	(noop-v-l 'noop-v-l))
      (setq *tasks* (cons drive-v-l1-l2 *tasks*))
      (setq *tasks* (cons drive-v-l2-l3 *tasks*))
      (setq *tasks* (cons get-to-v-l *tasks*))
      (setq *tasks* (cons get-to-v-l1 *tasks*))
      (setq *tasks* (cons get-to-v-l2 *tasks*))
      (setq *tasks* (cons get-to-v-l3 *tasks*))
      (setq *tasks* (cons noop-v-l *tasks*))
    ))

(defun set-objs ()
  (setf *objs* (make-obj :location '(l l1 l2 l3)
                :vehicle '(v))))

(defun set-domain ()
  (let* 
    ((transport (make-domain 
                       :name "transport"
                       :requirements '(:negative-preconditions :hierarchy :typing)
                       :types (set-objs)
                       :predicates '((road ?l1 ?l2 - location) (at ?v - vehicle ?x - location))
                       :task (set-task)
                       :methods (set-methods)
                       :actions (set-actions))))
   (setf (gethash 'transport *domain*) (cons transport *domain*))))

(defun set-methods ()
  (let* ((m-drive-to (make-methods 
          :name "m-drive-to"
          :parameters '(?v ?l1 ?l2)
          :tasks `(getf get-to-v-l2 *tasks*)
          :subtasks `(getf drive-v-l1-l2 *tasks*)
          ))
         (m-drive-to-via (make-methods 
          :name "m-drive-to-via"
          :parameters '(?v ?l2 ?l3)
          :tasks `(getf get-to-v-l3 *tasks*)
          :subtasks '(`(getf drive-v-l2-l3 *tasks*) `(getf get-to-v-l2 *tasks*)
          )))
         (m-i-am-there (make-methods 
          :name "m-i-am-there"
          :parameters '(?v ?l)
          :tasks `(getf get-to-v-l *tasks*)
          :subtasks `(getf noop-v-l *tasks*)
          ))
         )
    (setf (gethash 'm-drive-to *methods*) (cons m-drive-to *methods*))
    (setf (gethash 'm-drive-to-via *methods*) (cons m-drive-to-via *methods*))
    (setf (gethash 'm-i-am-there *methods*) (cons m-i-am-there *methods*))
    ))
(defun set-actions ()
  (let* ((drive (make-actions
          :name "drive"
          :parameters '(?v ?l1 ?l2)
          :preconditions '(at-v-l1 road-l1-l2)
          :effects '((not(at-v-l1)) (at-v-l2))
          ))
         (noop (make-actions
          :name "drive"
          :parameters '(?v ?l1 ?l2)
          :preconditions '(at-v-l1 road-l1-l2)
          :effects '((not(at-v-l1)) (at-v-l2))
          )))
    (setf (gethash 'drive *actions*) (cons drive *actions*))
    (setf (gethash 'noop *actions*) (cons noop *actions*))))
  
;-------------------------------------------
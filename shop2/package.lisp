;;;; package.lisp
(in-package :common-lisp-user)
(defpackage #:shop2
  (:use #:cl)
  (:export 
    #:shop2-operator
    #:get-domain-knowledge
    #:resolve-task
    #:primitivep
    #:update-primitive-task
    #:update-action-values
    #:update-nonprimitive-task
    #:action-satisfyp
    #:action-unifier
    #:method-satisfyp
    #:method-unifier
    #:modify-status
    #:substitute
    #:remove-task
    #:add-task
    #:update-task
    :read-hddl-domain
    :read-hddl-problem
    
    :*domain* :*problem*))
  ; (:import-from :readhtn :read-hddl-domain :read-hddl-problem))

; (defpackage :readhtn
;   (:use #:cl)
;   (:export
;     :read-hddl-domain
;     :read-hddl-problem))
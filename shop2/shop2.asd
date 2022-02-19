;;;; shop2.asd
(in-package :cl-user)

(asdf:defsystem :shop2
  :description "Describe shop2 here"
  :author "Your Name <your.name@example.com>"
  :license  "Specify license here"
  :version "0.0.1"
  :serial t
  :components ((:file "package")
               (:file "shop2")
               (:file "readhtn")))

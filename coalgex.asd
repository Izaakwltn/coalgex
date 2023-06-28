;;;; coalgex.asd
;;;;
;;;;

(asdf:defsystem :coalgex
  :description "A Simple Implementation of a Regex processing NFA in Coalton."
  :depends-on (#:coalton #:fiasco)
  :serial t
  :components ((:file "package")
               (:file "nfa")
               (:file "tests")))

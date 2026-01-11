(defpackage #:coalgex.matchable
  (:use
   #:coalton
   #:coalton-prelude)
  (:local-nicknames
   (#:list #:coalton-library/list)))

(in-package #:coalgex.matchable)

(named-readtables:in-readtable coalton:coalton)

(coalton-toplevel

  (define-class (Matchable :a)
    (%match (:a -> :a -> boolean)))

  (declare match-range ((Eq :a) (Matchable :a) =>
			(List :a)
			-> :a
			-> boolean))
  (define (match-range range input)
    (list:member input range))

  
  )

(coalton-toplevel

  (define (transitionable :state :edge)))

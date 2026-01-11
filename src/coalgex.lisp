(defpackage #:coalgex
  (:use
   #:coalton
   #:coalton-prelude)
  (:local-nicknames
   (#:vec #:coalton-library/vector)
   (#:iter #:coalton-library/iterator)
   (#:str  #:coalton-library/string)
   (#:list #:coalton-library/list)))

(in-package #:coalgex)

(named-readtables:in-readtable coalton:coalton)

(cl:defmacro %define-eq-transitionable-instance (transition-type)
  "Define transitionable instance for relevant types with `EQ` defined."
  `(define-instance (Transitionable ,transition-type)
    (define (%match val input)
      (== val input))))

(coalton-toplevel
  
  (define-type (Edge :a)
    ($match :a (State :a))
    ($Not (State :a))
    ($Any (State :a))
    ($Empty (State :a)))

  (define (edge-target edge)
    (match edge
      (($Match _ s)
       s)
      (($Not s)
       s)
      (($Any s)
       s)
      (($Empty s)
       s)))

  (define-type (State :a)
    (State (Vector (Edge :a)))
    (Accepting (Vector (Edge :a))))

  (define (edges state)
    (match state
      ((State edges)
       edges)
      ((Accepting edges)
       edges)))

  (define-class (Transitionable :a)
    ""
    (%match (:a -> :a -> boolean)))

  (declare match-range ((Eq :a) (Transitionable :a) => :a -> (List :a) -> boolean))
  (define (match-range input range)
    (list:member input range))

  (%define-eq-transitionable-instance Integer)
  (%define-eq-transitionable-instance IFix)
  (%define-eq-transitionable-instance UFix)
  (%define-eq-transitionable-instance Bit)
  (%define-eq-transitionable-instance I8)
  (%define-eq-transitionable-instance U8)
  (%define-eq-transitionable-instance I16)
  (%define-eq-transitionable-instance U16)
  (%define-eq-transitionable-instance I32)
  (%define-eq-transitionable-instance U32)
  (%define-eq-transitionable-instance I64)
  (%define-eq-transitionable-instance U64)
  (%define-eq-transitionable-instance F32)
  (%define-eq-transitionable-instance F64)
  (%define-eq-transitionable-instance Char)
  (%define-eq-transitionable-instance String)

  (declare match-edge (Transitionable :a =>
				      :a
				      -> (Edge :a)
				      -> Boolean))
  (define (match-edge input edge)
    "Match the value of an edge against an input."
    (match edge
      (($Match val _state)
       (%match val input))
      ;; Add negation rules
      (_ True)))
  
  
  #+ig(declare transition (Transitionable :a
				      => State :a
				      -> :a
				      -> (Optional (Vector (State :a)))))
  #+ig(define (transition state input)
    (let ((valid-states (vec:new)))
      (for edge in (edges state)
	(when (match-edge input edge)
	  (vec:push! (edge-target edge) valid-states)
	  Unit))
      (if (zero? (vec:length valid-states))
	  None
	  (Some valid-states)))))

;; transition should take a vector of states

(coalton-toplevel
  ;; transition should be generalized to take either a dfa state or an nfa state- dfa would take a 
  (declare transition (Transitionable :a
				      => (Vector (State :a))
				      -> :a
				      -> (Vector (state :a))))
  (define (transition states input)
    (let ((valid-states (vec:new)))
      (for state in states
	(for edge in (edges state)
	  (when (or (match edge
		      (($Empty _)
		       True)
		      (_ False))
		    (match-edge input edge))
	    (vec:push! (edge-target edge) valid-states)
	    Unit)))
      valid-states)))

(coalton-toplevel

  ;; so to chain these together, I need to find each accepting state and resolve it to point to the start of the next one

  (declare %is ((Transitionable :a) => :a -> (State :a)))
  (define (%is val)
    ;; this is the only one that should process anything but states
    (State (vec:make
	    ($Match val (Accepting (vec:new))))))
  
  (declare %any (Unit -> State :a))
  (define (%any)
    (State (vec:make ($Any (Accepting (vec:new))))))

  (declare %empty (Unit -> State :a))
  (define (%empty)
    (State (vec:make ($Empty (Accepting (vec:new))))))

  (declare %concat (State :a -> State :a -> State :a))
  (define (%concat st1 st2)
    (match st1
      ((State es)
       (State (map (fn (edge)
		     (match edge
		       (($Not st)
			($Not (%concat st st2)))
		       (($Match x st)
			($Match x (%concat st st2)))
		       (($Any st)
			($Any (%concat st st2)))
		       (($Empty st)
			($Empty (%concat st st2)))))
		   es)))
      ((Accepting _es)
       st2)))
  
  (declare %union (State :a -> State :a -> State :a))
  (define (%union st1 st2)
    (let ((end-state (Accepting (vec:new))))
      (State (vec:make ($Empty (%concat st1
					(%concat (%empty)
						 end-state)))
		       ($Empty (%concat st2
					(%concat (%empty)
						 end-state)))))))

  (declare %star (State :a -> State :a))
  (define (%star st)
    (%union (Accepting (vec:new))
	    (%concat st
		     (%union
		      (%empty)
		      (%concat (%empty) st))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(coalton-toplevel

  (declare %match-nfa ((Transitionable :a) =>
		       (Vector (State :a))
		       -> (list :a)
		       -> (Vector (State :a))))
  (define (%match-nfa states input)
    (match input
      ((Cons x xs)
       (%match-nfa
	(transition states x)
	xs))
      ((Nil)
       states)))

  (declare match-nfa ((Transitionable :a) => (State :a) -> (list :a) -> Boolean))
  (define (match-nfa start-state input)
    (match (iter:find! (fn (x)
			 (match x
			   ((Accepting _)
			    True)
			   (_
			    False)))
		       (iter:into-iter (%match-nfa (vec:make start-state) input)))
      ((Some _)
       True)
      ((None)
       False)))

  (define (match-string start-state str)
    (match-nfa start-state (the (List Char) (into str)))))


;; so first, build the nfa, then use the transition function to check

;; when input runs out/there isn't a match, as long as one of the transition states is accepting it's a sucess

;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;




(defpackage #:coalgex.nfa
  (:use
   #:coalton
   #:coalton-prelude)
  (:local-nicknames
   (#:vec  #:coalton-library/vector)
   (#:list #:coalton-library/list)
   (#:iter #:coalton-library/iterator))
  (:export
   #:NFA
   #:Node
   #:Accepting
   #:Edge
   #:$match
   #:$range
   #:$not
   #:$Any
   #:$Empty
   #:%any
   #:%union
   #:%concat
   #:%is
   #:%empty))

(in-package #:coalgex.nfa)

(named-readtables:in-readtable coalton:coalton)

;;;
;;; Building NFA
;;;

(coalton-toplevel

  (define-type (NFA :a)
    (Node (Vector (Edge :a))) 
    (Accepting (Vector (Edge :a))))

  (define-type (Edge :a)
    ($match :a        (NFA :a))
    ($range (List :a) (NFA :a))
    ($not             (NFA :a))
    ($Any             (NFA :a))
    ($Empty           (NFA :a)))

  (define-instance ((Eq :a) => Eq (NFA :a))
    (define (== a b)
      (match (Tuple a b)
        ((Tuple (Node x) (Node y))
         (== x y))
        ((Tuple (Accepting x) (Accepting y))
         (== x y))
        (_ False))))

  (define-instance ((Eq :a) => Eq (Edge :a))
    (define (== a b)
      (match (Tuple a b)
        ((Tuple ($match i x) ($match j y))
         (and (== i j)
              (== x y)))
        ((Tuple ($range is x) ($range js y))
         (and (== is js)
              (== x y)))
        ((Tuple ($not x) ($not y))
         (== x y))
        ((Tuple ($any x) ($any y))
         (== x y))
        ((Tuple ($empty x) ($empty y))
         (== x y))
        (_ (error "invalid edge constructor")))))

  (declare nfa-edges ((NFA :a) -> (Vector (Edge :a))))
  (define (nfa-edges nfa)
    (match nfa
      ((Node edges)
       edges)
      ((Accepting edges)
       edges)))
  
  (declare edge-target ((Edge :a) -> (NFA :a)))
  (define (edge-target edge)
    (match edge
      (($Match _ s)
       s)
      (($Range _ s)
       s)
      (($Not s)
       s)
      (($Any s)
       s)
      (($Empty s)
       s))))

;;;
;;; Matching inputs against edges
;;;

(coalton-toplevel

  (declare match-edge ((Eq :a) => (Edge :a) -> :a -> Boolean))
  (define (match-edge edge input)
    "Determine whether the input satisfies the edge."
    (match edge
      (($match val _target)
       (== val input))
      (($range vals _target)
       (list:member input vals))
      (_
       True))))

(coalton-toplevel
  
  (declare transition ((Eq :a) =>
		       (Vector (NFA :a))
		       -> :a
		       -> (Vector (NFA :a))))
  (define (transition states input)
    "Transition the NFA from one set of current states to the next."
    (let ((valid-states (vec:new)))
      (for state in states
	(for edge in (nfa-edges state)
	  (match edge
	    (($empty target)
	     ;; Skip empty edges
	     (vec:extend! valid-states
			  (nfa-edges target)))
	    (_
	     (when (match-edge edge input)
	       (vec:push! (edge-target edge) valid-states)
	       Unit)))))
      valid-states)))

;;;
;;; Tools for building nfa's
;;;

(coalton-toplevel

  (declare %empty (Unit -> NFA :a))
  (define (%empty)
    (Node (vec:make ($Empty (Accepting (vec:new))))))

  (declare %is ((Eq :a) => :a -> (NFA :a)))
  (define (%is val)
    (Node (vec:make
	   ($Match val (Accepting (Vec:new))))))

  (declare %range ((Eq :a) => (List :a) -> (NFA :a)))
  (define (%range vals)
    (Node (vec:make
	   ($range vals (Accepting (vec:new))))))

  (declare %any ((Eq :a) => Unit -> (NFA :a)))
  (define (%any)
    (Node (vec:make ($Any (Accepting (vec:new))))))

  (declare %concat ((NFA :a) -> (NFA :a) -> (NFA :a)))
  (define (%concat nfa1 nfa2)
    (match nfa1
      ((Node es)
       (Node (map (fn (edge)
		    (match edge
		      (($Not nfa)
		       ($Not (%concat nfa nfa2)))
		      (($Match x nfa)
		       ($Match x (%concat nfa nfa2)))
		      (($Range x nfa)
		       ($Range x (%concat nfa nfa2)))
		      (($Any nfa)
		       ($Any (%concat nfa nfa2)))
		      (($Empty nfa)
		       ($Empty (%concat nfa nfa2)))))
		  es)))
      ((Accepting _es)
       nfa2)))

  (declare %union (NFA :a -> NFA :a -> NFA :a))
  (define (%union nfa1 nfa2)
    (let ((end-state (Accepting (vec:new))))
      (Node (vec:make ($Empty (%concat nfa1
				       (%concat (%empty)
						end-state)))
		      ($Empty (%concat nfa2
				       (%concat (%empty)
						end-state)))))))

  (declare %star (NFA :a -> NFA :a))
  (define (%star st)
    (%union (Accepting (vec:new))
	    (%concat st
		     (%union
		      (%empty)
		      (%concat (%empty) st))))))

(coalton-toplevel

  (declare %match-nfa ((Eq :a) => (Vector (NFA :a)) -> (List :a) -> (Vector (NFA :a))))
  (define (%match-nfa states input)
    (match input
      ((cons x xs)
       (%match-nfa
	(transition states x)
	xs))
      ((Nil)
       states)))

  (define (match-nfa init-state input)
    (match (iter:find! (fn (x)
			 (match x
			   ((Accepting _)
			    True)
			   (_
			    False)))
		       (iter:into-iter (%match-nfa (vec:make init-state) input)))
      ((Some _)
       True)
      ((None)
       False)))

  (define (match-nfa-against-string nfa str)
    (match-nfa nfa (the (List Char) (into str)))))
 

(defpackage #:coalgex
  (:use
   #:coalton
   #:coalton-prelude)
  (:local-nicknames
   (#:vec #:coalton-library/vector)
   (#:iter #:coalton-library/iterator)
   (#:str  #:coalton-library/string)))

(in-package #:coalgex)

(named-readtables:in-readtable coalton:coalton)

(cl:defmacro %define-eq-transitionable-instance (transition-type)
  "Define transitionable instance for relevant types with `EQ` defined."
  `(define-instance (Transitionable ,transition-type)
    (define (%match val input)
      (== val input))))

(coalton-toplevel
  (define-type (Edge :a)
    (Edge :a (State :a))
    (EAny (State :a))
    (EEpsilon (State :a)))

  (define-type (State :a)
    (State (Vector (Edge :a)))
    (Accepting (Vector (Edge :a)))
    )

  (define (edges state)
    (match state
      ((State edges)
       edges)
      ((Accepting edges)
       edges)))

  (define-class (Transitionable :a)
    ""
    (%match (:a -> :a -> boolean)))

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
      ((Edge val _state)
       (%match val input))
      (_ True)))
  
  (declare transition (Transitionable :a
				      => State :a
				      -> :a
				      -> (Optional (State :a))))
  (define (transition state input)
    "This moves to the first possible state, returning `None` if there are no matching states."
    (let ((targets (iter:into-iter (edges state))))
      (match (iter:find! (match-edge input) targets)
	((Some (Edge _val stat))
	 (Some stat))
	(_ None)))))

(coalton-toplevel

  ;; so to chain these together, I need to find each accepting state and resolve it to point to the start of the next one

  (declare %is ((Transitionable :a) => :a -> (State :a)))
  (define (%is val)
    (State (vec:make
	    (Edge val (Accepting (vec:new))))))
  
  (declare %any (Unit -> State :a))
  (define (%any)
    (State (vec:make (EAny (Accepting (vec:new))))))

  (declare %union ((State :a) -> (State :a) -> (State :a)))
  (define (%union in1 in2)
    (let ((end-state (Accepting (vec:new))))
      (State (vec:make
	      (Edge in1 end-state)
	      (Edge in2 end-state)))))

  (declare %concat (:a -> :a -> (State :a)))
  (define (%concat in1 in2)
    (State (vec:make
	    (Edge
	     in1
	     (State (vec:make
		     (Edge
		      in2
		      (State (vec:new)))))))))

  #+ig  (define (string->dfa str)
	  (let ((build (fn (chars)
			 (match chars
			   ((Cons c cs)
			    (State (vec:make
				    (Edge c (build cs)))))
			   ((Nil)
			    (State (vec:new)))))))
	    (build (into str)))
	  ()
	  (iter:mconcat! %concat "" (chars str))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(coalton-toplevel

  (define (%or))

  (declare build-string-match-dfa (String -> (State Char)))
  (define (build-string-match-dfa str)
    (let ((build (fn (chars)
		   (match chars
		     ((Cons c cs)
		      (State (vec:make
			      (Edge c (build cs)))))
		     ((Nil)
		      (State (vec:new)))))))
      (build (into str))))

  
  (define other-example (State (vec:make (Edge 0 (State (vec:new)))
					 (Edge 1 (State (vec:make (Edge 3 (State (vec:new)))))))))

  
  (define example-dfa
    (State
     (vec:make
      (Edge 0 (State (vec:new)))

      (Edge 1 (State
	       (vec:make
		(Edge 2 (State (vec:new)))
		(Edge 3 (State (vec:new))))))))))

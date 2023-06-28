;;;; nfa.lisp
;;;;
;;;;

(in-package #:coalgex)

;;;
;;; Generating an NFA
;;;

(coalton-toplevel

;;;
;;; Defining Regular Expressions
;;;
  
  (define-type Regex
    (RChar String)
    (RCat Regex Regex)
    (RAlt Regex Regex)
    (RStar String)                ; eventually Regex instead of string
    (RPlus String)
    (REps))

;;;
;;; Converting the Regex into an NFA
;;;
  
  (define-struct Node
    (Index UFix)
    (Out (Cell (List UFix))))
  
  (define-struct Edge
    (Index UFix)
    (Start UFix)                        ; pointer to starting node
    (C String)                          ; maybe Regex instead
    (Dest UFix))                        ; pointer to end node

;;;
;;; Env for tracking nodes and edges
;;;
  
  (define-struct Env
    (nodestack (Vector Node))
    (edgestack (Vector Edge)))
  
  (define (init-env)
    (let e =  (Env
               (vec:with-initial-element 1 (Node 0 (cell:new (make-list 0))))
               (vec:with-initial-element 1 (Edge 0 0 "" 1))))
    (vec:push! (Node 1 (cell:new nil)) (.nodestack e))
    e)

  
;;;
;;; Operations for handling nodes and edges
;;;
  
  (declare last-node (Env -> Node))
  (define (last-node env)
    "Returns the last node in the stack."
    (vec:last-unsafe (.nodestack env)))

  (declare last-node-index (Env -> UFix))
  (define (last-node-index env)
    (.index (last-node env)))

  (declare last-edge (Env -> Edge))
  (define (last-edge env)
    (vec:last-unsafe (.edgestack env)))

  (declare last-edge-index (Env -> Ufix))
  (define (last-edge-index env)
    (.index (last-edge env)))

  (declare add-node (Node -> Env -> Unit)) ; this extra handling might unnecessary
  (define (add-node node env)
    "Adds a new node, replacing a previous version if necessary."
    (let i = (.index node))
    (let stack = (.nodestack env))
    (match (vec:index i  stack)
      ((Some _n) (vec:set! i node stack))
      (_None (progn (vec:push! node stack)
                    Unit))))

  (declare add-next-node (Env -> Unit))
  (define (add-next-node env)
    "Adds the next node."
    (add-node (Node (1+ (.index (vec:last-unsafe (.nodestack env))))
                    (cell:new Nil)
                    )
              env)
    Unit)

  (declare next-node (Env -> UFix))
  (define (next-node env)
    (add-next-node env)
    (last-node-index env))

  (declare add-edge-to-node (Edge -> Env -> Unit))
  (define (add-edge-to-node edge env)
    (cell:push! (.out (vec:index-unsafe (.start edge) (.nodestack env))) (.index edge))
    Unit
    )
  (declare add-edge (Edge -> Env -> Unit))
  (define (add-edge edge env)
    (vec:push! edge (.edgestack env))
    (add-edge-to-node edge env))

  ;;
  ;; NFA Building Algorithm
  ;;
  ;; - Start with an env init'd with two nodes and an empty/epsilon
  ;; edge between them.
  ;;
  ;; - Move through the Regex, converting each component into
  ;; equivalent nodes and edges
  ;;
  ;; - ????
  ;;
  ;; - Profit/NFA
  
  (define (make-NFA regex)
    "Builds an NFA graph using "
    (let env = (init-env))
    (let ((f (fn (regex)
               (match regex
                 ((RChar s)
                  (add-edge (Edge (1+ (last-edge-index env))
                                  (last-node-index env)
                                  s
                                  (next-node env))
                            env))
                 ((RCat r1 r2)
                  (progn (f REps)
                         (f r1)
                         (f Reps)
                         (f r2)))
                 ((RAlt r1 r2)
                  (let ((lni (last-node-index env)))
                    (progn (add-edge (Edge (1+ (last-edge-index env))
                                           lni
                                           ""
                                           (next-node env))
                                     env)
                           (f r1)
                           (add-edge (Edge (1+ (last-edge-index env))
                                           lni
                                           ""
                                           (next-node env))
                                     env)
                           (f r2))))
                 ((RStar s)
                  (let ((lni (last-node-index env)))
                    (add-edge (Edge (1+ (last-edge-index env))
                                    lni
                                    s
                                    lni)
                              env)))
                 ((RPlus s)
                  (f (RChar s))
                  (f (RStar s)))
                 (_REps (progn (add-edge (Edge (1+ (last-edge-index env))
                                               (last-node-index env)
                                               ""
                                               (next-node env))
                                         env)))))))
      (f regex))
    env)
  
  )

;;;
;;; Matching:
;;;

(coalton-toplevel
 
  ;;
  ;; Matching algorithm:
  ;;

  ;; Start at the initial node, with a string buffer:

  ;; For each node:
  ;;
  ;; - Travel each edge leading from the node, collecting the
  ;; concatenation of the edge string with the buffer
  ;;
  ;; - At each node, compare the buffer with the first buff-length
  ;;   characters of the string. If it doesn't match, give up on that
  ;;   branch
  ;;
  ;; - If it finds a complete match, return true, otherwise defaults to false in the end
  ;; 

  ;; Since this approach uses a print-buffer, the epsilons/empty
  ;; strings need not be handled separately: concatting null strings
  ;; won't affect the length of the string!
  
  (declare out-edges (Node -> Env -> (List Edge)))
  (define (out-edges node env)
    (map (fn (x)
           (vec:index-unsafe x (.edgestack env)))
         (cell:read (.out node))))

  (declare travel-edge (Edge -> String -> Env -> (Tuple Node String)))
  (define (travel-edge edge buffer env)
    (Tuple (vec:index-unsafe (.dest edge) (.nodestack env))
           (str:concat buffer (.c edge))))
  
  (define (rmatch regex string)
    "Checks whether a string satisfies a regex."
    (let nfa = (make-NFA regex))
    (let ismatch = (Cell:new False))
    (let ((f (fn (current-node string buffer)
               (cond
                 ((== string buffer)
                  (cell:Write! ismatch True)
                  Unit)
                 ((>= (str:length buffer) (str:length string))
                  Unit)
                 ((== buffer
                      (fst (str:split (str:length buffer) string)))
                  (iter:for-each! (fn (x)
                                    (let ((te (travel-edge x buffer nfa)))
                                      (f (fst te) string (snd te)))
                                    Unit)
                                  (iter:into-iter (out-edges current-node nfa))))
                 (True Unit)))))
      (f (vec:head-unsafe (.nodestack nfa)) string ""))
    (cell:read ismatch)))

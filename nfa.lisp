(defpackage #:coalgex (:use
   #:coalton
   #:coalton-prelude)
  (:local-nicknames (#:vec #:coalton-library/vector)
                    (#:cell #:coalton-library/cell)
                    (#:state #:coalton-library/monad/state)
                    (#:math #:coalton-library/math)
                    (#:str #:coalton-library/string)
                    (#:list #:coalton-library/list))
  ;(:export)
  )

(in-package #:coalgex)

(named-readtables:in-readtable coalton:coalton)

#+coalton-release
(cl:declaim #.coalton-impl/settings:*coalton-optimize-library*)

;; first step is building the regex nfa
;; this should be a structure of nfa nodes which can be traversed to verify the matching

;; Thompson NFA
;; https://swtch.com/~rsc/regexp/regexp1.html

(coalton-toplevel
  
 (define-struct Node
   "An NFA Node, containing a list of edge pointers."
   (Edges (vec:Vector UFix)))

  (define-type Edge
    "An NFA Edge, either a character for matching or an empty edge. "
    (EChar Char (Cell Ufix))
    (EAny (Cell UFix))
    ;(ENull (Cell UFix))
    )

  (define-struct NFA
    "An NFA graph consisting of nodes and edges."
    (nodestack (vec:Vector Node))
    (edgestack (vec:Vector Edge)))

  (declare init-env (Unit -> NFA))
  (define (init-env)
    "Initializes an environment with one empty node."
    (NFA (vec:with-initial-element 1 (Node (vec:new)))
         (vec:new))))

(coalton-toplevel

  (declare next-node (NFA -> UFix))
  (define (next-node (NFA nodestack _ ))
    "Returns a pointer to the next node."
    (vec:length nodestack))

  (declare current-node (NFA -> UFix))
  (define (current-node (NFA nodestack _ ))
    "Returns a pointer to the current node."
    (math:1- (vec:length nodestack)))

  (declare push-node (Node -> (state:ST NFA Unit)))
  (define (push-node node)
    "Pushes a node onto the nodestack."
    (do
     (env <- state:get)
     (pure (vec:push! node (.nodestack env)))
      (state:put env)))

  (declare new-node (Unit -> (state:ST NFA Unit)))
  (define (new-node)
    "Adds a new empty node to the nodestack."
    (push-node (Node (vec:new))))
  
  (declare add-node-edge (UFix -> UFix -> (state:ST NFA Unit)))
  (define (add-node-edge node edge)
    "Adds an edge pointer to an index node."
    (do
     (env <- state:get)
     (pure (match (vec:find-elem edge (.edges (vec:index-unsafe node (.nodestack env))))
             ((Some _)
              Unit)
             ((None)
              (vec:push! edge (.edges (vec:index-unsafe node (.nodestack env))))
              Unit)))
      (state:put env)))

  (declare add-edge (Edge -> (state:ST NFA Unit)))
  (define (add-edge edge)
    "Adds an edge."
    (do
     (env <- state:get)
     (pure (vec:push! edge (.edgestack env)))
      (state:put env)))

  (declare add-char-edge (Char -> UFix -> (state:ST NFA Unit)))
  (define (add-char-edge c target)
    "Adds a character edge."
    (do
     (env <- state:get)
     (pure (vec:push! (EChar c (cell:new target)) (.edgestack env)))
      (state:put env)))

  (declare add-EAny (UFix -> (state:ST NFA Unit)))
  (define (add-EAny target)
    "Adds a character edge."
    (do
     (env <- state:get)
     (pure (vec:push! (EAny (cell:new target)) (.edgestack env)))
      (state:put env)))

  (declare next-edge (NFA -> UFix))
  (define (next-edge (NFA _ edgestack))
    "Returns a pointer to the next edge."
    (vec:length edgestack))

  (declare current-edge (NFA -> UFix))
  (define (current-edge (NFA _ edgestack))
    "Returns a pointer to the current edge."
    (math:1- (vec:length edgestack)))

  (declare get-edge-target (Edge -> (Cell UFix)))
  (define (get-edge-target edge)
    "Returns the target of an edge."
    (match edge
      ((EChar _ target)
       target)
      ((EAny target)
       target)
      #+ig((ENull target)
       target)))
  
  (declare set-edge-target (UFix -> UFix -> (state:ST NFA Unit)))
  (define (set-edge-target edge-ptr target)
        "Sets the target node pointer for a given edge"
        (do
         (env <- state:get)
         (pure (cell:write! (get-edge-target (vec:index-unsafe edge-ptr (.edgestack env))) target))
          (state:put env))))

(coalton-toplevel

  (define-type Regex
    (RChar Char)
    (RConcat Regex Regex)
    (RAlt Regex Regex)
    (R? Regex)
    (R* Regex)
    (R+ Regex)
    (R.))

  (declare add-char (Char -> (state:ST NFA Unit)))
  (define (add-char char)
    "Adds a character edge to the NFA."
    (do
     (env <- state:get)
     (add-node-edge
      (current-node env)
      (next-edge env))
      (add-edge (EChar char
                       (cell:new (next-node env))))
      (new-node)))

  (declare add-any (Unit -> (state:ST NFA Unit)))
  (define (add-any)
    "Adds an edge matching any character to the NFA."
    (do
     (env <- state:get)
     (add-node-edge
      (current-node env)
      (next-edge env))
      (add-edge (EAny (cell:new (next-node env))))
      (new-node)))

  ;(declare add-null (Unit -> (state:ST NFA Unit)))
  #+ig(define (add-null)
    "Adds an edge matching any character to the NFA."
    (do
     (env <- state:get)
     (add-node-edge
      (current-node env)
      (next-edge env))
      (add-edge (ENull (cell:new (next-node env))))
      (new-node)))
  
  (declare add-concat (Regex -> Regex -> (state:ST NFA Unit)))
  (define (add-concat regex1 regex2)
    "Adds a concatenation."
    (do
     (NFAful regex1)
     (NFAful regex2)))

  (declare add-alt (Regex -> Regex -> (state:ST NFA Unit)))
  (define (add-alt regex1 regex2)
    (do
     (env <- state:get)
     (let current = (current-node env))
      (NFAful regex1)
      (let REnd = (current-edge env))
      (pure (vec:pop! (.nodestack env)))
      (add-node-edge current (next-edge env))
      (NFAful regex2)
      
      (set-edge-target REnd (current-node env))))

  (declare add-* (Regex -> (state:ST NFA Unit)))
  (define (add-* regex)
    (do
     (env <- state:get)
     (let current = (current-node env))
      (NFAful regex)
      (set-edge-target (current-edge env) current)
      (add-node-edge current (next-edge env))))

  (declare add-? (Regex -> (state:ST NFA Unit)))
  (define (add-? regex)
    (do
     (env <- state:get)
     (let current = (current-node env))
      (NFAful regex)
      (add-node-edge current (next-edge env))))

  (declare add-+ (Regex -> (state:ST NFA Unit)))
  (define (add-+ regex)
    (do
     (env <- state:get)
     (let current = (current-node env))
      (NFAful regex)
      (set-edge-target (current-edge env) current)
      (add-node-edge current (next-edge env))
      (NFAful regex)))

  ;; TODO
  #+inore(define (add.))
  
  (declare NFAful (Regex -> (state:ST NFA Unit)))
  (define (NFAful regex)
    "Builds an NFA."
    (match regex
      ((RChar c)
       (add-char c))
      ((R.)
       (add-any))
      ((RConcat regex1 regex2)
       (add-concat regex1 regex2))
      ((RAlt regex1 regex2)
       (add-alt regex1 regex2))
      ((R* regex)
       (add-* regex))
      ((R? regex)
       (add-? regex))
      ((R+ regex)
       (add-+ regex))))

  (declare make-nfa (Regex -> NFA))
  (define (make-nfa regex)
    (fst (state:run (NFAful regex) (init-env)))))

;;;
;;;
;;;
;;
;;;

(coalton-toplevel

  (define-struct TraverseState
    (str String)
    (nfa NFA))

  (declare get-node (UFix -> (state:ST TraverseState Node)))
  (define (get-node ptr)
    "Gets the indexth node in the NFA nodestack."
    (do
     (state <- state:get)
     (pure (vec:index-unsafe ptr (.nodestack (.nfa state))))))

  (declare get-edge (UFix -> (state:ST TraverseState Edge)))
  (define (get-edge ptr)
    "Gets the indexth edge in the NFA nodestack."
    (do
     (state <- state:get)
     (pure (vec:index-unsafe ptr (.edgestack (.nfa state))))))

  (declare first-node (Unit -> (state:ST TraverseState Node)))
  (define (first-node)
    "Gets the first node in the NFA."
    (get-node 0))

  (declare get-char (UFix -> (state:ST TraverseState (Optional Char))))
  (define (get-char char-ptr)
    "Finds the nth char in the search string."
    (do
     (state <- state:get)
     (pure (str:ref (.str state) char-ptr))))

  (declare get-node-edges (UFix -> (state:ST TraverseState (Vector UFix))))
  (define (get-node-edges node-ptr)
    (do
     (node <- (get-node node-ptr))
     (pure (match node
             ((Node edges)
              edges)))))

  ;;;
  ;;;
  ;;;
  
  (declare traverse-edge (UFix -> UFix -> (state:ST TraverseState Boolean)))
  (define (traverse-edge edge-ptr char-ptr)
    "Traverses an edge in an NFA."
    (do
     (char <- (get-char char-ptr))
     (edge <- (get-edge edge-ptr))
      (match char
        ((None)
         (pure False))
        ((Some x)
         (match edge
           ((EChar c targ)
            (if (== c x)
                (traverse-node (cell:read targ) (1+ char-ptr))
                (pure False)))
           ((EAny targ)
            (traverse-node (cell:read targ) (1+ char-ptr))))))))

  (declare traverse-node (UFix -> UFix -> (state:ST TraverseState Boolean)))
  (define (traverse-node node-ptr char-ptr)
    "Traverses a node in an NFA."
    (do
     (edges <- (get-node-edges node-ptr))
     (char <- (get-char char-ptr))
      (let empty-string = (match char
                            ((None)
                             True)
                            ((Some x)
                             False)))
      (if (and (vec:empty? edges)
               empty-string)
          (pure True)
          (while (not (vec:empty? edges))
            (do
             (m? <- (traverse-edge (unwrap (vec:pop! edges)) char-ptr))
             (cond (m?
                    (pure True)))))
          ;; not for, or iter-for-each! or map... I need each of these to happen independently 
  #+ig        (pure (map (fn (e)
                       (do
                        (m? <- (traverse-edge e char-ptr))))
                     edges))
#+ig          (let ((match? (cell:new False)))
            (pure (map (fn (e)))) (for e in edges
                                       (do 
                                        (m? <- (traverse-edge e char-ptr))
                                        (pure (cell:write! match? m?))))
            (match match?) (pure (cell:Read match?))))))

  #+ig(define (get-current-node)
        "Returns the current-node."
        (do
         (state <- state:get)
         (get-node (cell:read (.current-node state)))))

  #+ig(declare set-current-node (UFix -> (state:ST TraverseState Unit)))
  #+ig(define (set-current-node ptr)
        "Sets the current NFA node."
        (do
         (state <- state:get)
         (pure (cell:write! (.current-node state) ptr))
          (pure Unit)))
  
;;;;;; I don't think advancing the string is right because that shouldn't be global state

 ; (declare traverse-edge (UFix -> (state:ST TraverseState Boolean)))
 #+ig (define (traverse-edge ptr)
    (do
     (first-char <- (first-char))
     (match first-char
       ((None)
        (pure True))
       ((Some x)
        (match edge
          ((EChar c targ)))
        )
       )
      (edge <- (get-edge ptr))
      (match edge
        ((EChar c targ)
         ))))
  
                                        ;(declare traverse-node (Unit -> (state:ST TraverseState )))
  #+ig(define (traverse-node)
        (do
         (node <- (get-current-node))
         (traverse))
        )
  )


;; I don't know if I need this step...
#+ig(coalton-toplevel

  (define-struct DFAState
    "A State struct for handling NFA->DFA conversion."
    (DFA NFA)
    (NFA NFA)
    (Current-NFA-Node (Cell UFix)))
  
  (declare get-node (UFix -> (state:ST DFAState Node)))
  (define (get-node index)
    "Gets the indexth node in the NFA nodestack."
    (do
     (state <- state:get)
     (pure (vec:index-unsafe index (.nodestack (.NFA state))))))

  (declare get-edge (UFix -> (state:ST DFAState Edge)))
  (define (get-edge index)
    "Gets the indexth edge in the NFA nodestack."
    (do
     (state <- state:get)
     (pure (vec:index-unsafe index (.edgestack (.NFA state))))))

  (declare first-node (Unit -> (state:ST DFAState Node)))
  (define (first-node)
    "Gets the first node in the NFA."
    (get-node 0))

  (define (get-current-node)
    "Returns the current-node."
    (do
     (state <- state:get)
     (get-node (cell:read (.current-NFA-Node state)))))

  (declare set-current-node (UFix -> (state:ST DFAState Unit)))
  (define (set-current-node ptr)
    "Sets the current NFA node."
    (do
     (state <- state:get)
     (pure (cell:write! (.current-NFA-Node state) ptr))
      (pure Unit)))

  ;; so first 
  (declare epsilon-closure (Unit -> (State DFAState Unit)))
  (define (epsilon-closure)
    ())
                                        ;(declare DFAful (NFA -> (state:ST NFA Unit)))
  #+ig(define (DFAful nfa)
        ()
    
        )
  
                                        ;  (declare NFA->DFA (NFA -> NFA))
  #+ig  (define (NFA->DFA (NFA nodestack edgestack))
          (fst (state:run (DFAful nfa)
                          (init-env)))))

;; write regex parser, starting with tokeni

;; functions for printing

;; break input into iterator of chars
#+ig(coalton-toplevel

  (define-class traverse-state
    (Current-node UFix)
    (Current-edge UFix)) ;; for tracking the state 
  
  (declare traverse-edge (Char -> UFix -> (state:ST NFA Boolean)))
  (define (traverse-edge char edge)
    (do
     (env <- state:get)
     (let e = (vec:index-unsafe edge (.edgestack env)))
      (if (== char (.c e))
          (traverse-edges (get-edge-target e))
          (pure False))))

  (declare traverse-edges (Char -> UFix -> (state:ST NFA Boolean)))
  (define (traverse-edges char node)
    "Traverses all edges leading from a node."
    (do
     (env <- state:get)
     (let n = (vec:index-unsafe node (.nodestack env)))
      (if (vec:empty? (.edges n))
          (pure True)
          (for e in (.edges n)
               (traverse-edge e)))))
  
  (define (rmatch regex string)
    (let nfa = (make-NFA regex))
    (let chars = (str:chars string))
    (for c in chars))
  )


#+sb-package-locks
(sb-ext:lock-package "COALTON-LIBRARY/REGEX")

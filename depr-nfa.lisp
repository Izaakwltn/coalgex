;;;; This file is part of Coalgex.
;;;;
;;;; package.lisp
;;;;
;;;; Copyright Izaak Walton (c) 2023
;;;; 
;;;; Coalgex is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
;;;; Coalgex is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
;;;; You should have received a copy of the GNU General Public License along with Coalgex. If not, see <https://www.gnu.org/licenses/>. 
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
    (RQuest Regex)
    (RStar Regex)                 
    (RPlus Regex)
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
    "Initializes the NFA-building environment with two nodes and an edge connecting them."
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

  (declare add-node (Node -> Env -> Unit))
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
    "Adds a new node, returns the index to that node."
    (add-next-node env)
    (last-node-index env))

  (declare add-edge-to-node (Edge -> Env -> Unit))
  (define (add-edge-to-node edge env)
    "Updates the start node's out-list with this edge's index/pointer."
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
    "Builds an NFA graph."
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
                    (progn (f REps)
                           (f r1)
                           (add-edge (Edge (1+ (last-edge-index env))
                                           lni
                                           ""
                                           (next-node env))
                                     env)
                           (f r2))))
                 ((RQuest r)
                  (f (RAlt r (RChar ""))))
                 ((RStar r)
                  (let ((lni (last-node-index env)))
                    (f REps)
                    (f r)
                    (add-edge (Edge (1+ (last-edge-index env))
                                    (last-node-index env)
                                    ""
                                    lni)
                              env)))
                 ((RPlus r)
                  (f r)
                  (f (RStar r)))
                 (_REps (f (RChar "")))))))
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
    "Collects all of the edges leading from a given node."
    (map (fn (x)
           (vec:index-unsafe x (.edgestack env)))
         (cell:read (.out node))))

  (declare travel-edge (Edge -> String -> Env -> (Tuple Node String)))
  (define (travel-edge edge buffer env)
    "Travels along one edge, advancing the buffer and the current-node"
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
      (f (vec:head-unsafe (.nodestack nfa))
         (match string
           ("" (error "I'm afraid you've supplied an empty string."))
           (_ string))
         ""))
    (cell:read ismatch)))

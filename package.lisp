;;;; package.lisp
;;;;
;;;;

(defpackage #:coalgex
  (:use #:coalton
        #:coalton-prelude)
  (:local-nicknames (#:str #:coalton-library/string)
                    (#:char #:coalton-library/char)
                    (#:iter #:coalton-library/iterator)
                    (#:vec #:coalton-library/vector)
                    (#:cell #:coalton-library/cell)
                    (#:list #:coalton-library/list))
  (:export
   #:RChar
   #:RCat
   #:RAlt
   #:RStar
   #:RPlus
   #:REps

   #:make-NFA
   #:rmatch))

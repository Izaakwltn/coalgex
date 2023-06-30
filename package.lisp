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

(defpackage #:coalgex
  (:use #:coalton
        #:coalton-prelude)
  
  (:local-nicknames
   (#:str #:coalton-library/string)
   (#:char #:coalton-library/char)
   (#:iter #:coalton-library/iterator)
   (#:vec #:coalton-library/vector)
   (#:cell #:coalton-library/cell)
   (#:list #:coalton-library/list))
  
  (:export
   ;; Regex Type Constructors
   #:RChar
   #:RCat
   #:RAlt
   #:RStar
   #:RPlus
   #:REps
   ;; Top-level functions
   #:make-NFA
   #:rmatch))

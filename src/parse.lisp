;;;;
;;;;
;;;;

;; parsing regex strings into regex objects

(defpackage #:coalgex/parse
  (:use #:coalton
        #:coalton-prelude)
  (:local-nicknames (#:vec #:coalton-library/vector)
                    (#:cell #:coalton-library/cell)
                    (#:str #:coalton-library/string))
  (:export
   #:Regex
   #:RChar
   #:RCharset
   #:RConcat
   #:RAlt
   #:R?
   #:R*
   #:R+
   #:R.)
  )


(in-package #:coalgex/parse)

(coalton-toplevel

  (define-type Regex
    (RChar Char)
    (RCharset (List Char))
    (RConcat Regex Regex)
    (RAlt Regex Regex)
    (R? Regex)
    (R* Regex)
    (R+ Regex)
    (R.))
  
  (define-type token
    (TChar Char)
    (TDot)
    (TPlus)
    (TStar)
    (TQuest)
    (THat)
    (TDolla)
    (TLeftParen)
    (TRightParen)
    (TLeftBracket)
    (TRightBracket)
    (TLeftCurly)
    (TRightCurly)
    (TPipe)
    (TEscape))

  ;; I don't know if I should handle tokenization here or later.
  (define (tokenize rstring)
    (let toks = (vec:new))
    (let escaped = (cell:new False))
    (for c in (str:chars rstring)
         (cond ((== c #\\)
                (cell:write! escaped True)
                Unit)
               (True
                (vec:push!
                 (cond ((cell:read escaped)
                        (cell:write! escaped False)
                        (TChar c))
                       (True
                        (match c
                          (#\. TDot)
                          (#\+ TPLus)
                          (#\* TStar)
                          (#\? TQuest)
                          (#\^ TDolla)
                          (#\( TLeftParen)
                          (#\) TRightParen)
                          (#\[ TLeftBracket)
                          (#\] TRightBracket)
                          (#\{ TLeftCurly)
                          (#\} TRightCurly)
                          (#\| TPipe)
                          
                          (_ (TChar c)))))
                 toks)
                Unit)))
    toks)

  ;; define types for context types

  ;; reader macro for handling escape characters

  ;; I could take a pass and condense words
  
  

  )

(defpackage #:coalgex.regex
  (:use #:coalton
	#:coalton-prelude)
  (:local-nicknames
   (#:nfa #:coalgex.nfa)
   (#:list #:coalton-library/list)
   (#:vec #:coalton-library/vector)
   (#:cell #:coalton-library/cell)))

(in-package #:coalgex.regex)

;;;
;;; Tokenizing the regex
;;;

(coalton-toplevel

  #+ig"           PURPOSE                                  WHERE
\   Escape the next character                    Always, except when
                                                 escaped by another \
^   Match the beginning of the string            Not in []
      (or line, if /m is used)
^   Complement the [] class                      At the beginning of []
.   Match any single character except newline    Not in []
      (under /s, includes newline)
$   Match the end of the string                  Not in [], but can
      (or before newline at the end of the       mean interpolate a
      string; or before any newline if /m is     scalar
      used)
|   Alternation                                  Not in []
()  Grouping                                     Not in []
[   Start Bracketed Character class              Not in []
]   End Bracketed Character class                Only in [], and
                                                   not first
*   Matches the preceding element 0 or more      Not in []
      times
+   Matches the preceding element 1 or more      Not in []
      times
?   Matches the preceding element 0 or 1         Not in []
      times
{   Starts a sequence that gives number(s)       Not in []
      of times the preceding element can be
      matched
{   when following certain escape sequences
      starts a modifier to the meaning of the
      sequence
}   End sequence started by {
-   Indicates a range                            Only in [] interior
#   Beginning of comment, extends to line end    Only with /x modifier"
  ;; nfa builder should probably tokenize first

  (define-type regex-token
    (<Char Char)
    <Backslash
    <ForwardSlash
    <Dot
    <Dollar
    <Caret
    <Star
    <Plus
    <Minus
    <Question
    <Vertical
    <LeftParen
    <RightParen
    <LeftBracket
    <RightBracket
    <LeftCurly
    <RightCurly)

  (define (tokenize regex-string)
    "Returns a tokenized list of regex characters."
    (map (fn (c)
	   (match c
	     (#\\ <BackSlash)
	     (#\/ <ForwardSlash)
	     (#\. <Dot)
	     (#\$ <Dollar)
	     (#\^ <Caret)
	     (#\* <Star)
	     (#\+ <Plus)
	     (#\? <Question)
	     (#\- <Minus)
	     (#\| <Vertical)
	     (#\( <LeftParen)
	     (#\) <RightParen)
	     (#\[ <LeftBracket)
	     (#\] <RightBracket)
	     (_ (<Char c))))
	 (the (List Char) (into regex-string))))

  ;; take the tokens and group into their relevant groups- infix for union, prefix for brackets, postfix for others.

  )

;;;
;;; Building the NFA
;;;

(coalton-toplevel

  (define-struct NFABuilder
    (input (Vector Char))
    (pointer (Cell UFix))
    (stack (Vector (nfa:NFA Char))))

  (define (make-builder str)
    (NFABuilder (the (Vector Char)
		     (into (the (List Char) (into str))))
		(cell:new 0)
		(vec:new)))

  (declare next-input! (NFABuilder -> Char))
  (define (next-input! (NFABuilder input pointer _stack))
    (let ((next (vec:index-unsafe (cell:read pointer)
				  input)))
      (cell:increment! pointer)
      next))

  (declare push-to-stack! ((nfa:NFA Char) -> NFABuilder -> NFABuilder))
  (define (push-to-stack! value builder)
    (match builder
      ((NFABuilder _input _pointer stack)
       (vec:push! value stack)))
    builder)

  (declare escape (NFABuilder -> NFABuilder))
  (define (escape builder)
    (push-to-stack! (nfa:%is (next-input! builder))
		    builder))

  (declare consume (NFABuilder -> NFABuilder))
  (define (consume builder)
    (let ((input (next-input! builder)))
      (match input
	(#\\ (escape builder))
	;(#\[) make range
	
	(_
	 (push-to-stack! (nfa:%is input) builder))))
    )
  
  #+ig(define (build-nfa regex-string)
	(let ((builder (make-builder regex-string)))
      
	  builder)
	)

  )

(coalton-toplevel
  
  (declare nfa-builder (String -> (Vector (nfa:NFA Char))))
  (define (nfa-builder regex-string)
    "PERL style"
    (let ((build (fn (input)
		   (match input
		     ((cons x xs)
		      (match x
			;; If it's escaped, skip it
			(#\\
			 (nfa:%concat (nfa:%is (list:car xs))
				      (build (list:cdr xs))))
			;; Add special characters here
			;;
			;;
			(_ (nfa:%is x) )
			))
		     ((Nil)
		      (nfa:%empty))))))
      (vec:make (build (the (List Char) (into regex-string)))))))

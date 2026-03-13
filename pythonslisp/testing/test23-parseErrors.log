; ============================================================
; test70-parseErrors.lisp
; Parser-level syntax error tests.
; ============================================================

>>> ;;; Unmatched open paren
... (

%%% Syntax Error: "" (3,3)
%%% (
%%%   ^
%%% ) expected.
==>

>>> ;;; Unmatched close paren
... )

%%% Syntax Error: "" (2,2)
%%% )
%%%  ^
%%% Object expected.
==>

>>> ;;; Incomplete expression
... (+ 1

%%% Syntax Error: "" (3,6)
%%% (+ 1
%%%      ^
%%% ) expected.
==>

>>> ;;; Extra close paren
... (+ 1 2))

%%% Syntax Error: "" (2,9)
%%% (+ 1 2))
%%%         ^
%%% Object expected.
==>

>>> ;;; Unterminated string
... "unterminated

%%% Syntax Error: "" (3,15)
%%% "unterminated
%%%               ^
%%% '"' expected.  A string literal may be unterminated.
==>

>>> ;;; Quote with nothing after it
... '

%%% Syntax Error: "" (3,3)
%%% '
%%%   ^
%%% Unexpected end of input after quote.
==>

>>> ;;; Quote before close paren
... ')

%%% Syntax Error: "" (2,3)
%%% ')
%%%   ^
%%% Object expected.
==>

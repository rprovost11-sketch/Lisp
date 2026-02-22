; (null sexpr)
;
; Does sexpr evaluate to nil?
(alias null not)

; Setup some aliases for the otherwise oddly named predicates.
;
(alias isNil? null)
(alias endp   null)

(alias isNumber? numberp)

(alias isInteger? integerp)

(alias isRational? rationalp)

(alias isFloat? floatp)

(alias isSymbol? symbolp)

(alias isAtom? atom)

(alias isList? listp)

(alias isMap? mapp)

(alias isString? stringp)

(alias isFunction? functionp)

; CL-compatible I/O aliases
(alias write-line uwriteLn!)
(alias read-line  readLn!)
(alias prin1      write!)
(alias princ      uwrite!)

; list-length is the legacy name; length is the CL-compatible primary name
(alias list-length length)

; CL-compatible string output aliases
(alias princ-to-string ustring)
(alias prin1-to-string string)

; CL-compatible string comparison aliases (= /= < <= > >= already handle strings)
(alias string=  =)
(alias string/= /=)
(alias string<  <)
(alias string<= <=)
(alias string>  >)
(alias string>= >=)


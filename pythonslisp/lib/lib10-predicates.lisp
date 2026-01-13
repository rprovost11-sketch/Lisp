; (null sexpr)
;
; Does sexpr evaluate to nil?
(setf null not)

; Setup some aliases for the otherwise oddly named predicates.
;
(setf isNil? null)

(setf isNumber? numberp)

(setf isInteger? integerp)

(setf isRational? rationalp)

(setf isFloat? floatp)

(setf isSymbol? symbolp)

(setf isAtom? atom)

(setf isList? listp)

(setf isString? stringp)

(setf isFunction? functionp)



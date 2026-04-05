; (null sexpr)
;
; Does sexpr evaluate to nil?
(alias null not)

; undef! is the legacy name for makunbound
(alias undef! makunbound)

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

(alias isDict? dictp)

(alias isString? stringp)

(alias isFunction? functionp)

(alias is? eq)

; CL-compatible I/O aliases
(alias prin1      write!)
(alias princ      uwrite!)

; list-length is the legacy name; length is the CL-compatible primary name
(alias list-length length)

; CL-compatible string comparison aliases (= /= < <= > >= already handle strings)
(alias string=  =)
(alias string/= /=)
(alias string<  <)
(alias string<= <=)
(alias string>  >)
(alias string>= >=)

;;; Logical connectives

(defmacro and (&rest forms)
   "Evaluates forms left to right.  Returns nil at the first nil form.
Returns the value of the last form if all are truthy.  (and) returns t."
   (cond ((not forms) 't)
         ((not (cdr forms)) (car forms))
         (t `(if ,(car forms) (and ,@(cdr forms)) nil))))

(defmacro or (&rest forms)
   "Evaluates forms left to right.  Returns the first truthy value found.
Returns nil if all forms are nil.  (or) returns nil."
   (cond ((not forms) 'nil)
         ((not (cdr forms)) (car forms))
         (t (let ((var (gensym "OR")))
               `(let ((,var ,(car forms)))
                   (if ,var ,var (or ,@(cdr forms))))))))

;;; Sequence predicates

(defun notany (pred lst)
   "Returns T if no element of lst satisfies pred, NIL otherwise."
   (not (some pred lst)))

(defun notevery (pred lst)
   "Returns T if not every element of lst satisfies pred, NIL otherwise."
   (not (every pred lst)))


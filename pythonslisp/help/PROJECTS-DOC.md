# Projects

*Larger challenges for the IttyBittyLisp series.*

Each EVALUATOR doc ends with a short Challenges section -- small additions
that fit naturally at that stage.  This document collects the bigger
projects: ones that span multiple docs, require deeper design decisions,
or result in a qualitatively different interpreter.  They assume you have
worked through at least EVALUATOR1A, EVALUATOR1B, EVALUATOR2, and
PARSER-DOC.


## 1. A Working REPL

*Requires: PARSER-DOC + EVALUATOR1B or later.*

Wire the parser and evaluator together into a read-eval-print loop:

```
while True:
    source = input('> ')
    if source == 'quit':  break
    ast    = parse( source )
    result = lEval( ast, global_env )
    print( result )
```

This is only a skeleton.  Making it pleasant to use requires:

- **Multi-line input.** A single `input()` call drops the expression if the
  user hits Enter before closing a paren.  Count unmatched `(` and `)` as
  you read; keep prompting (with a continuation prompt like `  `) until
  the expression is balanced.

- **Error recovery.** A bad expression should print an error and return to
  the prompt -- not crash the REPL.  Wrap the eval in a `try/except` that
  catches `NameError`, `TypeError`, and your own interpreter exceptions.

- **History.** Python's `readline` module gives you arrow-key history for
  free on Linux/macOS.  Wire it in: `import readline` is enough.

- **Pretty-printing.** A flat Python `repr` is readable for small values
  but ugly for nested lists.  Write a `pprint` function that formats lists
  as `(a b c)`, indenting nested lists when they exceed a width limit.

Once the REPL is working you have a complete, self-contained Lisp
interpreter in roughly 80 lines.


## 2. Macros

*Requires: EVALUATOR1B (closures).*

A **macro** is a function that takes unevaluated AST and returns a new AST
that is then evaluated in its place.  It runs at expansion time, not
call time, so it can generate any code the evaluator understands.

Add a `defmacro` special form that stores the transformer function, and
an expansion step that runs before evaluation:

```python
macros = {}

def expand( expr ):
    if not isinstance( expr, list ) or len( expr ) == 0:
        return expr
    head = expr[0]
    if head in macros:
        transformer = macros[head]
        return expand( transformer( expr[1:] ) )   # expand recursively
    return [ expand( sub ) for sub in expr ]

def lEval( expr, env ):
    expr = expand( expr )
    ...                        # existing evaluator unchanged
```

With `defmacro` you can add `when`, `unless`, `and`, `or`, `cond`, and
`let*` entirely in Lisp, without touching the Python evaluator:

```lisp
(defmacro when (condition . body)
  (list 'if condition (cons 'progn body) '()))

(defmacro let* (bindings . body)
  (if (null? bindings)
      (cons 'progn body)
      (list 'let (list (car bindings))
            (list* 'let* (cdr bindings) body))))
```

The macro system is the point where the language becomes self-extending.
Nearly every high-level construct in a real Lisp -- `do`, `case`,
`defstruct`, `with-open-file` -- is a macro.

**Harder extension**: implement `quasiquote` (`` ` ``) and `unquote`
(`,`) in the parser (see the backquote challenge in PARSER-DOC), then
rewrite the macros above using template syntax instead of `list`/`cons`
calls.  The difference in readability is striking.


## 3. Tail Calls in the Macro Expander

*Requires: Project 2 (Macros) + EVALUATOR2 (TCO).*

Once you have both macros and the looping evaluator, there is a subtle
interaction: the expander must run before each loop iteration, not just
once at the top level.  Move the `expand` call inside the `while True:`
loop:

```python
def lEval( expr, env ):
    while True:
        expr = expand( expr )        # expand at every iteration
        ...
```

Without this, a macro that expands to a tail call will not get TCO
because the expansion happens before the loop and the tail call appears
as a raw function application the first time through.

Once the placement is correct, write a macro that wraps a tail-recursive
loop and verify that it runs a million iterations without stack overflow.


## 4. A Self-Hosting Evaluator

*Requires: Project 2 (Macros) + a reasonably complete primitive set.*

Implement `lEval` *in the Lisp you have built*:

```lisp
(defun leval (expr env)
  (cond
    ((symbol? expr)  (lookup expr env))
    ((not (list? expr))  expr)
    ((= (car expr) 'quote)  (cadr expr))
    ((= (car expr) 'if)
     (if (leval (cadr expr) env)
         (leval (caddr expr) env)
         (leval (cadddr expr) env)))
    (t
     (let ((fn   (leval (car expr) env))
           (args (map (lambda (a) (leval a env)) (cdr expr))))
       (apply fn args)))))
```

This is the **metacircular evaluator** -- a Lisp evaluator written in
Lisp.  It is not a curiosity: it is the canonical demonstration that
the language is powerful enough to describe itself.

Getting it to work requires:
- A complete environment representation in Lisp (association lists or
  a Lisp-level hash table)
- Lisp-level `apply` that can call both primitive and user-defined
  functions
- Bootstrapping: the meta-evaluator runs on top of your Python
  interpreter, which provides the primitive operations

Once it works, you can add features to the meta-evaluator (new special
forms, a different scoping rule) without touching any Python at all.


## 5. A Bytecode Compiler

*Requires: EVALUATOR2 (TCO) + a complete evaluator.*

A tree-walk interpreter traverses the AST on every evaluation.  A
**bytecode compiler** converts the AST once to a flat sequence of
simple instructions, then a small loop (the virtual machine) executes
them.  The VM loop is faster because it avoids the overhead of recursive
function calls and `isinstance` checks on every step.

Design a minimal instruction set:

| Instruction | Effect |
|---|---|
| `LOAD_CONST val` | push a literal value |
| `LOAD_VAR name` | look up `name` in env, push result |
| `STORE_VAR name` | pop top of stack, bind to `name` in env |
| `CALL n` | pop `n` args and the function, push result |
| `JUMP_IF_FALSE offset` | pop condition; if false, skip `offset` instructions |
| `JUMP offset` | unconditional jump |
| `RETURN` | pop and return the top of stack |

A compiler pass walks the AST and emits instructions:

```python
def compile_expr( expr, code ):
    if isinstance( expr, int ):
        code.append( ('LOAD_CONST', expr) )
    elif isinstance( expr, str ):
        code.append( ('LOAD_VAR', expr) )
    elif expr[0] == 'if':
        compile_expr( expr[1], code )          # condition
        j = len( code )
        code.append( ('JUMP_IF_FALSE', None) ) # placeholder
        compile_expr( expr[2], code )          # then
        code[j] = ('JUMP_IF_FALSE', len(code))
        ...
```

The VM is a `while` loop over the instruction list with a program counter
and an operand stack.  It is simpler than the tree-walk evaluator --
each instruction does exactly one thing.

**Further work**: implement a simple peephole optimizer that eliminates
redundant `LOAD`/`STORE` pairs, or add a `TAIL_CALL` instruction that
replaces the current frame instead of pushing a new one.


## 6. Continuations and `call/cc`

*Requires: EVALUATOR3-DOC (the CEK machine).*

A **continuation** is the rest of the computation from some point forward.
In a CEK machine the continuation is already reified as the `K` stack --
`call/cc` just captures it.

```lisp
; escape from a deep computation
(call/cc (lambda (k)
  (search-tree tree (lambda (node)
    (if (found? node)
        (k node)     ; jump out immediately with the answer
        nil)))))
```

The implementation:
- `call/cc` calls its argument with a special `Continuation` object
- Invoking the `Continuation` replaces the current `K` with the saved one
  and injects the argument as the current value -- an immediate long jump

A subtlety: the continuation captured by `call/cc` is an **escape**
continuation only if you never invoke it after the call/cc expression
has returned.  Full, re-invocable continuations (which can re-enter
completed computations) require that the entire continuation stack be
*copied*, not just referenced.  The copying version enables coroutines,
generators, and `amb`.

Start with escape continuations (simpler, useful immediately), then
consider what copying would require.


## 7. `dynamic-wind`

*Requires: Project 6 (call/cc).*

`dynamic-wind` is the control mechanism that ensures cleanup actions run
even when a continuation escape jumps past them -- the equivalent of
`try/finally` but interacting correctly with `call/cc`.

```lisp
(dynamic-wind
  (lambda () (open-resource))    ; before thunk -- runs on entry
  (lambda () (use-resource))     ; body thunk
  (lambda () (close-resource)))  ; after thunk -- runs on exit, always
```

The tricky part: if a continuation captured inside the body thunk is
invoked from outside, the *before* thunk must run again on re-entry and
the *after* thunk must run on re-exit.  This requires the VM to maintain
a "wind stack" that is consulted every time a continuation is invoked
or escapes.

`dynamic-wind` is the foundation for `with-exception-handler`,
`parameterize`, and composable resource management.  Implementing it
correctly is one of the harder exercises in the series -- but the design
insight (continuations need to know about dynamic extent) applies to
every serious language implementation.

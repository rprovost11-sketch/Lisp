# Modules Quick Reference

| Expression | Meaning |
|---|---|
| `(make-module 'name)` | Create empty module bound as NAME |
| `(load-module "f.lisp")` | Load file into module; name from filename stem |
| `(load-module "f.lisp" :name 'mod)` | Load into module named MOD |
| `(load-module "f.lisp" :name 'pkg:mod)` | Load into PKG:MOD, creating PKG |
| `mod:sym` | Read symbol SYM from module MOD |
| `(: mod sym)` | Same — long form |
| `(setf mod:sym val)` | Set SYM in MOD to VAL |
| `(mod:fn arg...)` | Call function FN in module MOD |
| `(load-extension "f.py" :name 'mod)` | Register Python primitives into MOD |
| `(modulep x)` | T if X is a module |
| `(module-name mod)` | Name of module as a string |
| `(module-symbols mod)` | Sorted list of symbols in module |

## Notes

- Multi-segment paths `pkg:sub:sym` navigate nested modules.
- `load-module` with `:name 'pkg:mod` creates intermediate packages automatically.
- `mod:sym` in `quote` — `'mod:sym` expands to `(: MOD SYM)`, not a symbol.

See `(help "modules-doc")` for full documentation and examples.

; ============================================================
; test25-tco.lisp
; Tail Call Optimization (TCO) tests for the CEK machine evaluator.
; These tests use large iteration counts (100000+) that would
; overflow Python's default recursion limit (~1000) without TCO.
; All function names are prefixed with tco- to avoid polluting
; the global namespace for other test files.
; ============================================================

; --- 1. Simple tail-recursive count-down ---

>>> ;;; define tco-count-down: simple tail-recursive countdown
... (defun tco-count-down (n)
...    (if (= n 0)
...       0
...       (tco-count-down (- n 1))))
...

==> (FUNCTION TCO-COUNT-DOWN (N) ... )

>>> ;;; count down from 100000 -- would crash without TCO
... (tco-count-down 100000)
...

==> 0

>>> ;;; count down from 0 returns immediately
... (tco-count-down 0)
...

==> 0

>>> ;;; count down from small value
... (tco-count-down 5)
...

==> 0

; --- 2. Tail-recursive accumulator factorial ---

>>> ;;; define tco-fact: tail-recursive factorial with accumulator
... (defun tco-fact (n acc)
...    (if (= n 0)
...       acc
...       (tco-fact (- n 1) (* n acc))))
...

==> (FUNCTION TCO-FACT (N ACC) ... )

>>> ;;; factorial of 10 with accumulator
... (tco-fact 10 1)
...

==> 3628800

>>> ;;; factorial of 0 returns accumulator
... (tco-fact 0 1)
...

==> 1

>>> ;;; factorial of 1
... (tco-fact 1 1)
...

==> 1

>>> ;;; factorial of 5
... (tco-fact 5 1)
...

==> 120

; --- 3. Mutual tail recursion (is-even / is-odd) ---

>>> ;;; define tco-even?: mutually tail-recursive even predicate
... (defun tco-even? (n)
...    (if (= n 0) t (tco-odd? (- n 1))))
...

==> (FUNCTION TCO-EVEN? (N) ... )

>>> ;;; define tco-odd?: mutually tail-recursive odd predicate
... (defun tco-odd? (n)
...    (if (= n 0) nil (tco-even? (- n 1))))
...

==> (FUNCTION TCO-ODD? (N) ... )

>>> ;;; 10000 is even -- would crash without TCO (mutual recursion)
... (tco-even? 10000)
...

==> T

>>> ;;; 10001 is odd -- mutual TCO
... (tco-odd? 10001)
...

==> T

>>> ;;; 1 is not even
... (tco-even? 1)
...

==> NIL

>>> ;;; 0 is even (base case)
... (tco-even? 0)
...

==> T

>>> ;;; 1 is odd (base case)
... (tco-odd? 1)
...

==> T

>>> ;;; 0 is not odd (base case)
... (tco-odd? 0)
...

==> NIL

; --- 4. TCO in let body (tail call as last expr of let) ---

>>> ;;; define tco-let-loop: tail call inside let body
... (defun tco-let-loop (n)
...    (let ((m (- n 1)))
...       (if (= m 0)
...          0
...          (tco-let-loop m))))
...

==> (FUNCTION TCO-LET-LOOP (N) ... )

>>> ;;; 100000 iterations through let -- would crash without TCO
... (tco-let-loop 100000)
...

==> 0

>>> ;;; let body tail call with n=1 (base: m=0)
... (tco-let-loop 1)
...

==> 0

; --- 5. TCO in let* body ---

>>> ;;; define tco-letstar-loop: tail call inside let* body
... (defun tco-letstar-loop (n)
...    (let* ((a n) (m (- a 1)))
...       (if (= m 0)
...          0
...          (tco-letstar-loop m))))
...

==> (FUNCTION TCO-LETSTAR-LOOP (N) ... )

>>> ;;; 100000 iterations through let* -- would crash without TCO
... (tco-letstar-loop 100000)
...

==> 0

>>> ;;; let* body tail call with n=1
... (tco-letstar-loop 1)
...

==> 0

; --- 6. TCO in cond body ---

>>> ;;; define tco-cond-loop: tail call in cond clause
... (defun tco-cond-loop (n)
...    (cond ((= n 0) 0)
...          (t (tco-cond-loop (- n 1)))))
...

==> (FUNCTION TCO-COND-LOOP (N) ... )

>>> ;;; 100000 iterations through cond -- would crash without TCO
... (tco-cond-loop 100000)
...

==> 0

>>> ;;; cond tail call with n=0 (base case)
... (tco-cond-loop 0)
...

==> 0

; --- 7. TCO in case body ---

>>> ;;; define tco-case-loop: tail call in case clause
... (defun tco-case-loop (n)
...    (case (= n 0)
...       (t 0)
...       (nil (tco-case-loop (- n 1)))))
...

==> (FUNCTION TCO-CASE-LOOP (N) ... )

>>> ;;; 100000 iterations through case -- would crash without TCO
... (tco-case-loop 100000)
...

==> 0

>>> ;;; case tail call with n=0 (base case)
... (tco-case-loop 0)
...

==> 0

; --- 8. TCO through progn (last expr of progn is a tail call) ---

>>> ;;; define tco-progn-loop: tail call as last expr in progn
... (defun tco-progn-loop (n)
...    (if (= n 0)
...       0
...       (progn
...          n
...          (tco-progn-loop (- n 1)))))
...

==> (FUNCTION TCO-PROGN-LOOP (N) ... )

>>> ;;; 100000 iterations through progn -- would crash without TCO
... (tco-progn-loop 100000)
...

==> 0

>>> ;;; progn tail call with n=0 (base case)
... (tco-progn-loop 0)
...

==> 0

; --- 9. TCO through funcall ---

>>> ;;; define tco-funcall-loop: tail call via funcall
... (defun tco-funcall-loop (n)
...    (if (= n 0)
...       0
...       (funcall 'tco-funcall-loop (- n 1))))
...

==> (FUNCTION TCO-FUNCALL-LOOP (N) ... )

>>> ;;; 100000 iterations via funcall -- would crash without TCO
... (tco-funcall-loop 100000)
...

==> 0

>>> ;;; funcall tail call with n=0 (base case)
... (tco-funcall-loop 0)
...

==> 0

; --- 10. TCO through apply ---

>>> ;;; define tco-apply-loop: tail call via apply
... (defun tco-apply-loop (n)
...    (if (= n 0)
...       0
...       (apply 'tco-apply-loop (list (- n 1)))))
...

==> (FUNCTION TCO-APPLY-LOOP (N) ... )

>>> ;;; 100000 iterations via apply -- would crash without TCO
... (tco-apply-loop 100000)
...

==> 0

>>> ;;; apply tail call with n=0 (base case)
... (tco-apply-loop 0)
...

==> 0

; --- 11. Non-tail recursion still works (small depth) ---

>>> ;;; define tco-non-tail: non-tail recursive function (+ wraps the call)
... (defun tco-non-tail (n)
...    (if (= n 0)
...       0
...       (+ 1 (tco-non-tail (- n 1)))))
...

==> (FUNCTION TCO-NON-TAIL (N) ... )

>>> ;;; non-tail recursion with depth 100 -- should still work
... (tco-non-tail 100)
...

==> 100

>>> ;;; non-tail base case
... (tco-non-tail 0)
...

==> 0

>>> ;;; non-tail small value
... (tco-non-tail 10)
...

==> 10

; --- 12. TCO with accumulator sum ---

>>> ;;; define tco-sum: tail-recursive sum with accumulator
... (defun tco-sum (n acc)
...    (if (= n 0)
...       acc
...       (tco-sum (- n 1) (+ acc n))))
...

==> (FUNCTION TCO-SUM (N ACC) ... )

>>> ;;; sum of 1..1000 = 500500
... (tco-sum 1000 0)
...

==> 500500

>>> ;;; sum of 1..100000 = 5000050000 -- would crash without TCO
... (tco-sum 100000 0)
...

==> 5000050000

>>> ;;; sum of 1..10 = 55
... (tco-sum 10 0)
...

==> 55

>>> ;;; sum of 0 returns accumulator
... (tco-sum 0 42)
...

==> 42

#lang racket
(require redex)

;; This version of the language contains `failing backtrack', synchronous
;; communication, and a 'sync' primitive. It also uses a slightly
;; generalized choose, which cycles through it's choices unless one of
;; the choices is an error. The `normal' choose can be recovered by
;; simply using (err "Fail") as the last choice.
;;
;; Backtracking can fail if a process has already `commited',
;; decided it will not backtrack past a certain point. If a process asks
;; it to do so, it fails, and takes an alterative path specified in the
;; backtrack expression.
;;
;; Abstract syntax
(define-language choose-backtrack
  [e v (e e) (o1 e) (o2 e e) (seq e ...) (if e e e) (let x = e in e)
     (err s) (choose k e e ...) (commit k) (backtrack i k e)
     (send i e) (recv i) (sync i k e)] 
  ;; The following are a 'pure' subset that can be evaluated as normal
  ;; lambda-calculus expressions, without choose, backtrack, commit,
  ;; sync, etc.
  [e/p v e/p/v (err s)]
  [e/p/v (e/p e/p) (o1 e/p) (o2 e/p e/p) (seq e/p ... e) (if e/p e e) 
       (let x = e/p in e) ]
  [o1 add1 sub1 iszero car cdr]
  [o2 + - * / ^ eq? < cons]
  [b number true false unit]
  [s string]
  [v b x (lambda x e)]
  [E hole (E e) (v E) (o1 E) (o2 E e) (o2 v E) (if E e e)
     (seq v ... E e ... e) (let x = E in e) (send i E)]
  ;; k variables represent choice points. In a real implementation, they
  ;; might be timestamps. Each variable should be unique (at least in a
  ;; single processes scope), and map to one continuation in the process
  ;; local store.
  [k (variable-prefix k)]
  ;; i variables are process identifiers.
  [i (variable-prefix i)]
  ;; D is the process local store. It contains a process identifier, and
  ;; a mapping of k vars to continutation.
  [D (i ((k e) ...))]
  ;; P is a short-hand for a closeed process. A process is a local
  ;; mapping and an expression. I'm not sure if it's used any more
  [P (D e)]
  [x variable-not-otherwise-mentioned])

;; Application of primitive functions
;; Look at all those runtime errors that should be type errors. 
;;
;; Delta reduces the normal, uninteresting, scheme-like language
;; primitive functions. 
;;
;; It seems to me there's room for abstraction here, since each new
;; primitive involves a predicate check + error, then a normal
;; evaluation which lifts a scheme expression into our language.
(define-metafunction choose-backtrack
  [(delta (iszero 0)) true]
  [(delta (iszero b)) false (side-condition (number? (term b)))]
  [(delta (iszero v)) (err "iszero applied to a value that's not a number")]
  [(delta (add1 b)) ,(add1 (term b)) (side-condition (number? (term b)))]
  [(delta (add1 v)) (err "add1 applied to a value that's not a number")]
  [(delta (sub1 0)) (err "sub1 0 is undefined")]
  [(delta (sub1 b)) ,(sub1 (term b)) (side-condition (number? (term b)))]
  [(delta (sub1 v)) (err "sub1 applied to a value that's not a number")]
  [(delta (+ b_1 b_2)) ,(+ (term b_1) (term b_2)) 
          (side-condition (and (number? (term b_1)) (number? (term b_2))))]
  [(delta (+ v_1 v_2)) (err "+ applied to non-numbers")]
  [(delta (- b_1 b_2)) ,(- (term b_1) (term b_2))
          (side-condition (and (number? (term b_1)) (number? (term b_2)) 
                               (>= (term b_1) (term b_2))))]
  [(delta (- v_1 v_2)) 
   (err "- applied to non-numbers or first number is not greater")]
  [(delta (* b_1 b_2)) ,(* (term b_1) (term b_2))
          (side-condition (and (number? (term b_1)) (number? (term b_2))))]
  [(delta (* v_1 v_2)) (err "* applied to non-numbers")]
  [(delta (/ b_1 0)) (err "division by zero")]
  [(delta (/ b_1 b_2)) ,(/ (term b_1) (term b_2))
          (side-condition (and (number? (term b_1)) (number? (term b_2))))]
  [(delta (/ v_1 v_2)) (err "/ applied to non-numbers")]
  [(delta (^ b_1 b_2)) ,(expt (term b_1) (term b_2))
          (side-condition (and (number? (term b_1)) (number? (term b_2))))]
  [(delta (^ v_1 v_2)) (err "^ applied to non-numbers")]
  [(delta (eq? b_1 b_2)) ,(if (eq? (term b_1) (term b_2)) (term true)
                            (term false))
          (side-condition (and (number? (term b_1)) (number? (term b_2))))]
  [(delta (eq? v_1 v_2)) (err "eq? applied to non-numbers")]
  [(delta (< b_1 b_2)) ,(if (< (term b_1) (term b_2)) (term true) 
                          (term false))
          (side-condition (and (number? (term b_1)) (number? (term b_2))))]
  [(delta (< v_1 v_2)) (err "< applied to non-numbers")]
  [(delta (cons v_1 v_2)) ,(cons (term v_1) (term v_2))]
  [(delta (car b)) ,(car (term v)) (side-condition (pair? (term v)))]
  [(delta (car v)) (err "car applies to non-pair")]
  [(delta (cdr b)) ,(cdr (term v)) (side-condition (pair? (term v)))]
  [(delta (cdr v)) (err "cdr applies to non-pair")])

;; Substitution
(define-metafunction choose-backtrack
  [(subst (lambda x_1 any_1) x_1 any_2) (lamdda x_1 any_1)]
  [(subst (lambda x_1 any_1) x_2 any_2)
   (lambda x_3 (subst (subst-var any_1 x_1 x_3) x_2 any_2))
   (where x_3 ,(variable-not-in (term (x_2 any_1 any_2)) (term x_1)))]
  [(subst (let x_1 = any_1 in any_3) x_1 any_2) 
   (let x_1 = (subst any_1 x_1 any_2) in any_3)]
  [(subst (let x_1 = any_1 in any_3) x_2 any_2)
   (let x_3 = (subst any_1 x_2 any_2) in
     (subst (subst-var any_3 x_1 x_3) x_2 any_2))
   (where x_3 ,(variable-not-in (term (x_2 any_3 any_2))
                                (term x_1)))]
  [(subst x_1 x_1 any_1) any_1]
  [(subst (any_2 ...) x_1 any_1) ((subst any_2 x_1 any_1) ...)]
  [(subst any_2 x_1 any_1) any_2])

(define-metafunction choose-backtrack
  [(subst-var (any_1 ...) x_1 x_2) ((subst-var any_1 x_1 x_2) ...)]
  [(subst-var x_1 x_1 x_2) x_2]
  [(subst-var any_1 x_1 x_2) any_1])

;; Single-step reduction for the pure lambda-calculus subset.
;; 
;; This simply reduces all the primitives of the uninteresting
;; scheme-like langauge.
(define-metafunction choose-backtrack
  [(local-pure-red (in-hole E (err s))) (err s)
   (side-condition (not (equal? (term hole) (term E))))]
  [(local-pure-red (in-hole E ((lambda x e) v)))
   (in-hole E (subst e x v))]
  [(local-pure-red (in-hole E (o1 b))) 
   (in-hole E (delta (o1 b)))]
  [(local-pure-red (in-hole E (o2 b_1 b_2))) 
   (in-hole E (delta (o2 b_1 b_2)))]
  [(local-pure-red (in-hole E (seq v ... e))) 
   (in-hole E e)]
  [(local-pure-red (in-hole E (if true e_1 e_2))) 
   (in-hole E e_1)]
  [(local-pure-red (in-hole E (if false e_1 e_2))) 
   (in-hole E e_2)]
  [(local-pure-red (in-hole E (let x = v in e))) 
   (in-hole E (subst e x v))])

;; Base reduction; nothing more than parallel lambda-calculus.
;;
;; This create a base reduction that simply applies the local-pure-red to
;; each 'pure' expressions (expressions without
;; choose/communication/sync). Each new reduction should extend this
;; one.
(define choose-red-base
  (reduction-relation
   choose-backtrack
   (--> (par P_0 ... (D (name exp (in-hole E e/p/v))) P_1 ...)
        (par P_0 ... (D (local-pure-red exp)) P_1 ...))))

;; This macro allows extending a relation with rules that touch only
;; their own stores, but don't interact with other processes.
(define-syntax local-extend-reduction
  (syntax-rules (-->)
    [(_ relation (--> e1 e2 etc ...) ...)
     (extend-reduction-relation 
       relation choose-backtrack
       (--> (par P_0 (... ...) e1 P_1 (... ...))
            (par P_0 (... ...) e2 P_1 (... ...))
            etc ...) ...)]))

;; This reduction extends the base reduction with process local
;; reductions for the 'non-pure' forms, like choose, commit, and
;; backtrack. These reductions might touch their own stores, but will
;; not interact with other processes.
(define choose-red-local
  (local-extend-reduction
    choose-red-base
    ;; local choose -- unique
    ;;
    ;; Choose can be reduced locally; it simply added a new k-var/
    ;; continuation pair to the store. If the k-var already exists in
    ;; the store, it must be removed first. 
    (--> ((i ((k e) ... (k_0 e_2) (k_1 e_3) ...)) 
          (name exp (in-hole E (choose k_0 e_0 e_1 ...))))
         ((i ((k e) ... (k_1 e_3) ...)) exp))
    ;; local choose -- other
    ;;
    ;; Choose puts the first expression into the current context, and
    ;; puts a slightly modified version of the current context and it's
    ;; filled-hole into the store. The hole is filled with a choose
    ;; expression whose choices have been rearranged to allow choose to
    ;; cycle through choices.
    (--> ((i ((k e) ...)) (in-hole E (choose k_0 e_0 e_1 ...)))
         ((i ((k e) ... (k_0 (in-hole E (choose k_0 e_1 ... e_0))))) 
          (in-hole E e_0))
         (side-condition
           (not (member (term k_0) (term (k ...))))))
    ;; local commit
    ;;
    ;; Commit simply removes all continuations from the store prior to a
    ;; named point.
    (--> ((i ((k_0 e_0) ... (k_1 e_1) (k_2 e_2) ...)) 
          (in-hole E (commit k_1)))
         ((i ((k_2 e_2) ...)) (in-hole E unit)))
    ;; local backtrack
    ;;
    ;; Local backtracking simply aborts the current continuation, and
    ;; uses the named one.
    (--> ((i (name K ((k_0 e) ... (k_1 e_1) (k_2 e_2) ...))) 
          (in-hole E (backtrack i k_1 e_3)))
         ((i K) e_1))
    (--> ((name S (i ((k_0 e_0) ...))) (in-hole E (backtrack i k_1 e_1)))
         (S (in-hole E e_1))
         (side-condition
           (not (member (term k_1) (term (k_0 ...))))))))

;; This macro is used to extend the parallel relation
;; symmetrically--allowing the rule to match no matter the process
;; order. It's useful when, for example, you want a process to be able
;; to send and receive no matter their syntactic ordering.
(define-syntax symmetric-extend-relation
  (syntax-rules (--> par)
    [(_ relation (--> (par e1 e2) (par e3 e4) etc ...) ...)
     (extend-reduction-relation 
       relation choose-backtrack
       (--> (par P_0 (... ...) e1 P_1 (... ...) e2 P_2 (... ...))
            (par P_0 (... ...) e3 P_1 (... ...) e4 P_2 (... ...))
            etc ...) ...
       (--> (par P_0 (... ...) e2 P_1 (... ...) e1 P_2 (... ...))
            (par P_0 (... ...) e4 P_1 (... ...) e3 P_2 (... ...))
            etc ...) ...)]))

;; This reduction extends the local 'non-pure' reduction with parallel
;; reductions that interact with other processes and stores.
(define choose-red-parallel
  (symmetric-extend-relation
    choose-red-local
    ;; Parallel backtrack
    ;;
    ;; A version of backtrack which works across threads. This allows
    ;; one thread to force another to backtrack.
    (--> (par ((name S0 (i_0 ((k_0 e_0) ...)))
               (in-hole E_0 (backtrack i_1 k_1 e)))
              ((name S1 (i_1 ((k_2 e_2) ... (k_1 e_1) (k_3 e_3) ...)))
               e_5))
         (par (S0 (in-hole E_0 unit)) (S1 e_1)))
    ;; If there is no such k-var, then the backtrack fails, and the
    ;; alternate path must be taken.
    ;;
    ;; XXX: This implementation is prone to a race condition (see
    ;; test e18 and e19).
    (--> (par ((name S0 (i_0 ((k_0 e_0) ...)))
               (in-hole E_0 (backtrack i_1 k_1 e)))
              (name P1 ((i_1 ((k_3 e_3) ...)) e_5)))
         (par (S0 (in-hole E_0 e)) P1)
         (side-condition
           (not (member (term k_1) (term (k_3 ...))))))
    ;; Sync
    ;;
    ;; Sync acts as a boundery that, when crossed via backtracking,
    ;; executes an expression. Sync is synchronous, and must be paired
    ;; between two processes.
    (--> (par ((i_0 ((k_0 e_0) ... (k_2 e_4) (k_4 e_6) ...)) 
               (in-hole E_0 (sync i_1 k_2 e_2)))
              ((i_1 ((k_1 e_1) ... (k_3 e_5) (k_5 e_7) ...)) 
               (in-hole E_1 (sync i_0 k_3 e_3))))
         (par ((i_0 ((k_0 e_0) ... (k_2 (seq e_2 e_4)) (k_4 e_6) ...)) 
               (in-hole E_0 unit))
              ((i_1 ((k_1 e_1) ... (k_3 (seq e_3 e_5)) (k_5 e_7) ...)) 
               (in-hole E_1 unit)) ))
    ;; Send/recv
    ;;
    ;; Primitive, synchronous communication. You can send values.
    (--> (par ((name S0 (i_0 ((k_0 e_0) ...))) (in-hole E_0 (send i_1 v)))
              ((name S1 (i_1 ((k_1 e_1) ...))) (in-hole E_1 (recv i_0))))
         (par (S0 (in-hole E_0 unit)) (S1 (in-hole E_1 v))))))

;; This macro allows writing term expressions for the parallel languages
;; a little more easily. If you give untagged expression, then it gives a
;; default process IDs and empty store. Don't use this form if you need
;; communication.
;;
;; You can also give another form to specify the process id's manually.
;; Use this form if you want communication. 
;;
;; See below for examples.
(define-syntax par-term
  (syntax-rules (id)
    [(_ (id i exp) ...) (term (par ((i ()) exp) ...))]
    [(_ exp ...) (term (par ((i ()) exp) ...))] ))

;; A few small examples
(define e0 (par-term 3))
(define e1 (par-term true))
(define e2 (par-term false))
(define e3 (par-term x))
(define e4 (par-term (lambda x (add1 x))))
(define e5 (par-term ((lambda x x) ((lambda y y) 3))))
(define e6 (par-term (add1 (sub1 (add1 (sub1 10))))))
(define e7 (par-term (if (iszero (sub1 1)) 5 6)))
(define e8 (par-term (if (iszero (add1 1)) 5 6)))
(define e9 (par-term (seq 1 2 (add1 3))))
(define e10 (par-term (let x = 6 in (let y = 2 in (+ x y)))))
(define e11 (par-term (let x = 6 in (let y = 2 in (- x y)))))
(define e12 (par-term (let x = 6 in (let y = 2 in (* x y)))))
(define e13 (par-term (let x = 6 in (let y = 2 in (^ x y)))))
(define e14 (par-term (let x = 6 in (let y = 2 in (/ x y)))))
(define e15 (par-term (let x = 6 in (let y = 0 in (add1 (/ x y))))))
(define e16 (par-term (add1 (choose k 1))))
(define e17 (par-term (seq (choose k 1) (commit k))))
(define e18 
  (par-term 
    (id i_1 (add1 (choose k 1 (err "Fail"))))
    (id i_2 (add1 (seq (backtrack i_1 k (err "Error")) 2)))))
(define e19 
  (par-term 
    (id i_1 (add1 (seq (backtrack i_2 k (err "Error")) 2)))
    (id i_2 (add1 (seq (choose k 1 (err "Fail")))))))
(define e20 
  (par-term
    (id i_1 (add1 (seq (choose k 1))))
    (id i_2 (seq (commit k) (add1 2)))))
(define e21
  (par-term (seq (seq 1 1) (seq 1 2) (seq 1 3))))
(define e22
  (par-term (seq ((lambda x x) 2) (seq 1 2) 3)))
(define e23
  (par-term
    (id i_1 (let x = (choose k 1 0) in
      (if (iszero x)
        (err "Success")
        (backtrack i_1 k (err "Error")))))))

;; This example uses choice and backtracking to find, in a distributed
;; way, an ordering on numbers. Each process has a list of numbers, and
;; the goal is to find an ordering such that P1 < P2 < P3. Each process
;; picks a number from it's list, and sends it to the previous process
;; in the chain. If they're not in order, the process first attempt to
;; backtrack locally, and if it cannot satisfy the constraint, asks the
;; sender to backtrack. 
(define e24
  (par-term 
    (id i_1
        (seq (choose k unit (backtrack i_2 k_2 (err "Error")) (err "Failure!"))
             (let x = (choose k_1 1 (seq (backtrack i_2 k_3 (err "Error")) 4) 
                                  (backtrack i_1 k (err "Error"))) in 
               (let y = (recv i_2) in
                 (let z = (recv i_3) in
                   (if (< x y)
                     (if (< y z)
                       (seq (commit k_1)
                            (send i_2 1)
                            (send i_3 1)
                            (err "Success"))
                       (backtrack i_1 k_1 (err "Error")))
                     (backtrack i_1 k_1 (err "Error"))))))))
    (id i_2
        (seq (choose k_2 unit (backtrack i_3 k_4 (err "Error")) (err "Failure!"))
             (let x = (choose k_1 2 5 (backtrack i_2 k_2 (err "Error"))) in 
               (seq (choose k_3 (send i_1 x) (send i_1 x))
               (recv i_1)
               (commit k_3)))))
    (id i_3
        (let x = (choose k_4 7 1 (err "Failure!")) in 
          (seq (choose k_5 (send i_1 x) (send i_1 x))
          (recv i_1)
          (commit k_5))))))

;; This is an implementation of the above algorithm in a more uniform
;; way. It requires all the local lists of numbers to be sorted for
;; greatest to least. 
;;
;; XXX: We'd like it to not have this restriction.
(define e25
  (par-term
    (id i_0
        (let x = (recv i_1) in
          (let y = (recv i_2) in 
            (let z = (recv i_3) in
              (if (< x y)
                (if (< y z)
                  (err "Success")
                  (err "Fail"))
                (err "Fail"))))))
    (id i_1
        (seq (choose k_0 unit (backtrack i_2 k_1 (err "Error")) (err "Fail"))
             (let x = (recv i_2) in
               (let y = (choose k_1 5 3 (backtrack i_1 k_0 (err "Error")) (err "Fail")) in 
                 (if (< y x)
                   (seq (send i_0 y)
                        (commit k_1))
                   (backtrack i_1 k_1 (err "Error")))))))
    (id i_2
        (seq (choose k_0 unit (backtrack i_3 k_1 (err "Error")) (err "Fail"))
             (let x = (recv i_3) in
               (let y = (choose k_1 6 2 (backtrack i_2 k_0 (err "Error")) (err "Fail")) in
                 (if (< y x)
                   (seq (send i_1 y)
                        (send i_0 y)
                        (commit k_1))
                   (backtrack i_2 k_1 (err "Error")))))))
    (id i_3
       (let y = (choose k_1 7 2 (err "Fail")) in
         (seq (send i_2 y)
              (send i_0 y)
              (commit k_1))))
    ))

;; This is a contrived test to test sync and commit
(define e26 
  (par-term
    (id i_0 (let x = (add1 (choose k 2 3)) in
              (seq (sync i_1 k (backtrack i_1 k (err "Error")))
                   (send i_1 x)
                   (if (iszero (recv i_1))
                     (seq (send i_1 1)
                          (commit k)
                          (err "Success"))
                     (backtrack i_0 k (err "Error"))))))
    (id i_1 (let x = (choose k 3 0) in
              (seq (sync i_0 k unit)
                   (recv i_0)
                   (send i_0 x)
                   (recv i_0)
                   (commit k)
                   (err "Success"))))))
;; This is a smaller contrive test for commit
(define e27
  (par-term 
    (id i_0 (let x = (choose k 1 2) in
              (seq (choose k_1 2 3)
                   (choose k_2 3 5)
                   (commit k_2)
                   (backtrack i_0 k (err "Success")))))))


;; Automatically check all the test cases still work. All test cases
;; should normalize, or reduce may not terminate.
;; 
;; XXX: I think there is actually some built-in redex tester. But I
;; didn't know that when I wrote all the testing stuff.
(require srfi/78)

;; This tries to normalize an expression. It may not terminate if there
;; are cycles.
(define (reduce e)
  (apply-reduction-relation* choose-red-parallel e #:cache-all? #t))

(check (reduce e0) => (list e0))
(check (reduce e1) => (list e1))
(check (reduce e2) => (list e2))
(check (reduce e3) => (list e3))
(check (reduce e4) => (list e4))
(check (reduce e5) => (list (par-term 3)))
(check (reduce e6) => (list (par-term 10)))
(check (reduce e7) => (list (par-term 5)))
(check (reduce e8) => (list (par-term 6)))
(check (reduce e10) => (list (par-term 8)))
(check (reduce e11) => (list (par-term 4)))
(check (reduce e12) => (list (par-term 12)))
(check (reduce e13) => (list (par-term 36)))
(check (reduce e14) => (list (par-term 3)))
(check (reduce e15) => (list (par-term (err "division by zero"))))
(check (reduce e16) => 
       (list (term (par ((i ((k (add1 (choose k 1))))) 2)))))
(check (reduce e17) => (list (par-term unit)))

;; XXX: e18 and e19 fail due to a race condition that exists due to how
;; failing backtracking is implemented. They have two normal forms, one
;; of which is not the one we want, and it's the one `reduce' finds
;; first. If we add a sync point before the backtrack, we can eliminate
;; the race condition. But, that is an unsatisfactory solution.
;;
;; I'm not sure what a better solution would be. Probably some way to
;; checking that the k in question had been introduced, but has now been
;; remove by a commit.
#;(check (reduce e18) => (list (term 
                               (par 
                                 ((i_1 
                                   ((k (add1 (choose k 1 (err "Fail"))))))
                                  (err "Fail") ) 
                                 ((i_2 ()) 3)))))
#;(check (reduce e19) => (list (term 
                               (par 
                                 ((i_1 ()) 3)
                                 ((i_2 
                                   ((k (add1 (choose k 1 (err "Fail"))))))
                                  (err "Fail"))))))
;; Changed behavior of commit/collect, so this doesn't work this way
;; anymore.
#;(check (reduce e20) => (list (par-term (id i_1 2) (id i_2 3))))
(check (reduce e21) => (list (par-term 3)))
(check (reduce e22) => (list (par-term 3)))
(check (reduce e23) => (list (term 
                               (par 
                                 ((i_1 ((k (let x = (choose k 1 0) in
                                             (if (iszero x)
                                               (err "Success")
                                               (backtrack i_1 k (err "Error"))))))) 
                                  (err "Success"))))))
(check (reduce e24) => (list (par-term (id i_1 (err "Success"))
                                       (id i_2 unit)
                                       (id i_3 unit))))
(check (reduce e25) => (list (par-term (id i_0 (err "Success"))
                                       (id i_1 unit)
                                       (id i_2 unit)
                                       (id i_3 unit))))
(check (reduce e26) => (list (par-term (id i_0 (err "Success"))
                                       (id i_1 (err "Success")))))
(check (reduce e27) => (list (par-term (id i_0 (err "Success")))))

;; Here is how to view the evaluation of the example expressions.
#;(traces choose-red-parallel e24)

;; Here is how to try to normalize an expression without manually
;; stepping through the trace graph.
#;(reduce e27)

#lang racket
(require redex)

;; Abstract syntax
(define-language choose-backtrack
  [e v (e e) (o1 e) (o2 e e) (seq e ...) (if e e e) (let x = e in e)
     (err s) (choose k e ...) (collect k) (backtrack i k)
     (send i e) (recv i)] 
  ;; The following are a 'pure' subset that can be evaluated as normal
  ;; lambda-calculus expressions, without choose, backtrack, or collect.
  [e/p v e/p/v]
  [e/p/v (e/p e/p) (o1 e/p) (o2 e/p e/p) (seq e/p ... e) (if e/p e e) 
       (let x = e/p in e)]
  [o1 add1 sub1 iszero]
  [o2 + - * / ^ eq? <]
  [b number true false]
  [s string]
  [v b x (lambda x e) k unit]
  [E hole (E e) (v E) (o1 E) (o2 E e) (o2 v E) (if E e e)
     (seq v ... E e ... e) (let x = E in e) (send i E)]
  [k (variable-prefix k)]
  [i (variable-prefix i)]
  [D (i ((k e) ...))]
  [P (D e)]
  [x variable-not-otherwise-mentioned])

;; Application of primitive functions
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
  [(delta (< v_1 v_2)) (err "< applied to non-numbers")])

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
(define choose-red-base
  (reduction-relation
   choose-backtrack
   (--> (par P_0 ... (D (name exp (in-hole E e/p/v))) P_1 ...)
        (par P_0 ... (D (local-pure-red exp)) P_1 ...))))

;; Extend a relation with rules that touch only their own stores, but
;; don't interact with other processes.
(define-syntax local-extend-reduction
  (syntax-rules (-->)
    [(_ relation (--> e1 e2 etc ...) ...)
     (extend-reduction-relation 
       relation choose-backtrack
       (--> (par P_0 (... ...) e1 P_1 (... ...))
            (par P_0 (... ...) e2 P_1 (... ...))
            etc ...) ...)]))

;; Extend the parallel langauge with local reductions. These reductions
;; might touch their own stores, but not other processes.
(define choose-red-local
  (local-extend-reduction
    choose-red-base
    ;; local choose -- unique
    (--> ((i ((k e) ... (k_0 e_0))) (in-hole E (choose k_0 e ...)))
         ((i ((k e) ...)) (in-hole E (choose k_0 e ...))))
    ;; local choose -- final
    (--> ((i ((k e) ...)) (in-hole E (choose k_0 e_0)))
         ((i ((k e) ... (k_0 (in-hole E (err "No more choices"))))) 
          (in-hole E e_0)))
    ;; local choose -- multi
    (--> ((i ((k e) ...)) (in-hole E (choose k_0 e_0 e_1 ... e_2)))
         ((i ((k e) ... (k_0 (in-hole E (choose k_0 e_1 ... e_2))))) 
          (in-hole E e_0)))
    ;; local collect
    (--> ((i ((k_0 e_0) ... (k_1 e_1) (k_2 e_2) ...)) 
          (in-hole E (collect k_1)))
         ((i ((k_0 e_0) ... (k_2 e_2) ...)) (in-hole E unit)))
    ;; local backtrack
    (--> ((i (name K ((k_0 e) ... (k_1 e_1) (k_2 e_2) ...))) 
          (in-hole E (backtrack i k_1)))
         ((i K) e_1))))

;; Used to extend the parallel relation symmetrically--allowing the
;; rule to match no matter the process order
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

;; Extend the base with parallel reductions that interact with other
;; processes and stores.
(define choose-red-parallel
  (symmetric-extend-relation
    choose-red-local
    ;; Parallel collect
    (--> (par ((i_0 ((k_0 e_0) ... (k_1 e_1) (k_2 e_2) ...)) 
               (in-hole E_0 e))
              ((i_1 ((k_3 e_3) ...)) (in-hole E_1 (collect k_1))))
         (par ((i_0 ((k_0 e_0) ... (k_2 e_2) ...)) (in-hole E_0 e))
              ((i_1 ((k_3 e_3) ...)) (in-hole E_1 unit))))
    ;; Parallel backtrack
    (--> (par ((name S0 (i_0 ((k_0 e_0) ...)))
               (in-hole E_0 (backtrack i_1 k_1)))
              ((name S1 (i_1 ((k_2 e_2) ... (k_1 e_1) (k_3 e_3) ...)))
               e))
         (par (S0 (in-hole E_0 unit)) (S1 e_1)))
    ;; Send/recv
    (--> (par ((name S0 (i_0 ((k_0 e_0) ...))) (in-hole E_0 (send i_1 v)))
              ((name S1 (i_1 ((k_1 e_1) ...))) (in-hole E_1 (recv i_0))))
         (par (S0 (in-hole E_0 unit)) (S1 (in-hole E_1 v))))))

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
(define e16 (par-term (add1 (seq (choose k) 1))))
(define e17 (par-term (seq (choose k) 1 (collect k))))
(define e18 
  (par-term 
    (id i_1 (add1 (choose k)))
    (id i_2 (add1 (seq (backtrack i_1 k) 2)))))
(define e19 
  (par-term 
    (id i_1 (add1 (seq (backtrack i_2 k) 2)))
    (id i_2 (add1 (seq (choose k) 1)))))
(define e20 
  (par-term
    (id i_1 (add1 (seq (choose k) 1)))
    (id i_2 (seq (collect k) (add1 2)))))
(define e21
  (par-term (seq (seq 1 1) (seq 1 2) (seq 1 3))))
(define e22
  (par-term (seq ((lambda x x) 2) (seq 1 2) 3)))
(define e23
  (par-term
    (id i_1 (let x = (choose k 1 0) in
      (if (iszero x)
        (err "Sucess!")
        (backtrack i_1 k))))))
(define e24
  (par-term 
    (id i_1
        (seq (choose k unit (backtrack i_2 k_2) (err "Failure!"))
             (let x = (choose k_1 1 (seq (backtrack i_2 k_3) 4) 
                                  (backtrack i_1 k)) in 
               (let y = (recv i_2) in
                 (let z = (recv i_3) in
                   (if (< x y)
                     (if (< y z)
                       (err "Success!")
                       (backtrack i_1 k_1))
                     (backtrack i_1 k_1)))))))
    (id i_2
        (seq (choose k_2 unit (backtrack i_3 k_4) (err "Failure!"))
             (let x = (choose k_1 2 5 (backtrack i_2 k_2)) in 
               (choose k_3 (send i_1 x) (send i_1 x)))))
    (id i_3
        (let x = (choose k_4 7 1 (err "Failure!")) in 
          (choose k_5 (send i_1 x) (send i_1 x))) )))

;; Here is how to view the evaluation of the example expressions
(traces choose-red-parallel e24)
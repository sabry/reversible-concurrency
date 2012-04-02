#lang racket
(require redex)

;; Abstract syntax
(define-language choose-jump
  [e v (e e) (o1 e) (o2 e e) (seq e e) (if e e e) (let x = e in e)
          (err s) (choose k e e) (jump i k) (collect k) (jump k)] 
  ;; Expressions that can take a step without a parallel reduction. That
  ;; is, they are not values, and they do not contain external jump 
  ;; subexpressions or collect subexpressions in evaluation context.
  ;; They can contain local jumps though. This seems like a little code
  ;; duplication, but it seemed the cleanest way to eliminate the
  ;; non-determinism
  [e/p v e/p/v]
  [e/p/v (e/p e/p) (o1 e/p) (o2 e/p e/p) (seq e/p e) (if e/p e e) 
       (let x = e/p in e) (err s) (choose k e e) (jump k)]
  [o1 add1 sub1 iszero]
  [o2 + - * / ^]
  [b number true false]
  [s string]
  [v b x (lambda x e) k unit]
  [E hole (E e) (v E) (o1 E) (o2 E e) (o2 v E) (if E e e)
     (seq E e) (let x = E in e)]
  [k (variable-prefix k)]
  [i (variable-prefix i)]
  [D (i ((k e) ...))]
  [P (D e)]
  [x variable-not-otherwise-mentioned])

;; Application of primitive functions
(define-metafunction choose-jump
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
  [(delta (^ v_1 v_2)) (err "^ applied to non-numbers")])

;; Substitution
(define-metafunction choose-jump
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

(define-metafunction choose-jump
  [(subst-var (any_1 ...) x_1 x_2) ((subst-var any_1 x_1 x_2) ...)]
  [(subst-var x_1 x_1 x_2) x_2]
  [(subst-var any_1 x_1 x_2) any_1])

;; Per process Single-step reduction
(define-metafunction choose-jump
  [(process-step (D (in-hole E (err s)))) (err s)
   (side-condition (not (equal? (term hole) (term E))))]
  [(process-step (D (in-hole E ((lambda x e) v))))
   (D (in-hole E (subst e x v)))]
  [(process-step (D (in-hole E (o1 b)))) 
   (D (in-hole E (delta (o1 b))))]
  [(process-step (D (in-hole E (o2 b_1 b_2)))) 
   (D (in-hole E (delta (o2 b_1 b_2))))]
  [(process-step (D (in-hole E (seq v e)))) 
   (D (in-hole E e))]
  [(process-step (D (in-hole E (if true e_1 e_2)))) 
   (D (in-hole E e_1))]
  [(process-step (D (in-hole E (if false e_1 e_2)))) 
   (D (in-hole E e_2))]
  [(process-step (D (in-hole E (let x = v in e)))) 
   (D (in-hole E (subst e x v)))]
  [(process-step ((i ((k_0 e_0) ...)) (in-hole E (choose k e_1 e_2))))
   ((i ((k_0 e_0) ... (k (in-hole E e_2)))) (in-hole E e_1))]
  [(process-step ((i ((k_0 e_0) ... (k_1 e_1) (k_2 e_2) ...)) 
                  (in-hole E (jump k_1))))      
   ((i ((k_0 e_0) ... (k_1 e_1) (k_2 e_2) ...)) (in-hole E e_1))])

;; Single-step reduction for local reductions.
(define choose-red-base
  (reduction-relation
   choose-jump
   (--> (par P_0 ... (name t ((i ((k e_1) ...)) (in-hole E e/p/v))) P_1 ...)
        (par P_0 ... (process-step t) P_1 ...))
   ;; Turn jumps in our thread into the internal jump form, so
   ;; process-step will work correctly.
   (--> (par P_0 ... ((i ((k e) ...)) (in-hole E (jump i k_1))) P_1 ...)
        (par P_0 ... ((i ((k e) ...)) (in-hole E (jump k_1))) P_1 ...))
   (--> (par P_0 ... ((i ((k_0 e_0) ... (k_1 e_1) (k_2 e_2) ...)) 
                      (in-hole E (collect k_1))) P_1 ...)
        (par P_0 ... ((i ((k_0 e_0) ... (k_2 e_2) ...)) (in-hole E unit)) 
             P_1 ...))))

;; Used to extend the parallel relation symmetrically -- allowing the
;; rule to match no matter the process order
(define-syntax symmetric-extend-relation
  (syntax-rules (--> par)
    [(_ relation (--> (par e1 e2) (par e3 e4)))
     (extend-reduction-relation 
       relation choose-jump
       (--> (par P_0 (... ...) e1 P_1 (... ...) e2 P_2 (... ...))
            (par P_0 (... ...) e3 P_1 (... ...) e4 P_2 (... ...)))
       (--> (par P_0 (... ...) e2 P_1 (... ...) e1 P_2 (... ...))
            (par P_0 (... ...) e4 P_1 (... ...) e3 P_2 (... ...))))]))

;; Now we can extend the parallel reduction by just specifying two
;; processes we care about. Much better
(define choose-red-jump
  (symmetric-extend-relation
    choose-red-base
    (--> (par ((i_0 ((k_2 e_2) ... (k_1 e_1) (k_3 e_3) ...))
               (in-hole E_4 e))
              ((i_1 ((k_0 e_0) ...)) (in-hole E_5 (jump i_0 k_1))))
         (par ((i_0 ((k_2 e_2) ... (k_1 e_1) (k_3 e_3) ...)) e_1)
              ((i_1 ((k_0 e_0) ...)) (in-hole E_5 unit))))))

(define choose-red-jump-collect
  (symmetric-extend-relation
    choose-red-jump
    (--> (par ((i_0 ((k_0 e_0) ... (k_1 e_1) (k_2 e_2) ...)) 
               (in-hole E_0 e))
              ((i_1 ((k_3 e_3) ...)) (in-hole E_1 (collect k_1))))
         (par ((i_0 ((k_0 e_0) ... (k_2 e_2) ...)) (in-hole E_0 e))
              ((i_1 ((k_3 e_3) ...)) (in-hole E_1 unit))))))

(define-syntax sr-term
   (syntax-rules ()
     [(sr-term id exp) (term ((id ()) exp))]
     [(sr-term exp) (term ((i ()) exp))] ))

(define-syntax par-term
  (syntax-rules (id)
    [(_ (id i exp) ...) (term (par ,(sr-term i exp) ...))]
    [(_ exp ...) (term (par ,(sr-term i exp) ...))] ))

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
(define e9 (par-term (seq (seq 1 2) (add1 3))))
(define e10 (par-term (let x = 6 in (let y = 2 in (+ x y)))))
(define e11 (par-term (let x = 6 in (let y = 2 in (- x y)))))
(define e12 (par-term (let x = 6 in (let y = 2 in (* x y)))))
(define e13 (par-term (let x = 6 in (let y = 2 in (^ x y)))))
(define e14 (par-term (let x = 6 in (let y = 2 in (/ x y)))))
(define e15 (par-term (let x = 6 in (let y = 0 in (add1 (/ x y))))))
(define e16 (par-term (add1 (choose k 1 2))))
(define e17 (par-term (seq (choose k 1 2) (collect k))))
(define e18 
  (par-term 
    (id i_1 (add1 (choose k 1 2)))
    (id i_2 (add1 (seq (jump i_1 k) 2)))))
(define e19 
  (par-term 
    (id i_1 (add1 (seq (jump i_2 k) 2)))
    (id i_2 (add1 (choose k 1 2)))))
(define e20 
  (par-term
    (id i_1 (add1 (choose k 1 2)))
    (id i_2 (seq (collect k) (add1 2)))))
(define e21
  (par-term
   (id i_1 
       (add1 (choose k_1 
                     (add1 (choose k_2 
                                   (seq (jump i_2 k_3) 
                                        (seq (jump i_1 k_1) 10)) 
                                   20)) 
                     30)))
   (id i_2 (sub1 (choose k_3 4 5)))))

;; Here is how to view the evaluation of the example expressions
(traces choose-red-jump-collect e21)

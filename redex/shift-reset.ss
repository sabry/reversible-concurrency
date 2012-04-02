#lang racket
(require redex)

;; Abstract syntax
(define-language shift-reset
  [e v (e e) (o1 e) (o2 e e) (seq e e) (if e e e) (let x = e in e) (err s)
     (shift k e) (reset e) (jump k e) (collect k)]
  [o1 add1 sub1 iszero]
  [o2 + - * / ^]
  [b number true false]
  [s string]
  [v b x (lambda x e) k unit]
  [E hole (E e) (v E) (o1 E) (o2 E e) (o2 v E) (if E e e)
     (seq E e) (let x = E in e) (reset E) (jump k E)]
  [F hole (F e) (v F) (o1 F) (o2 F e) (o2 v F) (if F e e)
     (seq F e) (let x = F in e)]
  [k (variable-prefix k)]
  [D ((k F) ...)]
  [x variable-not-otherwise-mentioned])

;; Application of primitive functions
(define-metafunction shift-reset
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
(define-metafunction shift-reset
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
  [(subst (shift x_1 any_1) x_1 any_2) (shift x_1 any_1)]
  [(subst (shift x_1 any_1) x_2 any_2)
   (shift x_3 (subst (subst-var any_1 x_1 x_3) x_2 any_2))
   (where x_3 ,(variable-not-in (term (x_2 any_1 any_2)) (term x_1)))]
  [(subst x_1 x_1 any_1) any_1]
  [(subst (any_2 ...) x_1 any_1) ((subst any_2 x_1 any_1) ...)]
  [(subst any_2 x_1 any_1) any_2])

(define-metafunction shift-reset
  [(subst-var (any_1 ...) x_1 x_2) ((subst-var any_1 x_1 x_2) ...)]
  [(subst-var x_1 x_1 x_2) x_2]
  [(subst-var any_1 x_1 x_2) any_1])

;; Single-step reduction
(define shift-reset-red
  (reduction-relation
   shift-reset
   (--> (D (in-hole E (err s))) (err s)
        (side-condition (not (equal? (term hole) (term E)))))
   (--> (D (in-hole E ((lambda x e) v))) (D (in-hole E (subst e x v))))
   (--> (D (in-hole E (o1 b))) (D (in-hole E (delta (o1 b)))))
   (--> (D (in-hole E (o2 b_1 b_2))) (D (in-hole E (delta (o2 b_1 b_2)))))
   (--> (D (in-hole E (seq v e))) (D (in-hole E e)))
   (--> (D (in-hole E (if true e_1 e_2))) (D (in-hole E e_1)))
   (--> (D (in-hole E (if false e_1 e_2))) (D (in-hole E e_2)))
   (--> (D (in-hole E (let x = v in e))) (D (in-hole E (subst e x v))))
   (--> (D (in-hole E (reset v))) (D (in-hole E v)))
   (--> (((k_0 F_0) ...) (in-hole E (reset (in-hole F (shift k_1 e)))))
        (((k_0 F_0) ... (k_1 F)) (in-hole E (reset e))))
   (--> (((k_0 F_0) ... (k_1 F_1) (k_2 F_2) ...) (in-hole E (jump k_1 v)))
        (((k_0 F_0) ... (k_1 F_1) (k_2 F_2) ...) (in-hole E (in-hole F_1 v))))
   (--> (((k_0 F_0) ... (k_1 F_1) (k_2 F_2) ...) (in-hole E (collect k_1)))
        (((k_0 F_0) ... (k_2 F_2) ...) (in-hole E unit)))))
   
(define-syntax sr-term
   (syntax-rules ()
     [(sr-term exp) (term (() exp))]))

;; A few small examples
(define e0 (sr-term 3))
(define e1 (sr-term true))
(define e2 (sr-term false))
(define e3 (sr-term x))
(define e4 (sr-term (lambda x (add1 x))))
(define e5 (sr-term ((lambda x x) ((lambda y y) 3))))
(define e6 (sr-term (add1 (sub1 (add1 (sub1 10))))))
(define e7 (sr-term (if (iszero (sub1 1)) 5 6)))
(define e8 (sr-term (if (iszero (add1 1)) 5 6)))
(define e9 (sr-term (seq (seq 1 2) (add1 3))))
(define e10 (sr-term (let x = 6 in (let y = 2 in (+ x y)))))
(define e11 (sr-term (let x = 6 in (let y = 2 in (- x y)))))
(define e12 (sr-term (let x = 6 in (let y = 2 in (* x y)))))
(define e13 (sr-term (let x = 6 in (let y = 2 in (^ x y)))))
(define e14 (sr-term (let x = 6 in (let y = 2 in (/ x y)))))
(define e15 (sr-term (let x = 6 in (let y = 0 in (add1 (/ x y))))))
(define e16 (sr-term (reset (reset (add1 3)))))
(define e17 (sr-term (reset (add1 (shift k (jump k (jump k 2)))))))
(define e18 (sr-term (add1 (reset (sub1 (shift k (jump k (jump k 2))))))))
(define e19 (sr-term (reset (add1 (shift k (iszero (- 10 (jump k 7))))))))

;; Here is how to view the evaluation of the example expressions
(traces shift-reset-red e19)

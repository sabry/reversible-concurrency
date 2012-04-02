#lang racket
(require redex)

;; Abstract syntax
(define-language backtrackpar
  [e v (e e) (o1 e) (o2 e e) (seq e e) (if e e e) (let x = e in e) (err s)
     (shift x e) (reset e)
     (choose e e) backtrack
     (par e e)] 
  [o1 add1 sub1 iszero isbacktrack]
  [o2 + - * / ^]
  [b number true false BACKTRACKING]
  [s string]
  [v b x (lambda x e)]
  [E hole (E e) (v E) (o1 E) (o2 E e) (o2 v E) (if E e e)
     (seq E e) (let x = E in e)
     (reset E) (par E e) (par e E)]
  ;; par is not allowed in F because we don't backtracking to magically
  ;; affect other processes running in parallel
  [F hole (F e) (v F) (o1 F) (o2 F e) (o2 v F) (if F e e)
     (seq F e) (let x = F in e)] 
  [x variable-not-otherwise-mentioned])

;; Application of primitive functions
(define-metafunction backtrackpar
  [(delta (isbacktrack BACKTRACKING)) true]
  [(delta (isbacktrack v)) false]
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
(define-metafunction backtrackpar
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

(define-metafunction backtrackpar
  [(subst-var (any_1 ...) x_1 x_2) ((subst-var any_1 x_1 x_2) ...)]
  [(subst-var x_1 x_1 x_2) x_2]
  [(subst-var any_1 x_1 x_2) any_1])

;; Single-step reduction
(define backtrackpar-red
  (reduction-relation
   backtrackpar
   (--> (in-hole E (err s)) (err s)
        (side-condition (not (equal? (term hole) (term E)))))
   (--> (in-hole E ((lambda x e) v)) (in-hole E (subst e x v)))
   (--> (in-hole E (o1 b)) (in-hole E (delta (o1 b))))
   (--> (in-hole E (o2 b_1 b_2)) (in-hole E (delta (o2 b_1 b_2))))
   (--> (in-hole E (seq v e)) (in-hole E e))
   (--> (in-hole E (if true e_1 e_2)) (in-hole E e_1))
   (--> (in-hole E (if false e_1 e_2)) (in-hole E e_2))
   (--> (in-hole E (let x = v in e)) (in-hole E (subst e x v)))
   (--> (in-hole E (reset v)) (in-hole E v))
   (--> (in-hole E (reset (in-hole F (shift x_1 e))))
        (in-hole E (reset (subst e x_1 (lambda x_2 (reset (in-hole F x_2))))))
        (fresh x_2))
   (--> (in-hole E backtrack) (in-hole E (shift x_1 BACKTRACKING))
        (fresh x_1))
   (--> (in-hole E (choose e_1 e_2)) 
        (in-hole E (reset
                    ((shift c_1
                            (let x_1 = (c_1 (lambda x_0 e_1)) 
                              in (if (isbacktrack x_1) 
                                     (c_1 (lambda x_0 e_2)) 
                                     x_1)))
                     0)))
        (fresh x_0)
        (fresh x_1)
        (fresh c_1))))
   
;; A few small examples
(define e0 (term 3))
(define e1 (term true))
(define e2 (term false))
(define e3 (term x))
(define e4 (term (lambda x (add1 x))))
(define e5 (term ((lambda x x) ((lambda y y) 3))))
(define e6 (term (add1 (sub1 (add1 (sub1 10))))))
(define e7 (term (if (iszero (sub1 1)) 5 6)))
(define e8 (term (if (iszero (add1 1)) 5 6)))
(define e9 (term (seq (seq 1 2) (add1 3))))
(define e10 (term (let x = 6 in (let y = 2 in (+ x y)))))
(define e11 (term (let x = 6 in (let y = 2 in (- x y)))))
(define e12 (term (let x = 6 in (let y = 2 in (* x y)))))
(define e13 (term (let x = 6 in (let y = 2 in (^ x y)))))
(define e14 (term (let x = 6 in (let y = 2 in (/ x y)))))
(define e15 (term (let x = 6 in (let y = 0 in (add1 (/ x y))))))
(define e16 (term (reset (reset (add1 3)))))
(define e17 (term (reset (add1 (shift c (c (c 2)))))))
(define e18 (term (add1 (reset (sub1 (shift c (c (c 2))))))))
(define e19 (term (reset (add1 (shift c (iszero (- 10 (c 7))))))))
(define e20 (term (choose 1 2)))
(define e21 (term (choose backtrack 1)))
(define e22 (term (choose (choose backtrack backtrack) (choose backtrack 2))))
(define e23 (term (par ,e10 ,e12)))


;; Here is how to view the evaluation of the example expressions
(traces backtrackpar-red e23)

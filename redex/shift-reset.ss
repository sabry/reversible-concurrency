#lang racket
(require redex)

;; Abstract syntax
(define-language shift-reset
  [e/v (e e) (o1 e) (o2 e e) (seq e e) (if e e e) (let x = e in e) (err s)
     (shift k e) (reset e) (jump i k e) (collect k)
     (let-match (x x) = e in e) (send v i k) (recv i k)]
  [e e/v v]
  [o1 add1 sub1 iszero]
  [o2 + - * / ^]
  [b number true false]
  [s string]
  [v b x (lambda x e) k (cons v_1 v_2) ()]
  [E hole (E e) (v E) (o1 E) (o2 E e) (o2 v E) (if E e e)
     (seq E e) (let x = E in e) (reset E) (jump i k E)
     (let-match (x x) = E in e)]
  [F hole (F e) (v F) (o1 F) (o2 F e) (o2 v F) (if F e e)
     (seq F e) (let x = F in e) (let-match (x x) = F in e)]
  [k (variable-prefix k)]
  [i (variable-prefix i)]
  [D (i ((k F) ...))]
  [P (D e/v)]
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

;; Per process Single-step reduction
(define-metafunction shift-reset
  [(process-step (D (in-hole E (err s)))) 
   (err s)
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
  [(process-step (D (in-hole E (reset v)))) 
   (D (in-hole E v))]
  [(process-step ((i ((k_0 F_0) ...)) 
                  (in-hole E (reset (in-hole F (shift k_1 e))))))      
   ((i ((k_0 F_0) ... (k_1 F))) (in-hole E (reset e)))]
  [(process-step ((i ((k_0 F_0) ... (k_1 F_1) (k_2 F_2) ...)) 
                  (in-hole E (jump i k_1 v))))      
   ((i ((k_0 F_0) ... (k_1 F_1) (k_2 F_2) ...)) 
    (in-hole E (in-hole F_1 v)))]
  [(process-step ((i ((k_0 F_0) ... (k_1 F_1) (k_2 F_2) ...)) 
                  (in-hole E (collect k_1))))      
   ((i ((k_0 F_0) ... (k_2 F_2) ...)) (in-hole E ()))]
  [(process-step (D (in-hole E v))) (D (in-hole E v))])

;; Single-step reduction
(define shift-reset-red
  (reduction-relation
   shift-reset
   (--> (par P_0 ... P_1 P_2 ...)
        (par P_0 ... (process-step P_1) P_2 ...))
   ;; This doesn't quite work, unless we want to allow sending
   ;; continuations (which sounds like an interesting idea by itself
   ;; but...). We need some way to communicate continuation id's, so we
   ;; can ask a process to backtrack to a specific continuation. Thus,
   ;; we need to add in communication. Perhaps a send/recv form like:
   ;;
   ;; (in-hole E (send v j k_1)) --> 
   ;;   (in-hole E (reset (in-hole F (shift k_1 (cons k_1 k_2)))))
   ;; (in-hole E (recv i k_2)) --> 
   ;;   (in-hole E (reset (in-hole F (shift k_2 (cons v (cons k_1 k_2))))))
   ;;
   ;;
   ;; Perhaps for now, we can use ??? = (err "Our send/recv partner told
   ;; us to backtrack, but we don't know what means yet.")
   ;;
   ;;
   ;; Need two rules, to deal with either order. This should be
   ;; abstracted in someway to allow me to write 1 function/macro that
   ;; creates both of these rules, for arbitrary thread contexts.
   ;; Something like
   ;; (lambda (T1 T2 T1' T2')
   ;;   ((--> (par P_0 ... T1 P_1 ... T2 P_2 ...)
   ;;         (par P_0 ... T1' P_2 ... T2' P_2 ...))
   ;;    (--> (par P_0 ... T2 P_1 ... T1 P_2 ...)
   ;;         (par P_0 ... T2' P_1 ... T1' P_2 ...))))
   (--> (par P_0 ... ((i_0 ((k_3 F_3) ... (k_1 F_1) (k_4 F_4) ...))
                      (in-hole E e))  
             P_1 ... ((i_1 ((k_0 F_0) ... (k_1 F_1) (k_2 F_2) ...)) 
                      (in-hole E (jump i_0 k_1 v)))
             P_2 ...)
        (par P_0 ... ((i_0 ((k_3 F_3) ... (k_1 F_1) (k_4 F_4) ...))
                      (in-hole E (in-hole F v)))
             P_1 ... ((i_1 ((k_0 F_0) ... (k_1 F_1) (k_2 F_2) ...)) 
                      (in-hole E ()))
             P_2 ...)
        (side-condition (not (equal? (term i_0) (term i_1)))))
   (--> (par P_0 ... ((i_1 ((k_0 F_0) ... (k_1 F_1) (k_2 F_2) ...)) 
                      (in-hole E (jump i_0 k_1 v))) 
             P_1 ... ((i_0 ((k_3 F_3) ... (k_1 F_1) (k_4 F_4) ...))
                      (in-hole E e))
             P_2 ...)
        (par P_0 ... ((i_1 ((k_0 F_0) ... (k_1 F_1) (k_2 F_2) ...)) 
                      (in-hole E ()))
             P_1 ... ((i_0 ((k_3 F_3) ... (k_1 F_1) (k_4 F_4) ...))
                      (in-hole E (in-hole F v)))
             P_2 ...)
        (side-condition (not (equal? (term i_0) (term i_1)))))))

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
(define e16 (par-term (reset (reset (add1 3)))))
(define e17 (par-term (reset (add1 (shift k (jump i k (jump i k 2)))))))
(define e18 (par-term (add1 (reset (sub1 (shift k (jump i k (jump i k 2))))))))
(define e19 (par-term (reset (add1 (shift k (iszero (- 10 (jump i k 7))))))))
(define e20 (par-term 
              (id i_1 
                  (reset (add1 (shift k (iszero (- 10 (jump i k 7)))))))
              (id i_2 
                  ())))
;; Here is how to view the evaluation of the example expressions
(traces shift-reset-red e19)

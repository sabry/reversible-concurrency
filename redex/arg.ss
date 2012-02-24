#lang racket
(require redex)

;; The language \alpha_\rho^\gamma. Pronounced ARGG, like the sound in my
;; head every time I have to think about the semantics of backtrack.

(define-language arg
  ;; Expressions: 
  ;;
  ;;   choose: Creates a choice point, taking the first expression, and
  ;;   saving a continuation. If the thread backtracks to this choice
  ;;   point, it uses the second expression.
  ;;   
  ;;   par: Executes two expressions in parallel.
  ;;
  ;;   newChan: Creates a new channel. Channels are assumed to have a
  ;;   single sender and single receiver. TODO: formalize that
  ;;
  ;;   backtrack: Returns to the last choice point, and causes all
  ;;   threads which have communicated with the current thread to
  ;;   backtrack to a specific time.
  ;;
  ;;   send: Sends a value on a channel. Currently synchronous, but
  ;;   should be easy to modify to make synchronous.
  ;;
  ;;   recv: Receives a value on a channel.
  ;;
  ;;   seq: Sequencing. Not particularly necessary, but useful since
  ;;   much of the language is stateful.
  ;;
  (e (choose e e) (par e e) (newChan) (backtrack) (send vars e) (recv vars)
     (e e) v (seq e e) (let ([x e] ...) e))
  
  ;; Values
  (v (lambda (x) e) number unit vars)

  ;; Vars allowed in send/recv
  (vars x ch)

  ;; Times are natural numbers plus a minimal and maximal time value, to
  ;; give us a total ordering. I'm not sure the total ordering is
  ;; necessary any longer.
  (T number max min)

  ;; Gamma is a mapping from channels to pairs of Times and
  ;; Continuations.
  (G (ch T K)) 

  ;; A Continuation contains the State and an evaluation context.
  (K empty ((G ...) K T e))

  ;; Global channel map. Maps channels to tags and times. Used for
  ;; backtracking: when a thread needs it's partners to backtrack, it
  ;; sets the tag and the time to when they should backtrack to.
  (D (ch tag T)) 

  ;; A process is a continuation, but I want another variable for use in
  ;; pattern matching
  (P K)

  ;; Evaluation contexts
  (E hole (E e) (v E) (seq E e) (seq v E) (send vars E) 
     (let ([x v] ... [x E] [x e] ...)
       e))
  
  ;; Tags for backtracking
  (tag idle backtrack)

  ;; Channels
  (ch (variable-prefix ch))
  
  ;; Variables
  (x variable-not-otherwise-mentioned))

;; XXX: These two might to be better off as meta-functions
(define (none-backtracking D G)
  (andmap (lambda (x) x) (map (lambda (x) 
         (cond
           [(assq (car x) D) => 
              (lambda (x) (eq? (cadr x) 'idle))]
           [else #t])) G)))


(define (backtrack-channels D G)
  (map (lambda (d_ch)
         (cond
           [(assq (car d_ch) G) => 
              (lambda (g_ch) `(,(car g_ch) backtrack ,(caddr g_ch)))]
           [else d_ch])) D))

(define-metafunction arg 
  subst : x e any -> any 
  ;; 1. x_1 bound, so don't continue in λ body 
  [(subst x_1 e_1 (lambda (x_1) e_2)) 
   (lambda (x_1) e_2)] 
  ;; 2. general purpose capture avoiding case 
  [(subst x_1 e_1 (lambda (x_2) e_2)) 
   (lambda (x_new)
     (subst x_1 e_1 (subst x_2 x_new e_2))) 
   (where x_new
      ,(variable-not-in 
        (term (x_1 e_1 e_2))
        (term x_2)))]
  ;; 3. replace x_1 with e_1 
  [(subst x_1 e_1 x_1) e_1] 
  ;; the last cases cover all other expressions 
  [(subst x_1 e_1 (any_2 ...)) ((subst x_1 e_1 any_2) ...)] 
  [(subst x_1 e_1 any) any])

;; Some math on time. 
;; TODO: Should probably make a module for these. I also need an ordering, 
;; for when backtracking needs to compare min and n.
(define (t-inc a)
  (case a
    ['min 1]
    ['max 'max]
    [else (+ a 1)]))

(define (t-max a b)
  (case a
    ['min b]
    ['max 'max]
    [else (case b
            ['min a]
            ['max 'max]
            [else (max a b)])]))

;; Judgment relation is ((D ...) (P ...)) --> ((D^ ...) (P^ ...))
(define step
  (reduction-relation
    arg
    ;; First, some house keeping 
    (--> ((D ...) (P_1 ... ((G ...) K T (in-hole E (seq v_1 v_2))) P_2
                       ...))
         ((D ...) (P_1 ... ((G ...) K T (in-hole E v_2)) P_2 ...))
         "Seq")
    (--> ((D ...) (P_1 ... ((G ...) K T (in-hole E ((lambda (x) e) v)))
                   P_2 ...))
         ((D ...) (P_1 ... ((G ...) K T (in-hole E (subst x v e))) P_2
                       ...))
         "App")
    ;; TODO: Implement multi-arity functions, then let in terms of that, 
    ;; because I have a feeling this version if broken. Anyway the 
    ;; implementation is bad.
    ;; TODO: Figure out shortcuts so I can define all the simple stuff 
    ;; in terms of another arrow
    (--> ((D ...) (P_1 ... ((G ...) K T (in-hole E (let () e))) P_2 ...))
         ((D ...) (P_1 ... ((G ...) K T (in-hole E e)) P_2 ...))
         "Let-base")
    (--> ((D ...) (P_1 ... ((G ...) K T (in-hole E (let ([x_1 v_1] ...
                                                         [x v])
                                                     e_body))) P_2 ...))
         ((D ...) (P_1 ... ((G ...) K T 
                                    (in-hole E (let ([x_1 v_1] ...)
                                                 (subst x v e_body))))
                  P_2 ...))
         "Let")
    ;; Now the real rules
    (--> ((D ...) (P_1 ... ((G ...) K T (in-hole E (newChan))) P_2 ...))
         ((D ... (ch idle T)) (P_1 ... ((G ... (ch T K)) K T 
                                                         (in-hole E ch))
                               P_2 ...))
         "newChan"
          (where ch
            ,(variable-not-in (term (D ...)) (term ch))))
    (--> ((D ... (ch idle T) D_1 ...)
          (P_1 ... (name K_s 
                ((G_s ... (ch T_chs K_chs) G_s1 ...)
                 K_cs
                 T_s
                 (in-hole E_s (send ch v)))) ;; Send Process
           P_2 ...
           (name K_r
                 ((G_p ... (ch T_chr K_chr) G_p1 ...)
                  K_cr
                  T_r
                  (in-hole E_r (recv ch)))) ;; Receive Process
           P_3 ...)) ;; Other processes
         ,(redex-let arg 
           ([T_m (t-inc (t-max (term T_r) (term T_s)))])
           (term
             ((D ... (ch idle T) D_1 ...)
              (P_1 ... 
               ((G_s ... (ch T_m K_s) G_s1 ...)
                K_s
                T_m 
                (in-hole E_s unit)) ;; Send Process
               P_2 ...
               ((G_p ... (ch T_m K_r) G_p1 ...)
                K_r
                T_m 
                (in-hole E_r v)) ;; Receive Process
               P_3 ...))))
         "Send/Recv"
         (side-condition
           (and 
             (none-backtracking (term (D ... D_1 ...)) 
                                (term (G_p ... G_p1 ...)))
             (none-backtracking (term (D ... D_1 ...)) 
                                (term (G_s ... G_s1 ...))))))
    ;; TODO: need some sort of special 'suicide' command for threads
    ;; that backtrack past a par.
    (--> ((D ...) (P_1 ... ((G ...) K T (in-hole E (par e_1 e_2))) P_2
                       ...))
         ((D ...) (P_1 ... ((G ...) K T (in-hole E e_1)) P_2 ... 
                       ((G ...) empty min e_2)))
         "Par"
         (side-condition
           (none-backtracking (term (D ...)) (term (G ...)))))
    (--> ((D ...) (P_1 ... ((G ...) K_c T (in-hole E (choose e_1 e_2)))
                       P_2 ...))
         ((D ...) (P_1 ... ((G ...) ((G ...) K_c T (in-hole E e_2)) T
                                    (in-hole E e_1)) P_2 ...))
         "Choose"
         (side-condition
           (none-backtracking (term (D ...)) (term (G ...)))))
    (--> ((D ...) (P_1 ... ((G ...) (name C ((G_c ...) K_c T_c e_c)) T
                                    (in-hole E (backtrack))) P_2 ...))
         (,(backtrack-channels (term (D ...)) (term (G_c ...))) 
           (P_1 ... C P_2 ...))
         "Backtrack")
    (--> ((D ... (ch backtrack T) D_1 ...) 
          (P_1 ... ((G ... (ch T_c K) G_1 ...) C T_s e) P_2 ...))
         ((D ... (ch backtrack T) D_1 ...)
          P_1 ... K P_2 ...)
         "Backtrack-GT"
         (side-condition 
             (> (term T_c) (term T))))
    (--> ((D ... (ch backtrack T) D_1 ...)
          (P_1 ... ((G ... (ch T_c K) G_1 ...) C T_s e) P_2 ...))
         ((D ... (ch idle T) D_1 ...)
          (P_1 ... K P_2 ...))
         "Backtrack-EQ"
         (side-condition 
             (eq? (term T_c) (term T))))
    (--> ((D ... (ch backtrack T) D_1 ...)
          (P_1 ... (name K_p ((G ... (ch T_c K) G_1 ...) C T_s E)) P_2
               ...))
         ((D ... (ch backtrack T_c) D_1 ...)
          (P_1 ... K_p P_2 ...))
         "Backtrack-LT"
         (side-condition 
             (< (term T_c) (term T))))))

;;; Start of testing stuff 
;;; TODO: Should put this in another module.

;; Example:
;; (traces-eval (let ([x (newChan)]) (par (send x 1) (recv x))))
;;
;; Will show a graphical trace of reduction sequences
(define-syntax traces-eval
  (syntax-rules ()
    [(_ x) (traces step (term (() ((() empty min x)))))]))

;; Example:
;; (eval (let ([x (newChan)]) (par (send x 1) (recv x)))) => '(unit 1)
;;
;; Returns a list of final values of all the processes
(define-syntax eval
  (syntax-rules ()
    [(_ x) (arg-eval (term x))]))

(define (extract-es ls)
  (map cadddr ls))

(define (arg-eval e_1)
    (let loop ([e (term (() ((() empty min ,e_1))))]
               [ans e_1])
      (let ([e_next (apply-reduction-relation step e)])
        (if (null? e_next)
            ans
            (redex-let arg ([((D ...) (P ...)) (car e_next)])
              (loop (car e_next) (extract-es (term (P ...)))))))))

;; TODO: Figure out how the property checker works, and create a simple
;; typing relation to make it check on well-typed terms.

;; Simple tests
(require test-engine/racket-tests)

(check-expect (eval ((lambda (x) 
                       (par (send x 1)
                            (recv x)))
                     (newChan)))
              '(unit 1))
(check-expect (eval ((lambda (x) 
                       (par (send x (seq 2 1))
                            (recv x)))
                     (newChan)))
              '(unit 1))
(check-expect (eval (let ([x (newChan)])
                      (par (send x 1)
                           (recv x))))
              '(unit 1))
(check-expect (eval (let ([x 2])
                      1)) '(1))
(check-expect (eval (let ([x 2])
                      x)) '(2))
(check-expect (eval (seq 1 2)) '(2))
(check-expect (eval (choose (backtrack) 1)) '(1))
(check-expect (eval (choose (choose (backtrack) (backtrack)) 2)) '(2))
(check-expect (eval (choose (backtrack) (choose (backtrack) 1))) '(1))
(test)
#lang racket
(require redex)

;; The language \alpha_\rho^\gamma. Pronounced ARGGGG
;; I should probably change that to something involving the actual
;; letters used here.
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
  ;;   single sender and single receiver. XXX: I haven't formalized that
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
  (e (choose e e) (par e e) (newChan) (backtrack) (send ch v) (recv ch)
     (e e) x v (seq e e))

  ;; Values
  (v (lambda (x) e) number unit)

  ;; State: A triple containing a Gamma, Continuation, and Time
  (S (G K T)) 

  ;; Times are natural numbers plus a minimal and maximal time value, to
  ;; give us a total ordering. I'm not sure the total ordering is
  ;; necessary any longer.
  (T number max min)

  ;; Gamma is a mapping from channels to pairs of Times and
  ;; Continuations.
  (G ((ch T K) ...)) 


  ;; A Continuation contains the State and an evaluation context.
  (K empty (G K T E))

  ;; Global channel map. Maps channels to tags and times. Used for
  ;; backtracking: when a thread needs it's partners to backtrack, it
  ;; sets the tag and the time to when they should backtrack to.
  (D ((ch tag T) ...)) 

  ;; Processes: Either parallel processes, a continuation, or empty.
  ;; XXX: Removed empty for now, as I'm not sure it'd needed
  (P (K ...))

  ;; Evaluation contexts
  ;; XXX: Removed send; add it back later
  (E hole (E e) (v E) (seq E e) (seq v E))
  
  ;; Tags
  (tag idle backtrack)

  ;; Channels
  (ch (variable-prefix ch))

  ;; Judgment
  (J (D P))

  ;; Variables
  (x (variable-not-otherwise-mentioned)))

(define (none-backtracking D G)
  (map (lambda (x) 
         (cond
           [(assq (car x) D) => 
              (lambda (x) (eq? (cadr x) 'idle))]
           [else #t])) G)) 

(define (backtrack-channels D G)
  (map (lambda (d_ch)
         (cond
           [(assq (car d_ch) G) => 
              (lambda (g_ch) `(,(car g_ch) backtrack ,(cadr g_ch)))]
           [else d_ch])) D))

(define-metafunction arg 
  subst : x any any -> any 
  ;; 1. x_1 bound, so don't continue in λ body 
  [(subst x_1 any_1 (lambda (x_1) any_2)) 
   (lambda (x_1) any_2)] 
  ;; 2. general purpose capture avoiding case 
  [(subst x_1 any_1 (lambda (x_2) any_2)) 
   (lambda (x_new)
     (subst x_1 any_1 (subst x_2 x_new any_2))) 
   (where (x_new) 
      ,(variable-not-in 
        (term (x_1 any_1 any_2))
        (term x_2)))]
  ;; 3. replace x_1 with e_1 
  [(subst x_1 any_1 x_1) any_1] 
  ;; the last cases cover all other expressions 
  [(subst x_1 any_1 (any_2 ...)) ((subst x_1 any_1 any_2) ...)] 
  [(subst x_1 any_1 any_2) any_2])


;; Judgment relation is D P => D' P'
;; (=> (D P) (D^ P^))
(define step
  (reduction-relation
    arg
    ;; First, some house keeping 
    (--> (D (G K T (in-hole E (seq v_1 v_2))) P ...)
         (D (G K T (in-hole E v_2)) P ...)
         "seq")
    (--> (D (G K T (in-hole E ((lambda (x) e) v))) P ...)
         (D (G K T (in-hole E (subst x v e))) P ...)
         "app")
    ;; Now, the real rules
    (--> (D (G K T (in-hole E (newChan))) P ...)
         (((ch idle T) . D) (((ch K T) . G) K T (in-hole E ch)) P ...)
         "newChan"
          (where (ch)
            ,(variable-not-in (term D) (term ch))))
    ;; XXX: Need to check ch is a member of D, can't use the informal
    ;; notation 
    (--> (D  ;(D (ch idle T))
          ((name K_s 
                (G_s ;(G_s (ch T_chs K_chs))
                 K_cs
                 T_s
                 (in-hole E_s (send ch v)))) ;; Send Process
           (name K_r
                 (G_p ;(G_p (ch T_chr K_chr))
                  K_cr
                  T_r
                  (in-hole E_r (recv ch)))) ;; Receive Process
           P ...)) ;; Other processes
         ,(term-let 
           ([time_m (+ 1 (max (term T_r) (term T_s)))]
            [G_s^ (remove (term (ch T_chs K_chs)) (term G_s))]
            [G_r^ (remove (term (ch T_chr K_chr)) (term G_r))])
           (term
             (D
              ((((ch time_m K_s) . G_s^)
                C_s
                time_m 
                (in-hole E_s unit)) ;; Send Process
               (((ch time_m K_r) . G_r^)
                C_r
                time_m 
                (in-hole E_r v)) ;; Receive Process
               P ...))))
         "Send/Recv"
         (side-condition
           (and 
             (none-backtracking (term D) (term G_r))
             (none-backtracking (term D) (term G_s))
             ;; XXX: I'm not sure this works.
             (member (term (ch idle T)) (term D))
             (member (term (ch T_chs K_chs)) (term G_s))
             (member (term (ch T_chr K_chr)) (term G_p)))))
    ;; XXX: need some sort of special 'suicide' command for threads
    ;; that backtrack past a par.
    (--> (D (G K T (in-hole E (par e_1 e_2))) P ...)
         (D (G K T (in-hole E e_1)) (()_ empty min e_2) P ...)
         "Par"
         (side-condition
           (none-backtracking (term D) (term G))))
    ;; XXX: Need to modify K to use e_2 
    (--> (D (name K (G K_c T (in-hole E (choose e_1 e_2)))) P ...)
         (D (G K T (in-hole E e_1)))
         "Choose"
         (side-condition
           (none-backtracking (term D) (term G))))
    ;; XXX: Need to define backtrack-channels
    (--> (D (G (name C (G_c K_c T_c E_c)) T (in-hole E (backtrack))) P ...)
         (,(backtrack-channels (term D) (term G_c)) C P ...)
         "Backtrack")
    (--> (D #;(D (ch backtrack T)) (G #;(G (ch T_c K)) C T_s E) P ...)
         (((ch backtrack T) . D) K P ...)
         "Backtrack-GT"
         (side-condition 
           (and 
             (> (term T_c) (term T))
             (member (term (ch backtrack T)) (term D))
             (member (term (ch T_c K)) (term G)))))
    (--> (D #;(D (ch backtrack T)) (G #;(G (ch T_c K)) C T_s E) P ...)
         (((ch idle T) . D) K P ...)
         "Backtrack-EQ"
         (side-condition 
           (and 
             (= (term T_c) (term T))
             (member (term (ch backtrack T)) (term D))
             (member (term (ch T_c K)) (term G)))))
    (--> (D #;(D (ch backtrack T)) (name K_p (G #;(G (ch T_c K)) C T_s E)) P ...)
         (((ch backtrack T_c) . D) K_p P ...)
         "Backtrack-LT"
         (side-condition 
           (and 
             (< (term T_c) (term T))
             (member (term (ch backtrack T)) (term D))
             (member (term (ch T_c K)) (term G)))))))



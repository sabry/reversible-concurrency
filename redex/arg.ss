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
  (G empty (G (ch T K))) 

  ;; A Continuation contains the State and an evaluation context.
  (K empty (G K T E))

  ;; Global channel map. Maps channels to tags and times. Used for
  ;; backtracking: when a thread needs it's partners to backtrack, it
  ;; sets the tag and the time to when they should backtrack to.
  (D empty (D (ch tag T))) 

  ;; Processes: Either parallel processes, a continuation, or empty.
  ;; XXX: Removed empty for now, as I'm not sure it'd needed
  (P K (P ...))

  ;; Evaluation contexts
  ;; XXX: Removed send; add it back later
  (E (E e) (v E) (seq E e) (seq v E))
  
  ;; Tags
  (tag idle backtrack)

  ;; Channels
  (ch (variable-prefix ch))

  ;; Variables
  (x (variable-not-otherwise-mentioned)))

;; Judgment relation is D P => D' P'
;; (=> (D P) (D^ P^))
(define step
  (reduction-relation
    arg
    (=> ((D (ch_i idle time)) (((G_s (ch_i ch_time ch_k))
                               C_s
                               T_s
                               (in-hole E_s (send ch_i v)))
                               ((G_p (ch_i ch_time^ ch_k^))
                               C_r
                               T_r
                               (in-hole E_r (recv ch_i)))
                               . P))
        )))

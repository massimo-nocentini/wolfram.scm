
(import
  (chicken sort)
  (chicken time)
  (aux unittest) (aux base) (aux hansei) wolfram srfi-1)

(define-wolfram W (->MathML 'MathML))

; setting some parameters
(op/plus (λ args (W `(Simplify (Plus ,@args)))))
(op/subtract (λ args (W `(Simplify (Subtract ,@args)))))
(op/times (λ args (W `(Simplify (Times ,@args)))))
(op/divide (λ args (W `(Simplify (Divide ,@args)))))
(op/greater (λ (a b) (> (W `(LeafCount ,a)) (W `(LeafCount ,b)))))

(define expected '(((V #f)
                    (Plus (Power (Plus -1 p) 10)
                          (Times (Power p 2)
                                 (Plus 45
                                       (Times -360 p)
                                       (Times 1470 (Power p 2))
                                       (Times -3780 (Power p 3))
                                       (Times 6510 (Power p 4))
                                       (Times -7560 (Power p 5))
                                       (Times 5715 (Power p 6))
                                       (Times -2550 (Power p 7))
                                       (Times 511 (Power p 8))))))
                   ((V #t)
                    (Times 2
                           p
                           (Plus 5
                                 (Times -45 p)
                                 (Times 240 (Power p 2))
                                 (Times -840 (Power p 3))
                                 (Times 2016 (Power p 4))
                                 (Times -3360 (Power p 5))
                                 (Times 3840 (Power p 6))
                                 (Times -2880 (Power p 7))
                                 (Times 1280 (Power p 8))
                                 (Times -256 (Power p 9)))))))

(define expanded-expected '(((V #f)
                             (Plus 1
                                   (Times -10 p)
                                   (Times 90 (Power p 2))
                                   (Times -480 (Power p 3))
                                   (Times 1680 (Power p 4))
                                   (Times -4032 (Power p 5))
                                   (Times 6720 (Power p 6))
                                   (Times -7680 (Power p 7))
                                   (Times 5760 (Power p 8))
                                   (Times -2560 (Power p 9))
                                   (Times 512 (Power p 10))))
                            ((V #t)
                             (Times 2
                                    p
                                    (Plus 5
                                          (Times -45 p)
                                          (Times 240 (Power p 2))
                                          (Times -840 (Power p 3))
                                          (Times 2016 (Power p 4))
                                          (Times -3360 (Power p 5))
                                          (Times 3840 (Power p 6))
                                          (Times -2880 (Power p 7))
                                          (Times 1280 (Power p 8))
                                          (Times -256 (Power p 9)))))))

(define-suite hansei-ve-suite

  ((doc r) `((structure/section "Variable elimination optimization")
             (p "In this document we report some tests about the following technique,")
             (cite/quote "Oleg Kiselyov" 
                         "The optimization of reification followed by (partial) flattening and reflection
			 gives rise to the inference technique known as " (b "variable elimination")  " "
                         (cite/a "https://www.sciencedirect.com/science/article/pii/S0004370299000594"
                                 "Dechter, Rina. Bucket elimination: A unifying framework for probabilistic inference.")
                         ". Its benefit is demonstrated by the following example, computing the XOR of n coin tosses.")
             (p "This trick transform a stochastic function " (code/inline "a -> b") " to a generally faster function:")
             (code/lang ocaml "let variable_elim f arg = reflect (exact_reify (fun () -> f arg))")
             (p "According to the idea shown in " (cite/a "testsuite-hansei-symbolic-suite.html" 
                                                          "Symbolic manipulation in Hansei: a new view on the " 
                                                          (i "wet grass model") ".")
                ", using symbolic manipulation, we show that the probability of XOR of 10 coin tosses  to be " (i "tail") " is")
             (container (escape ,(->MathML `(Expand ,(second (first expected))) rule/MathML/display/block)))
             (p "provided that " (i "head") " has probability " (math (m p)) 
                " to appear in each toss. Moreover, the coefficients are the 10th row of a known sequence " 
                (cite/a "https://oeis.org/A082137" 
                        "A082137: Square array of transforms of binomial coefficients, read by antidiagonals.") ".")))

  ((test/exponential _)

   (define result (probcc-reify/exact
                    (let loop ((p 'p) (n 10))
                      (cond
                        ((equal? 1 n) (probcc-coin p))
                        (else (let1 (r (loop p (sub1 n)))
                                    (not (equal? (probcc-coin p) r))))))))

   (⊦= expected result))

  ((test/var-elimination _)

   (define (flipxor-model p)
     (letrec ((loop (probcc-variable-elimination
                      (λ (n)
                          (cond
                            ((equal? 1 n) (probcc-coin p))
                            (else (let1 (r (loop (sub1 n)))
                                        (not (equal? (probcc-coin p) r)))))))))
       loop))

   (define res (probcc-reify/exact ((flipxor-model 'p) 10)))
   (⊦= expanded-expected res))

  ((test/bucket _)

   (define (flipxor-model p)
     (letrec ((loop (λ-probcc-bucket (n)
                                      (cond
                                        ((equal? 1 n) (probcc-coin p))
                                        (else (let1 (r (loop (sub1 n)))
                                                    (not (equal? (probcc-coin p) r))))))))
       loop))

   (define res (probcc-reify/exact ((flipxor-model 'p) 10)))
   (⊦= expanded-expected res))

  ((test/bucket/generic _)

   (define flipxor-model
     (letrec ((loop (λ-probcc-bucket (n)
                                      (cond
                                        ((null? (cdr n)) (probcc-coin (car n)))
                                        (else (let1 (r (loop (cdr n)))
                                                    (not (equal? (probcc-coin (car n)) r))))))))
       loop))

   (define res (probcc-reify/exact (flipxor-model '(a b c d e))))
   (⊦= '(((V #f)
            (Plus 1
                  (Times -1 c)
                  (Times -1 d)
                  (Times 2 c d)
                  (Times -1 e)
                  (Times 2 c e)
                  (Times 2 d e)
                  (Times -4 c d e)
                  (Times b
                         (Plus -1 (Times 2 c))
                         (Plus -1 (Times 2 d))
                         (Plus -1 (Times 2 e)))
                  (Times -1
                         a
                         (Plus -1 (Times 2 b))
                         (Plus -1 (Times 2 c))
                         (Plus -1 (Times 2 d))
                         (Plus -1 (Times 2 e)))))
           ((V #t)
            (Plus c
                  d
                  (Times -2 c d)
                  e
                  (Times -2 c e)
                  (Times -2 d e)
                  (Times 4 c d e)
                  (Times -1
                         b
                         (Plus -1 (Times 2 c))
                         (Plus -1 (Times 2 d))
                         (Plus -1 (Times 2 e)))
                  (Times a
                         (Plus -1 (Times 2 b))
                         (Plus -1 (Times 2 c))
                         (Plus -1 (Times 2 d))
                         (Plus -1 (Times 2 e)))))) res)
   `(doc (p "This test generalizes the previous one in the sense that each coin has a " (i "different") 
            " probability for " (i "head") "; in particular, the exact inference for the XOR of 5 such coins yields
	    that " (i tail) " has probability:")
         (container (escape ,(->MathML (second (first res)) rule/MathML/display/block)))
         (p "provided that coin's heads have probabilities " (math (m a)) ", " (math (m b)) ", "
            (math (m c)) ", "(math (m d)) " and "(math (m e)) ", respectively.")))
  )

(unittest/✓ hansei-ve-suite)










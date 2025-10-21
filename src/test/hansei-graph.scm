
(import
  (chicken sort)
  (chicken time)
  (chicken sort)
  (aux unittest) (aux base) (aux hansei) wolfram srfi-1)

(define-wolfram (W ->wolfram) (->MathML 'MathML))

; setting some parameters
(op/plus (λ args (W `(Simplify (Plus ,@args)))))
(op/subtract (λ args (W `(Simplify (Subtract ,@args)))))
(op/times (λ args (W `(Simplify (Times ,@args)))))
(op/divide (λ args (W `(Simplify (Divide ,@args)))))
(op/greater (λ (a b) (> (W `(LeafCount ,a)) (W `(LeafCount ,b)))))

(define (wolfram/joint-form result)
  `(MatrixForm
     (List ,@(letmap ((p result)) 
                     `(Rule (Prob ,@(map (λ (each)
                                             (cond
                                               ((procedure? each) 'proc)
                                               (else (second each)))) 
                                      (cadr (car p)))) ,(cadr p))))))

(define (probcc-distribution/wolfram p lst) 
  (probcc-distribution (map (λ (each) `(,each ,(W `(Subscript ,p ,each)))) lst)))

(define-suite hansei-graph-suite

  ((doc r) `((structure/section "Symbolic")))

  ((test/random-graphs/joint _)

   (define tgraph '(
                    ((Subscript a 0) (Subscript b 1))
                    ((Subscript a 1) (Subscript b 4))
                    ((Subscript b 2) (Subscript c 3))
   ))
  
   (define result 
     (probcc-reify/exact
       (let* ((max-t 10))
          (let R ((t 0) 
                  (v_i (probcc-distribution/wolfram 'w '((Subscript a 0) (Subscript b 0) (Subscript c 0))))
                  (track '()))
            (cond
              ((= t max-t) (reverse track))
              (else (let* ((v_j (probcc-distribution/wolfram 'w (letassoc/cdr (v_i tgraph) (else '())))))
                        (R (+ t 1) v_j (cons (list v_i v_j t) track)))))))))

   (define normalized (probcc-normalize result))
   (define result/W (map (λ (each) (match each (`((V ,lst) ,p) `(Rule ,lst ,p)))) normalized))

   #;(⊦= '(((V ((rain #t) (sprinkler #t) (grass-is-wet #t)))
            (Times r
                   s
                   (Plus v
                         (Times e (Plus -1 v) (Plus -1 w))
                         w
                         (Times -1 v w))))
           ((V ((rain #t) (sprinkler #f) (grass-is-wet #t)))
            (Times r (Plus -1 s) (Plus (Times e (Plus -1 w)) (Times -1 w))))
           ((V ((rain #f) (sprinkler #t) (grass-is-wet #t)))
            (Times (Plus -1 r) s (Plus (Times e (Plus -1 v)) (Times -1 v))))
           ((V ((rain #t) (sprinkler #t) (grass-is-wet #f)))
            (Times -1 (Plus -1 e) r s (Plus -1 v) (Plus -1 w)))
           ((V ((rain #t) (sprinkler #f) (grass-is-wet #f)))
            (Times -1 (Plus -1 e) r (Plus -1 s) (Plus -1 w)))
           ((V ((rain #f) (sprinkler #t) (grass-is-wet #f)))
            (Times -1 (Plus -1 e) (Plus -1 r) s (Plus -1 v)))
           ((V ((rain #f) (sprinkler #f) (grass-is-wet #f)))
            (Times -1 (Plus -1 e) (Plus -1 r) (Plus -1 s)))
           ((V ((rain #f) (sprinkler #f) (grass-is-wet #t)))
            (Times e (Plus -1 r) (Plus -1 s))))
         result)

   `(doc (p "If we remove the " (i "observation") (code/scheme `(first ,(caar normalized)))
            "from the previous test, then we can show the " (i "joint distribution"))
         (container (escape ,(->MathML `(MatrixForm (List ,@result/W))
                                       rule/MathML/display/block)))
         (p "where " (math (m (p rain sprinkler grass-is-wet))) " is the " (i "non-normalized") " probability density function.")))

  )

(unittest/✓ hansei-graph-suite)













(import aux unittest wolfram srfi-13)

(define-suite wolfram-suite

  ((doc r) `((structure/section "Introduction")))

  ((test/simple _)
   (let1 (env/pointer (make-env))
         (display env/pointer)
         (define link/pointer (openstring env/pointer))
         (display link/pointer)
         (display ((evaluate link/pointer) '(Plus x (Plus x x) x) #;(Times 3 4)))
         (display ((evaluate link/pointer) '(ToExpression "3+4/Sqrt[19.2]")))
         `(doc (escape ,((export-format link/pointer 'MathML) `(Plus x (Plus x x) x)))
	       (div (escape ,((export-format link/pointer 'MathML) `(FactorInteger 847932875))))
	       (div (@ (class "w3-center")) (escape ,((export-format link/pointer 'SVG) `(DiscretePlot 
							      (Length (FactorInteger n))
							      (List n 100))))))))
  
	 )

(unittest/✓ wolfram-suite)


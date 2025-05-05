
(import aux unittest wolfram srfi-13)

(define-suite wolfram-suite

  ((doc r) `((structure/section "Introduction")))

  ((test/simple _)
   (define-wolfram W (->MathML 'MathML) (->SVG 'SVG))

   (define expr '(Plus x (Plus x x) x))
   (⊦= '(Times 4 x) (W expr))
   (⊦= '(List (List 5 3) (List 47 1) (List 101 1) (List 1429 1)) (W '(FactorInteger 847932875)))
   (⊦= 3.91287092917528 (W '(ToExpression "3+4/Sqrt[19.2]")))
   `(doc (p "The simple expression " (escape ,(->MathML `(HoldForm ,expr))) 
	    " evaluates to " (escape ,(->MathML expr)) ". Moreover, the integer " (escape ,(->MathML 847932875)) 
	    " admits the factorization "
	    (escape ,(->MathML '(FactorInteger 847932875))) 
	    ". Finally, we show the distribution of the sizes of factorizations of the first 100 numbers:")
	 (center (escape ,(->SVG '(DiscretePlot (Length (FactorInteger n)) (List n 100)))))))
  )

(unittest/✓ wolfram-suite)


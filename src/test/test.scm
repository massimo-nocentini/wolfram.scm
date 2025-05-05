
(import aux unittest wolfram srfi-13)

(define-suite wolfram-suite

  ((doc r) `((structure/section "Introduction")))

  ((test/simple _)
   (define-wolfram W)
   (define ->MathML (export-format W 'MathML))

   (define expr '(Plus x (Plus x x) x))
   (⊦= '(Times 4 x) (W expr))
   (⊦= '(List (List 5 3) (List 47 1) (List 101 1) (List 1429 1)) (W '(FactorInteger 847932875)))
   (⊦= 3.91287092917528 (W '(ToExpression "3+4/Sqrt[19.2]")))
   `(doc (p "The simple expression " (code/scheme ,expr) " evaluates to " (escape ,(->MathML expr)))
	 (center (escape ,((export-format W 'MathML) `(FactorInteger 847932875))))
	 (center (escape ,((export-format W 'SVG) `(DiscretePlot (Length (FactorInteger n)) (List n 100)))))))
  )

(unittest/✓ wolfram-suite)


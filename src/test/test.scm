
(import aux unittest wolfram srfi-13)

(define-wolfram W (->MathML 'MathML) (->SVG 'SVG))

(define-suite wolfram-suite

  ((doc r) `((structure/section "Introduction")
             (code/lang "mathematica" "ArrayPlot[CellularAutomaton[30, {{1}, 0}, 50]]")
             (center (escape ,(->SVG '(ToExpression "ArrayPlot[CellularAutomaton[30, {{1}, 0}, 50]]"))))))

  ((test/simple _)
   (define expr '(Plus x (Plus x x) x))
   (⊦= '(Times 4 x) (W expr))
   (⊦= '(List (List 5 3) (List 47 1) (List 101 1) (List 1429 1)) (W '(FactorInteger 847932875)))
   (⊦= 3.91287092917528 (W '(ToExpression "3+4/Sqrt[19.2]")))
   `(doc (p "The simple expression " (escape ,(->MathML `(HoldForm ,expr))) 
            " evaluates to " (escape ,(->MathML expr)) ". Moreover, the integer " (escape ,(->MathML 847932875)) 
            " admits the factorization "
            (escape ,(->MathML '(FactorInteger 847932875))) 
            ". Finally, we show the distribution of the sizes of factorizations of the first 100 numbers:")
         (center (escape ,(->SVG '(DiscretePlot (Length (FactorInteger n)) (List n 100)))))
         (center (escape ,(->SVG '(ToExpression "Plot[Sin[x], {x, 0, 2 Pi}]"))))
         (center (escape ,(->SVG '(ToExpression "ComplexPlot3D[Sin[z], {z, -2 Pi - 2 I, 2 Pi + 2 I}, PlotLegends -> Automatic]"))))
         (escape ,(->MathML '(ToExpression "Series[Sin[x], {x, 0, 10}]")))
         ))
  )

((display/OutputForm W) `(ToExpression "Identity[x/Sqrt[5] + y^2 + 1/z]")))

(unittest/✓ wolfram-suite)










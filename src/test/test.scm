
(import aux wolfram)

(let1 (env/pointer (make-env))
(display env/pointer)
(define link/pointer (openstring env/pointer))
(display link/pointer)
(display ((evaluate link/pointer) '(Plus x (Plus x x) x) #;(Times 3 4)))
(display ((evaluate link/pointer) '(ToExpression  "3+4/Sqrt[19.2]")))
)
(display 'end)

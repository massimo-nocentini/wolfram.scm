
(import aux wolfram)

(let1 (env/pointer (make-env))
(display env/pointer)
(define link/pointer (openstring env/pointer))
(display link/pointer)
(display ((evaluate link/pointer) '(Times 3 4)))
)
(display 'end)

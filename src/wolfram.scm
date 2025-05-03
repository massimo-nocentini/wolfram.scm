
(module wolfram
  *
  (import scheme (chicken base) (chicken foreign) (chicken gc) aux)

  (foreign-declare "#include <wstp.h>")

  (define WSEOK (foreign-value "WSEOK" int))
  (define RETURNPKT (foreign-value "RETURNPKT" int))
  (define WSTKERR (foreign-value "WSTKERR" int))
  (define WSTKINT (foreign-value "WSTKINT" int))
  (define WSTKFUNC (foreign-value "WSTKFUNC" int))
  (define WSTKREAL (foreign-value "WSTKREAL" int))
  (define WSTKSTR (foreign-value "WSTKSTR" int))
  (define WSTKSTR (foreign-value "WSTKSTR" int))
  (define WSTKSYM (foreign-value "WSTKSYM" int))

  (define WSInitialize (foreign-lambda c-pointer "WSInitialize" c-pointer))
  (define WSDeinitialize (foreign-lambda void "WSDeinitialize" c-pointer))
  (define WSOpenString (foreign-lambda c-pointer "WSOpenString" c-pointer c-string (c-pointer int)))
  (define WSClose (foreign-lambda void "WSClose" c-pointer))
  (define WSActivate (foreign-lambda int "WSActivate" c-pointer))
  (define WSNextPacket (foreign-lambda int "WSNextPacket" c-pointer))
  (define WSNewPacket (foreign-lambda int "WSNewPacket" c-pointer))
  (define WSEndPacket (foreign-lambda int "WSEndPacket" c-pointer))
  (define WSFlush (foreign-lambda int "WSFlush" c-pointer))
  (define WSPutSymbol (foreign-lambda int "WSPutSymbol" c-pointer symbol))
  (define WSPutFunction (foreign-lambda int "WSPutFunction" c-pointer symbol int))
  (define WSGetNext (foreign-lambda int "WSGetNext" c-pointer))
  (define WSGetInteger64 (foreign-lambda int "WSGetInteger64" c-pointer (c-pointer integer64)))
  (define WSPutInteger64 (foreign-lambda int "WSPutInteger64" c-pointer integer64))

  (define-syntax ✓
    (syntax-rules ()
      ((✓ expr) (begin 
		    (when (zero? expr) 
		      (error (quote expr))) 
		    (void)))))

  (define (make-env)
    (let1 (handle (WSInitialize #f))
          (set-finalizer! handle WSDeinitialize)
          handle))

  (define (openstring env/pointer)
    (let-location ((i int))
                  (let1 (p (WSOpenString env/pointer
                                         "csi -linkmode connect -linkname 8081 -linkprotocol TCPIP -linkoptions 4"
                                         (location i)))
                        (unless (equal? WSEOK i) (error `(WSOpenString ,p)))
                        (set-finalizer! p WSClose)
                        (when (zero? (WSActivate p)) (error `(WSActivate ,p)))
                        p)))

  (define ((evaluate link) expr)
    (let put ((e expr))
      (cond
	((pair? e) (WSPutFunction link (car e) (length (cdr e))) (map put (cdr e)))
	((symbol? e) (WSPutSymbol link e))
	((integer? e) (WSPutInteger64 link e))
	(else (error 'put))))
    (when (zero? (WSEndPacket link)) (error `(WSEndPacket ,link)))
    (when (zero? (WSFlush link)) (error `(WSFlush ,link)))
    (let skip ()
      (unless (equal? (WSNextPacket link) RETURNPKT) 
	(WSNewPacket link) 
	(skip)))
    (let get ((wexpr (WSGetNext link)))
      (cond 
	((equal? wexpr WSTKINT) (let-location ((i integer64)) 
					      (when (zero? (WSGetInteger64 link (location i))) 
						(error `(WSGetInteger64 ,link ,i)))
					      i))
	(else (error 'else)))))
  )


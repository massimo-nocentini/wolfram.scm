
(module wolfram
  *
  (import scheme (chicken base) (chicken foreign) (chicken gc) (chicken process-context) srfi-1 srfi-13 (aux base))

  (foreign-declare "#include <wstp.h>")

  (define WSEOK (foreign-value "WSEOK" int))

  (define RETURNPKT (foreign-value "RETURNPKT" int))

  (define WSTKERR (foreign-value "WSTKERR" int))
  (define WSTKINT (foreign-value "WSTKINT" int))
  (define WSTKFUNC (foreign-value "WSTKFUNC" int))
  (define WSTKREAL (foreign-value "WSTKREAL" int))
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
  (define WSGetNext (foreign-lambda int "WSGetNext" c-pointer))

  (define WSGetInteger64 (foreign-lambda int "WSGetInteger64" c-pointer (c-pointer integer64)))
  (define WSPutInteger64 (foreign-lambda int "WSPutInteger64" c-pointer integer64))

  (define WSGetReal64 (foreign-lambda int "WSGetReal64" c-pointer (c-pointer double)))
  (define WSPutReal64 (foreign-lambda int "WSPutReal64" c-pointer double))

  (define WSGetSymbol (foreign-lambda int "WSGetSymbol" c-pointer (const (c-pointer symbol))))
  (define WSPutSymbol (foreign-lambda int "WSPutSymbol" c-pointer symbol))  

  (define WSGetUTF8String (foreign-lambda int "WSGetUTF8String" c-pointer (const (c-pointer unsigned-c-string)) (c-pointer int) (c-pointer int)))
  (define WSPutByteString (foreign-lambda int "WSPutByteString" c-pointer (const unsigned-c-string) int))

  (define WSPutFunction (foreign-lambda int "WSPutFunction" c-pointer symbol int))
  (define WSGetFunction (foreign-lambda int "WSGetFunction" c-pointer (const (c-pointer symbol)) (c-pointer int)))

  (define rule/MathML/display/block '(Rule "MathAttributes" (List (Rule "display" "block"))))

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

  (define (make-link env/pointer)
    (let-location ((i int))
                  (let* ((host (get-environment-variable "CHICKEN_WOLFRAM_HOST"))
                         (port (or (get-environment-variable "CHICKEN_WOLFRAM_PORT") "31415")); default port
                         (connection-string (string-append "csi -linkmode connect -linkname " 
                                                           port (if (string? host) (string-append "@" host) "") 
                                                           " -linkprotocol TCPIP -linkoptions 4"))
                         (p (WSOpenString env/pointer connection-string (location i))))
                    (unless (equal? WSEOK i) (error `(WSOpenString ,p)))
                    (set-finalizer! p WSClose)
                    (✓ (WSActivate p))
                    p)))

  (define-record wolfram-value env link eval expr)

  (set-record-printer! wolfram-value
                       (lambda (x out)
                         (let* ((W (wolfram-value-eval x))
                                (P ((default-outform-parameter) W)))
                           (display (P (wolfram-value-expr x)) out))))

  (define ((evaluate link) expr)    
    (✓ (WSPutFunction link 'EvaluatePacket 1)) 
    (let put ((e expr))
      (cond
        ((wolfram-value? e) (put (wolfram-value-expr e)))
        ((pair? e) (let1 (args (cdr e)) 
                         (✓ (WSPutFunction link (car e) (length args))) 
                         (map put args)))
        ((symbol? e) (✓ (WSPutSymbol link e)))
        ((boolean? e) (put (if (equal? e #t) 'True 'False)))
        ((string? e) (✓ (WSPutByteString link e (string-length e))))
        ((integer? e) (✓ (WSPutInteger64 link e)))
        ((procedure? e) (put (car (procedure-information e))))
        ((real? e) (✓ (WSPutReal64 link e)))
        (else (error `(put ,e)))))
    (✓ (WSEndPacket link))
    (✓ (WSFlush link))
    (let skip ()
      (unless (equal? (WSNextPacket link) RETURNPKT) 
        (✓ (WSNewPacket link))
        (skip)))
    (let get ((tokentype (WSGetNext link)))
      (cond
        ((equal? tokentype WSTKINT) (let-location ((i integer64)) 
                                                  (✓ (WSGetInteger64 link (location i)))
                                                  i))
        ((equal? tokentype WSTKREAL) (let-location ((i double)) 
                                                   (✓ (WSGetReal64 link (location i)))
                                                   i))
        ((equal? tokentype WSTKSYM) (let-location ((i symbol))
                                                  (✓ (WSGetSymbol link (location i)))
                                                  (cond
                                                    ((equal? i 'True) #t)
                                                    ((equal? i 'False) #f)
                                                    (else i))))
        ((equal? tokentype WSTKSTR) (let-location ((i unsigned-c-string) (n int))
                                                  (✓ (WSGetUTF8String link (location i) (location n) #f))
                                                  (assert (equal? n (string-length i)))
                                                  i))
        ((equal? tokentype WSTKFUNC) (let-location ((f symbol) (i int))
                                                   (✓ (WSGetFunction link (location f) (location i)))
                                                   (cons f (map (λ_ (get (WSGetNext link))) (iota i)))))
        (else (error `(WSGetNext ,tokentype))))))

  (define ((export-format W format) expr . args)
    (W `(ExportString ,expr ,(symbol->string format) ,@args)))  

  (define-syntax define-wolfram
    (syntax-rules ()
      ((define-wolfram (W ->wolfram) (E e) ...) (begin
                                                  (define env (make-env))
                                                  (define link (make-link env))
                                                  (define W (evaluate link))
                                                  (define (->wolfram v) (make-wolfram-value env link W v))
                                                  (define E (export-format W e)) ...))
      ((define-wolfram W (E e) ...) (define-wolfram (W _) (E e) ...))))

  (define (->string/form W form) (λ (expr) (W `(ToString ,expr ,form))))
  (define (->string/TeXForm W) (->string/form W 'TeXForm))
  (define (->string/OutputForm W) (->string/form W 'OutputForm))

  (define default-outform-parameter (make-parameter ->string/OutputForm))
  (define (display/OutputForm W) (o (λ_ (newline) (void)) display ((default-outform-parameter) W)))

  )































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
  (define WSOpenString (foreign-lambda c-pointer "WSOpenString" c-pointer (const c-string) (c-pointer int)))
  (define WSClose (foreign-lambda void "WSClose" c-pointer))
  (define WSActivate (foreign-lambda int "WSActivate" c-pointer))

  (define WSNextPacket (foreign-lambda int "WSNextPacket" c-pointer))
  (define WSNewPacket (foreign-lambda int "WSNewPacket" c-pointer))
  (define WSEndPacket (foreign-lambda int "WSEndPacket" c-pointer))
  (define WSFlush (foreign-lambda int "WSFlush" c-pointer))
  (define WSGetNext (foreign-lambda int "WSGetNext" c-pointer))

  (define WSGetInteger64 (foreign-lambda int "WSGetInteger64" c-pointer (c-pointer integer64)))
  (define WSPutInteger64 (foreign-lambda int "WSPutInteger64" c-pointer integer64))

  (define WSGetDouble (foreign-lambda int "WSGetDouble" c-pointer (c-pointer double)))
  (define WSPutDouble (foreign-lambda int "WSPutDouble" c-pointer double))

  (define WSGetUTF8Symbol (foreign-primitive ((c-pointer lp)) #<<TAG

    const unsigned char *symbol;
    int bytes;
    int characters;

    C_word res = C_SCHEME_UNDEFINED;

    if(WSGetUTF8Symbol(lp, &symbol, &bytes, &characters))
    {
        C_word *ptr = C_alloc(C_SIZEOF_INTERNED_SYMBOL(bytes));
        res = C_intern(&ptr, bytes, (char *)symbol);    
    }

    WSReleaseUTF8Symbol(lp, symbol, bytes);

    C_kontinue (C_k, res);

TAG
  ))

  (define WSPutSymbol (foreign-lambda int "WSPutSymbol" c-pointer symbol))

  (define WSGetUTF8String (foreign-primitive ((c-pointer lp)) #<<TAG

    const unsigned char *symbol;
    int bytes;
    int characters;

    C_word res = C_SCHEME_UNDEFINED;

    if(WSGetUTF8String(lp, &symbol, &bytes, &characters))
    {
        C_word *ptr = C_alloc(C_SIZEOF_STRING(bytes));
        res = C_string(&ptr, bytes, (char *)symbol);    
    }

    WSReleaseUTF8String(lp, symbol, bytes);

    C_kontinue (C_k, res);

TAG
  ))

  (define WSPutByteString (foreign-lambda int "WSPutByteString" c-pointer (const unsigned-c-string) int))

  (define WSPutFunction (foreign-lambda int "WSPutFunction" c-pointer symbol int))
  (define WSGetUTF8Function (foreign-primitive ((c-pointer lp) ((c-pointer int) n)) #<<TAG

    const unsigned char *symbol;
    int bytes;

    C_word res = C_SCHEME_UNDEFINED;

    if(WSGetUTF8Function(lp, &symbol, &bytes, n))
    {
        C_word *ptr = C_alloc(C_SIZEOF_INTERNED_SYMBOL(bytes));
        res = C_intern(&ptr, bytes, (char *)symbol);    
    }

    WSReleaseUTF8Symbol(lp, symbol, bytes);

    C_kontinue (C_k, res);

TAG
  ))

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
        ((real? e) (✓ (WSPutDouble link e)))
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
                                                   (✓ (WSGetDouble link (location i)))
                                                   i))
        ((equal? tokentype WSTKSYM) (let1 (i (WSGetUTF8Symbol link)) (✓ (if (void? i) 0 1))
                                      (cond
                                        ((equal? i 'True) #t)
                                        ((equal? i 'False) #f)
                                        (else i))))
        ((equal? tokentype WSTKSTR) (let1 (i (WSGetUTF8String link)) (✓ (if (void? i) 0 1))                                                  
                                      i))
        ((equal? tokentype WSTKFUNC) (let-location ((i int))
                                        (let1 (f (WSGetUTF8Function link (location i))) (✓ (if (void? f) 0 1))
                                          (cons f (map (λ_ (get (WSGetNext link))) (iota i))))))
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































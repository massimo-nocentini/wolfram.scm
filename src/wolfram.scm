
(module wolfram
  *
  (import scheme (chicken base) (chicken foreign) (chicken gc) aux)

  (foreign-declare "#include <wstp.h>")

  (define WSEOK (foreign-value "WSEOK" int))

  (define WSInitialize (foreign-lambda c-pointer "WSInitialize" c-pointer))
  (define WSDeinitialize (foreign-lambda void "WSDeinitialize" c-pointer))
  (define WSOpenString (foreign-lambda c-pointer "WSOpenString" c-pointer c-string (c-pointer int)))
  (define WSClose (foreign-lambda void "WSClose" c-pointer))
  (define WSActivate (foreign-lambda int "WSActivate" c-pointer))

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



  )

